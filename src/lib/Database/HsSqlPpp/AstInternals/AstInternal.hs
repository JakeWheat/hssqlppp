

-- UUAGC 0.9.19 (AstInternal.ag)
module Database.HsSqlPpp.AstInternals.AstInternal(
    -- {-# LANGUAGE DeriveDataTypeable,RankNTypes,ScopedTypeVariables #-}
    --from the ag files:
    --ast nodes
    Statement (..)
   ,QueryExpr (..)
   ,WithQueryList
   ,WithQuery(..)
   ,FnBody (..)
   --,SetClause (..)
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
   ,SQIdentifier(..)
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
   ,StringTypeNameListPair
   ,StringTypeNameListPairList
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
   -- typechecking
   ,typeCheckStatements
   ,typeCheckParameterizedStatement
   ,typeCheckScalarExpr
   ,typeCheckQueryExpr
   ,fixUpIdentifiers
   ,fixUpIdentifiersQE
   ,fixUpIdentifiersSE
) where

import Data.Maybe
import Data.List
import Control.Applicative
import Data.Data
import Data.Char
import Control.Monad.State
import Control.Arrow

import Data.Generics.PlateData
import Debug.Trace


import Database.HsSqlPpp.AstInternals.TypeType
import Database.HsSqlPpp.AstInternals.TypeChecking.TypeConversion
import Database.HsSqlPpp.AstInternals.AstAnnotation
import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
import Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindings
import Database.HsSqlPpp.Utils.Utils
import Database.HsSqlPpp.AstInternals.TypeChecking.ErrorUtils


{-# LINE 335 "AstInternal.ag" #-}



-- used for schema qualified identifiers
-- should be used in more places in the ast
{-# LINE 118 "AstInternal.hs" #-}

{-# LINE 400 "AstInternal.ag" #-}

data JoinType = Inner | LeftOuter| RightOuter | FullOuter | Cross
                deriving (Show,Eq,Typeable,Data)
{-# LINE 124 "AstInternal.hs" #-}

{-# LINE 412 "AstInternal.ag" #-}

data CopySource = CopyFilename String
                | Stdin
                  deriving (Show,Eq,Typeable,Data)
{-# LINE 131 "AstInternal.hs" #-}

{-# LINE 461 "AstInternal.ag" #-}

data SetValue
    = SetStr Annotation String
    | SetId Annotation String
    | SetNum Annotation Double
      deriving (Show,Eq,Typeable,Data)


data TriggerWhen = TriggerBefore | TriggerAfter
                   deriving (Show,Eq,Typeable,Data)
data TriggerEvent = TInsert| TUpdate | TDelete
                    deriving (Show,Eq,Typeable,Data)
data TriggerFire = EachRow | EachStatement
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 148 "AstInternal.hs" #-}

{-# LINE 490 "AstInternal.ag" #-}

data RaiseType = RNotice | RException | RError
                 deriving (Show,Eq,Typeable,Data)

data CombineType = Except | Union | Intersect | UnionAll
                   deriving (Show,Eq,Typeable,Data)

data Volatility = Volatile | Stable | Immutable
                  deriving (Show,Eq,Typeable,Data)

data Language = Sql | Plpgsql
                deriving (Show,Eq,Typeable,Data)
{-# LINE 163 "AstInternal.hs" #-}

{-# LINE 509 "AstInternal.ag" #-}

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

{-# LINE 194 "AstInternal.hs" #-}

{-# LINE 608 "AstInternal.ag" #-}

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

{-# LINE 239 "AstInternal.hs" #-}

{-# LINE 656 "AstInternal.ag" #-}

data FrameClause = FrameUnboundedPreceding
                 | FrameUnboundedFull
                 | FrameRowsUnboundedPreceding
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 247 "AstInternal.hs" #-}

{-# LINE 93 "./TypeChecking/TypeChecking.ag" #-}

-- | Takes an ast, checks against catalog passed, and adds
--   annotations, including types, type errors, and statement info.
--   Returns the updated catalog as well as the annotated ast.
typeCheckStatements :: Catalog -> StatementList -> (Catalog,StatementList)
typeCheckStatements cat sts =
    let t = sem_Root (Root $ fixUpIdentifiers cat sts)
        ta = wrap_Root t Inh_Root {cat_Inh_Root = cat
                                  ,lib_Inh_Root = emptyBindings
                                  ,idenv_Inh_Root = emptyIDEnv}
        tl = annotatedTree_Syn_Root ta
        cat1 = producedCat_Syn_Root ta
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
      tc = let t = sem_Root (Root $ fixUpIdentifiers cat [st])
               ta = wrap_Root t Inh_Root {cat_Inh_Root = cat
                                         ,lib_Inh_Root = emptyBindings
                                         ,idenv_Inh_Root = emptyIDEnv}
               tl = annotatedTree_Syn_Root ta
               --cat1 = producedCat_Syn_Root ta
           in case tl of
                Root [st1] -> Right st1
                _ -> error "impossible happened in typeCheckPS!"


-- | Testing utility, mainly used to check an expression for type errors
-- or to get its type.
typeCheckScalarExpr :: Catalog -> ScalarExpr -> ScalarExpr
typeCheckScalarExpr cat ex =
    let t = sem_ScalarExprRoot (ScalarExprRoot $ fixUpIdentifiersSE cat ex)
        rt = (annotatedTree_Syn_ScalarExprRoot
              (wrap_ScalarExprRoot t Inh_ScalarExprRoot {cat_Inh_ScalarExprRoot = cat
                                                        ,lib_Inh_ScalarExprRoot = emptyBindings
                                                        ,idenv_Inh_ScalarExprRoot = emptyIDEnv}))
    in case rt of
         ScalarExprRoot e -> e

{-# LINE 308 "AstInternal.hs" #-}

{-# LINE 66 "./TypeChecking/FixUpIdentifiers.ag" #-}


data IDEnv = IDEnv [(String, [String])]
             deriving Show
emptyIDEnv :: IDEnv
emptyIDEnv = IDEnv []

qualifyID :: IDEnv -> String -> Maybe (String,String)
qualifyID (IDEnv env) i =
  q env i
  where
    q [] _ = Nothing
    q ((t,cs):es) i' =
       if i' `elem` cs
       then Just (t,i')
       else q es i'

makeIDEnv :: String -- range qualifier
          -> [String] -- attribute names
          -> IDEnv
makeIDEnv t c = IDEnv [(t,c)]

makeIDEnvP :: [(String,[String])] -> IDEnv
makeIDEnvP x  = IDEnv x

unimplementedIDEnv :: IDEnv
unimplementedIDEnv = IDEnv []

joinIDEnvs :: IDEnv -> IDEnv -> IDEnv
joinIDEnvs (IDEnv a) (IDEnv b) = IDEnv $ a ++ b

expandStar :: IDEnv -> Maybe String --qualifier
           -> [(String,String)]
expandStar (IDEnv es) Nothing =
  flip concatMap es $ \(t,cs) -> map (t,) cs
expandStar (IDEnv es) (Just t) =
  maybe [(t,"*")] (map (t,)) $ lookup t es


{-# LINE 350 "AstInternal.hs" #-}

{-# LINE 166 "./TypeChecking/FixUpIdentifiers.ag" #-}


makeSelExps :: Annotation -> Annotation -> Annotation -> [(String,String)] -> [SelectItem]
makeSelExps sea a0 a1 is =
  flip map is $ \(q,c) -> addSIAlias $ SelExp sea $ QIdentifier a0 (Identifier a1 q) c

addSIAlias :: SelectItem -> SelectItem
addSIAlias s@(SelectItem _ _ _) = s
addSIAlias (SelExp ann ex) = SelectItem ann ex $ getColName ex
  where
    getColName (Identifier _ i) = i
    getColName (QIdentifier _ _ i) = i
    getColName (FunCall _ f _) | not (isOperatorName f) = f
    getColName (Cast _ _ (SimpleTypeName _ tn)) = tn
    getColName (WindowFn _ (FunCall _ f _) _ _ _ _) = f
    getColName _ = "?column?"

{-# LINE 370 "AstInternal.hs" #-}

{-# LINE 297 "./TypeChecking/FixUpIdentifiers.ag" #-}




{-# LINE 377 "AstInternal.hs" #-}

{-# LINE 316 "./TypeChecking/FixUpIdentifiers.ag" #-}

fixUpIdentifiers :: Catalog -> [Statement] -> [Statement]
fixUpIdentifiers cat sts =
    let t = sem_Root (Root sts)
        ta = wrap_Root t Inh_Root {cat_Inh_Root = cat
                                  ,lib_Inh_Root = emptyBindings
                                  ,idenv_Inh_Root = emptyIDEnv}
        tl = fixedUpIdentifiersTree_Syn_Root ta
    in case tl of
         Root r -> r

fixUpIdentifiersSE :: Catalog -> ScalarExpr -> ScalarExpr
fixUpIdentifiersSE cat sts =
    let t = sem_ScalarExprRoot (ScalarExprRoot sts)
        ta = wrap_ScalarExprRoot t Inh_ScalarExprRoot {cat_Inh_ScalarExprRoot = cat
                                  ,lib_Inh_ScalarExprRoot = emptyBindings
                                  ,idenv_Inh_ScalarExprRoot = emptyIDEnv}
        tl = fixedUpIdentifiersTree_Syn_ScalarExprRoot ta
    in case tl of
         ScalarExprRoot r -> r

fixUpIdentifiersQE :: Catalog -> QueryExpr -> QueryExpr
fixUpIdentifiersQE cat qe =
    let [QueryStatement _ qe'] = fixUpIdentifiers cat [QueryStatement emptyAnnotation qe]
    in qe'

{-# LINE 406 "AstInternal.hs" #-}

{-# LINE 383 "./TypeChecking/FixUpIdentifiers.ag" #-}

doAlias :: TableAlias -> [(String,[String])] -> ([(String,[String])],TableAlias)
doAlias (NoAlias a) [] = ([],NoAlias a)
doAlias (NoAlias a) cs@((t,_):ts) = if all (==t) $ map fst ts
                                then (cs,FullAlias a t $ concatMap snd cs)
                                else (cs,NoAlias a)
doAlias (TableAlias a t) cs = let cs' = concatMap snd cs
                              in ([(t, cs')], FullAlias a t cs')
doAlias f@(FullAlias _ t cs) _ = ([(t,cs)], f)
{-# LINE 418 "AstInternal.hs" #-}

{-# LINE 80 "./TypeChecking/Misc.ag" #-}


addTypeErrors :: Data a => [TypeError] -> a -> a
addTypeErrors es el = updateAnnotation u el
                      where
                        u a = a {errs = errs a ++ es}

setTypeAddErrors :: Data a => Et -> a -> a
setTypeAddErrors et el = updateAnnotation (setTypeAddErrorsA et) el

setTypeAddErrorsA :: Et -> Annotation -> Annotation
setTypeAddErrorsA et a =
    let a1 = a {errs = errs a ++ tes et}
    in case atype a1 of
         Just _ -> a1 {errs = errs a
                             ++ [InternalError $ "tried to set type a second time - " ++ show (etmt et)]}
         Nothing -> a1 {atype = etmt et}

allJust :: [Maybe a] -> Maybe [a]
allJust ts = sequence ts

-- bit dogdy, needs some thought
-- this is just to convert the new approach of using "." as an operator
-- to construct names, with the old approach which stuck the whole lot
-- in a string
getName :: ScalarExpr -> String
getName (Identifier _ i) = i
getName (FunCall _ "." [Identifier _ _,Identifier _ i]) = i
getName (FunCall _ "." [_,a]) = getName a
getName x = error $ "internal error getName called on: " ++ show x

getTName :: SQIdentifier -> String
getTName (SQIdentifier _ x@(_:_)) = last x
getTName x = error $ "internal error getName called on: " ++ show x


unwrapLookup :: (String,[String],Type) -> Type
unwrapLookup (_,_,t) = t

allAtts :: ([(String,Type)],[(String,Type)]) -> [(String,Type)]
allAtts (a,b) = a ++ b
{-# LINE 462 "AstInternal.hs" #-}

{-# LINE 162 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}


typeCheckValuesExpr :: Catalog -> [[Maybe Type]] -> Either [TypeError] Type
typeCheckValuesExpr cat rowsTs = do
        rts <- lmt $ allJust $ map allJust rowsTs
        let colNames = zipWith (++)
                           (repeat "column")
                           (map show [1..length $ head rowsTs])
        unionRelTypes cat rts colNames


typeCheckCombineSelect :: Catalog -> Type -> Type -> Either [TypeError] Type
typeCheckCombineSelect cat v1 v2 = do
    u1 <- unwrapSetOfComposite v1
    let colNames = map fst u1
    u2 <- unwrapSetOfComposite v2
    let colTypes1 = map snd u1
    let colTypes2 = map snd u2
    unionRelTypes cat [colTypes1,colTypes2] colNames

unionRelTypes :: Catalog -> [[Type]] -> [String] -> Either [TypeError] Type
unionRelTypes cat rowsTs colNames =
  let lengths = map length rowsTs
  in case () of
             _ | null rowsTs ->
                   Left [NoRowsGivenForValues]
               | not (all (==head lengths) lengths) ->
                   Left [ValuesListsMustBeSameLength]
               | otherwise ->
                   --i don't think this propagates all the errors, just the first set
                   mapM (resolveResultSetType cat) (transpose rowsTs) >>=
                     (return . SetOfType . CompositeType . zip colNames)

{-# LINE 498 "AstInternal.hs" #-}

{-# LINE 146 "./TypeChecking/QueryExprs/TableRefs.ag" #-}




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
funIdens :: Catalog -> String -> ScalarExpr -> Maybe Type -> Either [TypeError] (String,[(String,Type)])
funIdens cat alias fnVal ft = do
   errorWhen (case fnVal of
                FunCall _ _ _ -> False
                _ -> True)
             [ContextError "FunCall"]
   let (FunCall _ fnName _) = fnVal
       cn = if alias /= ""
                           then alias
                           else fnName
   attrs <- do
     fnt <- lmt ft
     case fnt of
       SetOfType (NamedCompositeType t) -> catCompositePublicAttrs cat [] t
       SetOfType x -> return [(cn,x)]
       y -> return [(cn,y)]
   return (cn, attrs)

getAlias :: String -> TableAlias -> String
getAlias def alias =
  case alias of
    NoAlias _ -> def
    TableAlias _ t -> t
    FullAlias _ t _ -> t

{-# LINE 546 "AstInternal.hs" #-}

{-# LINE 18 "./TypeChecking/QueryExprs/SelectLists.ag" #-}

{-data SiType = SiType (String,Maybe Type)
            | SiStarType [(String,Maybe Type)]-}
{-# LINE 552 "AstInternal.hs" #-}

{-# LINE 64 "./TypeChecking/QueryExprs/SelectLists.ag" #-}

--unwrapSetofs :: [(String,Type)] -> [(String,Type)]
--unwrapSetofs = map (\(n,t) -> (n, unwrapSetof t))

unwrapSetof :: Type -> Type
unwrapSetof (SetOfType u) = u
unwrapSetof v = v

{-# LINE 563 "AstInternal.hs" #-}

{-# LINE 51 "./TypeChecking/Ddl/CreateTable.ag" #-}

defaultSystemColumns :: [(String,Type)]
defaultSystemColumns = [("tableoid", ScalarType "oid")
                       ,("cmax", ScalarType "cid")
                       ,("xmax", ScalarType "xid")
                       ,("cmin", ScalarType "cid")
                       ,("xmin", ScalarType "xid")
                       ,("ctid", ScalarType "tid")]
{-# LINE 574 "AstInternal.hs" #-}

{-# LINE 31 "./TypeChecking/Ddl/CreateFunction.ag" #-}

data ParamName = NamedParam Int String
               | UnnamedParam Int
{-# LINE 580 "AstInternal.hs" #-}

{-# LINE 28 "./TypeChecking/ParameterizedStatements.ag" #-}

getPlaceholderTypes :: Data a => a -> [Maybe Type]
getPlaceholderTypes ex =
    [infType (getAnnotation x) | x <- universeBi ex
                               , isPlaceholder x]
    where
      isPlaceholder e = case e of
                          PositionalArg _ _ -> True
                          Placeholder _ -> True
                          _ -> False

{-# LINE 594 "AstInternal.hs" #-}
-- AlterTableAction --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative AddConstraint:
         child ann            : {Annotation}
         child con            : Constraint 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative AlterColumnDefault:
         child ann            : {Annotation}
         child nm             : {String}
         child def            : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data AlterTableAction  = AddConstraint (Annotation) (Constraint) 
                       | AlterColumnDefault (Annotation) (String) (ScalarExpr) 
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
                           IDEnv ->
                           LocalBindings ->
                           ( AlterTableAction,AlterTableAction,AlterTableAction)
data Inh_AlterTableAction  = Inh_AlterTableAction {cat_Inh_AlterTableAction :: Catalog,idenv_Inh_AlterTableAction :: IDEnv,lib_Inh_AlterTableAction :: LocalBindings}
data Syn_AlterTableAction  = Syn_AlterTableAction {annotatedTree_Syn_AlterTableAction :: AlterTableAction,fixedUpIdentifiersTree_Syn_AlterTableAction :: AlterTableAction,originalTree_Syn_AlterTableAction :: AlterTableAction}
wrap_AlterTableAction :: T_AlterTableAction  ->
                         Inh_AlterTableAction  ->
                         Syn_AlterTableAction 
wrap_AlterTableAction sem (Inh_AlterTableAction _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_AlterTableAction _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_AlterTableAction_AddConstraint :: Annotation ->
                                      T_Constraint  ->
                                      T_AlterTableAction 
sem_AlterTableAction_AddConstraint ann_ con_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: AlterTableAction
              _lhsOfixedUpIdentifiersTree :: AlterTableAction
              _lhsOoriginalTree :: AlterTableAction
              _conOcat :: Catalog
              _conOidenv :: IDEnv
              _conOlib :: LocalBindings
              _conIannotatedTree :: Constraint
              _conIfixedUpIdentifiersTree :: Constraint
              _conIoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  AddConstraint ann_ _conIannotatedTree
                  {-# LINE 667 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  AddConstraint ann_ _conIfixedUpIdentifiersTree
                  {-# LINE 672 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  AddConstraint ann_ _conIoriginalTree
                  {-# LINE 677 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 682 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 687 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 692 "AstInternal.hs" #-}
              -- copy rule (down)
              _conOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 697 "AstInternal.hs" #-}
              -- copy rule (down)
              _conOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 702 "AstInternal.hs" #-}
              -- copy rule (down)
              _conOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 707 "AstInternal.hs" #-}
              ( _conIannotatedTree,_conIfixedUpIdentifiersTree,_conIoriginalTree) =
                  (con_ _conOcat _conOidenv _conOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_AlterTableAction_AlterColumnDefault :: Annotation ->
                                           String ->
                                           T_ScalarExpr  ->
                                           T_AlterTableAction 
sem_AlterTableAction_AlterColumnDefault ann_ nm_ def_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _defOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: AlterTableAction
              _lhsOfixedUpIdentifiersTree :: AlterTableAction
              _lhsOoriginalTree :: AlterTableAction
              _defOcat :: Catalog
              _defOidenv :: IDEnv
              _defOlib :: LocalBindings
              _defIannotatedTree :: ScalarExpr
              _defIfixedUpIdentifiersTree :: ScalarExpr
              _defIoriginalTree :: ScalarExpr
              _defIuType :: (Maybe Type)
              -- "./TypeChecking/ParameterizedStatements.ag"(line 72, column 26)
              _defOexpectedType =
                  {-# LINE 72 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 734 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  AlterColumnDefault ann_ nm_ _defIannotatedTree
                  {-# LINE 739 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  AlterColumnDefault ann_ nm_ _defIfixedUpIdentifiersTree
                  {-# LINE 744 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  AlterColumnDefault ann_ nm_ _defIoriginalTree
                  {-# LINE 749 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 754 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 759 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 764 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 769 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 774 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 779 "AstInternal.hs" #-}
              ( _defIannotatedTree,_defIfixedUpIdentifiersTree,_defIoriginalTree,_defIuType) =
                  (def_ _defOcat _defOexpectedType _defOidenv _defOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- AlterTableActionList ----------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : AlterTableAction 
         child tl             : AlterTableActionList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type AlterTableActionList  = [(AlterTableAction)]
-- cata
sem_AlterTableActionList :: AlterTableActionList  ->
                            T_AlterTableActionList 
sem_AlterTableActionList list  =
    (Prelude.foldr sem_AlterTableActionList_Cons sem_AlterTableActionList_Nil (Prelude.map sem_AlterTableAction list) )
-- semantic domain
type T_AlterTableActionList  = Catalog ->
                               IDEnv ->
                               LocalBindings ->
                               ( AlterTableActionList,AlterTableActionList,AlterTableActionList)
data Inh_AlterTableActionList  = Inh_AlterTableActionList {cat_Inh_AlterTableActionList :: Catalog,idenv_Inh_AlterTableActionList :: IDEnv,lib_Inh_AlterTableActionList :: LocalBindings}
data Syn_AlterTableActionList  = Syn_AlterTableActionList {annotatedTree_Syn_AlterTableActionList :: AlterTableActionList,fixedUpIdentifiersTree_Syn_AlterTableActionList :: AlterTableActionList,originalTree_Syn_AlterTableActionList :: AlterTableActionList}
wrap_AlterTableActionList :: T_AlterTableActionList  ->
                             Inh_AlterTableActionList  ->
                             Syn_AlterTableActionList 
wrap_AlterTableActionList sem (Inh_AlterTableActionList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_AlterTableActionList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_AlterTableActionList_Cons :: T_AlterTableAction  ->
                                 T_AlterTableActionList  ->
                                 T_AlterTableActionList 
sem_AlterTableActionList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: AlterTableActionList
              _lhsOfixedUpIdentifiersTree :: AlterTableActionList
              _lhsOoriginalTree :: AlterTableActionList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: AlterTableAction
              _hdIfixedUpIdentifiersTree :: AlterTableAction
              _hdIoriginalTree :: AlterTableAction
              _tlIannotatedTree :: AlterTableActionList
              _tlIfixedUpIdentifiersTree :: AlterTableActionList
              _tlIoriginalTree :: AlterTableActionList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 854 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 859 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 864 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 869 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 874 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 879 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 884 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 889 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 894 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 899 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 904 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 909 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_AlterTableActionList_Nil :: T_AlterTableActionList 
sem_AlterTableActionList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: AlterTableActionList
              _lhsOfixedUpIdentifiersTree :: AlterTableActionList
              _lhsOoriginalTree :: AlterTableActionList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 927 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 932 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 937 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 942 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 947 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 952 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- AttributeDef ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         attrName             : String
         fixedUpIdentifiersTree : SELF 
         namedType            : Maybe Type
         originalTree         : SELF 
   alternatives:
      alternative AttributeDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         child def            : MaybeScalarExpr 
         child cons           : RowConstraintList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data AttributeDef  = AttributeDef (Annotation) (String) (TypeName) (MaybeScalarExpr) (RowConstraintList) 
                   deriving ( Data,Eq,Show,Typeable)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _ann _name _typ _def _cons )  =
    (sem_AttributeDef_AttributeDef _ann _name (sem_TypeName _typ ) (sem_MaybeScalarExpr _def ) (sem_RowConstraintList _cons ) )
-- semantic domain
type T_AttributeDef  = Catalog ->
                       IDEnv ->
                       LocalBindings ->
                       ( AttributeDef,String,AttributeDef,(Maybe Type),AttributeDef)
data Inh_AttributeDef  = Inh_AttributeDef {cat_Inh_AttributeDef :: Catalog,idenv_Inh_AttributeDef :: IDEnv,lib_Inh_AttributeDef :: LocalBindings}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef,attrName_Syn_AttributeDef :: String,fixedUpIdentifiersTree_Syn_AttributeDef :: AttributeDef,namedType_Syn_AttributeDef :: Maybe Type,originalTree_Syn_AttributeDef :: AttributeDef}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_AttributeDef _lhsOannotatedTree _lhsOattrName _lhsOfixedUpIdentifiersTree _lhsOnamedType _lhsOoriginalTree ))
sem_AttributeDef_AttributeDef :: Annotation ->
                                 String ->
                                 T_TypeName  ->
                                 T_MaybeScalarExpr  ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef ann_ name_ typ_ def_ cons_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOattrName :: String
              _lhsOnamedType :: (Maybe Type)
              _consOlib :: LocalBindings
              _lhsOannotatedTree :: AttributeDef
              _lhsOfixedUpIdentifiersTree :: AttributeDef
              _lhsOoriginalTree :: AttributeDef
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _defOcat :: Catalog
              _defOidenv :: IDEnv
              _defOlib :: LocalBindings
              _consOcat :: Catalog
              _consOidenv :: IDEnv
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              _defIannotatedTree :: MaybeScalarExpr
              _defIfixedUpIdentifiersTree :: MaybeScalarExpr
              _defIoriginalTree :: MaybeScalarExpr
              _defIuType :: (Maybe Type)
              _consIannotatedTree :: RowConstraintList
              _consIfixedUpIdentifiersTree :: RowConstraintList
              _consIoriginalTree :: RowConstraintList
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 83, column 9)
              _lhsOattrName =
                  {-# LINE 83 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  map toLower name_
                  {-# LINE 1039 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 84, column 9)
              _lhsOnamedType =
                  {-# LINE 84 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  _typInamedType
                  {-# LINE 1044 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 98, column 9)
              _consOlib =
                  {-# LINE 98 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  either (const _lhsIlib) id $ do
                  t <- lmt _typInamedType
                  lbUpdate _lhsIcat
                           (LBIds "attribute def" Nothing
                                  [(name_, t)]) _lhsIlib
                  {-# LINE 1053 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
                  {-# LINE 1058 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  AttributeDef ann_ name_ _typIfixedUpIdentifiersTree _defIfixedUpIdentifiersTree _consIfixedUpIdentifiersTree
                  {-# LINE 1063 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIoriginalTree _defIoriginalTree _consIoriginalTree
                  {-# LINE 1068 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1073 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1078 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1083 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1088 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1093 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1098 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1103 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1108 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1113 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1118 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1123 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
              ( _defIannotatedTree,_defIfixedUpIdentifiersTree,_defIoriginalTree,_defIuType) =
                  (def_ _defOcat _defOidenv _defOlib )
              ( _consIannotatedTree,_consIfixedUpIdentifiersTree,_consIoriginalTree) =
                  (cons_ _consOcat _consOidenv _consOlib )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree)))
-- AttributeDefList --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         attrs                : [(String, Maybe Type)]
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : AttributeDef 
         child tl             : AttributeDefList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type AttributeDefList  = [(AttributeDef)]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = Catalog ->
                           IDEnv ->
                           LocalBindings ->
                           ( AttributeDefList,([(String, Maybe Type)]),AttributeDefList,AttributeDefList)
data Inh_AttributeDefList  = Inh_AttributeDefList {cat_Inh_AttributeDefList :: Catalog,idenv_Inh_AttributeDefList :: IDEnv,lib_Inh_AttributeDefList :: LocalBindings}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList,attrs_Syn_AttributeDefList :: [(String, Maybe Type)],fixedUpIdentifiersTree_Syn_AttributeDefList :: AttributeDefList,originalTree_Syn_AttributeDefList :: AttributeDefList}
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_AttributeDefList _lhsOannotatedTree _lhsOattrs _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOattrs :: ([(String, Maybe Type)])
              _lhsOannotatedTree :: AttributeDefList
              _lhsOfixedUpIdentifiersTree :: AttributeDefList
              _lhsOoriginalTree :: AttributeDefList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: AttributeDef
              _hdIattrName :: String
              _hdIfixedUpIdentifiersTree :: AttributeDef
              _hdInamedType :: (Maybe Type)
              _hdIoriginalTree :: AttributeDef
              _tlIannotatedTree :: AttributeDefList
              _tlIattrs :: ([(String, Maybe Type)])
              _tlIfixedUpIdentifiersTree :: AttributeDefList
              _tlIoriginalTree :: AttributeDefList
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 88, column 12)
              _lhsOattrs =
                  {-# LINE 88 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 1207 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1212 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 1217 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 1222 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1227 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1232 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1237 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1242 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1247 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1252 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1257 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1262 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1267 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIattrName,_hdIfixedUpIdentifiersTree,_hdInamedType,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIattrs,_tlIfixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOattrs :: ([(String, Maybe Type)])
              _lhsOannotatedTree :: AttributeDefList
              _lhsOfixedUpIdentifiersTree :: AttributeDefList
              _lhsOoriginalTree :: AttributeDefList
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 89, column 11)
              _lhsOattrs =
                  {-# LINE 89 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  []
                  {-# LINE 1286 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1291 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 1296 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1301 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1306 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1311 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1316 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- CaseScalarExprListScalarExprPair ----------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         thenType             : Maybe Type
         whenTypes            : [Maybe Type]
   alternatives:
      alternative Tuple:
         child x1             : ScalarExprList 
         child x2             : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type CaseScalarExprListScalarExprPair  = ( (ScalarExprList),(ScalarExpr))
-- cata
sem_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair  ->
                                        T_CaseScalarExprListScalarExprPair 
sem_CaseScalarExprListScalarExprPair ( x1,x2)  =
    (sem_CaseScalarExprListScalarExprPair_Tuple (sem_ScalarExprList x1 ) (sem_ScalarExpr x2 ) )
-- semantic domain
type T_CaseScalarExprListScalarExprPair  = Catalog ->
                                           IDEnv ->
                                           LocalBindings ->
                                           ( CaseScalarExprListScalarExprPair,CaseScalarExprListScalarExprPair,CaseScalarExprListScalarExprPair,(Maybe Type),([Maybe Type]))
data Inh_CaseScalarExprListScalarExprPair  = Inh_CaseScalarExprListScalarExprPair {cat_Inh_CaseScalarExprListScalarExprPair :: Catalog,idenv_Inh_CaseScalarExprListScalarExprPair :: IDEnv,lib_Inh_CaseScalarExprListScalarExprPair :: LocalBindings}
data Syn_CaseScalarExprListScalarExprPair  = Syn_CaseScalarExprListScalarExprPair {annotatedTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair,fixedUpIdentifiersTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair,originalTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair,thenType_Syn_CaseScalarExprListScalarExprPair :: Maybe Type,whenTypes_Syn_CaseScalarExprListScalarExprPair :: [Maybe Type]}
wrap_CaseScalarExprListScalarExprPair :: T_CaseScalarExprListScalarExprPair  ->
                                         Inh_CaseScalarExprListScalarExprPair  ->
                                         Syn_CaseScalarExprListScalarExprPair 
wrap_CaseScalarExprListScalarExprPair sem (Inh_CaseScalarExprListScalarExprPair _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOthenType,_lhsOwhenTypes) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_CaseScalarExprListScalarExprPair _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOthenType _lhsOwhenTypes ))
sem_CaseScalarExprListScalarExprPair_Tuple :: T_ScalarExprList  ->
                                              T_ScalarExpr  ->
                                              T_CaseScalarExprListScalarExprPair 
sem_CaseScalarExprListScalarExprPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOwhenTypes :: ([Maybe Type])
              _lhsOthenType :: (Maybe Type)
              _x1OexpectedTypes :: ([Maybe Type])
              _x2OexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: CaseScalarExprListScalarExprPair
              _lhsOfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPair
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPair
              _x1Ocat :: Catalog
              _x1Oidenv :: IDEnv
              _x1Olib :: LocalBindings
              _x2Ocat :: Catalog
              _x2Oidenv :: IDEnv
              _x2Olib :: LocalBindings
              _x1IannotatedTree :: ScalarExprList
              _x1IfixedUpIdentifiersTree :: ScalarExprList
              _x1IoriginalTree :: ScalarExprList
              _x1IuType :: ([Maybe Type])
              _x2IannotatedTree :: ScalarExpr
              _x2IfixedUpIdentifiersTree :: ScalarExpr
              _x2IoriginalTree :: ScalarExpr
              _x2IuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 204, column 13)
              _lhsOwhenTypes =
                  {-# LINE 204 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _x1IuType
                  {-# LINE 1392 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 205, column 13)
              _lhsOthenType =
                  {-# LINE 205 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _x2IuType
                  {-# LINE 1397 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 76, column 13)
              _x1OexpectedTypes =
                  {-# LINE 76 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 1402 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 77, column 13)
              _x2OexpectedType =
                  {-# LINE 77 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 1407 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 1412 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (_x1IfixedUpIdentifiersTree,_x2IfixedUpIdentifiersTree)
                  {-# LINE 1417 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 1422 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1427 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1432 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1437 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1442 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1447 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1452 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1457 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1462 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1467 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1IfixedUpIdentifiersTree,_x1IoriginalTree,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedTypes _x1Oidenv _x1Olib )
              ( _x2IannotatedTree,_x2IfixedUpIdentifiersTree,_x2IoriginalTree,_x2IuType) =
                  (x2_ _x2Ocat _x2OexpectedType _x2Oidenv _x2Olib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOthenType,_lhsOwhenTypes)))
-- CaseScalarExprListScalarExprPairList ------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         thenTypes            : [Maybe Type]
         whenTypes            : [[Maybe Type]]
   alternatives:
      alternative Cons:
         child hd             : CaseScalarExprListScalarExprPair 
         child tl             : CaseScalarExprListScalarExprPairList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type CaseScalarExprListScalarExprPairList  = [(CaseScalarExprListScalarExprPair)]
-- cata
sem_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList  ->
                                            T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList list  =
    (Prelude.foldr sem_CaseScalarExprListScalarExprPairList_Cons sem_CaseScalarExprListScalarExprPairList_Nil (Prelude.map sem_CaseScalarExprListScalarExprPair list) )
-- semantic domain
type T_CaseScalarExprListScalarExprPairList  = Catalog ->
                                               IDEnv ->
                                               LocalBindings ->
                                               ( CaseScalarExprListScalarExprPairList,CaseScalarExprListScalarExprPairList,CaseScalarExprListScalarExprPairList,([Maybe Type]),([[Maybe Type]]))
data Inh_CaseScalarExprListScalarExprPairList  = Inh_CaseScalarExprListScalarExprPairList {cat_Inh_CaseScalarExprListScalarExprPairList :: Catalog,idenv_Inh_CaseScalarExprListScalarExprPairList :: IDEnv,lib_Inh_CaseScalarExprListScalarExprPairList :: LocalBindings}
data Syn_CaseScalarExprListScalarExprPairList  = Syn_CaseScalarExprListScalarExprPairList {annotatedTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList,fixedUpIdentifiersTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList,originalTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList,thenTypes_Syn_CaseScalarExprListScalarExprPairList :: [Maybe Type],whenTypes_Syn_CaseScalarExprListScalarExprPairList :: [[Maybe Type]]}
wrap_CaseScalarExprListScalarExprPairList :: T_CaseScalarExprListScalarExprPairList  ->
                                             Inh_CaseScalarExprListScalarExprPairList  ->
                                             Syn_CaseScalarExprListScalarExprPairList 
wrap_CaseScalarExprListScalarExprPairList sem (Inh_CaseScalarExprListScalarExprPairList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOwhenTypes) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_CaseScalarExprListScalarExprPairList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOthenTypes _lhsOwhenTypes ))
sem_CaseScalarExprListScalarExprPairList_Cons :: T_CaseScalarExprListScalarExprPair  ->
                                                 T_CaseScalarExprListScalarExprPairList  ->
                                                 T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOwhenTypes :: ([[Maybe Type]])
              _lhsOthenTypes :: ([Maybe Type])
              _lhsOannotatedTree :: CaseScalarExprListScalarExprPairList
              _lhsOfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPairList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: CaseScalarExprListScalarExprPair
              _hdIfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPair
              _hdIoriginalTree :: CaseScalarExprListScalarExprPair
              _hdIthenType :: (Maybe Type)
              _hdIwhenTypes :: ([Maybe Type])
              _tlIannotatedTree :: CaseScalarExprListScalarExprPairList
              _tlIfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _tlIoriginalTree :: CaseScalarExprListScalarExprPairList
              _tlIthenTypes :: ([Maybe Type])
              _tlIwhenTypes :: ([[Maybe Type]])
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 195, column 10)
              _lhsOwhenTypes =
                  {-# LINE 195 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _hdIwhenTypes : _tlIwhenTypes
                  {-# LINE 1552 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 196, column 10)
              _lhsOthenTypes =
                  {-# LINE 196 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _hdIthenType : _tlIthenTypes
                  {-# LINE 1557 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1562 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 1567 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 1572 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1577 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1582 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1587 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1592 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1597 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1602 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1607 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1612 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1617 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIoriginalTree,_hdIthenType,_hdIwhenTypes) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree,_tlIthenTypes,_tlIwhenTypes) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOwhenTypes)))
sem_CaseScalarExprListScalarExprPairList_Nil :: T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOwhenTypes :: ([[Maybe Type]])
              _lhsOthenTypes :: ([Maybe Type])
              _lhsOannotatedTree :: CaseScalarExprListScalarExprPairList
              _lhsOfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPairList
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 197, column 9)
              _lhsOwhenTypes =
                  {-# LINE 197 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  []
                  {-# LINE 1637 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 198, column 9)
              _lhsOthenTypes =
                  {-# LINE 198 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  []
                  {-# LINE 1642 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1647 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 1652 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1657 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1662 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1667 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1672 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOwhenTypes)))
-- Constraint --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative CheckConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative PrimaryKeyConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child x              : {[String]}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative ReferenceConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child atts           : {[String]}
         child table          : {String}
         child tableAtts      : {[String]}
         child onUpdate       : {Cascade}
         child onDelete       : {Cascade}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative UniqueConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child x              : {[String]}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data Constraint  = CheckConstraint (Annotation) (String) (ScalarExpr) 
                 | PrimaryKeyConstraint (Annotation) (String) ([String]) 
                 | ReferenceConstraint (Annotation) (String) ([String]) (String) ([String]) (Cascade) (Cascade) 
                 | UniqueConstraint (Annotation) (String) ([String]) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Constraint :: Constraint  ->
                  T_Constraint 
sem_Constraint (CheckConstraint _ann _name _expr )  =
    (sem_Constraint_CheckConstraint _ann _name (sem_ScalarExpr _expr ) )
sem_Constraint (PrimaryKeyConstraint _ann _name _x )  =
    (sem_Constraint_PrimaryKeyConstraint _ann _name _x )
sem_Constraint (ReferenceConstraint _ann _name _atts _table _tableAtts _onUpdate _onDelete )  =
    (sem_Constraint_ReferenceConstraint _ann _name _atts _table _tableAtts _onUpdate _onDelete )
sem_Constraint (UniqueConstraint _ann _name _x )  =
    (sem_Constraint_UniqueConstraint _ann _name _x )
-- semantic domain
type T_Constraint  = Catalog ->
                     IDEnv ->
                     LocalBindings ->
                     ( Constraint,Constraint,Constraint)
data Inh_Constraint  = Inh_Constraint {cat_Inh_Constraint :: Catalog,idenv_Inh_Constraint :: IDEnv,lib_Inh_Constraint :: LocalBindings}
data Syn_Constraint  = Syn_Constraint {annotatedTree_Syn_Constraint :: Constraint,fixedUpIdentifiersTree_Syn_Constraint :: Constraint,originalTree_Syn_Constraint :: Constraint}
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_Constraint _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_Constraint_CheckConstraint :: Annotation ->
                                  String ->
                                  T_ScalarExpr  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Constraint
              _lhsOfixedUpIdentifiersTree :: Constraint
              _lhsOoriginalTree :: Constraint
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ParameterizedStatements.ag"(line 80, column 23)
              _exprOexpectedType =
                  {-# LINE 80 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 1776 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CheckConstraint ann_ name_ _exprIannotatedTree
                  {-# LINE 1781 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CheckConstraint ann_ name_ _exprIfixedUpIdentifiersTree
                  {-# LINE 1786 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CheckConstraint ann_ name_ _exprIoriginalTree
                  {-# LINE 1791 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1796 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1801 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1806 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1811 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1816 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1821 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_Constraint_PrimaryKeyConstraint :: Annotation ->
                                       String ->
                                       ([String]) ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint ann_ name_ x_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOfixedUpIdentifiersTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  PrimaryKeyConstraint ann_ name_ x_
                  {-# LINE 1840 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  PrimaryKeyConstraint ann_ name_ x_
                  {-# LINE 1845 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  PrimaryKeyConstraint ann_ name_ x_
                  {-# LINE 1850 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1855 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1860 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1865 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_Constraint_ReferenceConstraint :: Annotation ->
                                      String ->
                                      ([String]) ->
                                      String ->
                                      ([String]) ->
                                      Cascade ->
                                      Cascade ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOfixedUpIdentifiersTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
                  {-# LINE 1886 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
                  {-# LINE 1891 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
                  {-# LINE 1896 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1901 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1906 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1911 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_Constraint_UniqueConstraint :: Annotation ->
                                   String ->
                                   ([String]) ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint ann_ name_ x_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOfixedUpIdentifiersTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  UniqueConstraint ann_ name_ x_
                  {-# LINE 1928 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  UniqueConstraint ann_ name_ x_
                  {-# LINE 1933 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  UniqueConstraint ann_ name_ x_
                  {-# LINE 1938 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1943 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1948 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1953 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- ConstraintList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : Constraint 
         child tl             : ConstraintList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type ConstraintList  = [(Constraint)]
-- cata
sem_ConstraintList :: ConstraintList  ->
                      T_ConstraintList 
sem_ConstraintList list  =
    (Prelude.foldr sem_ConstraintList_Cons sem_ConstraintList_Nil (Prelude.map sem_Constraint list) )
-- semantic domain
type T_ConstraintList  = Catalog ->
                         IDEnv ->
                         LocalBindings ->
                         ( ConstraintList,ConstraintList,ConstraintList)
data Inh_ConstraintList  = Inh_ConstraintList {cat_Inh_ConstraintList :: Catalog,idenv_Inh_ConstraintList :: IDEnv,lib_Inh_ConstraintList :: LocalBindings}
data Syn_ConstraintList  = Syn_ConstraintList {annotatedTree_Syn_ConstraintList :: ConstraintList,fixedUpIdentifiersTree_Syn_ConstraintList :: ConstraintList,originalTree_Syn_ConstraintList :: ConstraintList}
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ConstraintList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ConstraintList
              _lhsOfixedUpIdentifiersTree :: ConstraintList
              _lhsOoriginalTree :: ConstraintList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: Constraint
              _hdIfixedUpIdentifiersTree :: Constraint
              _hdIoriginalTree :: Constraint
              _tlIannotatedTree :: ConstraintList
              _tlIfixedUpIdentifiersTree :: ConstraintList
              _tlIoriginalTree :: ConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2026 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 2031 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 2036 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2041 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2046 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2051 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2056 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2061 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2066 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2071 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2076 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2081 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ConstraintList
              _lhsOfixedUpIdentifiersTree :: ConstraintList
              _lhsOoriginalTree :: ConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 2099 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 2104 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 2109 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2114 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2119 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2124 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- FnBody ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative PlpgsqlFnBody:
         child ann            : {Annotation}
         child blk            : Statement 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative SqlFnBody:
         child ann            : {Annotation}
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data FnBody  = PlpgsqlFnBody (Annotation) (Statement) 
             | SqlFnBody (Annotation) (StatementList) 
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
                 IDEnv ->
                 LocalBindings ->
                 ( FnBody,FnBody,FnBody)
data Inh_FnBody  = Inh_FnBody {cat_Inh_FnBody :: Catalog,idenv_Inh_FnBody :: IDEnv,lib_Inh_FnBody :: LocalBindings}
data Syn_FnBody  = Syn_FnBody {annotatedTree_Syn_FnBody :: FnBody,fixedUpIdentifiersTree_Syn_FnBody :: FnBody,originalTree_Syn_FnBody :: FnBody}
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_FnBody _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_FnBody_PlpgsqlFnBody :: Annotation ->
                            T_Statement  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody ann_ blk_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _blkOinProducedCat :: Catalog
              _lhsOannotatedTree :: FnBody
              _lhsOfixedUpIdentifiersTree :: FnBody
              _lhsOoriginalTree :: FnBody
              _blkOcat :: Catalog
              _blkOidenv :: IDEnv
              _blkOlib :: LocalBindings
              _blkIannotatedTree :: Statement
              _blkIcatUpdates :: ([CatalogUpdate])
              _blkIfixedUpIdentifiersTree :: Statement
              _blkIlibUpdates :: ([LocalBindingsUpdate])
              _blkIoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 104, column 9)
              _blkOinProducedCat =
                  {-# LINE 104 "./TypeChecking/Statements.ag" #-}
                  emptyCatalog
                  {-# LINE 2200 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _blkIannotatedTree
                  {-# LINE 2205 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  PlpgsqlFnBody ann_ _blkIfixedUpIdentifiersTree
                  {-# LINE 2210 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _blkIoriginalTree
                  {-# LINE 2215 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2220 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2225 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2230 "AstInternal.hs" #-}
              -- copy rule (down)
              _blkOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2235 "AstInternal.hs" #-}
              -- copy rule (down)
              _blkOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2240 "AstInternal.hs" #-}
              -- copy rule (down)
              _blkOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2245 "AstInternal.hs" #-}
              ( _blkIannotatedTree,_blkIcatUpdates,_blkIfixedUpIdentifiersTree,_blkIlibUpdates,_blkIoriginalTree) =
                  (blk_ _blkOcat _blkOidenv _blkOinProducedCat _blkOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_FnBody_SqlFnBody :: Annotation ->
                        T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody ann_ sts_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: FnBody
              _lhsOfixedUpIdentifiersTree :: FnBody
              _lhsOoriginalTree :: FnBody
              _stsOcat :: Catalog
              _stsOidenv :: IDEnv
              _stsOlib :: LocalBindings
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 129, column 9)
              _stsOcatUpdates =
                  {-# LINE 129 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 2273 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 130, column 9)
              _stsOlibUpdates =
                  {-# LINE 130 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 2278 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIannotatedTree
                  {-# LINE 2283 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SqlFnBody ann_ _stsIfixedUpIdentifiersTree
                  {-# LINE 2288 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIoriginalTree
                  {-# LINE 2293 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2298 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2303 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2308 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2313 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2318 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2323 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- InList ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         listType             : Either [TypeError] Type
         originalTree         : SELF 
   alternatives:
      alternative InList:
         child ann            : {Annotation}
         child exprs          : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative InSelect:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data InList  = InList (Annotation) (ScalarExprList) 
             | InSelect (Annotation) (QueryExpr) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_InList :: InList  ->
              T_InList 
sem_InList (InList _ann _exprs )  =
    (sem_InList_InList _ann (sem_ScalarExprList _exprs ) )
sem_InList (InSelect _ann _sel )  =
    (sem_InList_InSelect _ann (sem_QueryExpr _sel ) )
-- semantic domain
type T_InList  = Catalog ->
                 IDEnv ->
                 LocalBindings ->
                 ( InList,InList,(Either [TypeError] Type),InList)
data Inh_InList  = Inh_InList {cat_Inh_InList :: Catalog,idenv_Inh_InList :: IDEnv,lib_Inh_InList :: LocalBindings}
data Syn_InList  = Syn_InList {annotatedTree_Syn_InList :: InList,fixedUpIdentifiersTree_Syn_InList :: InList,listType_Syn_InList :: Either [TypeError] Type,originalTree_Syn_InList :: InList}
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_InList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOlistType _lhsOoriginalTree ))
sem_InList_InList :: Annotation ->
                     T_ScalarExprList  ->
                     T_InList 
sem_InList_InList ann_ exprs_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlistType :: (Either [TypeError] Type)
              _exprsOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: InList
              _lhsOfixedUpIdentifiersTree :: InList
              _lhsOoriginalTree :: InList
              _exprsOcat :: Catalog
              _exprsOidenv :: IDEnv
              _exprsOlib :: LocalBindings
              _exprsIannotatedTree :: ScalarExprList
              _exprsIfixedUpIdentifiersTree :: ScalarExprList
              _exprsIoriginalTree :: ScalarExprList
              _exprsIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 332, column 9)
              _lhsOlistType =
                  {-# LINE 332 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  mapM lmt _exprsIuType >>= resolveResultSetType _lhsIcat
                  {-# LINE 2402 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 149, column 14)
              _exprsOexpectedTypes =
                  {-# LINE 149 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 2407 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  InList ann_ _exprsIannotatedTree
                  {-# LINE 2412 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  InList ann_ _exprsIfixedUpIdentifiersTree
                  {-# LINE 2417 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  InList ann_ _exprsIoriginalTree
                  {-# LINE 2422 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2427 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2432 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2437 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2442 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2447 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2452 "AstInternal.hs" #-}
              ( _exprsIannotatedTree,_exprsIfixedUpIdentifiersTree,_exprsIoriginalTree,_exprsIuType) =
                  (exprs_ _exprsOcat _exprsOexpectedTypes _exprsOidenv _exprsOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOoriginalTree)))
sem_InList_InSelect :: Annotation ->
                       T_QueryExpr  ->
                       T_InList 
sem_InList_InSelect ann_ sel_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlistType :: (Either [TypeError] Type)
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: InList
              _lhsOfixedUpIdentifiersTree :: InList
              _lhsOoriginalTree :: InList
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 334, column 9)
              _lhsOlistType =
                  {-# LINE 334 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  do
                  st <- lmt (map snd <$> _selIuType)
                  case length st of
                            0 -> Left [InternalError
                                       "got subquery with no columns? in inselect"]
                            1 -> Right $ head st
                            _ -> Right $ AnonymousRecordType st
                  {-# LINE 2487 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 175, column 16)
              _selOexpectedTypes =
                  {-# LINE 175 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 2492 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  InSelect ann_ _selIannotatedTree
                  {-# LINE 2497 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  InSelect ann_ _selIfixedUpIdentifiersTree
                  {-# LINE 2502 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  InSelect ann_ _selIoriginalTree
                  {-# LINE 2507 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2512 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2517 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2522 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2527 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2532 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2537 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOoriginalTree)))
-- JoinExpr ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative JoinOn:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative JoinUsing:
         child ann            : {Annotation}
         child x              : {[String]}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data JoinExpr  = JoinOn (Annotation) (ScalarExpr) 
               | JoinUsing (Annotation) ([String]) 
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
                   IDEnv ->
                   LocalBindings ->
                   ( JoinExpr,JoinExpr,JoinExpr)
data Inh_JoinExpr  = Inh_JoinExpr {cat_Inh_JoinExpr :: Catalog,idenv_Inh_JoinExpr :: IDEnv,lib_Inh_JoinExpr :: LocalBindings}
data Syn_JoinExpr  = Syn_JoinExpr {annotatedTree_Syn_JoinExpr :: JoinExpr,fixedUpIdentifiersTree_Syn_JoinExpr :: JoinExpr,originalTree_Syn_JoinExpr :: JoinExpr}
wrap_JoinExpr :: T_JoinExpr  ->
                 Inh_JoinExpr  ->
                 Syn_JoinExpr 
wrap_JoinExpr sem (Inh_JoinExpr _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_JoinExpr _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_JoinExpr_JoinOn :: Annotation ->
                       T_ScalarExpr  ->
                       T_JoinExpr 
sem_JoinExpr_JoinOn ann_ expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: JoinExpr
              _lhsOfixedUpIdentifiersTree :: JoinExpr
              _lhsOoriginalTree :: JoinExpr
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ParameterizedStatements.ag"(line 94, column 14)
              _exprOexpectedType =
                  {-# LINE 94 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Just typeBool
                  {-# LINE 2614 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  JoinOn ann_ _exprIannotatedTree
                  {-# LINE 2619 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  JoinOn ann_ _exprIfixedUpIdentifiersTree
                  {-# LINE 2624 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  JoinOn ann_ _exprIoriginalTree
                  {-# LINE 2629 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2634 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2639 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2644 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2649 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2654 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2659 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_JoinExpr_JoinUsing :: Annotation ->
                          ([String]) ->
                          T_JoinExpr 
sem_JoinExpr_JoinUsing ann_ x_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: JoinExpr
              _lhsOfixedUpIdentifiersTree :: JoinExpr
              _lhsOoriginalTree :: JoinExpr
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  JoinUsing ann_ x_
                  {-# LINE 2677 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  JoinUsing ann_ x_
                  {-# LINE 2682 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  JoinUsing ann_ x_
                  {-# LINE 2687 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2692 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2697 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2702 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- MaybeBoolExpr -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type MaybeBoolExpr  = (Maybe (ScalarExpr))
-- cata
sem_MaybeBoolExpr :: MaybeBoolExpr  ->
                     T_MaybeBoolExpr 
sem_MaybeBoolExpr (Prelude.Just x )  =
    (sem_MaybeBoolExpr_Just (sem_ScalarExpr x ) )
sem_MaybeBoolExpr Prelude.Nothing  =
    sem_MaybeBoolExpr_Nothing
-- semantic domain
type T_MaybeBoolExpr  = Catalog ->
                        IDEnv ->
                        LocalBindings ->
                        ( MaybeBoolExpr,MaybeBoolExpr,MaybeBoolExpr)
data Inh_MaybeBoolExpr  = Inh_MaybeBoolExpr {cat_Inh_MaybeBoolExpr :: Catalog,idenv_Inh_MaybeBoolExpr :: IDEnv,lib_Inh_MaybeBoolExpr :: LocalBindings}
data Syn_MaybeBoolExpr  = Syn_MaybeBoolExpr {annotatedTree_Syn_MaybeBoolExpr :: MaybeBoolExpr,fixedUpIdentifiersTree_Syn_MaybeBoolExpr :: MaybeBoolExpr,originalTree_Syn_MaybeBoolExpr :: MaybeBoolExpr}
wrap_MaybeBoolExpr :: T_MaybeBoolExpr  ->
                      Inh_MaybeBoolExpr  ->
                      Syn_MaybeBoolExpr 
wrap_MaybeBoolExpr sem (Inh_MaybeBoolExpr _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_MaybeBoolExpr _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_MaybeBoolExpr_Just :: T_ScalarExpr  ->
                          T_MaybeBoolExpr 
sem_MaybeBoolExpr_Just just_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: MaybeBoolExpr
              _justOexpectedType :: (Maybe Type)
              _lhsOfixedUpIdentifiersTree :: MaybeBoolExpr
              _lhsOoriginalTree :: MaybeBoolExpr
              _justOcat :: Catalog
              _justOidenv :: IDEnv
              _justOlib :: LocalBindings
              _justIannotatedTree :: ScalarExpr
              _justIfixedUpIdentifiersTree :: ScalarExpr
              _justIoriginalTree :: ScalarExpr
              _justIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 72, column 9)
              _lhsOannotatedTree =
                  {-# LINE 72 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  let t = _justIuType
                  in if t `elem` [Nothing,Just typeBool]
                     then Just _justIannotatedTree
                     else Just $ addTypeErrors [ExpressionMustBeBool] _justIannotatedTree
                  {-# LINE 2774 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 97, column 12)
              _justOexpectedType =
                  {-# LINE 97 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Just typeBool
                  {-# LINE 2779 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 2784 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Just _justIfixedUpIdentifiersTree
                  {-# LINE 2789 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 2794 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2799 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2804 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2809 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2814 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2819 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIfixedUpIdentifiersTree,_justIoriginalTree,_justIuType) =
                  (just_ _justOcat _justOexpectedType _justOidenv _justOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_MaybeBoolExpr_Nothing :: T_MaybeBoolExpr 
sem_MaybeBoolExpr_Nothing  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: MaybeBoolExpr
              _lhsOfixedUpIdentifiersTree :: MaybeBoolExpr
              _lhsOoriginalTree :: MaybeBoolExpr
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 2835 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Nothing
                  {-# LINE 2840 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 2845 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2850 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2855 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2860 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- MaybeScalarExpr ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         uType                : Maybe Type
   alternatives:
      alternative Just:
         child just           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type MaybeScalarExpr  = (Maybe (ScalarExpr))
-- cata
sem_MaybeScalarExpr :: MaybeScalarExpr  ->
                       T_MaybeScalarExpr 
sem_MaybeScalarExpr (Prelude.Just x )  =
    (sem_MaybeScalarExpr_Just (sem_ScalarExpr x ) )
sem_MaybeScalarExpr Prelude.Nothing  =
    sem_MaybeScalarExpr_Nothing
-- semantic domain
type T_MaybeScalarExpr  = Catalog ->
                          IDEnv ->
                          LocalBindings ->
                          ( MaybeScalarExpr,MaybeScalarExpr,MaybeScalarExpr,(Maybe Type))
data Inh_MaybeScalarExpr  = Inh_MaybeScalarExpr {cat_Inh_MaybeScalarExpr :: Catalog,idenv_Inh_MaybeScalarExpr :: IDEnv,lib_Inh_MaybeScalarExpr :: LocalBindings}
data Syn_MaybeScalarExpr  = Syn_MaybeScalarExpr {annotatedTree_Syn_MaybeScalarExpr :: MaybeScalarExpr,fixedUpIdentifiersTree_Syn_MaybeScalarExpr :: MaybeScalarExpr,originalTree_Syn_MaybeScalarExpr :: MaybeScalarExpr,uType_Syn_MaybeScalarExpr :: Maybe Type}
wrap_MaybeScalarExpr :: T_MaybeScalarExpr  ->
                        Inh_MaybeScalarExpr  ->
                        Syn_MaybeScalarExpr 
wrap_MaybeScalarExpr sem (Inh_MaybeScalarExpr _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_MaybeScalarExpr _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOuType ))
sem_MaybeScalarExpr_Just :: T_ScalarExpr  ->
                            T_MaybeScalarExpr 
sem_MaybeScalarExpr_Just just_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOuType :: (Maybe Type)
              _justOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: MaybeScalarExpr
              _lhsOfixedUpIdentifiersTree :: MaybeScalarExpr
              _lhsOoriginalTree :: MaybeScalarExpr
              _justOcat :: Catalog
              _justOidenv :: IDEnv
              _justOlib :: LocalBindings
              _justIannotatedTree :: ScalarExpr
              _justIfixedUpIdentifiersTree :: ScalarExpr
              _justIoriginalTree :: ScalarExpr
              _justIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 65, column 12)
              _lhsOuType =
                  {-# LINE 65 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _justIuType
                  {-# LINE 2931 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 100, column 12)
              _justOexpectedType =
                  {-# LINE 100 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 2936 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 2941 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Just _justIfixedUpIdentifiersTree
                  {-# LINE 2946 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 2951 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2956 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2961 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2966 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2971 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2976 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2981 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIfixedUpIdentifiersTree,_justIoriginalTree,_justIuType) =
                  (just_ _justOcat _justOexpectedType _justOidenv _justOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_MaybeScalarExpr_Nothing :: T_MaybeScalarExpr 
sem_MaybeScalarExpr_Nothing  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOuType :: (Maybe Type)
              _lhsOannotatedTree :: MaybeScalarExpr
              _lhsOfixedUpIdentifiersTree :: MaybeScalarExpr
              _lhsOoriginalTree :: MaybeScalarExpr
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 66, column 15)
              _lhsOuType =
                  {-# LINE 66 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 2998 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3003 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Nothing
                  {-# LINE 3008 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3013 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3018 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3023 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3028 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
-- MaybeSelectList ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         listType             : [(String,Maybe Type)]
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : SelectList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
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
type T_MaybeSelectList  = Catalog ->
                          IDEnv ->
                          LocalBindings ->
                          ( MaybeSelectList,MaybeSelectList,([(String,Maybe Type)]),MaybeSelectList)
data Inh_MaybeSelectList  = Inh_MaybeSelectList {cat_Inh_MaybeSelectList :: Catalog,idenv_Inh_MaybeSelectList :: IDEnv,lib_Inh_MaybeSelectList :: LocalBindings}
data Syn_MaybeSelectList  = Syn_MaybeSelectList {annotatedTree_Syn_MaybeSelectList :: MaybeSelectList,fixedUpIdentifiersTree_Syn_MaybeSelectList :: MaybeSelectList,listType_Syn_MaybeSelectList :: [(String,Maybe Type)],originalTree_Syn_MaybeSelectList :: MaybeSelectList}
wrap_MaybeSelectList :: T_MaybeSelectList  ->
                        Inh_MaybeSelectList  ->
                        Syn_MaybeSelectList 
wrap_MaybeSelectList sem (Inh_MaybeSelectList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_MaybeSelectList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOlistType _lhsOoriginalTree ))
sem_MaybeSelectList_Just :: T_SelectList  ->
                            T_MaybeSelectList 
sem_MaybeSelectList_Just just_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlistType :: ([(String,Maybe Type)])
              _lhsOannotatedTree :: MaybeSelectList
              _lhsOfixedUpIdentifiersTree :: MaybeSelectList
              _lhsOoriginalTree :: MaybeSelectList
              _justOcat :: Catalog
              _justOidenv :: IDEnv
              _justOlib :: LocalBindings
              _justIannotatedTree :: SelectList
              _justIcidenv :: IDEnv
              _justIfixedUpIdentifiersTree :: SelectList
              _justIlibUpdates :: ([LocalBindingsUpdate])
              _justIlistType :: ([(String,Maybe Type)])
              _justIoriginalTree :: SelectList
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 38, column 12)
              _lhsOlistType =
                  {-# LINE 38 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  _justIlistType
                  {-# LINE 3100 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3105 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Just _justIfixedUpIdentifiersTree
                  {-# LINE 3110 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 3115 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3120 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3125 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3130 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3135 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3140 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3145 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIcidenv,_justIfixedUpIdentifiersTree,_justIlibUpdates,_justIlistType,_justIoriginalTree) =
                  (just_ _justOcat _justOidenv _justOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOoriginalTree)))
sem_MaybeSelectList_Nothing :: T_MaybeSelectList 
sem_MaybeSelectList_Nothing  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlistType :: ([(String,Maybe Type)])
              _lhsOannotatedTree :: MaybeSelectList
              _lhsOfixedUpIdentifiersTree :: MaybeSelectList
              _lhsOoriginalTree :: MaybeSelectList
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 39, column 15)
              _lhsOlistType =
                  {-# LINE 39 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  []
                  {-# LINE 3162 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3167 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Nothing
                  {-# LINE 3172 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3177 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3182 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3187 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3192 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOoriginalTree)))
-- OnExpr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : JoinExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type OnExpr  = (Maybe (JoinExpr))
-- cata
sem_OnExpr :: OnExpr  ->
              T_OnExpr 
sem_OnExpr (Prelude.Just x )  =
    (sem_OnExpr_Just (sem_JoinExpr x ) )
sem_OnExpr Prelude.Nothing  =
    sem_OnExpr_Nothing
-- semantic domain
type T_OnExpr  = Catalog ->
                 IDEnv ->
                 LocalBindings ->
                 ( OnExpr,OnExpr,OnExpr)
data Inh_OnExpr  = Inh_OnExpr {cat_Inh_OnExpr :: Catalog,idenv_Inh_OnExpr :: IDEnv,lib_Inh_OnExpr :: LocalBindings}
data Syn_OnExpr  = Syn_OnExpr {annotatedTree_Syn_OnExpr :: OnExpr,fixedUpIdentifiersTree_Syn_OnExpr :: OnExpr,originalTree_Syn_OnExpr :: OnExpr}
wrap_OnExpr :: T_OnExpr  ->
               Inh_OnExpr  ->
               Syn_OnExpr 
wrap_OnExpr sem (Inh_OnExpr _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_OnExpr _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_OnExpr_Just :: T_JoinExpr  ->
                   T_OnExpr 
sem_OnExpr_Just just_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: OnExpr
              _lhsOfixedUpIdentifiersTree :: OnExpr
              _lhsOoriginalTree :: OnExpr
              _justOcat :: Catalog
              _justOidenv :: IDEnv
              _justOlib :: LocalBindings
              _justIannotatedTree :: JoinExpr
              _justIfixedUpIdentifiersTree :: JoinExpr
              _justIoriginalTree :: JoinExpr
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3259 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Just _justIfixedUpIdentifiersTree
                  {-# LINE 3264 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 3269 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3274 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3279 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3284 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3289 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3294 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3299 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIfixedUpIdentifiersTree,_justIoriginalTree) =
                  (just_ _justOcat _justOidenv _justOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: OnExpr
              _lhsOfixedUpIdentifiersTree :: OnExpr
              _lhsOoriginalTree :: OnExpr
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3315 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Nothing
                  {-# LINE 3320 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3325 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3330 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3335 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3340 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- ParamDef ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
         pos                  : Int
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         namedType            : Maybe Type
         originalTree         : SELF 
         paramName            : ParamName
   alternatives:
      alternative ParamDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative ParamDefTp:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
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
type T_ParamDef  = Catalog ->
                   IDEnv ->
                   LocalBindings ->
                   Int ->
                   ( ParamDef,ParamDef,(Maybe Type),ParamDef,ParamName)
data Inh_ParamDef  = Inh_ParamDef {cat_Inh_ParamDef :: Catalog,idenv_Inh_ParamDef :: IDEnv,lib_Inh_ParamDef :: LocalBindings,pos_Inh_ParamDef :: Int}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef,fixedUpIdentifiersTree_Syn_ParamDef :: ParamDef,namedType_Syn_ParamDef :: Maybe Type,originalTree_Syn_ParamDef :: ParamDef,paramName_Syn_ParamDef :: ParamName}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIcat _lhsIidenv _lhsIlib _lhsIpos )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree,_lhsOparamName) =
             (sem _lhsIcat _lhsIidenv _lhsIlib _lhsIpos )
     in  (Syn_ParamDef _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnamedType _lhsOoriginalTree _lhsOparamName ))
sem_ParamDef_ParamDef :: Annotation ->
                         String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef ann_ name_ typ_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib
       _lhsIpos ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOparamName :: ParamName
              _lhsOannotatedTree :: ParamDef
              _lhsOfixedUpIdentifiersTree :: ParamDef
              _lhsOoriginalTree :: ParamDef
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 45, column 9)
              _lhsOnamedType =
                  {-# LINE 45 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  _typInamedType
                  {-# LINE 3423 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 47, column 9)
              _lhsOparamName =
                  {-# LINE 47 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  NamedParam _lhsIpos name_
                  {-# LINE 3428 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIannotatedTree
                  {-# LINE 3433 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ParamDef ann_ name_ _typIfixedUpIdentifiersTree
                  {-# LINE 3438 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIoriginalTree
                  {-# LINE 3443 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3448 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3453 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3458 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3463 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3468 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3473 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree,_lhsOparamName)))
sem_ParamDef_ParamDefTp :: Annotation ->
                           T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp ann_ typ_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib
       _lhsIpos ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOparamName :: ParamName
              _lhsOannotatedTree :: ParamDef
              _lhsOfixedUpIdentifiersTree :: ParamDef
              _lhsOoriginalTree :: ParamDef
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 45, column 9)
              _lhsOnamedType =
                  {-# LINE 45 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  _typInamedType
                  {-# LINE 3501 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 49, column 9)
              _lhsOparamName =
                  {-# LINE 49 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  UnnamedParam _lhsIpos
                  {-# LINE 3506 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIannotatedTree
                  {-# LINE 3511 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ParamDefTp ann_ _typIfixedUpIdentifiersTree
                  {-# LINE 3516 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIoriginalTree
                  {-# LINE 3521 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3526 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3531 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3536 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3541 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3546 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3551 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree,_lhsOparamName)))
-- ParamDefList ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
         pos                  : Int
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         params               : [(ParamName, Maybe Type)]
   alternatives:
      alternative Cons:
         child hd             : ParamDef 
         child tl             : ParamDefList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type ParamDefList  = [(ParamDef)]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = Catalog ->
                       IDEnv ->
                       LocalBindings ->
                       Int ->
                       ( ParamDefList,ParamDefList,ParamDefList,([(ParamName, Maybe Type)]))
data Inh_ParamDefList  = Inh_ParamDefList {cat_Inh_ParamDefList :: Catalog,idenv_Inh_ParamDefList :: IDEnv,lib_Inh_ParamDefList :: LocalBindings,pos_Inh_ParamDefList :: Int}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList,fixedUpIdentifiersTree_Syn_ParamDefList :: ParamDefList,originalTree_Syn_ParamDefList :: ParamDefList,params_Syn_ParamDefList :: [(ParamName, Maybe Type)]}
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIcat _lhsIidenv _lhsIlib _lhsIpos )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOparams) =
             (sem _lhsIcat _lhsIidenv _lhsIlib _lhsIpos )
     in  (Syn_ParamDefList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOparams ))
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib
       _lhsIpos ->
         (let _lhsOparams :: ([(ParamName, Maybe Type)])
              _hdOpos :: Int
              _tlOpos :: Int
              _lhsOannotatedTree :: ParamDefList
              _lhsOfixedUpIdentifiersTree :: ParamDefList
              _lhsOoriginalTree :: ParamDefList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ParamDef
              _hdIfixedUpIdentifiersTree :: ParamDef
              _hdInamedType :: (Maybe Type)
              _hdIoriginalTree :: ParamDef
              _hdIparamName :: ParamName
              _tlIannotatedTree :: ParamDefList
              _tlIfixedUpIdentifiersTree :: ParamDefList
              _tlIoriginalTree :: ParamDefList
              _tlIparams :: ([(ParamName, Maybe Type)])
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 53, column 13)
              _lhsOparams =
                  {-# LINE 53 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  ((_hdIparamName, _hdInamedType) : _tlIparams)
                  {-# LINE 3636 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 54, column 13)
              _hdOpos =
                  {-# LINE 54 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  _lhsIpos
                  {-# LINE 3641 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 55, column 13)
              _tlOpos =
                  {-# LINE 55 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  _lhsIpos + 1
                  {-# LINE 3646 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 3651 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 3656 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 3661 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3666 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3671 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3676 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3681 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3686 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3691 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3696 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3701 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3706 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInamedType,_hdIoriginalTree,_hdIparamName) =
                  (hd_ _hdOcat _hdOidenv _hdOlib _hdOpos )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree,_tlIparams) =
                  (tl_ _tlOcat _tlOidenv _tlOlib _tlOpos )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOparams)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib
       _lhsIpos ->
         (let _lhsOparams :: ([(ParamName, Maybe Type)])
              _lhsOannotatedTree :: ParamDefList
              _lhsOfixedUpIdentifiersTree :: ParamDefList
              _lhsOoriginalTree :: ParamDefList
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 52, column 12)
              _lhsOparams =
                  {-# LINE 52 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  []
                  {-# LINE 3726 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 3731 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 3736 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 3741 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3746 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3751 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3756 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOparams)))
-- QueryExpr ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         expectedTypes        : [Maybe Type]
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         cidenv               : IDEnv
         fixedUpIdentifiersTree : SELF 
         libUpdates           : [LocalBindingsUpdate]
         originalTree         : SELF 
         uType                : Maybe [(String,Type)]
   alternatives:
      alternative CombineSelect:
         child ann            : {Annotation}
         child ctype          : {CombineType}
         child sel1           : QueryExpr 
         child sel2           : QueryExpr 
         visit 0:
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
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
            local trefEnv     : _
            local includeCorrelations : _
            local newLib      : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Values:
         child ann            : {Annotation}
         child vll            : ScalarExprListList 
         visit 0:
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative WithSelect:
         child ann            : {Annotation}
         child withs          : WithQueryList 
         child ex             : QueryExpr 
         visit 0:
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data QueryExpr  = CombineSelect (Annotation) (CombineType) (QueryExpr) (QueryExpr) 
                | Select (Annotation) (Distinct) (SelectList) (TableRefList) (MaybeBoolExpr) (ScalarExprList) (MaybeBoolExpr) (ScalarExprDirectionPairList) (MaybeScalarExpr) (MaybeScalarExpr) 
                | Values (Annotation) (ScalarExprListList) 
                | WithSelect (Annotation) (WithQueryList) (QueryExpr) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_QueryExpr :: QueryExpr  ->
                 T_QueryExpr 
sem_QueryExpr (CombineSelect _ann _ctype _sel1 _sel2 )  =
    (sem_QueryExpr_CombineSelect _ann _ctype (sem_QueryExpr _sel1 ) (sem_QueryExpr _sel2 ) )
sem_QueryExpr (Select _ann _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selLimit _selOffset )  =
    (sem_QueryExpr_Select _ann _selDistinct (sem_SelectList _selSelectList ) (sem_TableRefList _selTref ) (sem_MaybeBoolExpr _selWhere ) (sem_ScalarExprList _selGroupBy ) (sem_MaybeBoolExpr _selHaving ) (sem_ScalarExprDirectionPairList _selOrderBy ) (sem_MaybeScalarExpr _selLimit ) (sem_MaybeScalarExpr _selOffset ) )
sem_QueryExpr (Values _ann _vll )  =
    (sem_QueryExpr_Values _ann (sem_ScalarExprListList _vll ) )
sem_QueryExpr (WithSelect _ann _withs _ex )  =
    (sem_QueryExpr_WithSelect _ann (sem_WithQueryList _withs ) (sem_QueryExpr _ex ) )
-- semantic domain
type T_QueryExpr  = Catalog ->
                    ([Maybe Type]) ->
                    IDEnv ->
                    LocalBindings ->
                    ( QueryExpr,IDEnv,QueryExpr,([LocalBindingsUpdate]),QueryExpr,(Maybe [(String,Type)]))
data Inh_QueryExpr  = Inh_QueryExpr {cat_Inh_QueryExpr :: Catalog,expectedTypes_Inh_QueryExpr :: [Maybe Type],idenv_Inh_QueryExpr :: IDEnv,lib_Inh_QueryExpr :: LocalBindings}
data Syn_QueryExpr  = Syn_QueryExpr {annotatedTree_Syn_QueryExpr :: QueryExpr,cidenv_Syn_QueryExpr :: IDEnv,fixedUpIdentifiersTree_Syn_QueryExpr :: QueryExpr,libUpdates_Syn_QueryExpr :: [LocalBindingsUpdate],originalTree_Syn_QueryExpr :: QueryExpr,uType_Syn_QueryExpr :: Maybe [(String,Type)]}
wrap_QueryExpr :: T_QueryExpr  ->
                  Inh_QueryExpr  ->
                  Syn_QueryExpr 
wrap_QueryExpr sem (Inh_QueryExpr _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )
     in  (Syn_QueryExpr _lhsOannotatedTree _lhsOcidenv _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOoriginalTree _lhsOuType ))
sem_QueryExpr_CombineSelect :: Annotation ->
                               CombineType ->
                               T_QueryExpr  ->
                               T_QueryExpr  ->
                               T_QueryExpr 
sem_QueryExpr_CombineSelect ann_ ctype_ sel1_ sel2_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOcidenv :: IDEnv
              _lhsOannotatedTree :: QueryExpr
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _lhsOuType :: (Maybe [(String,Type)])
              _lhsOfixedUpIdentifiersTree :: QueryExpr
              _lhsOoriginalTree :: QueryExpr
              _sel1Ocat :: Catalog
              _sel1OexpectedTypes :: ([Maybe Type])
              _sel1Oidenv :: IDEnv
              _sel1Olib :: LocalBindings
              _sel2Ocat :: Catalog
              _sel2OexpectedTypes :: ([Maybe Type])
              _sel2Oidenv :: IDEnv
              _sel2Olib :: LocalBindings
              _sel1IannotatedTree :: QueryExpr
              _sel1Icidenv :: IDEnv
              _sel1IfixedUpIdentifiersTree :: QueryExpr
              _sel1IlibUpdates :: ([LocalBindingsUpdate])
              _sel1IoriginalTree :: QueryExpr
              _sel1IuType :: (Maybe [(String,Type)])
              _sel2IannotatedTree :: QueryExpr
              _sel2Icidenv :: IDEnv
              _sel2IfixedUpIdentifiersTree :: QueryExpr
              _sel2IlibUpdates :: ([LocalBindingsUpdate])
              _sel2IoriginalTree :: QueryExpr
              _sel2IuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 230, column 21)
              _lhsOcidenv =
                  {-# LINE 230 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _sel1Icidenv
                  {-# LINE 3897 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 3902 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 116, column 9)
              _lhsOlibUpdates =
                  {-# LINE 116 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  []
                  {-# LINE 3907 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 143, column 9)
              _tpe =
                  {-# LINE 143 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  do
                  sel1t <- lmt ((SetOfType . CompositeType) <$> _sel1IuType)
                  sel2t <- lmt ((SetOfType . CompositeType) <$> _sel2IuType)
                  typeCheckCombineSelect _lhsIcat sel1t sel2t
                  {-# LINE 3915 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 149, column 9)
              _backTree =
                  {-# LINE 149 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  CombineSelect ann_ ctype_
                                _sel1IannotatedTree
                                _sel2IannotatedTree
                  {-# LINE 3922 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 160, column 9)
              _lhsOuType =
                  {-# LINE 160 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 3927 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CombineSelect ann_ ctype_ _sel1IannotatedTree _sel2IannotatedTree
                  {-# LINE 3932 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CombineSelect ann_ ctype_ _sel1IfixedUpIdentifiersTree _sel2IfixedUpIdentifiersTree
                  {-# LINE 3937 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CombineSelect ann_ ctype_ _sel1IoriginalTree _sel2IoriginalTree
                  {-# LINE 3942 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3947 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3952 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3957 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1OexpectedTypes =
                  {-# LINE 161 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 3962 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3967 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3972 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3977 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2OexpectedTypes =
                  {-# LINE 161 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 3982 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3987 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3992 "AstInternal.hs" #-}
              ( _sel1IannotatedTree,_sel1Icidenv,_sel1IfixedUpIdentifiersTree,_sel1IlibUpdates,_sel1IoriginalTree,_sel1IuType) =
                  (sel1_ _sel1Ocat _sel1OexpectedTypes _sel1Oidenv _sel1Olib )
              ( _sel2IannotatedTree,_sel2Icidenv,_sel2IfixedUpIdentifiersTree,_sel2IlibUpdates,_sel2IoriginalTree,_sel2IuType) =
                  (sel2_ _sel2Ocat _sel2OexpectedTypes _sel2Oidenv _sel2Olib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOuType)))
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
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOfixedUpIdentifiersTree :: QueryExpr
              _lhsOcidenv :: IDEnv
              _selSelectListOidenv :: IDEnv
              _selWhereOidenv :: IDEnv
              _selGroupByOidenv :: IDEnv
              _selHavingOidenv :: IDEnv
              _selOrderByOidenv :: IDEnv
              _lhsOannotatedTree :: QueryExpr
              _selSelectListOlib :: LocalBindings
              _selWhereOlib :: LocalBindings
              _selGroupByOlib :: LocalBindings
              _selOrderByOlib :: LocalBindings
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _lhsOuType :: (Maybe [(String,Type)])
              _selGroupByOexpectedTypes :: ([Maybe Type])
              _lhsOoriginalTree :: QueryExpr
              _selSelectListOcat :: Catalog
              _selTrefOcat :: Catalog
              _selTrefOidenv :: IDEnv
              _selTrefOlib :: LocalBindings
              _selWhereOcat :: Catalog
              _selGroupByOcat :: Catalog
              _selHavingOcat :: Catalog
              _selHavingOlib :: LocalBindings
              _selOrderByOcat :: Catalog
              _selLimitOcat :: Catalog
              _selLimitOidenv :: IDEnv
              _selLimitOlib :: LocalBindings
              _selOffsetOcat :: Catalog
              _selOffsetOidenv :: IDEnv
              _selOffsetOlib :: LocalBindings
              _selSelectListIannotatedTree :: SelectList
              _selSelectListIcidenv :: IDEnv
              _selSelectListIfixedUpIdentifiersTree :: SelectList
              _selSelectListIlibUpdates :: ([LocalBindingsUpdate])
              _selSelectListIlistType :: ([(String,Maybe Type)])
              _selSelectListIoriginalTree :: SelectList
              _selTrefIannotatedTree :: TableRefList
              _selTrefIfixedUpIdentifiersTree :: TableRefList
              _selTrefIlibUpdates :: ([LocalBindingsUpdate])
              _selTrefInewLib2 :: LocalBindings
              _selTrefIoriginalTree :: TableRefList
              _selTrefItrefIDs :: ([(String,[String])])
              _selWhereIannotatedTree :: MaybeBoolExpr
              _selWhereIfixedUpIdentifiersTree :: MaybeBoolExpr
              _selWhereIoriginalTree :: MaybeBoolExpr
              _selGroupByIannotatedTree :: ScalarExprList
              _selGroupByIfixedUpIdentifiersTree :: ScalarExprList
              _selGroupByIoriginalTree :: ScalarExprList
              _selGroupByIuType :: ([Maybe Type])
              _selHavingIannotatedTree :: MaybeBoolExpr
              _selHavingIfixedUpIdentifiersTree :: MaybeBoolExpr
              _selHavingIoriginalTree :: MaybeBoolExpr
              _selOrderByIannotatedTree :: ScalarExprDirectionPairList
              _selOrderByIfixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _selOrderByIoriginalTree :: ScalarExprDirectionPairList
              _selLimitIannotatedTree :: MaybeScalarExpr
              _selLimitIfixedUpIdentifiersTree :: MaybeScalarExpr
              _selLimitIoriginalTree :: MaybeScalarExpr
              _selLimitIuType :: (Maybe Type)
              _selOffsetIannotatedTree :: MaybeScalarExpr
              _selOffsetIfixedUpIdentifiersTree :: MaybeScalarExpr
              _selOffsetIoriginalTree :: MaybeScalarExpr
              _selOffsetIuType :: (Maybe Type)
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 187, column 9)
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 187 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Select ann_
                         selDistinct_
                         _selSelectListIfixedUpIdentifiersTree
                         _selTrefIfixedUpIdentifiersTree
                         _selWhereIfixedUpIdentifiersTree
                         _selGroupByIfixedUpIdentifiersTree
                         _selHavingIfixedUpIdentifiersTree
                         _selOrderByIfixedUpIdentifiersTree
                         _selLimitIfixedUpIdentifiersTree
                         _selOffsetIfixedUpIdentifiersTree
                  {-# LINE 4092 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 229, column 14)
              _lhsOcidenv =
                  {-# LINE 229 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _selSelectListIcidenv
                  {-# LINE 4097 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 270, column 14)
              _trefEnv =
                  {-# LINE 270 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  makeIDEnvP _selTrefItrefIDs
                  {-# LINE 4102 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 271, column 14)
              _includeCorrelations =
                  {-# LINE 271 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  joinIDEnvs _lhsIidenv _trefEnv
                  {-# LINE 4107 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 272, column 14)
              _selSelectListOidenv =
                  {-# LINE 272 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _trefEnv
                  {-# LINE 4112 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 273, column 14)
              _selWhereOidenv =
                  {-# LINE 273 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _includeCorrelations
                  {-# LINE 4117 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 274, column 14)
              _selGroupByOidenv =
                  {-# LINE 274 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _trefEnv
                  {-# LINE 4122 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 275, column 14)
              _selHavingOidenv =
                  {-# LINE 275 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _includeCorrelations
                  {-# LINE 4127 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 276, column 14)
              _selOrderByOidenv =
                  {-# LINE 276 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _trefEnv
                  {-# LINE 4132 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 4137 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 101, column 10)
              _newLib =
                  {-# LINE 101 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _selTrefInewLib2
                  {-# LINE 4142 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 102, column 10)
              _selSelectListOlib =
                  {-# LINE 102 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _newLib
                  {-# LINE 4147 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 103, column 10)
              _selWhereOlib =
                  {-# LINE 103 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _newLib
                  {-# LINE 4152 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 104, column 10)
              _selGroupByOlib =
                  {-# LINE 104 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _newLib
                  {-# LINE 4157 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 105, column 10)
              _selOrderByOlib =
                  {-# LINE 105 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _newLib
                  {-# LINE 4162 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 118, column 9)
              _lhsOlibUpdates =
                  {-# LINE 118 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _selSelectListIlibUpdates
                  {-# LINE 4167 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 130, column 9)
              _tpe =
                  {-# LINE 130 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  Right $ SetOfType $ CompositeType $ fromMaybe [] $ liftList  _selSelectListIlistType
                  {-# LINE 4172 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 132, column 9)
              _backTree =
                  {-# LINE 132 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  Select ann_
                         selDistinct_
                         _selSelectListIannotatedTree
                         _selTrefIannotatedTree
                         _selWhereIannotatedTree
                         _selGroupByIannotatedTree
                         _selHavingIannotatedTree
                         _selOrderByIannotatedTree
                         _selLimitIannotatedTree
                         _selOffsetIannotatedTree
                  {-# LINE 4186 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 160, column 9)
              _lhsOuType =
                  {-# LINE 160 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 4191 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 152, column 14)
              _selGroupByOexpectedTypes =
                  {-# LINE 152 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 4196 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Select ann_ selDistinct_ _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
                  {-# LINE 4201 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Select ann_ selDistinct_ _selSelectListIfixedUpIdentifiersTree _selTrefIfixedUpIdentifiersTree _selWhereIfixedUpIdentifiersTree _selGroupByIfixedUpIdentifiersTree _selHavingIfixedUpIdentifiersTree _selOrderByIfixedUpIdentifiersTree _selLimitIfixedUpIdentifiersTree _selOffsetIfixedUpIdentifiersTree
                  {-# LINE 4206 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Select ann_ selDistinct_ _selSelectListIoriginalTree _selTrefIoriginalTree _selWhereIoriginalTree _selGroupByIoriginalTree _selHavingIoriginalTree _selOrderByIoriginalTree _selLimitIoriginalTree _selOffsetIoriginalTree
                  {-# LINE 4211 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4216 "AstInternal.hs" #-}
              -- copy rule (down)
              _selSelectListOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4221 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4226 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4231 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4236 "AstInternal.hs" #-}
              -- copy rule (down)
              _selWhereOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4241 "AstInternal.hs" #-}
              -- copy rule (down)
              _selGroupByOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4246 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4251 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4256 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOrderByOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4261 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4266 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4271 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4276 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4281 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4286 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4291 "AstInternal.hs" #-}
              ( _selSelectListIannotatedTree,_selSelectListIcidenv,_selSelectListIfixedUpIdentifiersTree,_selSelectListIlibUpdates,_selSelectListIlistType,_selSelectListIoriginalTree) =
                  (selSelectList_ _selSelectListOcat _selSelectListOidenv _selSelectListOlib )
              ( _selTrefIannotatedTree,_selTrefIfixedUpIdentifiersTree,_selTrefIlibUpdates,_selTrefInewLib2,_selTrefIoriginalTree,_selTrefItrefIDs) =
                  (selTref_ _selTrefOcat _selTrefOidenv _selTrefOlib )
              ( _selWhereIannotatedTree,_selWhereIfixedUpIdentifiersTree,_selWhereIoriginalTree) =
                  (selWhere_ _selWhereOcat _selWhereOidenv _selWhereOlib )
              ( _selGroupByIannotatedTree,_selGroupByIfixedUpIdentifiersTree,_selGroupByIoriginalTree,_selGroupByIuType) =
                  (selGroupBy_ _selGroupByOcat _selGroupByOexpectedTypes _selGroupByOidenv _selGroupByOlib )
              ( _selHavingIannotatedTree,_selHavingIfixedUpIdentifiersTree,_selHavingIoriginalTree) =
                  (selHaving_ _selHavingOcat _selHavingOidenv _selHavingOlib )
              ( _selOrderByIannotatedTree,_selOrderByIfixedUpIdentifiersTree,_selOrderByIoriginalTree) =
                  (selOrderBy_ _selOrderByOcat _selOrderByOidenv _selOrderByOlib )
              ( _selLimitIannotatedTree,_selLimitIfixedUpIdentifiersTree,_selLimitIoriginalTree,_selLimitIuType) =
                  (selLimit_ _selLimitOcat _selLimitOidenv _selLimitOlib )
              ( _selOffsetIannotatedTree,_selOffsetIfixedUpIdentifiersTree,_selOffsetIoriginalTree,_selOffsetIuType) =
                  (selOffset_ _selOffsetOcat _selOffsetOidenv _selOffsetOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOuType)))
sem_QueryExpr_Values :: Annotation ->
                        T_ScalarExprListList  ->
                        T_QueryExpr 
sem_QueryExpr_Values ann_ vll_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOcidenv :: IDEnv
              _lhsOannotatedTree :: QueryExpr
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _lhsOuType :: (Maybe [(String,Type)])
              _vllOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: QueryExpr
              _lhsOoriginalTree :: QueryExpr
              _vllOcat :: Catalog
              _vllOidenv :: IDEnv
              _vllOlib :: LocalBindings
              _vllIannotatedTree :: ScalarExprListList
              _vllIfixedUpIdentifiersTree :: ScalarExprListList
              _vllIoriginalTree :: ScalarExprListList
              _vllIuType :: ([[Maybe Type]])
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 231, column 14)
              _lhsOcidenv =
                  {-# LINE 231 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  unimplementedIDEnv
                  {-# LINE 4336 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 4341 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 116, column 9)
              _lhsOlibUpdates =
                  {-# LINE 116 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  []
                  {-# LINE 4346 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 125, column 9)
              _tpe =
                  {-# LINE 125 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  typeCheckValuesExpr
                              _lhsIcat
                              _vllIuType
                  {-# LINE 4353 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 128, column 9)
              _backTree =
                  {-# LINE 128 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 4358 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 160, column 9)
              _lhsOuType =
                  {-# LINE 160 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 4363 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 166, column 14)
              _vllOexpectedTypes =
                  {-# LINE 166 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 4368 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 4373 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Values ann_ _vllIfixedUpIdentifiersTree
                  {-# LINE 4378 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Values ann_ _vllIoriginalTree
                  {-# LINE 4383 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4388 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4393 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4398 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4403 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4408 "AstInternal.hs" #-}
              ( _vllIannotatedTree,_vllIfixedUpIdentifiersTree,_vllIoriginalTree,_vllIuType) =
                  (vll_ _vllOcat _vllOexpectedTypes _vllOidenv _vllOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOuType)))
sem_QueryExpr_WithSelect :: Annotation ->
                            T_WithQueryList  ->
                            T_QueryExpr  ->
                            T_QueryExpr 
sem_QueryExpr_WithSelect ann_ withs_ ex_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOcidenv :: IDEnv
              _lhsOannotatedTree :: QueryExpr
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _exOcat :: Catalog
              _withsOcatUpdates :: ([CatalogUpdate])
              _lhsOuType :: (Maybe [(String,Type)])
              _lhsOfixedUpIdentifiersTree :: QueryExpr
              _lhsOoriginalTree :: QueryExpr
              _withsOcat :: Catalog
              _withsOidenv :: IDEnv
              _withsOlib :: LocalBindings
              _exOexpectedTypes :: ([Maybe Type])
              _exOidenv :: IDEnv
              _exOlib :: LocalBindings
              _withsIannotatedTree :: WithQueryList
              _withsIfixedUpIdentifiersTree :: WithQueryList
              _withsIoriginalTree :: WithQueryList
              _withsIproducedCat :: Catalog
              _exIannotatedTree :: QueryExpr
              _exIcidenv :: IDEnv
              _exIfixedUpIdentifiersTree :: QueryExpr
              _exIlibUpdates :: ([LocalBindingsUpdate])
              _exIoriginalTree :: QueryExpr
              _exIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 232, column 18)
              _lhsOcidenv =
                  {-# LINE 232 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _exIcidenv
                  {-# LINE 4450 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 4455 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 120, column 9)
              _lhsOlibUpdates =
                  {-# LINE 120 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _exIlibUpdates
                  {-# LINE 4460 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 153, column 9)
              _tpe =
                  {-# LINE 153 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  lmt ((SetOfType . CompositeType) <$> _exIuType)
                  {-# LINE 4465 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 154, column 9)
              _backTree =
                  {-# LINE 154 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  WithSelect ann_ _withsIannotatedTree _exIannotatedTree
                  {-# LINE 4470 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 155, column 9)
              _exOcat =
                  {-# LINE 155 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _withsIproducedCat
                  {-# LINE 4475 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 156, column 9)
              _withsOcatUpdates =
                  {-# LINE 156 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  []
                  {-# LINE 4480 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 160, column 9)
              _lhsOuType =
                  {-# LINE 160 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 4485 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  WithSelect ann_ _withsIannotatedTree _exIannotatedTree
                  {-# LINE 4490 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  WithSelect ann_ _withsIfixedUpIdentifiersTree _exIfixedUpIdentifiersTree
                  {-# LINE 4495 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  WithSelect ann_ _withsIoriginalTree _exIoriginalTree
                  {-# LINE 4500 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4505 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4510 "AstInternal.hs" #-}
              -- copy rule (down)
              _withsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4515 "AstInternal.hs" #-}
              -- copy rule (down)
              _withsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4520 "AstInternal.hs" #-}
              -- copy rule (down)
              _withsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4525 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOexpectedTypes =
                  {-# LINE 161 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 4530 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4535 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4540 "AstInternal.hs" #-}
              ( _withsIannotatedTree,_withsIfixedUpIdentifiersTree,_withsIoriginalTree,_withsIproducedCat) =
                  (withs_ _withsOcat _withsOcatUpdates _withsOidenv _withsOlib )
              ( _exIannotatedTree,_exIcidenv,_exIfixedUpIdentifiersTree,_exIlibUpdates,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedTypes _exOidenv _exOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOuType)))
-- Root --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         producedCat          : Catalog
         producedLib          : LocalBindings
   alternatives:
      alternative Root:
         child statements     : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
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
type T_Root  = Catalog ->
               IDEnv ->
               LocalBindings ->
               ( Root,Root,Root,Catalog,LocalBindings)
data Inh_Root  = Inh_Root {cat_Inh_Root :: Catalog,idenv_Inh_Root :: IDEnv,lib_Inh_Root :: LocalBindings}
data Syn_Root  = Syn_Root {annotatedTree_Syn_Root :: Root,fixedUpIdentifiersTree_Syn_Root :: Root,originalTree_Syn_Root :: Root,producedCat_Syn_Root :: Catalog,producedLib_Syn_Root :: LocalBindings}
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_Root _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOproducedCat _lhsOproducedLib ))
sem_Root_Root :: T_StatementList  ->
                 T_Root 
sem_Root_Root statements_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _statementsOcatUpdates :: ([CatalogUpdate])
              _statementsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Root
              _lhsOfixedUpIdentifiersTree :: Root
              _lhsOoriginalTree :: Root
              _lhsOproducedCat :: Catalog
              _lhsOproducedLib :: LocalBindings
              _statementsOcat :: Catalog
              _statementsOidenv :: IDEnv
              _statementsOlib :: LocalBindings
              _statementsIannotatedTree :: StatementList
              _statementsIfixedUpIdentifiersTree :: StatementList
              _statementsIoriginalTree :: StatementList
              _statementsIproducedCat :: Catalog
              _statementsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 107, column 12)
              _statementsOcatUpdates =
                  {-# LINE 107 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4613 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 108, column 12)
              _statementsOlibUpdates =
                  {-# LINE 108 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4618 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Root _statementsIannotatedTree
                  {-# LINE 4623 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Root _statementsIfixedUpIdentifiersTree
                  {-# LINE 4628 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Root _statementsIoriginalTree
                  {-# LINE 4633 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4638 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4643 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4648 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOproducedCat =
                  {-# LINE 27 "./TypeChecking/Statements.ag" #-}
                  _statementsIproducedCat
                  {-# LINE 4653 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOproducedLib =
                  {-# LINE 28 "./TypeChecking/Statements.ag" #-}
                  _statementsIproducedLib
                  {-# LINE 4658 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4663 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4668 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4673 "AstInternal.hs" #-}
              ( _statementsIannotatedTree,_statementsIfixedUpIdentifiersTree,_statementsIoriginalTree,_statementsIproducedCat,_statementsIproducedLib) =
                  (statements_ _statementsOcat _statementsOcatUpdates _statementsOidenv _statementsOlib _statementsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib)))
-- RowConstraint -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative NotNullConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative NullConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative RowCheckConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative RowPrimaryKeyConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative RowReferenceConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child table          : {String}
         child att            : {Maybe String}
         child onUpdate       : {Cascade}
         child onDelete       : {Cascade}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative RowUniqueConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data RowConstraint  = NotNullConstraint (Annotation) (String) 
                    | NullConstraint (Annotation) (String) 
                    | RowCheckConstraint (Annotation) (String) (ScalarExpr) 
                    | RowPrimaryKeyConstraint (Annotation) (String) 
                    | RowReferenceConstraint (Annotation) (String) (String) (Maybe String) (Cascade) (Cascade) 
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
    (sem_RowConstraint_RowReferenceConstraint _ann _name _table _att _onUpdate _onDelete )
sem_RowConstraint (RowUniqueConstraint _ann _name )  =
    (sem_RowConstraint_RowUniqueConstraint _ann _name )
-- semantic domain
type T_RowConstraint  = Catalog ->
                        IDEnv ->
                        LocalBindings ->
                        ( RowConstraint,RowConstraint,RowConstraint)
data Inh_RowConstraint  = Inh_RowConstraint {cat_Inh_RowConstraint :: Catalog,idenv_Inh_RowConstraint :: IDEnv,lib_Inh_RowConstraint :: LocalBindings}
data Syn_RowConstraint  = Syn_RowConstraint {annotatedTree_Syn_RowConstraint :: RowConstraint,fixedUpIdentifiersTree_Syn_RowConstraint :: RowConstraint,originalTree_Syn_RowConstraint :: RowConstraint}
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_RowConstraint _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_RowConstraint_NotNullConstraint :: Annotation ->
                                       String ->
                                       T_RowConstraint 
sem_RowConstraint_NotNullConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOfixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  NotNullConstraint ann_ name_
                  {-# LINE 4787 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  NotNullConstraint ann_ name_
                  {-# LINE 4792 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  NotNullConstraint ann_ name_
                  {-# LINE 4797 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4802 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4807 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4812 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_RowConstraint_NullConstraint :: Annotation ->
                                    String ->
                                    T_RowConstraint 
sem_RowConstraint_NullConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOfixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  NullConstraint ann_ name_
                  {-# LINE 4828 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  NullConstraint ann_ name_
                  {-# LINE 4833 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  NullConstraint ann_ name_
                  {-# LINE 4838 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4843 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4848 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4853 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_RowConstraint_RowCheckConstraint :: Annotation ->
                                        String ->
                                        T_ScalarExpr  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: RowConstraint
              _lhsOfixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ParameterizedStatements.ag"(line 103, column 26)
              _exprOexpectedType =
                  {-# LINE 103 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 4878 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  RowCheckConstraint ann_ name_ _exprIannotatedTree
                  {-# LINE 4883 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  RowCheckConstraint ann_ name_ _exprIfixedUpIdentifiersTree
                  {-# LINE 4888 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  RowCheckConstraint ann_ name_ _exprIoriginalTree
                  {-# LINE 4893 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4898 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4903 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4908 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4913 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4918 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4923 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_RowConstraint_RowPrimaryKeyConstraint :: Annotation ->
                                             String ->
                                             T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOfixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint ann_ name_
                  {-# LINE 4941 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  RowPrimaryKeyConstraint ann_ name_
                  {-# LINE 4946 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint ann_ name_
                  {-# LINE 4951 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4956 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4961 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4966 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_RowConstraint_RowReferenceConstraint :: Annotation ->
                                            String ->
                                            String ->
                                            (Maybe String) ->
                                            Cascade ->
                                            Cascade ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOfixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
                  {-# LINE 4986 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
                  {-# LINE 4991 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
                  {-# LINE 4996 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5001 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5006 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5011 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_RowConstraint_RowUniqueConstraint :: Annotation ->
                                         String ->
                                         T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOfixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  RowUniqueConstraint ann_ name_
                  {-# LINE 5027 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  RowUniqueConstraint ann_ name_
                  {-# LINE 5032 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  RowUniqueConstraint ann_ name_
                  {-# LINE 5037 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5042 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5047 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5052 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- RowConstraintList -------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : RowConstraint 
         child tl             : RowConstraintList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type RowConstraintList  = [(RowConstraint)]
-- cata
sem_RowConstraintList :: RowConstraintList  ->
                         T_RowConstraintList 
sem_RowConstraintList list  =
    (Prelude.foldr sem_RowConstraintList_Cons sem_RowConstraintList_Nil (Prelude.map sem_RowConstraint list) )
-- semantic domain
type T_RowConstraintList  = Catalog ->
                            IDEnv ->
                            LocalBindings ->
                            ( RowConstraintList,RowConstraintList,RowConstraintList)
data Inh_RowConstraintList  = Inh_RowConstraintList {cat_Inh_RowConstraintList :: Catalog,idenv_Inh_RowConstraintList :: IDEnv,lib_Inh_RowConstraintList :: LocalBindings}
data Syn_RowConstraintList  = Syn_RowConstraintList {annotatedTree_Syn_RowConstraintList :: RowConstraintList,fixedUpIdentifiersTree_Syn_RowConstraintList :: RowConstraintList,originalTree_Syn_RowConstraintList :: RowConstraintList}
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_RowConstraintList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraintList
              _lhsOfixedUpIdentifiersTree :: RowConstraintList
              _lhsOoriginalTree :: RowConstraintList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: RowConstraint
              _hdIfixedUpIdentifiersTree :: RowConstraint
              _hdIoriginalTree :: RowConstraint
              _tlIannotatedTree :: RowConstraintList
              _tlIfixedUpIdentifiersTree :: RowConstraintList
              _tlIoriginalTree :: RowConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5125 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 5130 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 5135 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5140 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5145 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5150 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5155 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 5160 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5165 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5170 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 5175 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5180 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraintList
              _lhsOfixedUpIdentifiersTree :: RowConstraintList
              _lhsOoriginalTree :: RowConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5198 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 5203 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5208 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5213 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5218 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5223 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- SQIdentifier ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         tbAnnotatedTree      : SQIdentifier 
         tbUType              : Maybe ([(String,Type)],[(String,Type)])
   alternatives:
      alternative SQIdentifier:
         child ann            : {Annotation}
         child is             : {[String]}
         visit 0:
            local tpe         : {E ([(String,Type)],[(String,Type)])}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data SQIdentifier  = SQIdentifier (Annotation) ([String]) 
                   deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SQIdentifier :: SQIdentifier  ->
                    T_SQIdentifier 
sem_SQIdentifier (SQIdentifier _ann _is )  =
    (sem_SQIdentifier_SQIdentifier _ann _is )
-- semantic domain
type T_SQIdentifier  = Catalog ->
                       IDEnv ->
                       LocalBindings ->
                       ( SQIdentifier,SQIdentifier,SQIdentifier,SQIdentifier,(Maybe ([(String,Type)],[(String,Type)])))
data Inh_SQIdentifier  = Inh_SQIdentifier {cat_Inh_SQIdentifier :: Catalog,idenv_Inh_SQIdentifier :: IDEnv,lib_Inh_SQIdentifier :: LocalBindings}
data Syn_SQIdentifier  = Syn_SQIdentifier {annotatedTree_Syn_SQIdentifier :: SQIdentifier,fixedUpIdentifiersTree_Syn_SQIdentifier :: SQIdentifier,originalTree_Syn_SQIdentifier :: SQIdentifier,tbAnnotatedTree_Syn_SQIdentifier :: SQIdentifier,tbUType_Syn_SQIdentifier :: Maybe ([(String,Type)],[(String,Type)])}
wrap_SQIdentifier :: T_SQIdentifier  ->
                     Inh_SQIdentifier  ->
                     Syn_SQIdentifier 
wrap_SQIdentifier sem (Inh_SQIdentifier _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_SQIdentifier _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOtbAnnotatedTree _lhsOtbUType ))
sem_SQIdentifier_SQIdentifier :: Annotation ->
                                 ([String]) ->
                                 T_SQIdentifier 
sem_SQIdentifier_SQIdentifier ann_ is_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _tpe :: (E ([(String,Type)],[(String,Type)]))
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _lhsOtbAnnotatedTree :: SQIdentifier
              _lhsOannotatedTree :: SQIdentifier
              _lhsOfixedUpIdentifiersTree :: SQIdentifier
              _lhsOoriginalTree :: SQIdentifier
              -- "./TypeChecking/Misc.ag"(line 67, column 9)
              _tpe =
                  {-# LINE 67 "./TypeChecking/Misc.ag" #-}
                  catCompositeAttrsPair _lhsIcat relationComposites (last is_)
                  {-# LINE 5287 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 68, column 9)
              _lhsOtbUType =
                  {-# LINE 68 "./TypeChecking/Misc.ag" #-}
                  either (const Nothing) Just _tpe
                  {-# LINE 5292 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 69, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 69 "./TypeChecking/Misc.ag" #-}
                  updateAnnotation
                    (\a -> a {errs = errs a ++ tes _tpe    }) _backTree
                  {-# LINE 5298 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 72, column 9)
              _backTree =
                  {-# LINE 72 "./TypeChecking/Misc.ag" #-}
                  SQIdentifier ann_ is_
                  {-# LINE 5303 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  SQIdentifier ann_ is_
                  {-# LINE 5308 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SQIdentifier ann_ is_
                  {-# LINE 5313 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  SQIdentifier ann_ is_
                  {-# LINE 5318 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5323 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5328 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5333 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType)))
-- ScalarExpr --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         expectedType         : Maybe Type
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         uType                : Maybe Type
   alternatives:
      alternative BooleanLit:
         child ann            : {Annotation}
         child b              : {Bool}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Case:
         child ann            : {Annotation}
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local whenTypes   : _
            local thenTypes   : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CaseSimple:
         child ann            : {Annotation}
         child value          : ScalarExpr 
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local whenTypes   : _
            local thenTypes   : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Cast:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         child tn             : TypeName 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Exists:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Extract:
         child ann            : {Annotation}
         child field          : {ExtractField}
         child e              : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative FloatLit:
         child ann            : {Annotation}
         child d              : {Double}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative FunCall:
         child ann            : {Annotation}
         child funName        : {String}
         child args           : ScalarExprList 
         visit 0:
            local _tup1       : _
            local tpe         : {Et}
            local prototype   : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Identifier:
         child ann            : {Annotation}
         child i              : {String}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative InPredicate:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         child i              : {Bool}
         child list           : InList 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative IntegerLit:
         child ann            : {Annotation}
         child i              : {Integer}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Interval:
         child ann            : {Annotation}
         child value          : {String}
         child field          : {IntervalField}
         child prec           : {Maybe Int}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative LiftOperator:
         child ann            : {Annotation}
         child oper           : {String}
         child flav           : {LiftFlavour}
         child args           : ScalarExprList 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative NullLit:
         child ann            : {Annotation}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Placeholder:
         child ann            : {Annotation}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative PositionalArg:
         child ann            : {Annotation}
         child p              : {Integer}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative QIdentifier:
         child ann            : {Annotation}
         child qual           : ScalarExpr 
         child i              : {String}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local qid         : {Maybe String}
            local backTree    : _
            local qAnnTreeNoUnrec : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative ScalarSubQuery:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative StringLit:
         child ann            : {Annotation}
         child value          : {String}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative TypedStringLit:
         child ann            : {Annotation}
         child tn             : TypeName 
         child value          : {String}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative WindowFn:
         child ann            : {Annotation}
         child fn             : ScalarExpr 
         child partitionBy    : ScalarExprList 
         child orderBy        : ScalarExprList 
         child dir            : {Direction}
         child frm            : {FrameClause}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data ScalarExpr  = BooleanLit (Annotation) (Bool) 
                 | Case (Annotation) (CaseScalarExprListScalarExprPairList) (MaybeScalarExpr) 
                 | CaseSimple (Annotation) (ScalarExpr) (CaseScalarExprListScalarExprPairList) (MaybeScalarExpr) 
                 | Cast (Annotation) (ScalarExpr) (TypeName) 
                 | Exists (Annotation) (QueryExpr) 
                 | Extract (Annotation) (ExtractField) (ScalarExpr) 
                 | FloatLit (Annotation) (Double) 
                 | FunCall (Annotation) (String) (ScalarExprList) 
                 | Identifier (Annotation) (String) 
                 | InPredicate (Annotation) (ScalarExpr) (Bool) (InList) 
                 | IntegerLit (Annotation) (Integer) 
                 | Interval (Annotation) (String) (IntervalField) (Maybe Int) 
                 | LiftOperator (Annotation) (String) (LiftFlavour) (ScalarExprList) 
                 | NullLit (Annotation) 
                 | Placeholder (Annotation) 
                 | PositionalArg (Annotation) (Integer) 
                 | QIdentifier (Annotation) (ScalarExpr) (String) 
                 | ScalarSubQuery (Annotation) (QueryExpr) 
                 | StringLit (Annotation) (String) 
                 | TypedStringLit (Annotation) (TypeName) (String) 
                 | WindowFn (Annotation) (ScalarExpr) (ScalarExprList) (ScalarExprList) (Direction) (FrameClause) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_ScalarExpr :: ScalarExpr  ->
                  T_ScalarExpr 
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
sem_ScalarExpr (FloatLit _ann _d )  =
    (sem_ScalarExpr_FloatLit _ann _d )
sem_ScalarExpr (FunCall _ann _funName _args )  =
    (sem_ScalarExpr_FunCall _ann _funName (sem_ScalarExprList _args ) )
sem_ScalarExpr (Identifier _ann _i )  =
    (sem_ScalarExpr_Identifier _ann _i )
sem_ScalarExpr (InPredicate _ann _expr _i _list )  =
    (sem_ScalarExpr_InPredicate _ann (sem_ScalarExpr _expr ) _i (sem_InList _list ) )
sem_ScalarExpr (IntegerLit _ann _i )  =
    (sem_ScalarExpr_IntegerLit _ann _i )
sem_ScalarExpr (Interval _ann _value _field _prec )  =
    (sem_ScalarExpr_Interval _ann _value _field _prec )
sem_ScalarExpr (LiftOperator _ann _oper _flav _args )  =
    (sem_ScalarExpr_LiftOperator _ann _oper _flav (sem_ScalarExprList _args ) )
sem_ScalarExpr (NullLit _ann )  =
    (sem_ScalarExpr_NullLit _ann )
sem_ScalarExpr (Placeholder _ann )  =
    (sem_ScalarExpr_Placeholder _ann )
sem_ScalarExpr (PositionalArg _ann _p )  =
    (sem_ScalarExpr_PositionalArg _ann _p )
sem_ScalarExpr (QIdentifier _ann _qual _i )  =
    (sem_ScalarExpr_QIdentifier _ann (sem_ScalarExpr _qual ) _i )
sem_ScalarExpr (ScalarSubQuery _ann _sel )  =
    (sem_ScalarExpr_ScalarSubQuery _ann (sem_QueryExpr _sel ) )
sem_ScalarExpr (StringLit _ann _value )  =
    (sem_ScalarExpr_StringLit _ann _value )
sem_ScalarExpr (TypedStringLit _ann _tn _value )  =
    (sem_ScalarExpr_TypedStringLit _ann (sem_TypeName _tn ) _value )
sem_ScalarExpr (WindowFn _ann _fn _partitionBy _orderBy _dir _frm )  =
    (sem_ScalarExpr_WindowFn _ann (sem_ScalarExpr _fn ) (sem_ScalarExprList _partitionBy ) (sem_ScalarExprList _orderBy ) _dir _frm )
-- semantic domain
type T_ScalarExpr  = Catalog ->
                     (Maybe Type) ->
                     IDEnv ->
                     LocalBindings ->
                     ( ScalarExpr,ScalarExpr,ScalarExpr,(Maybe Type))
data Inh_ScalarExpr  = Inh_ScalarExpr {cat_Inh_ScalarExpr :: Catalog,expectedType_Inh_ScalarExpr :: Maybe Type,idenv_Inh_ScalarExpr :: IDEnv,lib_Inh_ScalarExpr :: LocalBindings}
data Syn_ScalarExpr  = Syn_ScalarExpr {annotatedTree_Syn_ScalarExpr :: ScalarExpr,fixedUpIdentifiersTree_Syn_ScalarExpr :: ScalarExpr,originalTree_Syn_ScalarExpr :: ScalarExpr,uType_Syn_ScalarExpr :: Maybe Type}
wrap_ScalarExpr :: T_ScalarExpr  ->
                   Inh_ScalarExpr  ->
                   Syn_ScalarExpr 
wrap_ScalarExpr sem (Inh_ScalarExpr _lhsIcat _lhsIexpectedType _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedType _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExpr _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOuType ))
sem_ScalarExpr_BooleanLit :: Annotation ->
                             Bool ->
                             T_ScalarExpr 
sem_ScalarExpr_BooleanLit ann_ b_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 5685 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 5690 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 5695 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 87, column 19)
              _tpe =
                  {-# LINE 87 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Right typeBool
                  {-# LINE 5700 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 99, column 9)
              _backTree =
                  {-# LINE 99 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 5705 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 5710 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 5715 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 5720 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5725 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5730 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_Case :: Annotation ->
                       T_CaseScalarExprListScalarExprPairList  ->
                       T_MaybeScalarExpr  ->
                       T_ScalarExpr 
sem_ScalarExpr_Case ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _casesOcat :: Catalog
              _casesOidenv :: IDEnv
              _casesOlib :: LocalBindings
              _elsOcat :: Catalog
              _elsOidenv :: IDEnv
              _elsOlib :: LocalBindings
              _casesIannotatedTree :: CaseScalarExprListScalarExprPairList
              _casesIfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _casesIoriginalTree :: CaseScalarExprListScalarExprPairList
              _casesIthenTypes :: ([Maybe Type])
              _casesIwhenTypes :: ([[Maybe Type]])
              _elsIannotatedTree :: MaybeScalarExpr
              _elsIfixedUpIdentifiersTree :: MaybeScalarExpr
              _elsIoriginalTree :: MaybeScalarExpr
              _elsIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 5771 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 5776 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 5781 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 210, column 9)
              _whenTypes =
                  {-# LINE 210 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _casesIwhenTypes
                  {-# LINE 5786 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 211, column 9)
              _thenTypes =
                  {-# LINE 211 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIuType
                  {-# LINE 5791 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 215, column 9)
              _tpe =
                  {-# LINE 215 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  do
                  wt <- mapM lmt $ concat _whenTypes
                  errorWhen (any (/= typeBool) wt)
                      [WrongTypes typeBool wt]
                  tt <- mapM lmt _thenTypes
                  resolveResultSetType _lhsIcat tt
                  {-# LINE 5801 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 223, column 9)
              _backTree =
                  {-# LINE 223 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 5806 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 5811 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Case ann_ _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
                  {-# LINE 5816 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Case ann_ _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 5821 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5826 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5831 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5836 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 5841 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5846 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5851 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 5856 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5861 "AstInternal.hs" #-}
              ( _casesIannotatedTree,_casesIfixedUpIdentifiersTree,_casesIoriginalTree,_casesIthenTypes,_casesIwhenTypes) =
                  (cases_ _casesOcat _casesOidenv _casesOlib )
              ( _elsIannotatedTree,_elsIfixedUpIdentifiersTree,_elsIoriginalTree,_elsIuType) =
                  (els_ _elsOcat _elsOidenv _elsOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_CaseSimple :: Annotation ->
                             T_ScalarExpr  ->
                             T_CaseScalarExprListScalarExprPairList  ->
                             T_MaybeScalarExpr  ->
                             T_ScalarExpr 
sem_ScalarExpr_CaseSimple ann_ value_ cases_ els_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _valueOcat :: Catalog
              _valueOexpectedType :: (Maybe Type)
              _valueOidenv :: IDEnv
              _valueOlib :: LocalBindings
              _casesOcat :: Catalog
              _casesOidenv :: IDEnv
              _casesOlib :: LocalBindings
              _elsOcat :: Catalog
              _elsOidenv :: IDEnv
              _elsOlib :: LocalBindings
              _valueIannotatedTree :: ScalarExpr
              _valueIfixedUpIdentifiersTree :: ScalarExpr
              _valueIoriginalTree :: ScalarExpr
              _valueIuType :: (Maybe Type)
              _casesIannotatedTree :: CaseScalarExprListScalarExprPairList
              _casesIfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _casesIoriginalTree :: CaseScalarExprListScalarExprPairList
              _casesIthenTypes :: ([Maybe Type])
              _casesIwhenTypes :: ([[Maybe Type]])
              _elsIannotatedTree :: MaybeScalarExpr
              _elsIfixedUpIdentifiersTree :: MaybeScalarExpr
              _elsIoriginalTree :: MaybeScalarExpr
              _elsIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 5915 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 5920 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 5925 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 210, column 9)
              _whenTypes =
                  {-# LINE 210 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _casesIwhenTypes
                  {-# LINE 5930 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 211, column 9)
              _thenTypes =
                  {-# LINE 211 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIuType
                  {-# LINE 5935 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 228, column 9)
              _tpe =
                  {-# LINE 228 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  do
                  wt <- mapM lmt $ concat _whenTypes
                  vt <- lmt _valueIuType
                  _ <- resolveResultSetType _lhsIcat (vt : wt)
                  tt <- mapM lmt _thenTypes
                  resolveResultSetType _lhsIcat tt
                  {-# LINE 5945 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 235, column 9)
              _backTree =
                  {-# LINE 235 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  CaseSimple ann_
                             _valueIannotatedTree
                             _casesIannotatedTree
                             _elsIannotatedTree
                  {-# LINE 5953 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 5958 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CaseSimple ann_ _valueIfixedUpIdentifiersTree _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
                  {-# LINE 5963 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIoriginalTree _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 5968 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5973 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5978 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5983 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOexpectedType =
                  {-# LINE 67 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 5988 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 5993 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5998 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6003 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6008 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6013 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6018 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6023 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6028 "AstInternal.hs" #-}
              ( _valueIannotatedTree,_valueIfixedUpIdentifiersTree,_valueIoriginalTree,_valueIuType) =
                  (value_ _valueOcat _valueOexpectedType _valueOidenv _valueOlib )
              ( _casesIannotatedTree,_casesIfixedUpIdentifiersTree,_casesIoriginalTree,_casesIthenTypes,_casesIwhenTypes) =
                  (cases_ _casesOcat _casesOidenv _casesOlib )
              ( _elsIannotatedTree,_elsIfixedUpIdentifiersTree,_elsIoriginalTree,_elsIuType) =
                  (els_ _elsOcat _elsOidenv _elsOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_Cast :: Annotation ->
                       T_ScalarExpr  ->
                       T_TypeName  ->
                       T_ScalarExpr 
sem_ScalarExpr_Cast ann_ expr_ tn_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _exprOcat :: Catalog
              _exprOexpectedType :: (Maybe Type)
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _tnOcat :: Catalog
              _tnOidenv :: IDEnv
              _tnOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              _tnIannotatedTree :: TypeName
              _tnIfixedUpIdentifiersTree :: TypeName
              _tnInamedType :: (Maybe Type)
              _tnIoriginalTree :: TypeName
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6075 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6080 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6085 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 116, column 12)
              _tpe =
                  {-# LINE 116 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  lmt _tnInamedType
                  {-# LINE 6090 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 117, column 12)
              _backTree =
                  {-# LINE 117 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 6095 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 6100 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Cast ann_ _exprIfixedUpIdentifiersTree _tnIfixedUpIdentifiersTree
                  {-# LINE 6105 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Cast ann_ _exprIoriginalTree _tnIoriginalTree
                  {-# LINE 6110 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6115 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6120 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6125 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOexpectedType =
                  {-# LINE 67 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 6130 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6135 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6140 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6145 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6150 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6155 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
              ( _tnIannotatedTree,_tnIfixedUpIdentifiersTree,_tnInamedType,_tnIoriginalTree) =
                  (tn_ _tnOcat _tnOidenv _tnOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_Exists :: Annotation ->
                         T_QueryExpr  ->
                         T_ScalarExpr 
sem_ScalarExpr_Exists ann_ sel_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6194 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6199 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6204 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 289, column 9)
              _tpe =
                  {-# LINE 289 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Right typeBool
                  {-# LINE 6209 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 290, column 9)
              _backTree =
                  {-# LINE 290 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 6214 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 173, column 29)
              _selOexpectedTypes =
                  {-# LINE 173 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 6219 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 6224 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Exists ann_ _selIfixedUpIdentifiersTree
                  {-# LINE 6229 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Exists ann_ _selIoriginalTree
                  {-# LINE 6234 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6239 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6244 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6249 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6254 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6259 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_Extract :: Annotation ->
                          ExtractField ->
                          T_ScalarExpr  ->
                          T_ScalarExpr 
sem_ScalarExpr_Extract ann_ field_ e_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _lhsOuType :: (Maybe Type)
              _eOcat :: Catalog
              _eOexpectedType :: (Maybe Type)
              _eOidenv :: IDEnv
              _eOlib :: LocalBindings
              _eIannotatedTree :: ScalarExpr
              _eIfixedUpIdentifiersTree :: ScalarExpr
              _eIoriginalTree :: ScalarExpr
              _eIuType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Extract ann_ field_ _eIannotatedTree
                  {-# LINE 6288 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Extract ann_ field_ _eIfixedUpIdentifiersTree
                  {-# LINE 6293 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Extract ann_ field_ _eIoriginalTree
                  {-# LINE 6298 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6303 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6308 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6313 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOuType =
                  {-# LINE 39 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _eIuType
                  {-# LINE 6318 "AstInternal.hs" #-}
              -- copy rule (down)
              _eOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6323 "AstInternal.hs" #-}
              -- copy rule (down)
              _eOexpectedType =
                  {-# LINE 67 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 6328 "AstInternal.hs" #-}
              -- copy rule (down)
              _eOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6333 "AstInternal.hs" #-}
              -- copy rule (down)
              _eOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6338 "AstInternal.hs" #-}
              ( _eIannotatedTree,_eIfixedUpIdentifiersTree,_eIoriginalTree,_eIuType) =
                  (e_ _eOcat _eOexpectedType _eOidenv _eOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_FloatLit :: Annotation ->
                           Double ->
                           T_ScalarExpr 
sem_ScalarExpr_FloatLit ann_ d_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6365 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6370 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6375 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 86, column 17)
              _tpe =
                  {-# LINE 86 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Right typeNumeric
                  {-# LINE 6380 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 97, column 9)
              _backTree =
                  {-# LINE 97 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 6385 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 6390 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 6395 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 6400 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6405 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6410 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_FunCall :: Annotation ->
                          String ->
                          T_ScalarExprList  ->
                          T_ScalarExpr 
sem_ScalarExpr_FunCall ann_ funName_ args_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _argsOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _argsOcat :: Catalog
              _argsOidenv :: IDEnv
              _argsOlib :: LocalBindings
              _argsIannotatedTree :: ScalarExprList
              _argsIfixedUpIdentifiersTree :: ScalarExprList
              _argsIoriginalTree :: ScalarExprList
              _argsIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6443 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6448 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 130, column 9)
              __tup1 =
                  {-# LINE 130 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  either (\e -> (Left e, Nothing)) id $ do
                  args <- mapM lmt _argsIuType
                  efp <- findCallMatch _lhsIcat
                                       funName_
                                       args
                  let (_,_,r,_) = efp
                  return (Right r, Just efp)
                  {-# LINE 6459 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 130, column 9)
              (_tpe,_) =
                  {-# LINE 130 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  __tup1
                  {-# LINE 6464 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 130, column 9)
              (_,_prototype) =
                  {-# LINE 131 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  __tup1
                  {-# LINE 6469 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 140, column 9)
              _backTree =
                  {-# LINE 140 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 6474 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 132, column 9)
              _argsOexpectedTypes =
                  {-# LINE 132 "./TypeChecking/ParameterizedStatements.ag" #-}
                  maybe [] id $
                  case (funName_,_lhsIexpectedType) of
                    ("!rowctor", Just (AnonymousRecordType ts)) -> return $ map Just ts
                    _ -> do
                         (_,t,_,_) <- _prototype
                         return $ map Just t
                  {-# LINE 6484 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 6489 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  FunCall ann_ funName_ _argsIfixedUpIdentifiersTree
                  {-# LINE 6494 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIoriginalTree
                  {-# LINE 6499 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6504 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6509 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6514 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6519 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6524 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsIfixedUpIdentifiersTree,_argsIoriginalTree,_argsIuType) =
                  (args_ _argsOcat _argsOexpectedTypes _argsOidenv _argsOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_Identifier :: Annotation ->
                             String ->
                             T_ScalarExpr 
sem_ScalarExpr_Identifier ann_ i_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 131, column 9)
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 131 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  case qualifyID _lhsIidenv i_ of
                    Nothing -> Identifier ann_ i_
                    Just (t,i) -> QIdentifier ann_ (Identifier ann_ t) i
                  {-# LINE 6548 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6558 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6563 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6568 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 248, column 9)
              _tpe =
                  {-# LINE 248 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  case lookupLocalBinding _lhsIlib "" i_ of
                                        Right Nothing -> Left []
                                        Right (Just t) -> Right t
                                        Left e -> Left e
                  {-# LINE 6576 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 253, column 9)
              _backTree =
                  {-# LINE 253 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 6581 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 6586 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 6591 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 6596 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6601 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_InPredicate :: Annotation ->
                              T_ScalarExpr  ->
                              Bool ->
                              T_InList  ->
                              T_ScalarExpr 
sem_ScalarExpr_InPredicate ann_ expr_ i_ list_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _exprOcat :: Catalog
              _exprOexpectedType :: (Maybe Type)
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _listOcat :: Catalog
              _listOidenv :: IDEnv
              _listOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              _listIannotatedTree :: InList
              _listIfixedUpIdentifiersTree :: InList
              _listIlistType :: (Either [TypeError] Type)
              _listIoriginalTree :: InList
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6643 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6648 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6653 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 317, column 9)
              _tpe =
                  {-# LINE 317 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  do
                  lt <- _listIlistType
                  expt <- lmt _exprIuType
                  _ <- resolveResultSetType _lhsIcat [expt, lt]
                  return typeBool
                  {-# LINE 6662 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 322, column 9)
              _backTree =
                  {-# LINE 322 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  InPredicate ann_
                              _exprIannotatedTree
                              i_
                              _listIannotatedTree
                  {-# LINE 6670 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                  {-# LINE 6675 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  InPredicate ann_ _exprIfixedUpIdentifiersTree i_ _listIfixedUpIdentifiersTree
                  {-# LINE 6680 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  InPredicate ann_ _exprIoriginalTree i_ _listIoriginalTree
                  {-# LINE 6685 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6690 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6695 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6700 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOexpectedType =
                  {-# LINE 67 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 6705 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6710 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6715 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6720 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6725 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6730 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
              ( _listIannotatedTree,_listIfixedUpIdentifiersTree,_listIlistType,_listIoriginalTree) =
                  (list_ _listOcat _listOidenv _listOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_IntegerLit :: Annotation ->
                             Integer ->
                             T_ScalarExpr 
sem_ScalarExpr_IntegerLit ann_ i_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6759 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6764 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6769 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 84, column 19)
              _tpe =
                  {-# LINE 84 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Right typeInt
                  {-# LINE 6774 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 93, column 9)
              _backTree =
                  {-# LINE 93 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 6779 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 6784 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 6789 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 6794 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6799 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6804 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_Interval :: Annotation ->
                           String ->
                           IntervalField ->
                           (Maybe Int) ->
                           T_ScalarExpr 
sem_ScalarExpr_Interval ann_ value_ field_ prec_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6831 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6836 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6841 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 104, column 16)
              _tpe =
                  {-# LINE 104 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Right $ ScalarType "interval"
                  {-# LINE 6846 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 105, column 16)
              _backTree =
                  {-# LINE 105 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Interval ann_ value_ field_ prec_
                  {-# LINE 6851 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Interval ann_ value_ field_ prec_
                  {-# LINE 6856 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Interval ann_ value_ field_ prec_
                  {-# LINE 6861 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Interval ann_ value_ field_ prec_
                  {-# LINE 6866 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6871 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6876 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_LiftOperator :: Annotation ->
                               String ->
                               LiftFlavour ->
                               T_ScalarExprList  ->
                               T_ScalarExpr 
sem_ScalarExpr_LiftOperator ann_ oper_ flav_ args_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _argsOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _argsOcat :: Catalog
              _argsOidenv :: IDEnv
              _argsOlib :: LocalBindings
              _argsIannotatedTree :: ScalarExprList
              _argsIfixedUpIdentifiersTree :: ScalarExprList
              _argsIoriginalTree :: ScalarExprList
              _argsIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6911 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6916 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6921 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 163, column 9)
              _tpe =
                  {-# LINE 163 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  do
                  at <- mapM lmt _argsIuType
                  errorWhen (length at /= 2)
                            [AnyAllError $ "must have two args, got " ++ show at]
                  let [aType,bType] = at
                  errorWhen (not $ isArrayType bType)
                            [AnyAllError $ "second arg must be array, got " ++ show at]
                  elemType <- unwrapArray $ bType
                  resType <- fmap (\(_,_,r,_) -> r) $ findCallMatch _lhsIcat
                                                                    oper_
                                                                    [aType,elemType]
                  errorWhen (resType /= typeBool)
                            [AnyAllError $ "operator must have bool return, got " ++ show resType]
                  return resType
                  {-# LINE 6939 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 177, column 9)
              _backTree =
                  {-# LINE 177 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIannotatedTree
                  {-# LINE 6944 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 140, column 9)
              _argsOexpectedTypes =
                  {-# LINE 140 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 6949 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIannotatedTree
                  {-# LINE 6954 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIfixedUpIdentifiersTree
                  {-# LINE 6959 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIoriginalTree
                  {-# LINE 6964 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6969 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6974 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6979 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6984 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6989 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsIfixedUpIdentifiersTree,_argsIoriginalTree,_argsIuType) =
                  (args_ _argsOcat _argsOexpectedTypes _argsOidenv _argsOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_NullLit :: Annotation ->
                          T_ScalarExpr 
sem_ScalarExpr_NullLit ann_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7015 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7020 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7025 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 89, column 16)
              _tpe =
                  {-# LINE 89 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Right UnknownType
                  {-# LINE 7030 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 101, column 9)
              _backTree =
                  {-# LINE 101 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  NullLit ann_
                  {-# LINE 7035 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 7040 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  NullLit ann_
                  {-# LINE 7045 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 7050 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7055 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7060 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_Placeholder :: Annotation ->
                              T_ScalarExpr 
sem_ScalarExpr_Placeholder ann_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7084 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7089 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7094 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 283, column 9)
              _tpe =
                  {-# LINE 283 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Right UnknownType
                  {-# LINE 7099 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 284, column 9)
              _backTree =
                  {-# LINE 284 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Placeholder ann_
                  {-# LINE 7104 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Placeholder ann_
                  {-# LINE 7109 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Placeholder ann_
                  {-# LINE 7114 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Placeholder ann_
                  {-# LINE 7119 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7124 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7129 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_PositionalArg :: Annotation ->
                                Integer ->
                                T_ScalarExpr 
sem_ScalarExpr_PositionalArg ann_ p_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7154 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7159 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7164 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 277, column 9)
              _tpe =
                  {-# LINE 277 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  unwrapLookup <$> lbLookupID _lhsIlib ['$':show p_]
                  {-# LINE 7169 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 278, column 9)
              _backTree =
                  {-# LINE 278 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 7174 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 7179 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 7184 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 7189 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7194 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7199 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_QIdentifier :: Annotation ->
                              T_ScalarExpr  ->
                              String ->
                              T_ScalarExpr 
sem_ScalarExpr_QIdentifier ann_ qual_ i_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _qid :: (Maybe String)
              _lhsOoriginalTree :: ScalarExpr
              _qualOcat :: Catalog
              _qualOexpectedType :: (Maybe Type)
              _qualOidenv :: IDEnv
              _qualOlib :: LocalBindings
              _qualIannotatedTree :: ScalarExpr
              _qualIfixedUpIdentifiersTree :: ScalarExpr
              _qualIoriginalTree :: ScalarExpr
              _qualIuType :: (Maybe Type)
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 137, column 9)
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 137 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  QIdentifier ann_ _qualIoriginalTree i_
                  {-# LINE 7229 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7239 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7244 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7249 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 255, column 9)
              _tpe =
                  {-# LINE 255 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  case _qid     of
                            Nothing -> Left [InternalError "dot selection not implemented"]
                            Just q -> case lookupLocalBinding _lhsIlib q i_ of
                                        Right Nothing -> Left []
                                        Right (Just t) -> Right t
                                        Left e -> Left e
                  {-# LINE 7259 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 268, column 9)
              _qid =
                  {-# LINE 268 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  case _backTree     of
                     QIdentifier _ (Identifier _ q) _ -> Just q
                     _ -> Nothing
                  {-# LINE 7266 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 271, column 9)
              _backTree =
                  {-# LINE 271 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  QIdentifier ann_ _qAnnTreeNoUnrec     i_
                  {-# LINE 7271 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 273, column 9)
              _qAnnTreeNoUnrec =
                  {-# LINE 273 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation (\a -> a {errs = []}) _qualIannotatedTree
                  {-# LINE 7276 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  QIdentifier ann_ _qualIannotatedTree i_
                  {-# LINE 7281 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  QIdentifier ann_ _qualIfixedUpIdentifiersTree i_
                  {-# LINE 7286 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  QIdentifier ann_ _qualIoriginalTree i_
                  {-# LINE 7291 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7296 "AstInternal.hs" #-}
              -- copy rule (down)
              _qualOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7301 "AstInternal.hs" #-}
              -- copy rule (down)
              _qualOexpectedType =
                  {-# LINE 67 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 7306 "AstInternal.hs" #-}
              -- copy rule (down)
              _qualOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7311 "AstInternal.hs" #-}
              -- copy rule (down)
              _qualOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7316 "AstInternal.hs" #-}
              ( _qualIannotatedTree,_qualIfixedUpIdentifiersTree,_qualIoriginalTree,_qualIuType) =
                  (qual_ _qualOcat _qualOexpectedType _qualOidenv _qualOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_ScalarSubQuery :: Annotation ->
                                 T_QueryExpr  ->
                                 T_ScalarExpr 
sem_ScalarExpr_ScalarSubQuery ann_ sel_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7353 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7358 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7363 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 301, column 9)
              _tpe =
                  {-# LINE 301 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  do
                  selType <- lmt (map snd <$> _selIuType)
                  case length selType of
                    0 -> Left [InternalError "no columns in scalar subquery?"]
                    1 -> Right $ head selType
                    _ -> Right $ AnonymousRecordType selType
                  {-# LINE 7373 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 309, column 9)
              _backTree =
                  {-# LINE 309 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 7378 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 173, column 29)
              _selOexpectedTypes =
                  {-# LINE 173 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 7383 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 7388 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ScalarSubQuery ann_ _selIfixedUpIdentifiersTree
                  {-# LINE 7393 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIoriginalTree
                  {-# LINE 7398 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7403 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7408 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7413 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7418 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7423 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_StringLit :: Annotation ->
                            String ->
                            T_ScalarExpr 
sem_ScalarExpr_StringLit ann_ value_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7450 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7455 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7460 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 85, column 18)
              _tpe =
                  {-# LINE 85 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Right UnknownType
                  {-# LINE 7465 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 95, column 9)
              _backTree =
                  {-# LINE 95 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 7470 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 7475 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 7480 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 7485 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7490 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7495 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_TypedStringLit :: Annotation ->
                                 T_TypeName  ->
                                 String ->
                                 T_ScalarExpr 
sem_ScalarExpr_TypedStringLit ann_ tn_ value_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _tnOcat :: Catalog
              _tnOidenv :: IDEnv
              _tnOlib :: LocalBindings
              _tnIannotatedTree :: TypeName
              _tnIfixedUpIdentifiersTree :: TypeName
              _tnInamedType :: (Maybe Type)
              _tnIoriginalTree :: TypeName
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7528 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7533 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7538 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 121, column 10)
              _tpe =
                  {-# LINE 121 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  lmt _tnInamedType
                  {-# LINE 7543 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 122, column 10)
              _backTree =
                  {-# LINE 122 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  TypedStringLit ann_ _tnIannotatedTree value_
                  {-# LINE 7548 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  TypedStringLit ann_ _tnIannotatedTree value_
                  {-# LINE 7553 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  TypedStringLit ann_ _tnIfixedUpIdentifiersTree value_
                  {-# LINE 7558 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  TypedStringLit ann_ _tnIoriginalTree value_
                  {-# LINE 7563 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7568 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7573 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7578 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7583 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7588 "AstInternal.hs" #-}
              ( _tnIannotatedTree,_tnIfixedUpIdentifiersTree,_tnInamedType,_tnIoriginalTree) =
                  (tn_ _tnOcat _tnOidenv _tnOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExpr_WindowFn :: Annotation ->
                           T_ScalarExpr  ->
                           T_ScalarExprList  ->
                           T_ScalarExprList  ->
                           Direction ->
                           FrameClause ->
                           T_ScalarExpr 
sem_ScalarExpr_WindowFn ann_ fn_ partitionBy_ orderBy_ dir_ frm_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _partitionByOexpectedTypes :: ([Maybe Type])
              _orderByOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _fnOcat :: Catalog
              _fnOexpectedType :: (Maybe Type)
              _fnOidenv :: IDEnv
              _fnOlib :: LocalBindings
              _partitionByOcat :: Catalog
              _partitionByOidenv :: IDEnv
              _partitionByOlib :: LocalBindings
              _orderByOcat :: Catalog
              _orderByOidenv :: IDEnv
              _orderByOlib :: LocalBindings
              _fnIannotatedTree :: ScalarExpr
              _fnIfixedUpIdentifiersTree :: ScalarExpr
              _fnIoriginalTree :: ScalarExpr
              _fnIuType :: (Maybe Type)
              _partitionByIannotatedTree :: ScalarExprList
              _partitionByIfixedUpIdentifiersTree :: ScalarExprList
              _partitionByIoriginalTree :: ScalarExprList
              _partitionByIuType :: ([Maybe Type])
              _orderByIannotatedTree :: ScalarExprList
              _orderByIfixedUpIdentifiersTree :: ScalarExprList
              _orderByIoriginalTree :: ScalarExprList
              _orderByIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 23, column 9)
              _lhsOannotatedTree =
                  {-# LINE 23 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7643 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 37, column 9)
              _prototype =
                  {-# LINE 37 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7648 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 45, column 9)
              _lhsOuType =
                  {-# LINE 45 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7653 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 144, column 9)
              _tpe =
                  {-# LINE 144 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  lmt _fnIuType
                  {-# LINE 7658 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 145, column 9)
              _backTree =
                  {-# LINE 145 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  WindowFn ann_
                           _fnIannotatedTree
                           _partitionByIannotatedTree
                           _orderByIannotatedTree
                           dir_
                           frm_
                  {-# LINE 7668 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 142, column 9)
              _partitionByOexpectedTypes =
                  {-# LINE 142 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 7673 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 143, column 9)
              _orderByOexpectedTypes =
                  {-# LINE 143 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 7678 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree dir_ frm_
                  {-# LINE 7683 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  WindowFn ann_ _fnIfixedUpIdentifiersTree _partitionByIfixedUpIdentifiersTree _orderByIfixedUpIdentifiersTree dir_ frm_
                  {-# LINE 7688 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  WindowFn ann_ _fnIoriginalTree _partitionByIoriginalTree _orderByIoriginalTree dir_ frm_
                  {-# LINE 7693 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7698 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7703 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7708 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOexpectedType =
                  {-# LINE 67 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 7713 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7718 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7723 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7728 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7733 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7738 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7743 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7748 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7753 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIfixedUpIdentifiersTree,_fnIoriginalTree,_fnIuType) =
                  (fn_ _fnOcat _fnOexpectedType _fnOidenv _fnOlib )
              ( _partitionByIannotatedTree,_partitionByIfixedUpIdentifiersTree,_partitionByIoriginalTree,_partitionByIuType) =
                  (partitionBy_ _partitionByOcat _partitionByOexpectedTypes _partitionByOidenv _partitionByOlib )
              ( _orderByIannotatedTree,_orderByIfixedUpIdentifiersTree,_orderByIoriginalTree,_orderByIuType) =
                  (orderBy_ _orderByOcat _orderByOexpectedTypes _orderByOidenv _orderByOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
-- ScalarExprDirectionPair -------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExpr 
         child x2             : {Direction}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type ScalarExprDirectionPair  = ( (ScalarExpr),(Direction))
-- cata
sem_ScalarExprDirectionPair :: ScalarExprDirectionPair  ->
                               T_ScalarExprDirectionPair 
sem_ScalarExprDirectionPair ( x1,x2)  =
    (sem_ScalarExprDirectionPair_Tuple (sem_ScalarExpr x1 ) x2 )
-- semantic domain
type T_ScalarExprDirectionPair  = Catalog ->
                                  IDEnv ->
                                  LocalBindings ->
                                  ( ScalarExprDirectionPair,ScalarExprDirectionPair,ScalarExprDirectionPair)
data Inh_ScalarExprDirectionPair  = Inh_ScalarExprDirectionPair {cat_Inh_ScalarExprDirectionPair :: Catalog,idenv_Inh_ScalarExprDirectionPair :: IDEnv,lib_Inh_ScalarExprDirectionPair :: LocalBindings}
data Syn_ScalarExprDirectionPair  = Syn_ScalarExprDirectionPair {annotatedTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair,fixedUpIdentifiersTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair,originalTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair}
wrap_ScalarExprDirectionPair :: T_ScalarExprDirectionPair  ->
                                Inh_ScalarExprDirectionPair  ->
                                Syn_ScalarExprDirectionPair 
wrap_ScalarExprDirectionPair sem (Inh_ScalarExprDirectionPair _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprDirectionPair _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ScalarExprDirectionPair_Tuple :: T_ScalarExpr  ->
                                     Direction ->
                                     T_ScalarExprDirectionPair 
sem_ScalarExprDirectionPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _x1OexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: ScalarExprDirectionPair
              _lhsOfixedUpIdentifiersTree :: ScalarExprDirectionPair
              _lhsOoriginalTree :: ScalarExprDirectionPair
              _x1Ocat :: Catalog
              _x1Oidenv :: IDEnv
              _x1Olib :: LocalBindings
              _x1IannotatedTree :: ScalarExpr
              _x1IfixedUpIdentifiersTree :: ScalarExpr
              _x1IoriginalTree :: ScalarExpr
              _x1IuType :: (Maybe Type)
              -- "./TypeChecking/ParameterizedStatements.ag"(line 83, column 13)
              _x1OexpectedType =
                  {-# LINE 83 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 7823 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,x2_)
                  {-# LINE 7828 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (_x1IfixedUpIdentifiersTree,x2_)
                  {-# LINE 7833 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,x2_)
                  {-# LINE 7838 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7843 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7848 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7853 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7858 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7863 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7868 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1IfixedUpIdentifiersTree,_x1IoriginalTree,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedType _x1Oidenv _x1Olib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- ScalarExprDirectionPairList ---------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExprDirectionPair 
         child tl             : ScalarExprDirectionPairList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type ScalarExprDirectionPairList  = [(ScalarExprDirectionPair)]
-- cata
sem_ScalarExprDirectionPairList :: ScalarExprDirectionPairList  ->
                                   T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList list  =
    (Prelude.foldr sem_ScalarExprDirectionPairList_Cons sem_ScalarExprDirectionPairList_Nil (Prelude.map sem_ScalarExprDirectionPair list) )
-- semantic domain
type T_ScalarExprDirectionPairList  = Catalog ->
                                      IDEnv ->
                                      LocalBindings ->
                                      ( ScalarExprDirectionPairList,ScalarExprDirectionPairList,ScalarExprDirectionPairList)
data Inh_ScalarExprDirectionPairList  = Inh_ScalarExprDirectionPairList {cat_Inh_ScalarExprDirectionPairList :: Catalog,idenv_Inh_ScalarExprDirectionPairList :: IDEnv,lib_Inh_ScalarExprDirectionPairList :: LocalBindings}
data Syn_ScalarExprDirectionPairList  = Syn_ScalarExprDirectionPairList {annotatedTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList,fixedUpIdentifiersTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList,originalTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList}
wrap_ScalarExprDirectionPairList :: T_ScalarExprDirectionPairList  ->
                                    Inh_ScalarExprDirectionPairList  ->
                                    Syn_ScalarExprDirectionPairList 
wrap_ScalarExprDirectionPairList sem (Inh_ScalarExprDirectionPairList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprDirectionPairList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ScalarExprDirectionPairList_Cons :: T_ScalarExprDirectionPair  ->
                                        T_ScalarExprDirectionPairList  ->
                                        T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprDirectionPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _lhsOoriginalTree :: ScalarExprDirectionPairList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ScalarExprDirectionPair
              _hdIfixedUpIdentifiersTree :: ScalarExprDirectionPair
              _hdIoriginalTree :: ScalarExprDirectionPair
              _tlIannotatedTree :: ScalarExprDirectionPairList
              _tlIfixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _tlIoriginalTree :: ScalarExprDirectionPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 7943 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 7948 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 7953 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7958 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7963 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7968 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7973 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7978 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7983 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7988 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7993 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7998 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_ScalarExprDirectionPairList_Nil :: T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprDirectionPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _lhsOoriginalTree :: ScalarExprDirectionPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8016 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 8021 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8026 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8031 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8036 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8041 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- ScalarExprList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         expectedTypes        : [Maybe Type]
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         uType                : [Maybe Type]
   alternatives:
      alternative Cons:
         child hd             : ScalarExpr 
         child tl             : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type ScalarExprList  = [(ScalarExpr)]
-- cata
sem_ScalarExprList :: ScalarExprList  ->
                      T_ScalarExprList 
sem_ScalarExprList list  =
    (Prelude.foldr sem_ScalarExprList_Cons sem_ScalarExprList_Nil (Prelude.map sem_ScalarExpr list) )
-- semantic domain
type T_ScalarExprList  = Catalog ->
                         ([Maybe Type]) ->
                         IDEnv ->
                         LocalBindings ->
                         ( ScalarExprList,ScalarExprList,ScalarExprList,([Maybe Type]))
data Inh_ScalarExprList  = Inh_ScalarExprList {cat_Inh_ScalarExprList :: Catalog,expectedTypes_Inh_ScalarExprList :: [Maybe Type],idenv_Inh_ScalarExprList :: IDEnv,lib_Inh_ScalarExprList :: LocalBindings}
data Syn_ScalarExprList  = Syn_ScalarExprList {annotatedTree_Syn_ScalarExprList :: ScalarExprList,fixedUpIdentifiersTree_Syn_ScalarExprList :: ScalarExprList,originalTree_Syn_ScalarExprList :: ScalarExprList,uType_Syn_ScalarExprList :: [Maybe Type]}
wrap_ScalarExprList :: T_ScalarExprList  ->
                       Inh_ScalarExprList  ->
                       Syn_ScalarExprList 
wrap_ScalarExprList sem (Inh_ScalarExprList _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOuType ))
sem_ScalarExprList_Cons :: T_ScalarExpr  ->
                           T_ScalarExprList  ->
                           T_ScalarExprList 
sem_ScalarExprList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOuType :: ([Maybe Type])
              _hdOexpectedType :: (Maybe Type)
              _tlOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: ScalarExprList
              _lhsOfixedUpIdentifiersTree :: ScalarExprList
              _lhsOoriginalTree :: ScalarExprList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ScalarExpr
              _hdIfixedUpIdentifiersTree :: ScalarExpr
              _hdIoriginalTree :: ScalarExpr
              _hdIuType :: (Maybe Type)
              _tlIannotatedTree :: ScalarExprList
              _tlIfixedUpIdentifiersTree :: ScalarExprList
              _tlIoriginalTree :: ScalarExprList
              _tlIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 48, column 12)
              _lhsOuType =
                  {-# LINE 48 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _hdIuType : _tlIuType
                  {-# LINE 8123 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 86, column 12)
              _hdOexpectedType =
                  {-# LINE 86 "./TypeChecking/ParameterizedStatements.ag" #-}
                  case _lhsIexpectedTypes of
                    (t:_) -> t
                    _ -> Nothing
                  {-# LINE 8130 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 89, column 12)
              _tlOexpectedTypes =
                  {-# LINE 89 "./TypeChecking/ParameterizedStatements.ag" #-}
                  case _lhsIexpectedTypes of
                  (_:ts) -> ts
                  _ -> []
                  {-# LINE 8137 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8142 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 8147 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 8152 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8157 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8162 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8167 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8172 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8177 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8182 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8187 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8192 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8197 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIoriginalTree,_hdIuType) =
                  (hd_ _hdOcat _hdOexpectedType _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree,_tlIuType) =
                  (tl_ _tlOcat _tlOexpectedTypes _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExprList_Nil :: T_ScalarExprList 
sem_ScalarExprList_Nil  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOuType :: ([Maybe Type])
              _lhsOannotatedTree :: ScalarExprList
              _lhsOfixedUpIdentifiersTree :: ScalarExprList
              _lhsOoriginalTree :: ScalarExprList
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 49, column 11)
              _lhsOuType =
                  {-# LINE 49 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  []
                  {-# LINE 8217 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8222 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 8227 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8232 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8237 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8242 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8247 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
-- ScalarExprListList ------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         expectedTypes        : [Maybe Type]
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         uType                : [[Maybe Type]]
   alternatives:
      alternative Cons:
         child hd             : ScalarExprList 
         child tl             : ScalarExprListList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type ScalarExprListList  = [(ScalarExprList)]
-- cata
sem_ScalarExprListList :: ScalarExprListList  ->
                          T_ScalarExprListList 
sem_ScalarExprListList list  =
    (Prelude.foldr sem_ScalarExprListList_Cons sem_ScalarExprListList_Nil (Prelude.map sem_ScalarExprList list) )
-- semantic domain
type T_ScalarExprListList  = Catalog ->
                             ([Maybe Type]) ->
                             IDEnv ->
                             LocalBindings ->
                             ( ScalarExprListList,ScalarExprListList,ScalarExprListList,([[Maybe Type]]))
data Inh_ScalarExprListList  = Inh_ScalarExprListList {cat_Inh_ScalarExprListList :: Catalog,expectedTypes_Inh_ScalarExprListList :: [Maybe Type],idenv_Inh_ScalarExprListList :: IDEnv,lib_Inh_ScalarExprListList :: LocalBindings}
data Syn_ScalarExprListList  = Syn_ScalarExprListList {annotatedTree_Syn_ScalarExprListList :: ScalarExprListList,fixedUpIdentifiersTree_Syn_ScalarExprListList :: ScalarExprListList,originalTree_Syn_ScalarExprListList :: ScalarExprListList,uType_Syn_ScalarExprListList :: [[Maybe Type]]}
wrap_ScalarExprListList :: T_ScalarExprListList  ->
                           Inh_ScalarExprListList  ->
                           Syn_ScalarExprListList 
wrap_ScalarExprListList sem (Inh_ScalarExprListList _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprListList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOuType ))
sem_ScalarExprListList_Cons :: T_ScalarExprList  ->
                               T_ScalarExprListList  ->
                               T_ScalarExprListList 
sem_ScalarExprListList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOuType :: ([[Maybe Type]])
              _hdOexpectedTypes :: ([Maybe Type])
              _tlOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: ScalarExprListList
              _lhsOfixedUpIdentifiersTree :: ScalarExprListList
              _lhsOoriginalTree :: ScalarExprListList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ScalarExprList
              _hdIfixedUpIdentifiersTree :: ScalarExprList
              _hdIoriginalTree :: ScalarExprList
              _hdIuType :: ([Maybe Type])
              _tlIannotatedTree :: ScalarExprListList
              _tlIfixedUpIdentifiersTree :: ScalarExprListList
              _tlIoriginalTree :: ScalarExprListList
              _tlIuType :: ([[Maybe Type]])
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 58, column 12)
              _lhsOuType =
                  {-# LINE 58 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  _hdIuType : _tlIuType
                  {-# LINE 8329 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 169, column 12)
              _hdOexpectedTypes =
                  {-# LINE 169 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 8334 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 170, column 12)
              _tlOexpectedTypes =
                  {-# LINE 170 "./TypeChecking/ParameterizedStatements.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 8339 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8344 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 8349 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 8354 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8359 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8364 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8369 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8374 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8379 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8384 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8389 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8394 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8399 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIoriginalTree,_hdIuType) =
                  (hd_ _hdOcat _hdOexpectedTypes _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree,_tlIuType) =
                  (tl_ _tlOcat _tlOexpectedTypes _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExprListList_Nil :: T_ScalarExprListList 
sem_ScalarExprListList_Nil  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOuType :: ([[Maybe Type]])
              _lhsOannotatedTree :: ScalarExprListList
              _lhsOfixedUpIdentifiersTree :: ScalarExprListList
              _lhsOoriginalTree :: ScalarExprListList
              -- "./TypeChecking/ScalarExprs/ScalarExprs.ag"(line 59, column 11)
              _lhsOuType =
                  {-# LINE 59 "./TypeChecking/ScalarExprs/ScalarExprs.ag" #-}
                  []
                  {-# LINE 8419 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8424 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 8429 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8434 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8439 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8444 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8449 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
-- ScalarExprListStatementListPair -----------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExprList 
         child x2             : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type ScalarExprListStatementListPair  = ( (ScalarExprList),(StatementList))
-- cata
sem_ScalarExprListStatementListPair :: ScalarExprListStatementListPair  ->
                                       T_ScalarExprListStatementListPair 
sem_ScalarExprListStatementListPair ( x1,x2)  =
    (sem_ScalarExprListStatementListPair_Tuple (sem_ScalarExprList x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ScalarExprListStatementListPair  = Catalog ->
                                          IDEnv ->
                                          LocalBindings ->
                                          ( ScalarExprListStatementListPair,ScalarExprListStatementListPair,ScalarExprListStatementListPair)
data Inh_ScalarExprListStatementListPair  = Inh_ScalarExprListStatementListPair {cat_Inh_ScalarExprListStatementListPair :: Catalog,idenv_Inh_ScalarExprListStatementListPair :: IDEnv,lib_Inh_ScalarExprListStatementListPair :: LocalBindings}
data Syn_ScalarExprListStatementListPair  = Syn_ScalarExprListStatementListPair {annotatedTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair,fixedUpIdentifiersTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair,originalTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair}
wrap_ScalarExprListStatementListPair :: T_ScalarExprListStatementListPair  ->
                                        Inh_ScalarExprListStatementListPair  ->
                                        Syn_ScalarExprListStatementListPair 
wrap_ScalarExprListStatementListPair sem (Inh_ScalarExprListStatementListPair _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprListStatementListPair _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ScalarExprListStatementListPair_Tuple :: T_ScalarExprList  ->
                                             T_StatementList  ->
                                             T_ScalarExprListStatementListPair 
sem_ScalarExprListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _x2OcatUpdates :: ([CatalogUpdate])
              _x2OlibUpdates :: ([LocalBindingsUpdate])
              _x1OexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: ScalarExprListStatementListPair
              _lhsOfixedUpIdentifiersTree :: ScalarExprListStatementListPair
              _lhsOoriginalTree :: ScalarExprListStatementListPair
              _x1Ocat :: Catalog
              _x1Oidenv :: IDEnv
              _x1Olib :: LocalBindings
              _x2Ocat :: Catalog
              _x2Oidenv :: IDEnv
              _x2Olib :: LocalBindings
              _x1IannotatedTree :: ScalarExprList
              _x1IfixedUpIdentifiersTree :: ScalarExprList
              _x1IoriginalTree :: ScalarExprList
              _x1IuType :: ([Maybe Type])
              _x2IannotatedTree :: StatementList
              _x2IfixedUpIdentifiersTree :: StatementList
              _x2IoriginalTree :: StatementList
              _x2IproducedCat :: Catalog
              _x2IproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 121, column 9)
              _x2OcatUpdates =
                  {-# LINE 121 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 8523 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 122, column 9)
              _x2OlibUpdates =
                  {-# LINE 122 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 8528 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 146, column 13)
              _x1OexpectedTypes =
                  {-# LINE 146 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 8533 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 8538 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (_x1IfixedUpIdentifiersTree,_x2IfixedUpIdentifiersTree)
                  {-# LINE 8543 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 8548 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8553 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8558 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8563 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8568 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8573 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8578 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8583 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8588 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8593 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1IfixedUpIdentifiersTree,_x1IoriginalTree,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedTypes _x1Oidenv _x1Olib )
              ( _x2IannotatedTree,_x2IfixedUpIdentifiersTree,_x2IoriginalTree,_x2IproducedCat,_x2IproducedLib) =
                  (x2_ _x2Ocat _x2OcatUpdates _x2Oidenv _x2Olib _x2OlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- ScalarExprListStatementListPairList -------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExprListStatementListPair 
         child tl             : ScalarExprListStatementListPairList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type ScalarExprListStatementListPairList  = [(ScalarExprListStatementListPair)]
-- cata
sem_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList  ->
                                           T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList list  =
    (Prelude.foldr sem_ScalarExprListStatementListPairList_Cons sem_ScalarExprListStatementListPairList_Nil (Prelude.map sem_ScalarExprListStatementListPair list) )
-- semantic domain
type T_ScalarExprListStatementListPairList  = Catalog ->
                                              IDEnv ->
                                              LocalBindings ->
                                              ( ScalarExprListStatementListPairList,ScalarExprListStatementListPairList,ScalarExprListStatementListPairList)
data Inh_ScalarExprListStatementListPairList  = Inh_ScalarExprListStatementListPairList {cat_Inh_ScalarExprListStatementListPairList :: Catalog,idenv_Inh_ScalarExprListStatementListPairList :: IDEnv,lib_Inh_ScalarExprListStatementListPairList :: LocalBindings}
data Syn_ScalarExprListStatementListPairList  = Syn_ScalarExprListStatementListPairList {annotatedTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList,fixedUpIdentifiersTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList,originalTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList}
wrap_ScalarExprListStatementListPairList :: T_ScalarExprListStatementListPairList  ->
                                            Inh_ScalarExprListStatementListPairList  ->
                                            Syn_ScalarExprListStatementListPairList 
wrap_ScalarExprListStatementListPairList sem (Inh_ScalarExprListStatementListPairList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprListStatementListPairList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ScalarExprListStatementListPairList_Cons :: T_ScalarExprListStatementListPair  ->
                                                T_ScalarExprListStatementListPairList  ->
                                                T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprListStatementListPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _lhsOoriginalTree :: ScalarExprListStatementListPairList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ScalarExprListStatementListPair
              _hdIfixedUpIdentifiersTree :: ScalarExprListStatementListPair
              _hdIoriginalTree :: ScalarExprListStatementListPair
              _tlIannotatedTree :: ScalarExprListStatementListPairList
              _tlIfixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _tlIoriginalTree :: ScalarExprListStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8670 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 8675 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 8680 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8685 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8690 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8695 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8700 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8705 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8710 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8715 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8720 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8725 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_ScalarExprListStatementListPairList_Nil :: T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprListStatementListPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _lhsOoriginalTree :: ScalarExprListStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8743 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 8748 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8753 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8758 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8763 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8768 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- ScalarExprRoot ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative ScalarExprRoot:
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data ScalarExprRoot  = ScalarExprRoot (ScalarExpr) 
                     deriving ( Show)
-- cata
sem_ScalarExprRoot :: ScalarExprRoot  ->
                      T_ScalarExprRoot 
sem_ScalarExprRoot (ScalarExprRoot _expr )  =
    (sem_ScalarExprRoot_ScalarExprRoot (sem_ScalarExpr _expr ) )
-- semantic domain
type T_ScalarExprRoot  = Catalog ->
                         IDEnv ->
                         LocalBindings ->
                         ( ScalarExprRoot,ScalarExprRoot,ScalarExprRoot)
data Inh_ScalarExprRoot  = Inh_ScalarExprRoot {cat_Inh_ScalarExprRoot :: Catalog,idenv_Inh_ScalarExprRoot :: IDEnv,lib_Inh_ScalarExprRoot :: LocalBindings}
data Syn_ScalarExprRoot  = Syn_ScalarExprRoot {annotatedTree_Syn_ScalarExprRoot :: ScalarExprRoot,fixedUpIdentifiersTree_Syn_ScalarExprRoot :: ScalarExprRoot,originalTree_Syn_ScalarExprRoot :: ScalarExprRoot}
wrap_ScalarExprRoot :: T_ScalarExprRoot  ->
                       Inh_ScalarExprRoot  ->
                       Syn_ScalarExprRoot 
wrap_ScalarExprRoot sem (Inh_ScalarExprRoot _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprRoot _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ScalarExprRoot_ScalarExprRoot :: T_ScalarExpr  ->
                                     T_ScalarExprRoot 
sem_ScalarExprRoot_ScalarExprRoot expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: ScalarExprRoot
              _lhsOfixedUpIdentifiersTree :: ScalarExprRoot
              _lhsOoriginalTree :: ScalarExprRoot
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ParameterizedStatements.ag"(line 125, column 22)
              _exprOexpectedType =
                  {-# LINE 125 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 8831 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ScalarExprRoot _exprIannotatedTree
                  {-# LINE 8836 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ScalarExprRoot _exprIfixedUpIdentifiersTree
                  {-# LINE 8841 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ScalarExprRoot _exprIoriginalTree
                  {-# LINE 8846 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8851 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8856 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8861 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8866 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8871 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8876 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- ScalarExprStatementListPair ---------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExpr 
         child x2             : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type ScalarExprStatementListPair  = ( (ScalarExpr),(StatementList))
-- cata
sem_ScalarExprStatementListPair :: ScalarExprStatementListPair  ->
                                   T_ScalarExprStatementListPair 
sem_ScalarExprStatementListPair ( x1,x2)  =
    (sem_ScalarExprStatementListPair_Tuple (sem_ScalarExpr x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ScalarExprStatementListPair  = Catalog ->
                                      IDEnv ->
                                      LocalBindings ->
                                      ( ScalarExprStatementListPair,ScalarExprStatementListPair,ScalarExprStatementListPair)
data Inh_ScalarExprStatementListPair  = Inh_ScalarExprStatementListPair {cat_Inh_ScalarExprStatementListPair :: Catalog,idenv_Inh_ScalarExprStatementListPair :: IDEnv,lib_Inh_ScalarExprStatementListPair :: LocalBindings}
data Syn_ScalarExprStatementListPair  = Syn_ScalarExprStatementListPair {annotatedTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair,fixedUpIdentifiersTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair,originalTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair}
wrap_ScalarExprStatementListPair :: T_ScalarExprStatementListPair  ->
                                    Inh_ScalarExprStatementListPair  ->
                                    Syn_ScalarExprStatementListPair 
wrap_ScalarExprStatementListPair sem (Inh_ScalarExprStatementListPair _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprStatementListPair _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ScalarExprStatementListPair_Tuple :: T_ScalarExpr  ->
                                         T_StatementList  ->
                                         T_ScalarExprStatementListPair 
sem_ScalarExprStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _x2OcatUpdates :: ([CatalogUpdate])
              _x2OlibUpdates :: ([LocalBindingsUpdate])
              _x1OexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: ScalarExprStatementListPair
              _lhsOfixedUpIdentifiersTree :: ScalarExprStatementListPair
              _lhsOoriginalTree :: ScalarExprStatementListPair
              _x1Ocat :: Catalog
              _x1Oidenv :: IDEnv
              _x1Olib :: LocalBindings
              _x2Ocat :: Catalog
              _x2Oidenv :: IDEnv
              _x2Olib :: LocalBindings
              _x1IannotatedTree :: ScalarExpr
              _x1IfixedUpIdentifiersTree :: ScalarExpr
              _x1IoriginalTree :: ScalarExpr
              _x1IuType :: (Maybe Type)
              _x2IannotatedTree :: StatementList
              _x2IfixedUpIdentifiersTree :: StatementList
              _x2IoriginalTree :: StatementList
              _x2IproducedCat :: Catalog
              _x2IproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 125, column 9)
              _x2OcatUpdates =
                  {-# LINE 125 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 8952 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 126, column 9)
              _x2OlibUpdates =
                  {-# LINE 126 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 8957 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 128, column 13)
              _x1OexpectedType =
                  {-# LINE 128 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 8962 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 8967 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (_x1IfixedUpIdentifiersTree,_x2IfixedUpIdentifiersTree)
                  {-# LINE 8972 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 8977 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8982 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8987 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8992 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8997 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9002 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9007 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9012 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9017 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9022 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1IfixedUpIdentifiersTree,_x1IoriginalTree,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedType _x1Oidenv _x1Olib )
              ( _x2IannotatedTree,_x2IfixedUpIdentifiersTree,_x2IoriginalTree,_x2IproducedCat,_x2IproducedLib) =
                  (x2_ _x2Ocat _x2OcatUpdates _x2Oidenv _x2Olib _x2OlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- ScalarExprStatementListPairList -----------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExprStatementListPair 
         child tl             : ScalarExprStatementListPairList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type ScalarExprStatementListPairList  = [(ScalarExprStatementListPair)]
-- cata
sem_ScalarExprStatementListPairList :: ScalarExprStatementListPairList  ->
                                       T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList list  =
    (Prelude.foldr sem_ScalarExprStatementListPairList_Cons sem_ScalarExprStatementListPairList_Nil (Prelude.map sem_ScalarExprStatementListPair list) )
-- semantic domain
type T_ScalarExprStatementListPairList  = Catalog ->
                                          IDEnv ->
                                          LocalBindings ->
                                          ( ScalarExprStatementListPairList,ScalarExprStatementListPairList,ScalarExprStatementListPairList)
data Inh_ScalarExprStatementListPairList  = Inh_ScalarExprStatementListPairList {cat_Inh_ScalarExprStatementListPairList :: Catalog,idenv_Inh_ScalarExprStatementListPairList :: IDEnv,lib_Inh_ScalarExprStatementListPairList :: LocalBindings}
data Syn_ScalarExprStatementListPairList  = Syn_ScalarExprStatementListPairList {annotatedTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList,fixedUpIdentifiersTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList,originalTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList}
wrap_ScalarExprStatementListPairList :: T_ScalarExprStatementListPairList  ->
                                        Inh_ScalarExprStatementListPairList  ->
                                        Syn_ScalarExprStatementListPairList 
wrap_ScalarExprStatementListPairList sem (Inh_ScalarExprStatementListPairList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprStatementListPairList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ScalarExprStatementListPairList_Cons :: T_ScalarExprStatementListPair  ->
                                            T_ScalarExprStatementListPairList  ->
                                            T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprStatementListPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _lhsOoriginalTree :: ScalarExprStatementListPairList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ScalarExprStatementListPair
              _hdIfixedUpIdentifiersTree :: ScalarExprStatementListPair
              _hdIoriginalTree :: ScalarExprStatementListPair
              _tlIannotatedTree :: ScalarExprStatementListPairList
              _tlIfixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _tlIoriginalTree :: ScalarExprStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 9099 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 9104 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 9109 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9114 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9119 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9124 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9129 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9134 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9139 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9144 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9149 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9154 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_ScalarExprStatementListPairList_Nil :: T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprStatementListPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _lhsOoriginalTree :: ScalarExprStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 9172 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 9177 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 9182 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9187 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9192 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9197 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- SelectItem --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         itemType             : (String,Maybe Type)
         originalTree         : SELF 
         seIdTree             : [SelectItem]
   alternatives:
      alternative SelExp:
         child ann            : {Annotation}
         child ex             : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative SelectItem:
         child ann            : {Annotation}
         child ex             : ScalarExpr 
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data SelectItem  = SelExp (Annotation) (ScalarExpr) 
                 | SelectItem (Annotation) (ScalarExpr) (String) 
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
                     IDEnv ->
                     LocalBindings ->
                     ( SelectItem,SelectItem,((String,Maybe Type)),SelectItem,([SelectItem]))
data Inh_SelectItem  = Inh_SelectItem {cat_Inh_SelectItem :: Catalog,idenv_Inh_SelectItem :: IDEnv,lib_Inh_SelectItem :: LocalBindings}
data Syn_SelectItem  = Syn_SelectItem {annotatedTree_Syn_SelectItem :: SelectItem,fixedUpIdentifiersTree_Syn_SelectItem :: SelectItem,itemType_Syn_SelectItem :: (String,Maybe Type),originalTree_Syn_SelectItem :: SelectItem,seIdTree_Syn_SelectItem :: [SelectItem]}
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOitemType,_lhsOoriginalTree,_lhsOseIdTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_SelectItem _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOitemType _lhsOoriginalTree _lhsOseIdTree ))
sem_SelectItem_SelExp :: Annotation ->
                         T_ScalarExpr  ->
                         T_SelectItem 
sem_SelectItem_SelExp ann_ ex_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOseIdTree :: ([SelectItem])
              _lhsOitemType :: ((String,Maybe Type))
              _exOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: SelectItem
              _lhsOfixedUpIdentifiersTree :: SelectItem
              _lhsOoriginalTree :: SelectItem
              _exOcat :: Catalog
              _exOidenv :: IDEnv
              _exOlib :: LocalBindings
              _exIannotatedTree :: ScalarExpr
              _exIfixedUpIdentifiersTree :: ScalarExpr
              _exIoriginalTree :: ScalarExpr
              _exIuType :: (Maybe Type)
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 150, column 14)
              _lhsOseIdTree =
                  {-# LINE 150 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  case _exIfixedUpIdentifiersTree of
                    Identifier a "*" -> let s = expandStar _lhsIidenv Nothing
                                            in if null s
                                               then [addSIAlias $ SelExp ann_ _exIfixedUpIdentifiersTree]
                                               else makeSelExps ann_ a a s
                    QIdentifier a0 (Identifier a1 q) "*" ->
                       let s = expandStar _lhsIidenv $ Just q
                       in if null s
                          then [addSIAlias $ SelExp ann_ _exIfixedUpIdentifiersTree]
                          else makeSelExps ann_ a0 a1 s
                    _ -> [addSIAlias $ SelExp ann_ _exIfixedUpIdentifiersTree]
                  {-# LINE 9287 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 32, column 9)
              _annotatedTree =
                  {-# LINE 32 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  SelExp ann_ _exIannotatedTree
                  {-# LINE 9292 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 60, column 9)
              _lhsOitemType =
                  {-# LINE 60 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  ("", Nothing)
                  {-# LINE 9297 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 110, column 25)
              _exOexpectedType =
                  {-# LINE 110 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 9302 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SelExp ann_ _exIfixedUpIdentifiersTree
                  {-# LINE 9307 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  SelExp ann_ _exIoriginalTree
                  {-# LINE 9312 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9317 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9322 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9327 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9332 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9337 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9342 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIfixedUpIdentifiersTree,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedType _exOidenv _exOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOitemType,_lhsOoriginalTree,_lhsOseIdTree)))
sem_SelectItem_SelectItem :: Annotation ->
                             T_ScalarExpr  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem ann_ ex_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOseIdTree :: ([SelectItem])
              _lhsOitemType :: ((String,Maybe Type))
              _exOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: SelectItem
              _lhsOfixedUpIdentifiersTree :: SelectItem
              _lhsOoriginalTree :: SelectItem
              _exOcat :: Catalog
              _exOidenv :: IDEnv
              _exOlib :: LocalBindings
              _exIannotatedTree :: ScalarExpr
              _exIfixedUpIdentifiersTree :: ScalarExpr
              _exIoriginalTree :: ScalarExpr
              _exIuType :: (Maybe Type)
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 163, column 18)
              _lhsOseIdTree =
                  {-# LINE 163 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  [SelectItem ann_ _exIfixedUpIdentifiersTree name_]
                  {-# LINE 9371 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 34, column 9)
              _annotatedTree =
                  {-# LINE 34 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  SelectItem ann_ _exIannotatedTree name_
                  {-# LINE 9376 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 62, column 9)
              _lhsOitemType =
                  {-# LINE 62 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  (name_, unwrapSetof `fmap` _exIuType)
                  {-# LINE 9381 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 110, column 25)
              _exOexpectedType =
                  {-# LINE 110 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 9386 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SelectItem ann_ _exIfixedUpIdentifiersTree name_
                  {-# LINE 9391 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  SelectItem ann_ _exIoriginalTree name_
                  {-# LINE 9396 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9401 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9406 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9411 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9416 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9421 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9426 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIfixedUpIdentifiersTree,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedType _exOidenv _exOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOitemType,_lhsOoriginalTree,_lhsOseIdTree)))
-- SelectItemList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         listType             : [(String,Maybe Type)]
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : SelectItem 
         child tl             : SelectItemList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type SelectItemList  = [(SelectItem)]
-- cata
sem_SelectItemList :: SelectItemList  ->
                      T_SelectItemList 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList  = Catalog ->
                         IDEnv ->
                         LocalBindings ->
                         ( SelectItemList,SelectItemList,([(String,Maybe Type)]),SelectItemList)
data Inh_SelectItemList  = Inh_SelectItemList {cat_Inh_SelectItemList :: Catalog,idenv_Inh_SelectItemList :: IDEnv,lib_Inh_SelectItemList :: LocalBindings}
data Syn_SelectItemList  = Syn_SelectItemList {annotatedTree_Syn_SelectItemList :: SelectItemList,fixedUpIdentifiersTree_Syn_SelectItemList :: SelectItemList,listType_Syn_SelectItemList :: [(String,Maybe Type)],originalTree_Syn_SelectItemList :: SelectItemList}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_SelectItemList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOlistType _lhsOoriginalTree ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOfixedUpIdentifiersTree :: SelectItemList
              _lhsOlistType :: ([(String,Maybe Type)])
              _lhsOannotatedTree :: SelectItemList
              _lhsOoriginalTree :: SelectItemList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: SelectItem
              _hdIfixedUpIdentifiersTree :: SelectItem
              _hdIitemType :: ((String,Maybe Type))
              _hdIoriginalTree :: SelectItem
              _hdIseIdTree :: ([SelectItem])
              _tlIannotatedTree :: SelectItemList
              _tlIfixedUpIdentifiersTree :: SelectItemList
              _tlIlistType :: ([(String,Maybe Type)])
              _tlIoriginalTree :: SelectItemList
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 141, column 12)
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 141 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _hdIseIdTree ++ _tlIfixedUpIdentifiersTree
                  {-# LINE 9506 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 42, column 12)
              _lhsOlistType =
                  {-# LINE 42 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  _hdIitemType : _tlIlistType
                  {-# LINE 9511 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 9516 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 9521 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 9526 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9531 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9536 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9541 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9546 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9551 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9556 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9561 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9566 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIitemType,_hdIoriginalTree,_hdIseIdTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIlistType,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOoriginalTree)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOfixedUpIdentifiersTree :: SelectItemList
              _lhsOlistType :: ([(String,Maybe Type)])
              _lhsOannotatedTree :: SelectItemList
              _lhsOoriginalTree :: SelectItemList
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 142, column 11)
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 142 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 9585 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 43, column 11)
              _lhsOlistType =
                  {-# LINE 43 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  []
                  {-# LINE 9590 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 9595 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 9600 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 9605 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9610 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9615 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOoriginalTree)))
-- SelectList --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         cidenv               : IDEnv
         fixedUpIdentifiersTree : SELF 
         libUpdates           : [LocalBindingsUpdate]
         listType             : [(String,Maybe Type)]
         originalTree         : SELF 
   alternatives:
      alternative SelectList:
         child ann            : {Annotation}
         child items          : SelectItemList 
         child into           : ScalarExprList 
         visit 0:
            local intoFroms   : {E ([(String,Type)],[(String,Type)])}
            local tpe         : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data SelectList  = SelectList (Annotation) (SelectItemList) (ScalarExprList) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _ann _items _into )  =
    (sem_SelectList_SelectList _ann (sem_SelectItemList _items ) (sem_ScalarExprList _into ) )
-- semantic domain
type T_SelectList  = Catalog ->
                     IDEnv ->
                     LocalBindings ->
                     ( SelectList,IDEnv,SelectList,([LocalBindingsUpdate]),([(String,Maybe Type)]),SelectList)
data Inh_SelectList  = Inh_SelectList {cat_Inh_SelectList :: Catalog,idenv_Inh_SelectList :: IDEnv,lib_Inh_SelectList :: LocalBindings}
data Syn_SelectList  = Syn_SelectList {annotatedTree_Syn_SelectList :: SelectList,cidenv_Syn_SelectList :: IDEnv,fixedUpIdentifiersTree_Syn_SelectList :: SelectList,libUpdates_Syn_SelectList :: [LocalBindingsUpdate],listType_Syn_SelectList :: [(String,Maybe Type)],originalTree_Syn_SelectList :: SelectList}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOlistType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_SelectList _lhsOannotatedTree _lhsOcidenv _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOlistType _lhsOoriginalTree ))
sem_SelectList_SelectList :: Annotation ->
                             T_SelectItemList  ->
                             T_ScalarExprList  ->
                             T_SelectList 
sem_SelectList_SelectList ann_ items_ into_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOcidenv :: IDEnv
              _lhsOlistType :: ([(String,Maybe Type)])
              _intoFroms :: (E ([(String,Type)],[(String,Type)]))
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: SelectList
              _intoOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: SelectList
              _lhsOoriginalTree :: SelectList
              _itemsOcat :: Catalog
              _itemsOidenv :: IDEnv
              _itemsOlib :: LocalBindings
              _intoOcat :: Catalog
              _intoOidenv :: IDEnv
              _intoOlib :: LocalBindings
              _itemsIannotatedTree :: SelectItemList
              _itemsIfixedUpIdentifiersTree :: SelectItemList
              _itemsIlistType :: ([(String,Maybe Type)])
              _itemsIoriginalTree :: SelectItemList
              _intoIannotatedTree :: ScalarExprList
              _intoIfixedUpIdentifiersTree :: ScalarExprList
              _intoIoriginalTree :: ScalarExprList
              _intoIuType :: ([Maybe Type])
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 124, column 18)
              _lhsOcidenv =
                  {-# LINE 124 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  makeIDEnv "" $ flip map _itemsIfixedUpIdentifiersTree
                                   $ \(SelectItem _ _ n) -> n
                  {-# LINE 9699 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 77, column 9)
              _lhsOlistType =
                  {-# LINE 77 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  _itemsIlistType
                  {-# LINE 9704 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 79, column 9)
              _intoFroms =
                  {-# LINE 79 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  returnWhen (_intoIoriginalTree == []) ([],[]) $ do
                  it <- lmt intoTypes
                  let ft = fromMaybe [] $ liftList _itemsIlistType
                  return (it,ft)
                  where
                    intoTypes :: Maybe [(String,Type)]
                    intoTypes = do
                                ts <- sequence _intoIuType
                                let ns = map getName _intoIoriginalTree
                                return $ zip ns ts
                  {-# LINE 9718 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 93, column 9)
              _tpe =
                  {-# LINE 93 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  returnWhen (_intoIoriginalTree == []) () $ do
                  (it,ft) <- _intoFroms
                  checkAssignmentsValid _lhsIcat (map snd ft) (map snd it)
                  {-# LINE 9725 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 98, column 9)
              _lhsOlibUpdates =
                  {-# LINE 98 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  maybe [] id $ do
                  _ <- etmt _tpe
                  (it,ft) <- etmt _intoFroms
                  return $ case it of
                    [(n,PgRecord _)] -> [LBIds "set record actual fields from select into"
                                               Nothing
                                               [(n,PgRecord $ Just $ CompositeType ft)]]
                    _ -> []
                  {-# LINE 9737 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/SelectLists.ag"(line 131, column 9)
              _lhsOannotatedTree =
                  {-# LINE 131 "./TypeChecking/QueryExprs/SelectLists.ag" #-}
                  addTypeErrors (tes _tpe    ) $
                  SelectList ann_
                             _itemsIannotatedTree
                             _intoIannotatedTree
                  {-# LINE 9745 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 106, column 18)
              _intoOexpectedTypes =
                  {-# LINE 106 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 9750 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  SelectList ann_ _itemsIannotatedTree _intoIannotatedTree
                  {-# LINE 9755 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SelectList ann_ _itemsIfixedUpIdentifiersTree _intoIfixedUpIdentifiersTree
                  {-# LINE 9760 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  SelectList ann_ _itemsIoriginalTree _intoIoriginalTree
                  {-# LINE 9765 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9770 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9775 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9780 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9785 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9790 "AstInternal.hs" #-}
              -- copy rule (down)
              _intoOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9795 "AstInternal.hs" #-}
              -- copy rule (down)
              _intoOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9800 "AstInternal.hs" #-}
              -- copy rule (down)
              _intoOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9805 "AstInternal.hs" #-}
              ( _itemsIannotatedTree,_itemsIfixedUpIdentifiersTree,_itemsIlistType,_itemsIoriginalTree) =
                  (items_ _itemsOcat _itemsOidenv _itemsOlib )
              ( _intoIannotatedTree,_intoIfixedUpIdentifiersTree,_intoIoriginalTree,_intoIuType) =
                  (into_ _intoOcat _intoOexpectedTypes _intoOidenv _intoOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOlistType,_lhsOoriginalTree)))
-- Statement ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         inProducedCat        : Catalog
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         catUpdates           : [CatalogUpdate]
         fixedUpIdentifiersTree : SELF 
         libUpdates           : [LocalBindingsUpdate]
         originalTree         : SELF 
   alternatives:
      alternative AlterSequence:
         child ann            : {Annotation}
         child name           : {String}
         child ownedBy        : SQIdentifier 
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative AlterTable:
         child ann            : {Annotation}
         child name           : {String}
         child actions        : AlterTableActionList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Assignment:
         child ann            : {Annotation}
         child target         : ScalarExpr 
         child value          : ScalarExpr 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Block:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child vars           : VarDefList 
         child sts            : StatementList 
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CaseStatement:
         child ann            : {Annotation}
         child cases          : ScalarExprListStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CaseStatementSimple:
         child ann            : {Annotation}
         child val            : ScalarExpr 
         child cases          : ScalarExprListStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative ContinueStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Copy:
         child ann            : {Annotation}
         child table          : {String}
         child targetCols     : {[String]}
         child source         : {CopySource}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CopyData:
         child ann            : {Annotation}
         child insData        : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CreateDomain:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         child checkName      : {String}
         child check          : MaybeBoolExpr 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local statementType : {Maybe StatementType}
            local catUpdates  : {[CatalogUpdate]}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CreateFunction:
         child ann            : {Annotation}
         child name           : {String}
         child params         : ParamDefList 
         child rettype        : TypeName 
         child rep            : {Replace}
         child lang           : {Language}
         child body           : FnBody 
         child vol            : {Volatility}
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local catUpdates  : {[CatalogUpdate]}
            local backTree    : _
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CreateLanguage:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local statementType : {Maybe StatementType}
            local catUpdates  : {[CatalogUpdate]}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CreateSequence:
         child ann            : {Annotation}
         child name           : {String}
         child incr           : {Integer}
         child min            : {Integer}
         child max            : {Integer}
         child start          : {Integer}
         child cache          : {Integer}
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CreateTable:
         child ann            : {Annotation}
         child name           : {String}
         child atts           : AttributeDefList 
         child cons           : ConstraintList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local catUpdates  : {[CatalogUpdate]}
            local attrs       : {[(String,Type)]}
            local statementType : {Maybe StatementType}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CreateTableAs:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : QueryExpr 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local catUpdates  : {[CatalogUpdate]}
            local attrs       : {Either [TypeError] [(String,Type)]}
            local backTree    : _
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CreateTrigger:
         child ann            : {Annotation}
         child name           : {String}
         child wh             : {TriggerWhen}
         child events         : {[TriggerEvent]}
         child tbl            : {String}
         child firing         : {TriggerFire}
         child fnName         : {String}
         child fnArgs         : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CreateType:
         child ann            : {Annotation}
         child name           : {String}
         child atts           : TypeAttributeDefList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local attrs       : _
            local backTree    : _
            local statementType : {Maybe StatementType}
            local catUpdates  : {[CatalogUpdate]}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative CreateView:
         child ann            : {Annotation}
         child name           : {String}
         child colNames       : {Maybe [String]}
         child expr           : QueryExpr 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Delete:
         child ann            : {Annotation}
         child table          : SQIdentifier 
         child using          : TableRefList 
         child whr            : MaybeBoolExpr 
         child returning      : MaybeSelectList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local statementType : {Maybe StatementType}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local lib         : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative DropFunction:
         child ann            : {Annotation}
         child ifE            : {IfExists}
         child sigs           : StringTypeNameListPairList 
         child cascade        : {Cascade}
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative DropSomething:
         child ann            : {Annotation}
         child dropType       : {DropType}
         child ifE            : {IfExists}
         child names          : {[String]}
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Execute:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative ExecuteInto:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         child targets        : {[String]}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative ExitStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative ForIntegerStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child var            : ScalarExpr 
         child from           : ScalarExpr 
         child to             : ScalarExpr 
         child sts            : StatementList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local implicitVar : _
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative ForQueryStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child var            : ScalarExpr 
         child sel            : QueryExpr 
         child sts            : StatementList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative If:
         child ann            : {Annotation}
         child cases          : ScalarExprStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Insert:
         child ann            : {Annotation}
         child table          : SQIdentifier 
         child targetCols     : {[String]}
         child insData        : QueryExpr 
         child returning      : MaybeSelectList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local statementType : {Maybe StatementType}
            local columnTypes : {Either [TypeError] [(String,Type)]}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative LoopStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Notify:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative NullStatement:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Perform:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative QueryStatement:
         child ann            : {Annotation}
         child ex             : QueryExpr 
         visit 0:
            local tpe         : {Either [TypeError] Type}
            local statementType : {Maybe StatementType}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local libUpdates  : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Raise:
         child ann            : {Annotation}
         child level          : {RaiseType}
         child message        : {String}
         child args           : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Return:
         child ann            : {Annotation}
         child value          : MaybeScalarExpr 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative ReturnNext:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative ReturnQuery:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Set:
         child ann            : {Annotation}
         child name           : {String}
         child values         : {[SetValue]}
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Truncate:
         child ann            : {Annotation}
         child tables         : {[String]}
         child restartIdentity : {RestartIdentity}
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Update:
         child ann            : {Annotation}
         child table          : SQIdentifier 
         child assigns        : ScalarExprList 
         child fromList       : TableRefList 
         child whr            : MaybeBoolExpr 
         child returning      : MaybeSelectList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local statementType : {Maybe StatementType}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local lib         : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative WhileStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child expr           : ScalarExpr 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data Statement  = AlterSequence (Annotation) (String) (SQIdentifier) 
                | AlterTable (Annotation) (String) (AlterTableActionList) 
                | Assignment (Annotation) (ScalarExpr) (ScalarExpr) 
                | Block (Annotation) (Maybe String) (VarDefList) (StatementList) 
                | CaseStatement (Annotation) (ScalarExprListStatementListPairList) (StatementList) 
                | CaseStatementSimple (Annotation) (ScalarExpr) (ScalarExprListStatementListPairList) (StatementList) 
                | ContinueStatement (Annotation) (Maybe String) 
                | Copy (Annotation) (String) ([String]) (CopySource) 
                | CopyData (Annotation) (String) 
                | CreateDomain (Annotation) (String) (TypeName) (String) (MaybeBoolExpr) 
                | CreateFunction (Annotation) (String) (ParamDefList) (TypeName) (Replace) (Language) (FnBody) (Volatility) 
                | CreateLanguage (Annotation) (String) 
                | CreateSequence (Annotation) (String) (Integer) (Integer) (Integer) (Integer) (Integer) 
                | CreateTable (Annotation) (String) (AttributeDefList) (ConstraintList) 
                | CreateTableAs (Annotation) (String) (QueryExpr) 
                | CreateTrigger (Annotation) (String) (TriggerWhen) ([TriggerEvent]) (String) (TriggerFire) (String) (ScalarExprList) 
                | CreateType (Annotation) (String) (TypeAttributeDefList) 
                | CreateView (Annotation) (String) (Maybe [String]) (QueryExpr) 
                | Delete (Annotation) (SQIdentifier) (TableRefList) (MaybeBoolExpr) (MaybeSelectList) 
                | DropFunction (Annotation) (IfExists) (StringTypeNameListPairList) (Cascade) 
                | DropSomething (Annotation) (DropType) (IfExists) ([String]) (Cascade) 
                | Execute (Annotation) (ScalarExpr) 
                | ExecuteInto (Annotation) (ScalarExpr) ([String]) 
                | ExitStatement (Annotation) (Maybe String) 
                | ForIntegerStatement (Annotation) (Maybe String) (ScalarExpr) (ScalarExpr) (ScalarExpr) (StatementList) 
                | ForQueryStatement (Annotation) (Maybe String) (ScalarExpr) (QueryExpr) (StatementList) 
                | If (Annotation) (ScalarExprStatementListPairList) (StatementList) 
                | Insert (Annotation) (SQIdentifier) ([String]) (QueryExpr) (MaybeSelectList) 
                | LoopStatement (Annotation) (Maybe String) (StatementList) 
                | Notify (Annotation) (String) 
                | NullStatement (Annotation) 
                | Perform (Annotation) (ScalarExpr) 
                | QueryStatement (Annotation) (QueryExpr) 
                | Raise (Annotation) (RaiseType) (String) (ScalarExprList) 
                | Return (Annotation) (MaybeScalarExpr) 
                | ReturnNext (Annotation) (ScalarExpr) 
                | ReturnQuery (Annotation) (QueryExpr) 
                | Set (Annotation) (String) ([SetValue]) 
                | Truncate (Annotation) ([String]) (RestartIdentity) (Cascade) 
                | Update (Annotation) (SQIdentifier) (ScalarExprList) (TableRefList) (MaybeBoolExpr) (MaybeSelectList) 
                | WhileStatement (Annotation) (Maybe String) (ScalarExpr) (StatementList) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (AlterSequence _ann _name _ownedBy )  =
    (sem_Statement_AlterSequence _ann _name (sem_SQIdentifier _ownedBy ) )
sem_Statement (AlterTable _ann _name _actions )  =
    (sem_Statement_AlterTable _ann _name (sem_AlterTableActionList _actions ) )
sem_Statement (Assignment _ann _target _value )  =
    (sem_Statement_Assignment _ann (sem_ScalarExpr _target ) (sem_ScalarExpr _value ) )
sem_Statement (Block _ann _lb _vars _sts )  =
    (sem_Statement_Block _ann _lb (sem_VarDefList _vars ) (sem_StatementList _sts ) )
sem_Statement (CaseStatement _ann _cases _els )  =
    (sem_Statement_CaseStatement _ann (sem_ScalarExprListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (CaseStatementSimple _ann _val _cases _els )  =
    (sem_Statement_CaseStatementSimple _ann (sem_ScalarExpr _val ) (sem_ScalarExprListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (ContinueStatement _ann _lb )  =
    (sem_Statement_ContinueStatement _ann _lb )
sem_Statement (Copy _ann _table _targetCols _source )  =
    (sem_Statement_Copy _ann _table _targetCols _source )
sem_Statement (CopyData _ann _insData )  =
    (sem_Statement_CopyData _ann _insData )
sem_Statement (CreateDomain _ann _name _typ _checkName _check )  =
    (sem_Statement_CreateDomain _ann _name (sem_TypeName _typ ) _checkName (sem_MaybeBoolExpr _check ) )
sem_Statement (CreateFunction _ann _name _params _rettype _rep _lang _body _vol )  =
    (sem_Statement_CreateFunction _ann _name (sem_ParamDefList _params ) (sem_TypeName _rettype ) _rep _lang (sem_FnBody _body ) _vol )
sem_Statement (CreateLanguage _ann _name )  =
    (sem_Statement_CreateLanguage _ann _name )
sem_Statement (CreateSequence _ann _name _incr _min _max _start _cache )  =
    (sem_Statement_CreateSequence _ann _name _incr _min _max _start _cache )
sem_Statement (CreateTable _ann _name _atts _cons )  =
    (sem_Statement_CreateTable _ann _name (sem_AttributeDefList _atts ) (sem_ConstraintList _cons ) )
sem_Statement (CreateTableAs _ann _name _expr )  =
    (sem_Statement_CreateTableAs _ann _name (sem_QueryExpr _expr ) )
sem_Statement (CreateTrigger _ann _name _wh _events _tbl _firing _fnName _fnArgs )  =
    (sem_Statement_CreateTrigger _ann _name _wh _events _tbl _firing _fnName (sem_ScalarExprList _fnArgs ) )
sem_Statement (CreateType _ann _name _atts )  =
    (sem_Statement_CreateType _ann _name (sem_TypeAttributeDefList _atts ) )
sem_Statement (CreateView _ann _name _colNames _expr )  =
    (sem_Statement_CreateView _ann _name _colNames (sem_QueryExpr _expr ) )
sem_Statement (Delete _ann _table _using _whr _returning )  =
    (sem_Statement_Delete _ann (sem_SQIdentifier _table ) (sem_TableRefList _using ) (sem_MaybeBoolExpr _whr ) (sem_MaybeSelectList _returning ) )
sem_Statement (DropFunction _ann _ifE _sigs _cascade )  =
    (sem_Statement_DropFunction _ann _ifE (sem_StringTypeNameListPairList _sigs ) _cascade )
sem_Statement (DropSomething _ann _dropType _ifE _names _cascade )  =
    (sem_Statement_DropSomething _ann _dropType _ifE _names _cascade )
sem_Statement (Execute _ann _expr )  =
    (sem_Statement_Execute _ann (sem_ScalarExpr _expr ) )
sem_Statement (ExecuteInto _ann _expr _targets )  =
    (sem_Statement_ExecuteInto _ann (sem_ScalarExpr _expr ) _targets )
sem_Statement (ExitStatement _ann _lb )  =
    (sem_Statement_ExitStatement _ann _lb )
sem_Statement (ForIntegerStatement _ann _lb _var _from _to _sts )  =
    (sem_Statement_ForIntegerStatement _ann _lb (sem_ScalarExpr _var ) (sem_ScalarExpr _from ) (sem_ScalarExpr _to ) (sem_StatementList _sts ) )
sem_Statement (ForQueryStatement _ann _lb _var _sel _sts )  =
    (sem_Statement_ForQueryStatement _ann _lb (sem_ScalarExpr _var ) (sem_QueryExpr _sel ) (sem_StatementList _sts ) )
sem_Statement (If _ann _cases _els )  =
    (sem_Statement_If _ann (sem_ScalarExprStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (Insert _ann _table _targetCols _insData _returning )  =
    (sem_Statement_Insert _ann (sem_SQIdentifier _table ) _targetCols (sem_QueryExpr _insData ) (sem_MaybeSelectList _returning ) )
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
    (sem_Statement_Update _ann (sem_SQIdentifier _table ) (sem_ScalarExprList _assigns ) (sem_TableRefList _fromList ) (sem_MaybeBoolExpr _whr ) (sem_MaybeSelectList _returning ) )
sem_Statement (WhileStatement _ann _lb _expr _sts )  =
    (sem_Statement_WhileStatement _ann _lb (sem_ScalarExpr _expr ) (sem_StatementList _sts ) )
-- semantic domain
type T_Statement  = Catalog ->
                    IDEnv ->
                    Catalog ->
                    LocalBindings ->
                    ( Statement,([CatalogUpdate]),Statement,([LocalBindingsUpdate]),Statement)
data Inh_Statement  = Inh_Statement {cat_Inh_Statement :: Catalog,idenv_Inh_Statement :: IDEnv,inProducedCat_Inh_Statement :: Catalog,lib_Inh_Statement :: LocalBindings}
data Syn_Statement  = Syn_Statement {annotatedTree_Syn_Statement :: Statement,catUpdates_Syn_Statement :: [CatalogUpdate],fixedUpIdentifiersTree_Syn_Statement :: Statement,libUpdates_Syn_Statement :: [LocalBindingsUpdate],originalTree_Syn_Statement :: Statement}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIcat _lhsIidenv _lhsIinProducedCat _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIinProducedCat _lhsIlib )
     in  (Syn_Statement _lhsOannotatedTree _lhsOcatUpdates _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOoriginalTree ))
sem_Statement_AlterSequence :: Annotation ->
                               String ->
                               T_SQIdentifier  ->
                               T_Statement 
sem_Statement_AlterSequence ann_ name_ ownedBy_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _ownedByOcat :: Catalog
              _ownedByOidenv :: IDEnv
              _ownedByOlib :: LocalBindings
              _ownedByIannotatedTree :: SQIdentifier
              _ownedByIfixedUpIdentifiersTree :: SQIdentifier
              _ownedByIoriginalTree :: SQIdentifier
              _ownedByItbAnnotatedTree :: SQIdentifier
              _ownedByItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10441 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10446 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10451 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  AlterSequence ann_ name_ _ownedByIannotatedTree
                  {-# LINE 10456 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  AlterSequence ann_ name_ _ownedByIfixedUpIdentifiersTree
                  {-# LINE 10461 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  AlterSequence ann_ name_ _ownedByIoriginalTree
                  {-# LINE 10466 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10471 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 10476 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10481 "AstInternal.hs" #-}
              -- copy rule (down)
              _ownedByOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10486 "AstInternal.hs" #-}
              -- copy rule (down)
              _ownedByOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10491 "AstInternal.hs" #-}
              -- copy rule (down)
              _ownedByOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10496 "AstInternal.hs" #-}
              ( _ownedByIannotatedTree,_ownedByIfixedUpIdentifiersTree,_ownedByIoriginalTree,_ownedByItbAnnotatedTree,_ownedByItbUType) =
                  (ownedBy_ _ownedByOcat _ownedByOidenv _ownedByOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_AlterTable :: Annotation ->
                            String ->
                            T_AlterTableActionList  ->
                            T_Statement 
sem_Statement_AlterTable ann_ name_ actions_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _actionsOcat :: Catalog
              _actionsOidenv :: IDEnv
              _actionsOlib :: LocalBindings
              _actionsIannotatedTree :: AlterTableActionList
              _actionsIfixedUpIdentifiersTree :: AlterTableActionList
              _actionsIoriginalTree :: AlterTableActionList
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10524 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10529 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  AlterTable ann_ name_ _actionsIannotatedTree
                  {-# LINE 10534 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  AlterTable ann_ name_ _actionsIfixedUpIdentifiersTree
                  {-# LINE 10539 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  AlterTable ann_ name_ _actionsIoriginalTree
                  {-# LINE 10544 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10549 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 10554 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10559 "AstInternal.hs" #-}
              -- copy rule (down)
              _actionsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10564 "AstInternal.hs" #-}
              -- copy rule (down)
              _actionsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10569 "AstInternal.hs" #-}
              -- copy rule (down)
              _actionsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10574 "AstInternal.hs" #-}
              ( _actionsIannotatedTree,_actionsIfixedUpIdentifiersTree,_actionsIoriginalTree) =
                  (actions_ _actionsOcat _actionsOidenv _actionsOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Assignment :: Annotation ->
                            T_ScalarExpr  ->
                            T_ScalarExpr  ->
                            T_Statement 
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _valueOexpectedType :: (Maybe Type)
              _targetOexpectedType :: (Maybe Type)
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _targetOcat :: Catalog
              _targetOidenv :: IDEnv
              _targetOlib :: LocalBindings
              _valueOcat :: Catalog
              _valueOidenv :: IDEnv
              _valueOlib :: LocalBindings
              _targetIannotatedTree :: ScalarExpr
              _targetIfixedUpIdentifiersTree :: ScalarExpr
              _targetIoriginalTree :: ScalarExpr
              _targetIuType :: (Maybe Type)
              _valueIannotatedTree :: ScalarExpr
              _valueIfixedUpIdentifiersTree :: ScalarExpr
              _valueIoriginalTree :: ScalarExpr
              _valueIuType :: (Maybe Type)
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 10618 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 10623 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10628 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10633 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 20, column 9)
              _tpe =
                  {-# LINE 20 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  do
                  fromType <- lmt _valueIuType
                  toType <- lmt _targetIuType
                  checkAssignmentValid _lhsIcat fromType toType
                  return $ Pseudo Void
                  {-# LINE 10642 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 26, column 9)
              _backTree =
                  {-# LINE 26 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  Assignment ann_ _targetIannotatedTree _valueIannotatedTree
                  {-# LINE 10647 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 27, column 9)
              _catUpdates =
                  {-# LINE 27 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  []
                  {-# LINE 10652 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 28, column 9)
              _statementType =
                  {-# LINE 28 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 10657 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 113, column 18)
              _valueOexpectedType =
                  {-# LINE 113 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 10662 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 119, column 18)
              _targetOexpectedType =
                  {-# LINE 119 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 10667 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Assignment ann_ _targetIannotatedTree _valueIannotatedTree
                  {-# LINE 10672 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Assignment ann_ _targetIfixedUpIdentifiersTree _valueIfixedUpIdentifiersTree
                  {-# LINE 10677 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Assignment ann_ _targetIoriginalTree _valueIoriginalTree
                  {-# LINE 10682 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 10687 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10692 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10697 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10702 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10707 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10712 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10717 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10722 "AstInternal.hs" #-}
              ( _targetIannotatedTree,_targetIfixedUpIdentifiersTree,_targetIoriginalTree,_targetIuType) =
                  (target_ _targetOcat _targetOexpectedType _targetOidenv _targetOlib )
              ( _valueIannotatedTree,_valueIfixedUpIdentifiersTree,_valueIoriginalTree,_valueIuType) =
                  (value_ _valueOcat _valueOexpectedType _valueOidenv _valueOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Block :: Annotation ->
                       (Maybe String) ->
                       T_VarDefList  ->
                       T_StatementList  ->
                       T_Statement 
sem_Statement_Block ann_ lb_ vars_ sts_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlib :: LocalBindings
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _varsOcat :: Catalog
              _varsOidenv :: IDEnv
              _varsOlib :: LocalBindings
              _stsOcat :: Catalog
              _stsOidenv :: IDEnv
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _varsIannotatedTree :: VarDefList
              _varsIdefs :: ([(String,Maybe Type)])
              _varsIfixedUpIdentifiersTree :: VarDefList
              _varsIoriginalTree :: VarDefList
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10764 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 100, column 13)
              _lhsOcatUpdates =
                  {-# LINE 100 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10769 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 101, column 13)
              _stsOcatUpdates =
                  {-# LINE 101 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10774 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Block.ag"(line 22, column 9)
              _stsOlib =
                  {-# LINE 22 "./TypeChecking/Plpgsql/Block.ag" #-}
                  fromRight _lhsIlib $
                  lbUpdate _lhsIcat
                           (LBIds "declarations" lb_ $ mapMaybe lv _varsIdefs)
                           _lhsIlib
                  where
                    lv (_,Nothing) = Nothing
                    lv (s,Just t) = Just (s,t)
                  {-# LINE 10785 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Block ann_ lb_ _varsIannotatedTree _stsIannotatedTree
                  {-# LINE 10790 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Block ann_ lb_ _varsIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
                  {-# LINE 10795 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Block ann_ lb_ _varsIoriginalTree _stsIoriginalTree
                  {-# LINE 10800 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10805 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 10810 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10815 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOlibUpdates =
                  {-# LINE 19 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10820 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10825 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10830 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10835 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10840 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10845 "AstInternal.hs" #-}
              -- copy rule (from local)
              _stsOlibUpdates =
                  {-# LINE 23 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10850 "AstInternal.hs" #-}
              ( _varsIannotatedTree,_varsIdefs,_varsIfixedUpIdentifiersTree,_varsIoriginalTree) =
                  (vars_ _varsOcat _varsOidenv _varsOlib )
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CaseStatement :: Annotation ->
                               T_ScalarExprListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _elsOcatUpdates :: ([CatalogUpdate])
              _elsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _casesOcat :: Catalog
              _casesOidenv :: IDEnv
              _casesOlib :: LocalBindings
              _elsOcat :: Catalog
              _elsOidenv :: IDEnv
              _elsOlib :: LocalBindings
              _casesIannotatedTree :: ScalarExprListStatementListPairList
              _casesIfixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _casesIoriginalTree :: ScalarExprListStatementListPairList
              _elsIannotatedTree :: StatementList
              _elsIfixedUpIdentifiersTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedCat :: Catalog
              _elsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10890 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10895 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  {-# LINE 134 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10900 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  {-# LINE 135 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10905 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatement ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 10910 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CaseStatement ann_ _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
                  {-# LINE 10915 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatement ann_ _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 10920 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10925 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 10930 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10935 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10940 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10945 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10950 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10955 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10960 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10965 "AstInternal.hs" #-}
              ( _casesIannotatedTree,_casesIfixedUpIdentifiersTree,_casesIoriginalTree) =
                  (cases_ _casesOcat _casesOidenv _casesOlib )
              ( _elsIannotatedTree,_elsIfixedUpIdentifiersTree,_elsIoriginalTree,_elsIproducedCat,_elsIproducedLib) =
                  (els_ _elsOcat _elsOcatUpdates _elsOidenv _elsOlib _elsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CaseStatementSimple :: Annotation ->
                                     T_ScalarExpr  ->
                                     T_ScalarExprListStatementListPairList  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_CaseStatementSimple ann_ val_ cases_ els_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _elsOcatUpdates :: ([CatalogUpdate])
              _elsOlibUpdates :: ([LocalBindingsUpdate])
              _valOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _valOcat :: Catalog
              _valOidenv :: IDEnv
              _valOlib :: LocalBindings
              _casesOcat :: Catalog
              _casesOidenv :: IDEnv
              _casesOlib :: LocalBindings
              _elsOcat :: Catalog
              _elsOidenv :: IDEnv
              _elsOlib :: LocalBindings
              _valIannotatedTree :: ScalarExpr
              _valIfixedUpIdentifiersTree :: ScalarExpr
              _valIoriginalTree :: ScalarExpr
              _valIuType :: (Maybe Type)
              _casesIannotatedTree :: ScalarExprListStatementListPairList
              _casesIfixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _casesIoriginalTree :: ScalarExprListStatementListPairList
              _elsIannotatedTree :: StatementList
              _elsIfixedUpIdentifiersTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedCat :: Catalog
              _elsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11014 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11019 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  {-# LINE 134 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11024 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  {-# LINE 135 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11029 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 114, column 27)
              _valOexpectedType =
                  {-# LINE 114 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 11034 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatementSimple ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 11039 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CaseStatementSimple ann_ _valIfixedUpIdentifiersTree _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
                  {-# LINE 11044 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatementSimple ann_ _valIoriginalTree _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 11049 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11054 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11059 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11064 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11069 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11074 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11079 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11084 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11089 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11094 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11099 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11104 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11109 "AstInternal.hs" #-}
              ( _valIannotatedTree,_valIfixedUpIdentifiersTree,_valIoriginalTree,_valIuType) =
                  (val_ _valOcat _valOexpectedType _valOidenv _valOlib )
              ( _casesIannotatedTree,_casesIfixedUpIdentifiersTree,_casesIoriginalTree) =
                  (cases_ _casesOcat _casesOidenv _casesOlib )
              ( _elsIannotatedTree,_elsIfixedUpIdentifiersTree,_elsIoriginalTree,_elsIproducedCat,_elsIproducedLib) =
                  (els_ _elsOcat _elsOcatUpdates _elsOidenv _elsOlib _elsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ContinueStatement :: Annotation ->
                                   (Maybe String) ->
                                   T_Statement 
sem_Statement_ContinueStatement ann_ lb_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11134 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11139 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ContinueStatement ann_ lb_
                  {-# LINE 11144 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ContinueStatement ann_ lb_
                  {-# LINE 11149 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ContinueStatement ann_ lb_
                  {-# LINE 11154 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11159 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11164 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11169 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Copy :: Annotation ->
                      String ->
                      ([String]) ->
                      CopySource ->
                      T_Statement 
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11190 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11195 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Copy ann_ table_ targetCols_ source_
                  {-# LINE 11200 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Copy ann_ table_ targetCols_ source_
                  {-# LINE 11205 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Copy ann_ table_ targetCols_ source_
                  {-# LINE 11210 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11215 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11220 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11225 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CopyData :: Annotation ->
                          String ->
                          T_Statement 
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11244 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11249 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 11254 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 11259 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 11264 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11269 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11274 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11279 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateDomain :: Annotation ->
                              String ->
                              T_TypeName  ->
                              String ->
                              T_MaybeBoolExpr  ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ checkName_ check_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _checkOlib :: LocalBindings
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _checkOcat :: Catalog
              _checkOidenv :: IDEnv
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              _checkIannotatedTree :: MaybeBoolExpr
              _checkIfixedUpIdentifiersTree :: MaybeBoolExpr
              _checkIoriginalTree :: MaybeBoolExpr
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11320 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11325 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11330 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11335 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 65, column 9)
              _tpe =
                  {-# LINE 65 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11340 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 66, column 9)
              _backTree =
                  {-# LINE 66 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree checkName_ _checkIannotatedTree
                  {-# LINE 11345 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 67, column 9)
              _statementType =
                  {-# LINE 67 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 11350 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 68, column 9)
              _catUpdates =
                  {-# LINE 68 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  maybe [] (\t -> [CatCreateDomain (DomainType name_) t]) _typInamedType
                  {-# LINE 11355 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 70, column 9)
              _checkOlib =
                  {-# LINE 70 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  either (const _lhsIlib) id $ do
                  nt <- lmt _typInamedType
                  lbUpdate _lhsIcat
                    (LBIds "domain check value" Nothing [("value", nt)])
                    _lhsIlib
                  {-# LINE 11364 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree checkName_ _checkIannotatedTree
                  {-# LINE 11369 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateDomain ann_ name_ _typIfixedUpIdentifiersTree checkName_ _checkIfixedUpIdentifiersTree
                  {-# LINE 11374 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIoriginalTree checkName_ _checkIoriginalTree
                  {-# LINE 11379 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11384 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11389 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11394 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11399 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11404 "AstInternal.hs" #-}
              -- copy rule (down)
              _checkOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11409 "AstInternal.hs" #-}
              -- copy rule (down)
              _checkOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11414 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
              ( _checkIannotatedTree,_checkIfixedUpIdentifiersTree,_checkIoriginalTree) =
                  (check_ _checkOcat _checkOidenv _checkOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateFunction :: Annotation ->
                                String ->
                                T_ParamDefList  ->
                                T_TypeName  ->
                                Replace ->
                                Language ->
                                T_FnBody  ->
                                Volatility ->
                                T_Statement 
sem_Statement_CreateFunction ann_ name_ params_ rettype_ rep_ lang_ body_ vol_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _bodyOlib :: LocalBindings
              _paramsOpos :: Int
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _bodyOcat :: Catalog
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _paramsOcat :: Catalog
              _paramsOidenv :: IDEnv
              _paramsOlib :: LocalBindings
              _rettypeOcat :: Catalog
              _rettypeOidenv :: IDEnv
              _rettypeOlib :: LocalBindings
              _bodyOidenv :: IDEnv
              _paramsIannotatedTree :: ParamDefList
              _paramsIfixedUpIdentifiersTree :: ParamDefList
              _paramsIoriginalTree :: ParamDefList
              _paramsIparams :: ([(ParamName, Maybe Type)])
              _rettypeIannotatedTree :: TypeName
              _rettypeIfixedUpIdentifiersTree :: TypeName
              _rettypeInamedType :: (Maybe Type)
              _rettypeIoriginalTree :: TypeName
              _bodyIannotatedTree :: FnBody
              _bodyIfixedUpIdentifiersTree :: FnBody
              _bodyIoriginalTree :: FnBody
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11470 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11475 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11480 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11485 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 63, column 9)
              _bodyOlib =
                  {-# LINE 63 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  either (const _lhsIlib) id $ do
                  _ <- lmt _rettypeInamedType
                  lbUpdate _lhsIcat (LBIds (name_ ++ " parameters") (Just name_) paramsNoPos) _lhsIlib
                  >>= lbUpdate _lhsIcat (LBIds (name_ ++ " parameters") Nothing paramsPosOnly)
                  where
                    paramsPosOnly :: [(String,Type)]
                    paramsPosOnly = mapMaybe prm _paramsIparams
                    prm :: (ParamName,Maybe Type) -> Maybe (String,Type)
                    prm (NamedParam p _,Just t) = Just ("$" ++ show p, t)
                    prm (UnnamedParam p,Just t) = Just ("$" ++ show p, t)
                    prm _ = Nothing
                    paramsNoPos :: [(String,Type)]
                    paramsNoPos = mapMaybe pnp _paramsIparams
                    pnp :: (ParamName,Maybe Type) -> Maybe (String,Type)
                    pnp (NamedParam _ n,Just t) = Just (n,t)
                    pnp _ = Nothing
                  {-# LINE 11505 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 79, column 9)
              _paramsOpos =
                  {-# LINE 79 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  1
                  {-# LINE 11510 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 88, column 9)
              _tpe =
                  {-# LINE 88 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11515 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 89, column 9)
              _catUpdates =
                  {-# LINE 89 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  either (const []) id $ do
                  let ps = mapMaybe lpt _paramsIparams
                  rt <- lmt _rettypeInamedType
                  return [CatCreateFunction FunName
                                            (map toLower name_)
                                            ps
                                            rt
                                            False]
                  where
                    lpt (_,Just t) = Just t
                    lpt _ = Nothing
                  {-# LINE 11530 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 101, column 9)
              _backTree =
                  {-# LINE 101 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  CreateFunction ann_
                                 name_
                                 _paramsIannotatedTree
                                 _rettypeIannotatedTree
                                 rep_
                                 lang_
                                 _bodyIannotatedTree
                                 vol_
                  {-# LINE 11542 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 109, column 9)
              _statementType =
                  {-# LINE 109 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  Nothing
                  {-# LINE 11547 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateFunction.ag"(line 110, column 9)
              _bodyOcat =
                  {-# LINE 110 "./TypeChecking/Ddl/CreateFunction.ag" #-}
                  _lhsIinProducedCat
                  {-# LINE 11552 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CreateFunction ann_ name_ _paramsIannotatedTree _rettypeIannotatedTree rep_ lang_ _bodyIannotatedTree vol_
                  {-# LINE 11557 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateFunction ann_ name_ _paramsIfixedUpIdentifiersTree _rettypeIfixedUpIdentifiersTree rep_ lang_ _bodyIfixedUpIdentifiersTree vol_
                  {-# LINE 11562 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CreateFunction ann_ name_ _paramsIoriginalTree _rettypeIoriginalTree rep_ lang_ _bodyIoriginalTree vol_
                  {-# LINE 11567 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11572 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11577 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11582 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11587 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11592 "AstInternal.hs" #-}
              -- copy rule (down)
              _rettypeOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11597 "AstInternal.hs" #-}
              -- copy rule (down)
              _rettypeOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11602 "AstInternal.hs" #-}
              -- copy rule (down)
              _rettypeOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11607 "AstInternal.hs" #-}
              -- copy rule (down)
              _bodyOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11612 "AstInternal.hs" #-}
              ( _paramsIannotatedTree,_paramsIfixedUpIdentifiersTree,_paramsIoriginalTree,_paramsIparams) =
                  (params_ _paramsOcat _paramsOidenv _paramsOlib _paramsOpos )
              ( _rettypeIannotatedTree,_rettypeIfixedUpIdentifiersTree,_rettypeInamedType,_rettypeIoriginalTree) =
                  (rettype_ _rettypeOcat _rettypeOidenv _rettypeOlib )
              ( _bodyIannotatedTree,_bodyIfixedUpIdentifiersTree,_bodyIoriginalTree) =
                  (body_ _bodyOcat _bodyOidenv _bodyOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateLanguage :: Annotation ->
                                String ->
                                T_Statement 
sem_Statement_CreateLanguage ann_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11643 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11648 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11653 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11658 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 78, column 9)
              _tpe =
                  {-# LINE 78 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11663 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 79, column 9)
              _backTree =
                  {-# LINE 79 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11668 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 80, column 9)
              _statementType =
                  {-# LINE 80 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 11673 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 81, column 9)
              _catUpdates =
                  {-# LINE 81 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  [CatCreateFunction FunName "plpgsql_call_handler" [] (Pseudo LanguageHandler) False
                  ,CatCreateFunction FunName "plpgsql_validator" [ScalarType "oid"] (Pseudo Void) False]
                  {-# LINE 11679 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11684 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11689 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11694 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11699 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11704 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateSequence :: Annotation ->
                                String ->
                                Integer ->
                                Integer ->
                                Integer ->
                                Integer ->
                                Integer ->
                                T_Statement 
sem_Statement_CreateSequence ann_ name_ incr_ min_ max_ start_ cache_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11728 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11733 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11738 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
                  {-# LINE 11743 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
                  {-# LINE 11748 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
                  {-# LINE 11753 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11758 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11763 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11768 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateTable :: Annotation ->
                             String ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _attrs :: ([(String,Type)])
              _statementType :: (Maybe StatementType)
              _consOlib :: LocalBindings
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _attsOcat :: Catalog
              _attsOidenv :: IDEnv
              _attsOlib :: LocalBindings
              _consOcat :: Catalog
              _consOidenv :: IDEnv
              _attsIannotatedTree :: AttributeDefList
              _attsIattrs :: ([(String, Maybe Type)])
              _attsIfixedUpIdentifiersTree :: AttributeDefList
              _attsIoriginalTree :: AttributeDefList
              _consIannotatedTree :: ConstraintList
              _consIfixedUpIdentifiersTree :: ConstraintList
              _consIoriginalTree :: ConstraintList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11809 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11814 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11819 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11824 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 31, column 9)
              _tpe =
                  {-# LINE 31 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11829 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 32, column 9)
              _catUpdates =
                  {-# LINE 32 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  [CatCreateTable name_ _attrs     defaultSystemColumns]
                  {-# LINE 11834 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 35, column 9)
              _attrs =
                  {-# LINE 35 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  mapMaybe okAt _attsIattrs
                  where
                    okAt (s, Just t) = Just (s,t)
                    okAt (_,Nothing) = Nothing
                  {-# LINE 11842 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 40, column 9)
              _statementType =
                  {-# LINE 40 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  Nothing
                  {-# LINE 11847 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 41, column 9)
              _backTree =
                  {-# LINE 41 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  CreateTable ann_
                              name_
                              _attsIannotatedTree
                              _consIannotatedTree
                  {-# LINE 11855 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 45, column 9)
              _consOlib =
                  {-# LINE 45 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  case lbUpdate _lhsIcat
                         (LBIds "attributedefs" Nothing _attrs    )
                         _lhsIlib of
                     Left x -> error $ "statement-createtable-cons.lib " ++ show x
                     Right e -> e
                  {-# LINE 11864 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
                  {-# LINE 11869 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateTable ann_ name_ _attsIfixedUpIdentifiersTree _consIfixedUpIdentifiersTree
                  {-# LINE 11874 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIoriginalTree _consIoriginalTree
                  {-# LINE 11879 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11884 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11889 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11894 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11899 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11904 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11909 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11914 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIattrs,_attsIfixedUpIdentifiersTree,_attsIoriginalTree) =
                  (atts_ _attsOcat _attsOidenv _attsOlib )
              ( _consIannotatedTree,_consIfixedUpIdentifiersTree,_consIoriginalTree) =
                  (cons_ _consOcat _consOidenv _consOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateTableAs :: Annotation ->
                               String ->
                               T_QueryExpr  ->
                               T_Statement 
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _attrs :: (Either [TypeError] [(String,Type)])
              _statementType :: (Maybe StatementType)
              _exprOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: QueryExpr
              _exprIcidenv :: IDEnv
              _exprIfixedUpIdentifiersTree :: QueryExpr
              _exprIlibUpdates :: ([LocalBindingsUpdate])
              _exprIoriginalTree :: QueryExpr
              _exprIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11955 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11960 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11965 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11970 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 64, column 9)
              _tpe =
                  {-# LINE 64 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  CompositeType <$> lmt _exprIuType
                  {-# LINE 11975 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 65, column 9)
              _catUpdates =
                  {-# LINE 65 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  either (const []) id $ do
                  ats <- _attrs
                  return [CatCreateTable name_ ats defaultSystemColumns]
                  {-# LINE 11982 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 71, column 9)
              _attrs =
                  {-# LINE 71 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  lmt _exprIuType
                  {-# LINE 11987 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 73, column 9)
              _backTree =
                  {-# LINE 73 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 11992 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/CreateTable.ag"(line 74, column 9)
              _statementType =
                  {-# LINE 74 "./TypeChecking/Ddl/CreateTable.ag" #-}
                  Nothing
                  {-# LINE 11997 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 177, column 32)
              _exprOexpectedTypes =
                  {-# LINE 177 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 12002 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 12007 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateTableAs ann_ name_ _exprIfixedUpIdentifiersTree
                  {-# LINE 12012 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIoriginalTree
                  {-# LINE 12017 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12022 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12027 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12032 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12037 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12042 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIcidenv,_exprIfixedUpIdentifiersTree,_exprIlibUpdates,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedTypes _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateTrigger :: Annotation ->
                               String ->
                               TriggerWhen ->
                               ([TriggerEvent]) ->
                               String ->
                               TriggerFire ->
                               String ->
                               T_ScalarExprList  ->
                               T_Statement 
sem_Statement_CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ fnArgs_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _fnArgsOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _fnArgsOcat :: Catalog
              _fnArgsOidenv :: IDEnv
              _fnArgsOlib :: LocalBindings
              _fnArgsIannotatedTree :: ScalarExprList
              _fnArgsIfixedUpIdentifiersTree :: ScalarExprList
              _fnArgsIoriginalTree :: ScalarExprList
              _fnArgsIuType :: ([Maybe Type])
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12077 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12082 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 155, column 21)
              _fnArgsOexpectedTypes =
                  {-# LINE 155 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 12087 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIannotatedTree
                  {-# LINE 12092 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIfixedUpIdentifiersTree
                  {-# LINE 12097 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIoriginalTree
                  {-# LINE 12102 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12107 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12112 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12117 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnArgsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12122 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnArgsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12127 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnArgsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12132 "AstInternal.hs" #-}
              ( _fnArgsIannotatedTree,_fnArgsIfixedUpIdentifiersTree,_fnArgsIoriginalTree,_fnArgsIuType) =
                  (fnArgs_ _fnArgsOcat _fnArgsOexpectedTypes _fnArgsOidenv _fnArgsOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateType :: Annotation ->
                            String ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _attsOcat :: Catalog
              _attsOidenv :: IDEnv
              _attsOlib :: LocalBindings
              _attsIannotatedTree :: TypeAttributeDefList
              _attsIattrs :: ([(String, Maybe Type)])
              _attsIfixedUpIdentifiersTree :: TypeAttributeDefList
              _attsIoriginalTree :: TypeAttributeDefList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 12167 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12172 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12177 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12182 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 48, column 9)
              _tpe =
                  {-# LINE 48 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12187 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 49, column 9)
              _attrs =
                  {-# LINE 49 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  mapMaybe okAt _attsIattrs
                  where
                    okAt (s, Just t) = Just (s,t)
                    okAt (_,Nothing) = Nothing
                  {-# LINE 12195 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 53, column 9)
              _backTree =
                  {-# LINE 53 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 12200 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 54, column 9)
              _statementType =
                  {-# LINE 54 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 12205 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 55, column 9)
              _catUpdates =
                  {-# LINE 55 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  [CatCreateComposite name_ _attrs    ]
                  {-# LINE 12210 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 12215 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateType ann_ name_ _attsIfixedUpIdentifiersTree
                  {-# LINE 12220 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIoriginalTree
                  {-# LINE 12225 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12230 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12235 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12240 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12245 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12250 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIattrs,_attsIfixedUpIdentifiersTree,_attsIoriginalTree) =
                  (atts_ _attsOcat _attsOidenv _attsOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateView :: Annotation ->
                            String ->
                            (Maybe [String]) ->
                            T_QueryExpr  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ colNames_ expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _exprOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: QueryExpr
              _exprIcidenv :: IDEnv
              _exprIfixedUpIdentifiersTree :: QueryExpr
              _exprIlibUpdates :: ([LocalBindingsUpdate])
              _exprIoriginalTree :: QueryExpr
              _exprIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 12289 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12294 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12299 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12304 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 15, column 9)
              _tpe =
                  {-# LINE 15 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12309 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 16, column 9)
              _backTree =
                  {-# LINE 16 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  CreateView ann_ name_ colNames_ _exprIannotatedTree
                  {-# LINE 12314 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 17, column 9)
              _catUpdates =
                  {-# LINE 17 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  maybe [] (\a -> [CatCreateView name_ a]) _exprIuType
                  {-# LINE 12319 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 19, column 9)
              _statementType =
                  {-# LINE 19 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 12324 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 177, column 32)
              _exprOexpectedTypes =
                  {-# LINE 177 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 12329 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  CreateView ann_ name_ colNames_ _exprIannotatedTree
                  {-# LINE 12334 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateView ann_ name_ colNames_ _exprIfixedUpIdentifiersTree
                  {-# LINE 12339 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  CreateView ann_ name_ colNames_ _exprIoriginalTree
                  {-# LINE 12344 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12349 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12354 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12359 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12364 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12369 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIcidenv,_exprIfixedUpIdentifiersTree,_exprIlibUpdates,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedTypes _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Delete :: Annotation ->
                        T_SQIdentifier  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Delete ann_ table_ using_ whr_ returning_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _whrOlib :: LocalBindings
              _returningOlib :: LocalBindings
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _tableOcat :: Catalog
              _tableOidenv :: IDEnv
              _tableOlib :: LocalBindings
              _usingOcat :: Catalog
              _usingOidenv :: IDEnv
              _usingOlib :: LocalBindings
              _whrOcat :: Catalog
              _whrOidenv :: IDEnv
              _returningOcat :: Catalog
              _returningOidenv :: IDEnv
              _tableIannotatedTree :: SQIdentifier
              _tableIfixedUpIdentifiersTree :: SQIdentifier
              _tableIoriginalTree :: SQIdentifier
              _tableItbAnnotatedTree :: SQIdentifier
              _tableItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _usingIannotatedTree :: TableRefList
              _usingIfixedUpIdentifiersTree :: TableRefList
              _usingIlibUpdates :: ([LocalBindingsUpdate])
              _usingInewLib2 :: LocalBindings
              _usingIoriginalTree :: TableRefList
              _usingItrefIDs :: ([(String,[String])])
              _whrIannotatedTree :: MaybeBoolExpr
              _whrIfixedUpIdentifiersTree :: MaybeBoolExpr
              _whrIoriginalTree :: MaybeBoolExpr
              _returningIannotatedTree :: MaybeSelectList
              _returningIfixedUpIdentifiersTree :: MaybeSelectList
              _returningIlistType :: ([(String,Maybe Type)])
              _returningIoriginalTree :: MaybeSelectList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 12429 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12434 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12439 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12444 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Delete.ag"(line 13, column 9)
              _tpe =
                  {-# LINE 13 "./TypeChecking/Dml/Delete.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12449 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Delete.ag"(line 14, column 9)
              _statementType =
                  {-# LINE 14 "./TypeChecking/Dml/Delete.ag" #-}
                  do
                  pt <- sequence $ getPlaceholderTypes _whrIannotatedTree
                  lt <- liftList _returningIlistType
                  return (pt,lt)
                  {-# LINE 12457 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Delete.ag"(line 19, column 9)
              _backTree =
                  {-# LINE 19 "./TypeChecking/Dml/Delete.ag" #-}
                  Delete ann_ _tableItbAnnotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
                  {-# LINE 12462 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Delete.ag"(line 20, column 9)
              _catUpdates =
                  {-# LINE 20 "./TypeChecking/Dml/Delete.ag" #-}
                  []
                  {-# LINE 12467 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Delete.ag"(line 22, column 9)
              _lib =
                  {-# LINE 22 "./TypeChecking/Dml/Delete.ag" #-}
                  either (const _lhsIlib) id $ do
                  a <- lmt (allAtts <$> _tableItbUType)
                  lbUpdate _lhsIcat (LBIds "delete table attrs" (Just $ getTName _tableIannotatedTree) a) _lhsIlib
                  {-# LINE 12474 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Delete.ag"(line 26, column 9)
              _whrOlib =
                  {-# LINE 26 "./TypeChecking/Dml/Delete.ag" #-}
                  _lib
                  {-# LINE 12479 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Delete.ag"(line 27, column 9)
              _returningOlib =
                  {-# LINE 27 "./TypeChecking/Dml/Delete.ag" #-}
                  _lib
                  {-# LINE 12484 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Delete ann_ _tableIannotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
                  {-# LINE 12489 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Delete ann_ _tableIfixedUpIdentifiersTree _usingIfixedUpIdentifiersTree _whrIfixedUpIdentifiersTree _returningIfixedUpIdentifiersTree
                  {-# LINE 12494 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Delete ann_ _tableIoriginalTree _usingIoriginalTree _whrIoriginalTree _returningIoriginalTree
                  {-# LINE 12499 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12504 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12509 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12514 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12519 "AstInternal.hs" #-}
              -- copy rule (from local)
              _tableOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 12524 "AstInternal.hs" #-}
              -- copy rule (down)
              _usingOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12529 "AstInternal.hs" #-}
              -- copy rule (down)
              _usingOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12534 "AstInternal.hs" #-}
              -- copy rule (from local)
              _usingOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 12539 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12544 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12549 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12554 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12559 "AstInternal.hs" #-}
              ( _tableIannotatedTree,_tableIfixedUpIdentifiersTree,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType) =
                  (table_ _tableOcat _tableOidenv _tableOlib )
              ( _usingIannotatedTree,_usingIfixedUpIdentifiersTree,_usingIlibUpdates,_usingInewLib2,_usingIoriginalTree,_usingItrefIDs) =
                  (using_ _usingOcat _usingOidenv _usingOlib )
              ( _whrIannotatedTree,_whrIfixedUpIdentifiersTree,_whrIoriginalTree) =
                  (whr_ _whrOcat _whrOidenv _whrOlib )
              ( _returningIannotatedTree,_returningIfixedUpIdentifiersTree,_returningIlistType,_returningIoriginalTree) =
                  (returning_ _returningOcat _returningOidenv _returningOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_DropFunction :: Annotation ->
                              IfExists ->
                              T_StringTypeNameListPairList  ->
                              Cascade ->
                              T_Statement 
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _sigsOcat :: Catalog
              _sigsOidenv :: IDEnv
              _sigsOlib :: LocalBindings
              _sigsIannotatedTree :: StringTypeNameListPairList
              _sigsIfixedUpIdentifiersTree :: StringTypeNameListPairList
              _sigsIfnSigs :: ([(String,[Maybe Type])])
              _sigsIoriginalTree :: StringTypeNameListPairList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 12601 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12606 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12611 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12616 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/Drops.ag"(line 10, column 9)
              _tpe =
                  {-# LINE 10 "./TypeChecking/Ddl/Drops.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12621 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/Drops.ag"(line 11, column 9)
              _backTree =
                  {-# LINE 11 "./TypeChecking/Ddl/Drops.ag" #-}
                  DropFunction ann_ ifE_ _sigsIannotatedTree cascade_
                  {-# LINE 12626 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/Drops.ag"(line 12, column 9)
              _catUpdates =
                  {-# LINE 12 "./TypeChecking/Ddl/Drops.ag" #-}
                  either (const []) id $
                  Right $ map mcu $ mapMaybe goodSig _sigsIfnSigs
                  where
                    mcu :: (String,[Type]) -> CatalogUpdate
                    mcu (nm,args) = CatDropFunction ifE nm args
                    ifE = ifE_ == IfExists
                    goodSig :: (String,[Maybe Type]) -> Maybe (String,[Type])
                    goodSig (s, ts) = do
                                  ts1 <- sequence ts
                                  return (s,ts1)
                  {-# LINE 12640 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/Drops.ag"(line 23, column 9)
              _statementType =
                  {-# LINE 23 "./TypeChecking/Ddl/Drops.ag" #-}
                  Nothing
                  {-# LINE 12645 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  DropFunction ann_ ifE_ _sigsIannotatedTree cascade_
                  {-# LINE 12650 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  DropFunction ann_ ifE_ _sigsIfixedUpIdentifiersTree cascade_
                  {-# LINE 12655 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  DropFunction ann_ ifE_ _sigsIoriginalTree cascade_
                  {-# LINE 12660 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12665 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12670 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12675 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12680 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12685 "AstInternal.hs" #-}
              ( _sigsIannotatedTree,_sigsIfixedUpIdentifiersTree,_sigsIfnSigs,_sigsIoriginalTree) =
                  (sigs_ _sigsOcat _sigsOidenv _sigsOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_DropSomething :: Annotation ->
                               DropType ->
                               IfExists ->
                               ([String]) ->
                               Cascade ->
                               T_Statement 
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12709 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12714 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
                  {-# LINE 12719 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
                  {-# LINE 12724 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
                  {-# LINE 12729 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12734 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12739 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12744 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Execute :: Annotation ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12771 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12776 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 116, column 9)
              _exprOexpectedType =
                  {-# LINE 116 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 12781 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Execute ann_ _exprIannotatedTree
                  {-# LINE 12786 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Execute ann_ _exprIfixedUpIdentifiersTree
                  {-# LINE 12791 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Execute ann_ _exprIoriginalTree
                  {-# LINE 12796 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12801 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12806 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12811 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12816 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12821 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12826 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ExecuteInto :: Annotation ->
                             T_ScalarExpr  ->
                             ([String]) ->
                             T_Statement 
sem_Statement_ExecuteInto ann_ expr_ targets_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12856 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12861 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 116, column 9)
              _exprOexpectedType =
                  {-# LINE 116 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 12866 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIannotatedTree targets_
                  {-# LINE 12871 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ExecuteInto ann_ _exprIfixedUpIdentifiersTree targets_
                  {-# LINE 12876 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIoriginalTree targets_
                  {-# LINE 12881 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12886 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12891 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12896 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12901 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12906 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12911 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ExitStatement :: Annotation ->
                               (Maybe String) ->
                               T_Statement 
sem_Statement_ExitStatement ann_ lb_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12932 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12937 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ExitStatement ann_ lb_
                  {-# LINE 12942 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ExitStatement ann_ lb_
                  {-# LINE 12947 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ExitStatement ann_ lb_
                  {-# LINE 12952 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12957 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12962 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12967 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ForIntegerStatement :: Annotation ->
                                     (Maybe String) ->
                                     T_ScalarExpr  ->
                                     T_ScalarExpr  ->
                                     T_ScalarExpr  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement ann_ lb_ var_ from_ to_ sts_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _stsOlib :: LocalBindings
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _fromOexpectedType :: (Maybe Type)
              _toOexpectedType :: (Maybe Type)
              _varOexpectedType :: (Maybe Type)
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _varOcat :: Catalog
              _varOidenv :: IDEnv
              _varOlib :: LocalBindings
              _fromOcat :: Catalog
              _fromOidenv :: IDEnv
              _fromOlib :: LocalBindings
              _toOcat :: Catalog
              _toOidenv :: IDEnv
              _toOlib :: LocalBindings
              _stsOcat :: Catalog
              _stsOidenv :: IDEnv
              _varIannotatedTree :: ScalarExpr
              _varIfixedUpIdentifiersTree :: ScalarExpr
              _varIoriginalTree :: ScalarExpr
              _varIuType :: (Maybe Type)
              _fromIannotatedTree :: ScalarExpr
              _fromIfixedUpIdentifiersTree :: ScalarExpr
              _fromIoriginalTree :: ScalarExpr
              _fromIuType :: (Maybe Type)
              _toIannotatedTree :: ScalarExpr
              _toIfixedUpIdentifiersTree :: ScalarExpr
              _toIoriginalTree :: ScalarExpr
              _toIuType :: (Maybe Type)
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 13030 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 13035 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 13040 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13045 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13050 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13055 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 32, column 9)
              _tpe =
                  {-# LINE 32 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  do
                  fromType <- lmt _fromIuType
                  toType <- lmt _toIuType
                  errorWhen (fromType /= toType) [FromToTypesNotSame fromType toType]
                  case _varIuType of
                    Just t -> checkAssignmentValid _lhsIcat fromType t
                    Nothing -> return ()
                  return $ Pseudo Void
                  {-# LINE 13067 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 41, column 9)
              _implicitVar =
                  {-# LINE 41 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  case _varIannotatedTree of
                      Identifier a i | errs a == [UnrecognisedIdentifier i] -> True
                      _ -> False
                  {-# LINE 13074 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 44, column 9)
              _stsOlib =
                  {-# LINE 44 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  if _implicitVar
                  then either (const _lhsIlib) id $ do
                       ft <- lmt _fromIuType
                       lbUpdate _lhsIcat
                          (LBIds "local for loop variable" Nothing [((getName _varIannotatedTree),ft)]) _lhsIlib
                  else _lhsIlib
                  {-# LINE 13084 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 52, column 9)
              _backTree =
                  {-# LINE 52 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  let i = if _implicitVar
                          then let (Identifier a i') = _varIannotatedTree
                               in Identifier a { errs = []} i'
                          else _varIannotatedTree
                  in ForIntegerStatement ann_ lb_ i _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 13093 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 58, column 9)
              _catUpdates =
                  {-# LINE 58 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  []
                  {-# LINE 13098 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 59, column 9)
              _statementType =
                  {-# LINE 59 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 13103 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 117, column 27)
              _fromOexpectedType =
                  {-# LINE 117 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 13108 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 118, column 27)
              _toOexpectedType =
                  {-# LINE 118 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 13113 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 120, column 45)
              _varOexpectedType =
                  {-# LINE 120 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 13118 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ForIntegerStatement ann_ lb_ _varIannotatedTree _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 13123 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ForIntegerStatement ann_ lb_ _varIfixedUpIdentifiersTree _fromIfixedUpIdentifiersTree _toIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
                  {-# LINE 13128 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ForIntegerStatement ann_ lb_ _varIoriginalTree _fromIoriginalTree _toIoriginalTree _stsIoriginalTree
                  {-# LINE 13133 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13138 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13143 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13148 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13153 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13158 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13163 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13168 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13173 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13178 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13183 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13188 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13193 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13198 "AstInternal.hs" #-}
              ( _varIannotatedTree,_varIfixedUpIdentifiersTree,_varIoriginalTree,_varIuType) =
                  (var_ _varOcat _varOexpectedType _varOidenv _varOlib )
              ( _fromIannotatedTree,_fromIfixedUpIdentifiersTree,_fromIoriginalTree,_fromIuType) =
                  (from_ _fromOcat _fromOexpectedType _fromOidenv _fromOlib )
              ( _toIannotatedTree,_toIfixedUpIdentifiersTree,_toIoriginalTree,_toIuType) =
                  (to_ _toOcat _toOexpectedType _toOidenv _toOlib )
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ForQueryStatement :: Annotation ->
                                   (Maybe String) ->
                                   T_ScalarExpr  ->
                                   T_QueryExpr  ->
                                   T_StatementList  ->
                                   T_Statement 
sem_Statement_ForQueryStatement ann_ lb_ var_ sel_ sts_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _stsOlib :: LocalBindings
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _varOexpectedType :: (Maybe Type)
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _varOcat :: Catalog
              _varOidenv :: IDEnv
              _varOlib :: LocalBindings
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _stsOcat :: Catalog
              _stsOidenv :: IDEnv
              _varIannotatedTree :: ScalarExpr
              _varIfixedUpIdentifiersTree :: ScalarExpr
              _varIoriginalTree :: ScalarExpr
              _varIuType :: (Maybe Type)
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 13262 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 13267 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 13272 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13277 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13282 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13287 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 64, column 9)
              _tpe =
                  {-# LINE 64 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  do
                  st <- lmt (CompositeType <$> _selIuType)
                  toType <- lmt _varIuType
                  checkAssignmentValid _lhsIcat st toType
                  return $ Pseudo Void
                  {-# LINE 13296 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 74, column 9)
              _stsOlib =
                  {-# LINE 74 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  either (const _lhsIlib) id $ do
                  _ <- _tpe
                  st <- lmt (CompositeType <$> _selIuType)
                  lbUpdate _lhsIcat (LBIds "for loop record type" Nothing [(getName _varIannotatedTree,st)]) _lhsIlib
                  {-# LINE 13304 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 80, column 9)
              _backTree =
                  {-# LINE 80 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  ForQueryStatement ann_ lb_ _varIannotatedTree _selIannotatedTree _stsIannotatedTree
                  {-# LINE 13309 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 81, column 9)
              _catUpdates =
                  {-# LINE 81 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  []
                  {-# LINE 13314 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 82, column 9)
              _statementType =
                  {-# LINE 82 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 13319 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 120, column 45)
              _varOexpectedType =
                  {-# LINE 120 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 13324 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 179, column 9)
              _selOexpectedTypes =
                  {-# LINE 179 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 13329 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ForQueryStatement ann_ lb_ _varIannotatedTree _selIannotatedTree _stsIannotatedTree
                  {-# LINE 13334 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ForQueryStatement ann_ lb_ _varIfixedUpIdentifiersTree _selIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
                  {-# LINE 13339 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ForQueryStatement ann_ lb_ _varIoriginalTree _selIoriginalTree _stsIoriginalTree
                  {-# LINE 13344 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13349 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13354 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13359 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13364 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13369 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13374 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13379 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13384 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13389 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13394 "AstInternal.hs" #-}
              ( _varIannotatedTree,_varIfixedUpIdentifiersTree,_varIoriginalTree,_varIuType) =
                  (var_ _varOcat _varOexpectedType _varOidenv _varOlib )
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_If :: Annotation ->
                    T_ScalarExprStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _elsOcatUpdates :: ([CatalogUpdate])
              _elsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _casesOcat :: Catalog
              _casesOidenv :: IDEnv
              _casesOlib :: LocalBindings
              _elsOcat :: Catalog
              _elsOidenv :: IDEnv
              _elsOlib :: LocalBindings
              _casesIannotatedTree :: ScalarExprStatementListPairList
              _casesIfixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _casesIoriginalTree :: ScalarExprStatementListPairList
              _elsIannotatedTree :: StatementList
              _elsIfixedUpIdentifiersTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedCat :: Catalog
              _elsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13436 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13441 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  {-# LINE 134 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13446 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  {-# LINE 135 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13451 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  If ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 13456 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  If ann_ _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
                  {-# LINE 13461 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  If ann_ _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 13466 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13471 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13476 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13481 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13486 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13491 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13496 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13501 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13506 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13511 "AstInternal.hs" #-}
              ( _casesIannotatedTree,_casesIfixedUpIdentifiersTree,_casesIoriginalTree) =
                  (cases_ _casesOcat _casesOidenv _casesOlib )
              ( _elsIannotatedTree,_elsIfixedUpIdentifiersTree,_elsIoriginalTree,_elsIproducedCat,_elsIproducedLib) =
                  (els_ _elsOcat _elsOcatUpdates _elsOidenv _elsOlib _elsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Insert :: Annotation ->
                        T_SQIdentifier  ->
                        ([String]) ->
                        T_QueryExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _columnTypes :: (Either [TypeError] [(String,Type)])
              _catUpdates :: ([CatalogUpdate])
              _insDataOexpectedTypes :: ([Maybe Type])
              _returningOlib :: LocalBindings
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _tableOcat :: Catalog
              _tableOidenv :: IDEnv
              _tableOlib :: LocalBindings
              _insDataOcat :: Catalog
              _insDataOidenv :: IDEnv
              _insDataOlib :: LocalBindings
              _returningOcat :: Catalog
              _returningOidenv :: IDEnv
              _tableIannotatedTree :: SQIdentifier
              _tableIfixedUpIdentifiersTree :: SQIdentifier
              _tableIoriginalTree :: SQIdentifier
              _tableItbAnnotatedTree :: SQIdentifier
              _tableItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _insDataIannotatedTree :: QueryExpr
              _insDataIcidenv :: IDEnv
              _insDataIfixedUpIdentifiersTree :: QueryExpr
              _insDataIlibUpdates :: ([LocalBindingsUpdate])
              _insDataIoriginalTree :: QueryExpr
              _insDataIuType :: (Maybe [(String,Type)])
              _returningIannotatedTree :: MaybeSelectList
              _returningIfixedUpIdentifiersTree :: MaybeSelectList
              _returningIlistType :: ([(String,Maybe Type)])
              _returningIoriginalTree :: MaybeSelectList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 13569 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 13574 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 13579 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13584 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Insert.ag"(line 14, column 9)
              _tpe =
                  {-# LINE 14 "./TypeChecking/Dml/Insert.ag" #-}
                  either Left (const $ Right $ Pseudo Void) _columnTypes
                  {-# LINE 13589 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Insert.ag"(line 15, column 9)
              _statementType =
                  {-# LINE 15 "./TypeChecking/Dml/Insert.ag" #-}
                  Just (catMaybes $ getPlaceholderTypes _insDataIannotatedTree
                       ,fromMaybe [] $ liftList _returningIlistType)
                  {-# LINE 13595 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Insert.ag"(line 20, column 9)
              _columnTypes =
                  {-# LINE 20 "./TypeChecking/Dml/Insert.ag" #-}
                  do
                  atts <- lmt (allAtts <$> _tableItbUType)
                  pAtts <- lmt (fst <$> _tableItbUType)
                  tAtts <- case targetCols_ of
                                [] -> return pAtts
                                _ -> mapM (lkpA atts) targetCols_
                  expAtts <- lmt _insDataIuType
                  checkAssignmentsValid _lhsIcat (map snd expAtts) (map snd tAtts)
                  return tAtts
                  where
                    lkpA :: [(String,Type)] -> String -> E (String,Type)
                    lkpA m n = maybe (Left [UnrecognisedIdentifier n])
                                     (\t -> Right (n,t))
                                     $ lookup n m
                  {-# LINE 13613 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Insert.ag"(line 36, column 9)
              _backTree =
                  {-# LINE 36 "./TypeChecking/Dml/Insert.ag" #-}
                  Insert ann_ _tableItbAnnotatedTree
                         targetCols_
                         _insDataIannotatedTree
                         _returningIannotatedTree
                  {-# LINE 13621 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Insert.ag"(line 40, column 9)
              _catUpdates =
                  {-# LINE 40 "./TypeChecking/Dml/Insert.ag" #-}
                  []
                  {-# LINE 13626 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Insert.ag"(line 41, column 9)
              _insDataOexpectedTypes =
                  {-# LINE 41 "./TypeChecking/Dml/Insert.ag" #-}
                  maybe [] id $ do
                  ts <- etmt $ _columnTypes
                  return $ map (Just . snd) ts
                  {-# LINE 13633 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Insert.ag"(line 45, column 9)
              _returningOlib =
                  {-# LINE 45 "./TypeChecking/Dml/Insert.ag" #-}
                  either (const _lhsIlib) id $ do
                    atts <- lmt (allAtts <$> _tableItbUType)
                    lbUpdate _lhsIcat (LBIds "insert target table" (Just $ getTName _tableIannotatedTree) atts) _lhsIlib
                  {-# LINE 13640 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Insert ann_ _tableIannotatedTree targetCols_ _insDataIannotatedTree _returningIannotatedTree
                  {-# LINE 13645 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Insert ann_ _tableIfixedUpIdentifiersTree targetCols_ _insDataIfixedUpIdentifiersTree _returningIfixedUpIdentifiersTree
                  {-# LINE 13650 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Insert ann_ _tableIoriginalTree targetCols_ _insDataIoriginalTree _returningIoriginalTree
                  {-# LINE 13655 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13660 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13665 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13670 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13675 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13680 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13685 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13690 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13695 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13700 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13705 "AstInternal.hs" #-}
              ( _tableIannotatedTree,_tableIfixedUpIdentifiersTree,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType) =
                  (table_ _tableOcat _tableOidenv _tableOlib )
              ( _insDataIannotatedTree,_insDataIcidenv,_insDataIfixedUpIdentifiersTree,_insDataIlibUpdates,_insDataIoriginalTree,_insDataIuType) =
                  (insData_ _insDataOcat _insDataOexpectedTypes _insDataOidenv _insDataOlib )
              ( _returningIannotatedTree,_returningIfixedUpIdentifiersTree,_returningIlistType,_returningIoriginalTree) =
                  (returning_ _returningOcat _returningOidenv _returningOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_LoopStatement :: Annotation ->
                               (Maybe String) ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_LoopStatement ann_ lb_ sts_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _stsOcat :: Catalog
              _stsOidenv :: IDEnv
              _stsOlib :: LocalBindings
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13741 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13746 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13751 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13756 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  LoopStatement ann_ lb_ _stsIannotatedTree
                  {-# LINE 13761 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  LoopStatement ann_ lb_ _stsIfixedUpIdentifiersTree
                  {-# LINE 13766 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  LoopStatement ann_ lb_ _stsIoriginalTree
                  {-# LINE 13771 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13776 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13781 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13786 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13791 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13796 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13801 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Notify :: Annotation ->
                        String ->
                        T_Statement 
sem_Statement_Notify ann_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13822 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13827 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13832 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Notify ann_ name_
                  {-# LINE 13837 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Notify ann_ name_
                  {-# LINE 13842 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Notify ann_ name_
                  {-# LINE 13847 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13852 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13857 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13862 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_NullStatement :: Annotation ->
                               T_Statement 
sem_Statement_NullStatement ann_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13880 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13885 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 13890 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  NullStatement ann_
                  {-# LINE 13895 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 13900 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13905 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13910 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13915 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Perform :: Annotation ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13942 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13947 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 116, column 9)
              _exprOexpectedType =
                  {-# LINE 116 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 13952 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Perform ann_ _exprIannotatedTree
                  {-# LINE 13957 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Perform ann_ _exprIfixedUpIdentifiersTree
                  {-# LINE 13962 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Perform ann_ _exprIoriginalTree
                  {-# LINE 13967 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13972 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13977 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13982 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13987 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13992 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13997 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_QueryStatement :: Annotation ->
                                T_QueryExpr  ->
                                T_Statement 
sem_Statement_QueryStatement ann_ ex_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _exOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exOcat :: Catalog
              _exOidenv :: IDEnv
              _exOlib :: LocalBindings
              _exIannotatedTree :: QueryExpr
              _exIcidenv :: IDEnv
              _exIfixedUpIdentifiersTree :: QueryExpr
              _exIlibUpdates :: ([LocalBindingsUpdate])
              _exIoriginalTree :: QueryExpr
              _exIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 14034 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 14039 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 14044 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 14, column 9)
              _tpe =
                  {-# LINE 14 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 14049 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 15, column 9)
              _statementType =
                  {-# LINE 15 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  do
                  pt <- sequence $ getPlaceholderTypes _exIannotatedTree
                  st <- _exIuType
                  return (pt
                         ,case st of
                            [(_,(Pseudo Void))] -> []
                            t -> t)
                  {-# LINE 14060 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 23, column 9)
              _backTree =
                  {-# LINE 23 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  QueryStatement ann_ _exIannotatedTree
                  {-# LINE 14065 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 24, column 9)
              _catUpdates =
                  {-# LINE 24 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  []
                  {-# LINE 14070 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 112, column 9)
              _libUpdates =
                  {-# LINE 112 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _exIlibUpdates
                  {-# LINE 14075 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 180, column 22)
              _exOexpectedTypes =
                  {-# LINE 180 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 14080 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  QueryStatement ann_ _exIannotatedTree
                  {-# LINE 14085 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  QueryStatement ann_ _exIfixedUpIdentifiersTree
                  {-# LINE 14090 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  QueryStatement ann_ _exIoriginalTree
                  {-# LINE 14095 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14100 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14105 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14110 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14115 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14120 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIcidenv,_exIfixedUpIdentifiersTree,_exIlibUpdates,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedTypes _exOidenv _exOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Raise :: Annotation ->
                       RaiseType ->
                       String ->
                       T_ScalarExprList  ->
                       T_Statement 
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _argsOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _argsOcat :: Catalog
              _argsOidenv :: IDEnv
              _argsOlib :: LocalBindings
              _argsIannotatedTree :: ScalarExprList
              _argsIfixedUpIdentifiersTree :: ScalarExprList
              _argsIoriginalTree :: ScalarExprList
              _argsIuType :: ([Maybe Type])
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14151 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14156 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 156, column 13)
              _argsOexpectedTypes =
                  {-# LINE 156 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 14161 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Raise ann_ level_ message_ _argsIannotatedTree
                  {-# LINE 14166 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Raise ann_ level_ message_ _argsIfixedUpIdentifiersTree
                  {-# LINE 14171 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Raise ann_ level_ message_ _argsIoriginalTree
                  {-# LINE 14176 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14181 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14186 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14191 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14196 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14201 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14206 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsIfixedUpIdentifiersTree,_argsIoriginalTree,_argsIuType) =
                  (args_ _argsOcat _argsOexpectedTypes _argsOidenv _argsOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Return :: Annotation ->
                        T_MaybeScalarExpr  ->
                        T_Statement 
sem_Statement_Return ann_ value_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _valueOcat :: Catalog
              _valueOidenv :: IDEnv
              _valueOlib :: LocalBindings
              _valueIannotatedTree :: MaybeScalarExpr
              _valueIfixedUpIdentifiersTree :: MaybeScalarExpr
              _valueIoriginalTree :: MaybeScalarExpr
              _valueIuType :: (Maybe Type)
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 14240 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 14245 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 14250 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14255 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 12, column 9)
              _tpe =
                  {-# LINE 12 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  maybe (Right $ Pseudo Void) Right _valueIuType
                  {-# LINE 14260 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 13, column 9)
              _backTree =
                  {-# LINE 13 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 14265 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 14, column 9)
              _catUpdates =
                  {-# LINE 14 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  []
                  {-# LINE 14270 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql/Plpgsql.ag"(line 15, column 9)
              _statementType =
                  {-# LINE 15 "./TypeChecking/Plpgsql/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 14275 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 14280 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Return ann_ _valueIfixedUpIdentifiersTree
                  {-# LINE 14285 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Return ann_ _valueIoriginalTree
                  {-# LINE 14290 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14295 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14300 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14305 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14310 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14315 "AstInternal.hs" #-}
              ( _valueIannotatedTree,_valueIfixedUpIdentifiersTree,_valueIoriginalTree,_valueIuType) =
                  (value_ _valueOcat _valueOidenv _valueOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ReturnNext :: Annotation ->
                            T_ScalarExpr  ->
                            T_Statement 
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14344 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14349 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 116, column 9)
              _exprOexpectedType =
                  {-# LINE 116 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 14354 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIannotatedTree
                  {-# LINE 14359 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ReturnNext ann_ _exprIfixedUpIdentifiersTree
                  {-# LINE 14364 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIoriginalTree
                  {-# LINE 14369 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14374 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14379 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14384 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14389 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14394 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14399 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ReturnQuery :: Annotation ->
                             T_QueryExpr  ->
                             T_Statement 
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14430 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14435 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 179, column 9)
              _selOexpectedTypes =
                  {-# LINE 179 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 14440 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIannotatedTree
                  {-# LINE 14445 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ReturnQuery ann_ _selIfixedUpIdentifiersTree
                  {-# LINE 14450 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIoriginalTree
                  {-# LINE 14455 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14460 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14465 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14470 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14475 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14480 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14485 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Set :: Annotation ->
                     String ->
                     ([SetValue]) ->
                     T_Statement 
sem_Statement_Set ann_ name_ values_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14507 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14512 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14517 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Set ann_ name_ values_
                  {-# LINE 14522 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Set ann_ name_ values_
                  {-# LINE 14527 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Set ann_ name_ values_
                  {-# LINE 14532 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14537 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14542 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14547 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Truncate :: Annotation ->
                          ([String]) ->
                          RestartIdentity ->
                          Cascade ->
                          T_Statement 
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14568 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14573 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Truncate ann_ tables_ restartIdentity_ cascade_
                  {-# LINE 14578 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Truncate ann_ tables_ restartIdentity_ cascade_
                  {-# LINE 14583 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Truncate ann_ tables_ restartIdentity_ cascade_
                  {-# LINE 14588 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14593 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14598 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14603 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Update :: Annotation ->
                        T_SQIdentifier  ->
                        T_ScalarExprList  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Update ann_ table_ assigns_ fromList_ whr_ returning_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _whrOlib :: LocalBindings
              _assignsOlib :: LocalBindings
              _returningOlib :: LocalBindings
              _assignsOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _tableOcat :: Catalog
              _tableOidenv :: IDEnv
              _tableOlib :: LocalBindings
              _assignsOcat :: Catalog
              _assignsOidenv :: IDEnv
              _fromListOcat :: Catalog
              _fromListOidenv :: IDEnv
              _fromListOlib :: LocalBindings
              _whrOcat :: Catalog
              _whrOidenv :: IDEnv
              _returningOcat :: Catalog
              _returningOidenv :: IDEnv
              _tableIannotatedTree :: SQIdentifier
              _tableIfixedUpIdentifiersTree :: SQIdentifier
              _tableIoriginalTree :: SQIdentifier
              _tableItbAnnotatedTree :: SQIdentifier
              _tableItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _assignsIannotatedTree :: ScalarExprList
              _assignsIfixedUpIdentifiersTree :: ScalarExprList
              _assignsIoriginalTree :: ScalarExprList
              _assignsIuType :: ([Maybe Type])
              _fromListIannotatedTree :: TableRefList
              _fromListIfixedUpIdentifiersTree :: TableRefList
              _fromListIlibUpdates :: ([LocalBindingsUpdate])
              _fromListInewLib2 :: LocalBindings
              _fromListIoriginalTree :: TableRefList
              _fromListItrefIDs :: ([(String,[String])])
              _whrIannotatedTree :: MaybeBoolExpr
              _whrIfixedUpIdentifiersTree :: MaybeBoolExpr
              _whrIoriginalTree :: MaybeBoolExpr
              _returningIannotatedTree :: MaybeSelectList
              _returningIfixedUpIdentifiersTree :: MaybeSelectList
              _returningIlistType :: ([(String,Maybe Type)])
              _returningIoriginalTree :: MaybeSelectList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 14670 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 14675 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 14680 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14685 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Update.ag"(line 13, column 9)
              _tpe =
                  {-# LINE 13 "./TypeChecking/Dml/Update.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 14690 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Update.ag"(line 18, column 9)
              _statementType =
                  {-# LINE 18 "./TypeChecking/Dml/Update.ag" #-}
                  do
                  pt <- sequence $ getPlaceholderTypes _assignsIannotatedTree
                                   ++ getPlaceholderTypes _whrIannotatedTree
                  return (pt,fromMaybe [] $ liftList _returningIlistType)
                  {-# LINE 14698 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Update.ag"(line 24, column 9)
              _backTree =
                  {-# LINE 24 "./TypeChecking/Dml/Update.ag" #-}
                  Update ann_
                         _tableItbAnnotatedTree
                         _assignsIannotatedTree
                         _fromListIannotatedTree
                         _whrIannotatedTree
                         _returningIannotatedTree
                  {-# LINE 14708 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Update.ag"(line 30, column 9)
              _catUpdates =
                  {-# LINE 30 "./TypeChecking/Dml/Update.ag" #-}
                  []
                  {-# LINE 14713 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Update.ag"(line 35, column 9)
              _lib =
                  {-# LINE 35 "./TypeChecking/Dml/Update.ag" #-}
                  either (const _lhsIlib) id $ do
                  a <- lmt (allAtts <$> _tableItbUType)
                  lbUpdate _lhsIcat (LBIds "updated table attrs" (Just $ getTName _tableIannotatedTree) a) _lhsIlib
                  {-# LINE 14720 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Update.ag"(line 39, column 9)
              _whrOlib =
                  {-# LINE 39 "./TypeChecking/Dml/Update.ag" #-}
                  _lib
                  {-# LINE 14725 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Update.ag"(line 40, column 9)
              _assignsOlib =
                  {-# LINE 40 "./TypeChecking/Dml/Update.ag" #-}
                  _lib
                  {-# LINE 14730 "AstInternal.hs" #-}
              -- "./TypeChecking/Dml/Update.ag"(line 41, column 9)
              _returningOlib =
                  {-# LINE 41 "./TypeChecking/Dml/Update.ag" #-}
                  _lib
                  {-# LINE 14735 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 157, column 14)
              _assignsOexpectedTypes =
                  {-# LINE 157 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 14740 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Update ann_ _tableIannotatedTree _assignsIannotatedTree _fromListIannotatedTree _whrIannotatedTree _returningIannotatedTree
                  {-# LINE 14745 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Update ann_ _tableIfixedUpIdentifiersTree _assignsIfixedUpIdentifiersTree _fromListIfixedUpIdentifiersTree _whrIfixedUpIdentifiersTree _returningIfixedUpIdentifiersTree
                  {-# LINE 14750 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Update ann_ _tableIoriginalTree _assignsIoriginalTree _fromListIoriginalTree _whrIoriginalTree _returningIoriginalTree
                  {-# LINE 14755 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14760 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14765 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14770 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14775 "AstInternal.hs" #-}
              -- copy rule (from local)
              _tableOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 14780 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14785 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14790 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromListOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14795 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromListOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14800 "AstInternal.hs" #-}
              -- copy rule (from local)
              _fromListOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 14805 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14810 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14815 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14820 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14825 "AstInternal.hs" #-}
              ( _tableIannotatedTree,_tableIfixedUpIdentifiersTree,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType) =
                  (table_ _tableOcat _tableOidenv _tableOlib )
              ( _assignsIannotatedTree,_assignsIfixedUpIdentifiersTree,_assignsIoriginalTree,_assignsIuType) =
                  (assigns_ _assignsOcat _assignsOexpectedTypes _assignsOidenv _assignsOlib )
              ( _fromListIannotatedTree,_fromListIfixedUpIdentifiersTree,_fromListIlibUpdates,_fromListInewLib2,_fromListIoriginalTree,_fromListItrefIDs) =
                  (fromList_ _fromListOcat _fromListOidenv _fromListOlib )
              ( _whrIannotatedTree,_whrIfixedUpIdentifiersTree,_whrIoriginalTree) =
                  (whr_ _whrOcat _whrOidenv _whrOlib )
              ( _returningIannotatedTree,_returningIfixedUpIdentifiersTree,_returningIlistType,_returningIoriginalTree) =
                  (returning_ _returningOcat _returningOidenv _returningOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_WhileStatement :: Annotation ->
                                (Maybe String) ->
                                T_ScalarExpr  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement ann_ lb_ expr_ sts_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _stsOcat :: Catalog
              _stsOidenv :: IDEnv
              _stsOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprIoriginalTree :: ScalarExpr
              _exprIuType :: (Maybe Type)
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14874 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14879 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14884 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14889 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 116, column 9)
              _exprOexpectedType =
                  {-# LINE 116 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 14894 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  WhileStatement ann_ lb_ _exprIannotatedTree _stsIannotatedTree
                  {-# LINE 14899 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  WhileStatement ann_ lb_ _exprIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
                  {-# LINE 14904 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  WhileStatement ann_ lb_ _exprIoriginalTree _stsIoriginalTree
                  {-# LINE 14909 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14914 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14919 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14924 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14929 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14934 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14939 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14944 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14949 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14954 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
-- StatementList -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         catUpdates           : [CatalogUpdate]
         idenv                : IDEnv
         lib                  : LocalBindings
         libUpdates           : [LocalBindingsUpdate]
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         producedCat          : Catalog
         producedLib          : LocalBindings
   alternatives:
      alternative Cons:
         child hd             : Statement 
         child tl             : StatementList 
         visit 0:
            local newCat      : _
            local newLib      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local newCat      : _
            local newLib      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type StatementList  = [(Statement)]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_Statement list) )
-- semantic domain
type T_StatementList  = Catalog ->
                        ([CatalogUpdate]) ->
                        IDEnv ->
                        LocalBindings ->
                        ([LocalBindingsUpdate]) ->
                        ( StatementList,StatementList,StatementList,Catalog,LocalBindings)
data Inh_StatementList  = Inh_StatementList {cat_Inh_StatementList :: Catalog,catUpdates_Inh_StatementList :: [CatalogUpdate],idenv_Inh_StatementList :: IDEnv,lib_Inh_StatementList :: LocalBindings,libUpdates_Inh_StatementList :: [LocalBindingsUpdate]}
data Syn_StatementList  = Syn_StatementList {annotatedTree_Syn_StatementList :: StatementList,fixedUpIdentifiersTree_Syn_StatementList :: StatementList,originalTree_Syn_StatementList :: StatementList,producedCat_Syn_StatementList :: Catalog,producedLib_Syn_StatementList :: LocalBindings}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIcat _lhsIcatUpdates _lhsIidenv _lhsIlib _lhsIlibUpdates )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib) =
             (sem _lhsIcat _lhsIcatUpdates _lhsIidenv _lhsIlib _lhsIlibUpdates )
     in  (Syn_StatementList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOproducedCat _lhsOproducedLib ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIcatUpdates
       _lhsIidenv
       _lhsIlib
       _lhsIlibUpdates ->
         (let _hdOcat :: Catalog
              _tlOcat :: Catalog
              _hdOlib :: LocalBindings
              _tlOlib :: LocalBindings
              _lhsOproducedCat :: Catalog
              _lhsOproducedLib :: LocalBindings
              _tlOcatUpdates :: ([CatalogUpdate])
              _tlOlibUpdates :: ([LocalBindingsUpdate])
              _hdOinProducedCat :: Catalog
              _lhsOannotatedTree :: StatementList
              _lhsOfixedUpIdentifiersTree :: StatementList
              _lhsOoriginalTree :: StatementList
              _hdOidenv :: IDEnv
              _tlOidenv :: IDEnv
              _hdIannotatedTree :: Statement
              _hdIcatUpdates :: ([CatalogUpdate])
              _hdIfixedUpIdentifiersTree :: Statement
              _hdIlibUpdates :: ([LocalBindingsUpdate])
              _hdIoriginalTree :: Statement
              _tlIannotatedTree :: StatementList
              _tlIfixedUpIdentifiersTree :: StatementList
              _tlIoriginalTree :: StatementList
              _tlIproducedCat :: Catalog
              _tlIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 56, column 9)
              _newCat =
                  {-# LINE 56 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
                  {-# LINE 15052 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 57, column 9)
              _newLib =
                  {-# LINE 57 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIlib $ foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _lhsIlibUpdates
                  {-# LINE 15057 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 59, column 9)
              _hdOcat =
                  {-# LINE 59 "./TypeChecking/Statements.ag" #-}
                  _newCat
                  {-# LINE 15062 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _tlOcat =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  _newCat
                  {-# LINE 15067 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 61, column 9)
              _hdOlib =
                  {-# LINE 61 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 15072 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 62, column 9)
              _tlOlib =
                  {-# LINE 62 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 15077 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOproducedCat =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedCat
                  {-# LINE 15082 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 67, column 9)
              _lhsOproducedLib =
                  {-# LINE 67 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedLib
                  {-# LINE 15087 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _tlOcatUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  _hdIcatUpdates
                  {-# LINE 15092 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 71, column 9)
              _tlOlibUpdates =
                  {-# LINE 71 "./TypeChecking/Statements.ag" #-}
                  _hdIlibUpdates
                  {-# LINE 15097 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 97, column 12)
              _hdOinProducedCat =
                  {-# LINE 97 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedCat
                  {-# LINE 15102 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 15107 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 15112 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 15117 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15122 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15127 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15132 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15137 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15142 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIcatUpdates,_hdIfixedUpIdentifiersTree,_hdIlibUpdates,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOinProducedCat _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree,_tlIproducedCat,_tlIproducedLib) =
                  (tl_ _tlOcat _tlOcatUpdates _tlOidenv _tlOlib _tlOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIcat
       _lhsIcatUpdates
       _lhsIidenv
       _lhsIlib
       _lhsIlibUpdates ->
         (let _lhsOproducedCat :: Catalog
              _lhsOproducedLib :: LocalBindings
              _lhsOannotatedTree :: StatementList
              _lhsOfixedUpIdentifiersTree :: StatementList
              _lhsOoriginalTree :: StatementList
              -- "./TypeChecking/Statements.ag"(line 56, column 9)
              _newCat =
                  {-# LINE 56 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
                  {-# LINE 15164 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 57, column 9)
              _newLib =
                  {-# LINE 57 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIlib $ foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _lhsIlibUpdates
                  {-# LINE 15169 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 73, column 9)
              _lhsOproducedCat =
                  {-# LINE 73 "./TypeChecking/Statements.ag" #-}
                  _newCat
                  {-# LINE 15174 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 74, column 9)
              _lhsOproducedLib =
                  {-# LINE 74 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 15179 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15184 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 15189 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15194 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15199 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15204 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15209 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib)))
-- StringTypeNameListPair --------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         fnSig                : (String,[Maybe Type])
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : {String}
         child x2             : TypeNameList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type StringTypeNameListPair  = ( (String),(TypeNameList))
-- cata
sem_StringTypeNameListPair :: StringTypeNameListPair  ->
                              T_StringTypeNameListPair 
sem_StringTypeNameListPair ( x1,x2)  =
    (sem_StringTypeNameListPair_Tuple x1 (sem_TypeNameList x2 ) )
-- semantic domain
type T_StringTypeNameListPair  = Catalog ->
                                 IDEnv ->
                                 LocalBindings ->
                                 ( StringTypeNameListPair,StringTypeNameListPair,((String,[Maybe Type])),StringTypeNameListPair)
data Inh_StringTypeNameListPair  = Inh_StringTypeNameListPair {cat_Inh_StringTypeNameListPair :: Catalog,idenv_Inh_StringTypeNameListPair :: IDEnv,lib_Inh_StringTypeNameListPair :: LocalBindings}
data Syn_StringTypeNameListPair  = Syn_StringTypeNameListPair {annotatedTree_Syn_StringTypeNameListPair :: StringTypeNameListPair,fixedUpIdentifiersTree_Syn_StringTypeNameListPair :: StringTypeNameListPair,fnSig_Syn_StringTypeNameListPair :: (String,[Maybe Type]),originalTree_Syn_StringTypeNameListPair :: StringTypeNameListPair}
wrap_StringTypeNameListPair :: T_StringTypeNameListPair  ->
                               Inh_StringTypeNameListPair  ->
                               Syn_StringTypeNameListPair 
wrap_StringTypeNameListPair sem (Inh_StringTypeNameListPair _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSig,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_StringTypeNameListPair _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOfnSig _lhsOoriginalTree ))
sem_StringTypeNameListPair_Tuple :: String ->
                                    T_TypeNameList  ->
                                    T_StringTypeNameListPair 
sem_StringTypeNameListPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOfnSig :: ((String,[Maybe Type]))
              _lhsOannotatedTree :: StringTypeNameListPair
              _lhsOfixedUpIdentifiersTree :: StringTypeNameListPair
              _lhsOoriginalTree :: StringTypeNameListPair
              _x2Ocat :: Catalog
              _x2Oidenv :: IDEnv
              _x2Olib :: LocalBindings
              _x2IannotatedTree :: TypeNameList
              _x2IfixedUpIdentifiersTree :: TypeNameList
              _x2InamedTypes :: ([Maybe Type])
              _x2IoriginalTree :: TypeNameList
              -- "./TypeChecking/Ddl/Drops.ag"(line 32, column 13)
              _lhsOfnSig =
                  {-# LINE 32 "./TypeChecking/Ddl/Drops.ag" #-}
                  (x1_, _x2InamedTypes)
                  {-# LINE 15274 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (x1_,_x2IannotatedTree)
                  {-# LINE 15279 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (x1_,_x2IfixedUpIdentifiersTree)
                  {-# LINE 15284 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (x1_,_x2IoriginalTree)
                  {-# LINE 15289 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15294 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15299 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15304 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15309 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15314 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15319 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2IfixedUpIdentifiersTree,_x2InamedTypes,_x2IoriginalTree) =
                  (x2_ _x2Ocat _x2Oidenv _x2Olib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSig,_lhsOoriginalTree)))
-- StringTypeNameListPairList ----------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         fnSigs               : [(String,[Maybe Type])]
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : StringTypeNameListPair 
         child tl             : StringTypeNameListPairList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type StringTypeNameListPairList  = [(StringTypeNameListPair)]
-- cata
sem_StringTypeNameListPairList :: StringTypeNameListPairList  ->
                                  T_StringTypeNameListPairList 
sem_StringTypeNameListPairList list  =
    (Prelude.foldr sem_StringTypeNameListPairList_Cons sem_StringTypeNameListPairList_Nil (Prelude.map sem_StringTypeNameListPair list) )
-- semantic domain
type T_StringTypeNameListPairList  = Catalog ->
                                     IDEnv ->
                                     LocalBindings ->
                                     ( StringTypeNameListPairList,StringTypeNameListPairList,([(String,[Maybe Type])]),StringTypeNameListPairList)
data Inh_StringTypeNameListPairList  = Inh_StringTypeNameListPairList {cat_Inh_StringTypeNameListPairList :: Catalog,idenv_Inh_StringTypeNameListPairList :: IDEnv,lib_Inh_StringTypeNameListPairList :: LocalBindings}
data Syn_StringTypeNameListPairList  = Syn_StringTypeNameListPairList {annotatedTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList,fixedUpIdentifiersTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList,fnSigs_Syn_StringTypeNameListPairList :: [(String,[Maybe Type])],originalTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList}
wrap_StringTypeNameListPairList :: T_StringTypeNameListPairList  ->
                                   Inh_StringTypeNameListPairList  ->
                                   Syn_StringTypeNameListPairList 
wrap_StringTypeNameListPairList sem (Inh_StringTypeNameListPairList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSigs,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_StringTypeNameListPairList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOfnSigs _lhsOoriginalTree ))
sem_StringTypeNameListPairList_Cons :: T_StringTypeNameListPair  ->
                                       T_StringTypeNameListPairList  ->
                                       T_StringTypeNameListPairList 
sem_StringTypeNameListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOfnSigs :: ([(String,[Maybe Type])])
              _lhsOannotatedTree :: StringTypeNameListPairList
              _lhsOfixedUpIdentifiersTree :: StringTypeNameListPairList
              _lhsOoriginalTree :: StringTypeNameListPairList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: StringTypeNameListPair
              _hdIfixedUpIdentifiersTree :: StringTypeNameListPair
              _hdIfnSig :: ((String,[Maybe Type]))
              _hdIoriginalTree :: StringTypeNameListPair
              _tlIannotatedTree :: StringTypeNameListPairList
              _tlIfixedUpIdentifiersTree :: StringTypeNameListPairList
              _tlIfnSigs :: ([(String,[Maybe Type])])
              _tlIoriginalTree :: StringTypeNameListPairList
              -- "./TypeChecking/Ddl/Drops.ag"(line 27, column 12)
              _lhsOfnSigs =
                  {-# LINE 27 "./TypeChecking/Ddl/Drops.ag" #-}
                  _hdIfnSig : _tlIfnSigs
                  {-# LINE 15398 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 15403 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 15408 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 15413 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15418 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15423 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15428 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15433 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15438 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15443 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15448 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15453 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15458 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIfnSig,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIfnSigs,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSigs,_lhsOoriginalTree)))
sem_StringTypeNameListPairList_Nil :: T_StringTypeNameListPairList 
sem_StringTypeNameListPairList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOfnSigs :: ([(String,[Maybe Type])])
              _lhsOannotatedTree :: StringTypeNameListPairList
              _lhsOfixedUpIdentifiersTree :: StringTypeNameListPairList
              _lhsOoriginalTree :: StringTypeNameListPairList
              -- "./TypeChecking/Ddl/Drops.ag"(line 28, column 11)
              _lhsOfnSigs =
                  {-# LINE 28 "./TypeChecking/Ddl/Drops.ag" #-}
                  []
                  {-# LINE 15477 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15482 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 15487 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15492 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15497 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15502 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15507 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSigs,_lhsOoriginalTree)))
-- TableAlias --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         expectedNumCols      : Maybe Int
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative FullAlias:
         child ann            : {Annotation}
         child tb             : {String}
         child cols           : {[String]}
         visit 0:
            local errs        : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative NoAlias:
         child ann            : {Annotation}
         visit 0:
            local backTree    : _
            local errs        : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative TableAlias:
         child ann            : {Annotation}
         child tb             : {String}
         visit 0:
            local backTree    : _
            local errs        : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data TableAlias  = FullAlias (Annotation) (String) ([String]) 
                 | NoAlias (Annotation) 
                 | TableAlias (Annotation) (String) 
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
                     (Maybe Int) ->
                     IDEnv ->
                     LocalBindings ->
                     ( TableAlias,TableAlias,TableAlias)
data Inh_TableAlias  = Inh_TableAlias {cat_Inh_TableAlias :: Catalog,expectedNumCols_Inh_TableAlias :: Maybe Int,idenv_Inh_TableAlias :: IDEnv,lib_Inh_TableAlias :: LocalBindings}
data Syn_TableAlias  = Syn_TableAlias {annotatedTree_Syn_TableAlias :: TableAlias,fixedUpIdentifiersTree_Syn_TableAlias :: TableAlias,originalTree_Syn_TableAlias :: TableAlias}
wrap_TableAlias :: T_TableAlias  ->
                   Inh_TableAlias  ->
                   Syn_TableAlias 
wrap_TableAlias sem (Inh_TableAlias _lhsIcat _lhsIexpectedNumCols _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIexpectedNumCols _lhsIidenv _lhsIlib )
     in  (Syn_TableAlias _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_TableAlias_FullAlias :: Annotation ->
                            String ->
                            ([String]) ->
                            T_TableAlias 
sem_TableAlias_FullAlias ann_ tb_ cols_  =
    (\ _lhsIcat
       _lhsIexpectedNumCols
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: TableAlias
              _lhsOfixedUpIdentifiersTree :: TableAlias
              _lhsOoriginalTree :: TableAlias
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 258, column 9)
              _lhsOannotatedTree =
                  {-# LINE 258 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 15594 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 260, column 9)
              _errs =
                  {-# LINE 260 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  case _lhsIexpectedNumCols of
                        Nothing -> []
                        Just n -> if n == length cols_
                                  then []
                                  else [WrongNumberOfAliasCols n $ length cols_]
                  {-# LINE 15603 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 265, column 9)
              _backTree =
                  {-# LINE 265 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  FullAlias ann_ tb_ cols_
                  {-# LINE 15608 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  FullAlias ann_ tb_ cols_
                  {-# LINE 15613 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  FullAlias ann_ tb_ cols_
                  {-# LINE 15618 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  FullAlias ann_ tb_ cols_
                  {-# LINE 15623 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15628 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15633 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_TableAlias_NoAlias :: Annotation ->
                          T_TableAlias 
sem_TableAlias_NoAlias ann_  =
    (\ _lhsIcat
       _lhsIexpectedNumCols
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: TableAlias
              _lhsOfixedUpIdentifiersTree :: TableAlias
              _lhsOoriginalTree :: TableAlias
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 258, column 9)
              _lhsOannotatedTree =
                  {-# LINE 258 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 15649 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 268, column 15)
              _backTree =
                  {-# LINE 268 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  NoAlias ann_
                  {-# LINE 15654 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 269, column 15)
              _errs =
                  {-# LINE 269 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  []
                  {-# LINE 15659 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  NoAlias ann_
                  {-# LINE 15664 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  NoAlias ann_
                  {-# LINE 15669 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  NoAlias ann_
                  {-# LINE 15674 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15679 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15684 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_TableAlias_TableAlias :: Annotation ->
                             String ->
                             T_TableAlias 
sem_TableAlias_TableAlias ann_ tb_  =
    (\ _lhsIcat
       _lhsIexpectedNumCols
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: TableAlias
              _lhsOfixedUpIdentifiersTree :: TableAlias
              _lhsOoriginalTree :: TableAlias
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 258, column 9)
              _lhsOannotatedTree =
                  {-# LINE 258 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 15701 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 266, column 18)
              _backTree =
                  {-# LINE 266 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  TableAlias ann_ tb_
                  {-# LINE 15706 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 267, column 18)
              _errs =
                  {-# LINE 267 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  []
                  {-# LINE 15711 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  TableAlias ann_ tb_
                  {-# LINE 15716 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  TableAlias ann_ tb_
                  {-# LINE 15721 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  TableAlias ann_ tb_
                  {-# LINE 15726 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15731 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15736 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- TableRef ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         libUpdates           : [LocalBindingsUpdate]
         newLib2              : LocalBindings
         originalTree         : SELF 
         trefIDs              : [(String,[String])]
   alternatives:
      alternative FunTref:
         child ann            : {Annotation}
         child fn             : ScalarExpr 
         child alias          : TableAlias 
         visit 0:
            local _tup2       : {([(String,[String])],TableRef)}
            local errs        : _
            local eqfunIdens  : {Either [TypeError] (String,[(String,Type)])}
            local qfunIdens   : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
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
            local _tup3       : {([(String,[String])],TableRef)}
            local errs        : _
            local joinErrors  : _
            local libUpdates  : _
            local newLib      : {Either [TypeError] LocalBindings}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative SubTref:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         child alias          : TableAlias 
         visit 0:
            local _tup4       : {([(String,[String])],TableRef)}
            local errs        : _
            local selectAttrs : {Either [TypeError] [(String,Type)]}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Tref:
         child ann            : {Annotation}
         child tbl            : SQIdentifier 
         child alias          : TableAlias 
         visit 0:
            local _tup5       : {([(String,[String])],TableRef)}
            local errs        : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data TableRef  = FunTref (Annotation) (ScalarExpr) (TableAlias) 
               | JoinTref (Annotation) (TableRef) (Natural) (JoinType) (TableRef) (OnExpr) (TableAlias) 
               | SubTref (Annotation) (QueryExpr) (TableAlias) 
               | Tref (Annotation) (SQIdentifier) (TableAlias) 
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
    (sem_TableRef_Tref _ann (sem_SQIdentifier _tbl ) (sem_TableAlias _alias ) )
-- semantic domain
type T_TableRef  = Catalog ->
                   IDEnv ->
                   LocalBindings ->
                   ( TableRef,TableRef,([LocalBindingsUpdate]),LocalBindings,TableRef,([(String,[String])]))
data Inh_TableRef  = Inh_TableRef {cat_Inh_TableRef :: Catalog,idenv_Inh_TableRef :: IDEnv,lib_Inh_TableRef :: LocalBindings}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef,fixedUpIdentifiersTree_Syn_TableRef :: TableRef,libUpdates_Syn_TableRef :: [LocalBindingsUpdate],newLib2_Syn_TableRef :: LocalBindings,originalTree_Syn_TableRef :: TableRef,trefIDs_Syn_TableRef :: [(String,[String])]}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewLib2,_lhsOoriginalTree,_lhsOtrefIDs) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TableRef _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOnewLib2 _lhsOoriginalTree _lhsOtrefIDs ))
sem_TableRef_FunTref :: Annotation ->
                        T_ScalarExpr  ->
                        T_TableAlias  ->
                        T_TableRef 
sem_TableRef_FunTref ann_ fn_ alias_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let __tup2 :: (([(String,[String])],TableRef))
              _lhsOtrefIDs :: ([(String,[String])])
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOannotatedTree :: TableRef
              _eqfunIdens :: (Either [TypeError] (String,[(String,Type)]))
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOnewLib2 :: LocalBindings
              _aliasOexpectedNumCols :: (Maybe Int)
              _fnOexpectedType :: (Maybe Type)
              _lhsOoriginalTree :: TableRef
              _fnOcat :: Catalog
              _fnOidenv :: IDEnv
              _fnOlib :: LocalBindings
              _aliasOcat :: Catalog
              _aliasOidenv :: IDEnv
              _aliasOlib :: LocalBindings
              _fnIannotatedTree :: ScalarExpr
              _fnIfixedUpIdentifiersTree :: ScalarExpr
              _fnIoriginalTree :: ScalarExpr
              _fnIuType :: (Maybe Type)
              _aliasIannotatedTree :: TableAlias
              _aliasIfixedUpIdentifiersTree :: TableAlias
              _aliasIoriginalTree :: TableAlias
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 372, column 15)
              __tup2 =
                  {-# LINE 372 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  let (FunCall _ f _) = _fnIfixedUpIdentifiersTree
                      (trs,al) = doAlias _aliasIannotatedTree [(f,[f])]
                  in (trs,FunTref ann_ _fnIfixedUpIdentifiersTree al)
                  {-# LINE 15875 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 372, column 15)
              (_lhsOtrefIDs,_) =
                  {-# LINE 372 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup2
                  {-# LINE 15880 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 372, column 15)
              (_,_lhsOfixedUpIdentifiersTree) =
                  {-# LINE 372 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup2
                  {-# LINE 15885 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 55, column 9)
              _lhsOannotatedTree =
                  {-# LINE 55 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 15890 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 102, column 9)
              _errs =
                  {-# LINE 102 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  case _eqfunIdens of
                    Left e -> e
                    Right _ -> []
                  {-# LINE 15897 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 108, column 9)
              _eqfunIdens =
                  {-# LINE 108 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  funIdens _lhsIcat (getAlias "" _aliasIoriginalTree) _fnIannotatedTree _fnIuType
                  {-# LINE 15902 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 109, column 9)
              _lhsOlibUpdates =
                  {-# LINE 109 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  [LBTref "fn"
                                  (fst _qfunIdens    )
                                  (snd _qfunIdens    )
                                  []]
                  {-# LINE 15910 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 113, column 9)
              _qfunIdens =
                  {-# LINE 113 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  fromRight ("",[]) _eqfunIdens
                  {-# LINE 15915 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 219, column 9)
              _lhsOnewLib2 =
                  {-# LINE 219 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  emptyBindings
                  {-# LINE 15920 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 232, column 9)
              _backTree =
                  {-# LINE 232 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  FunTref ann_ _fnIannotatedTree _aliasIannotatedTree
                  {-# LINE 15925 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 253, column 9)
              _aliasOexpectedNumCols =
                  {-# LINE 253 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  Nothing
                  {-# LINE 15930 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 122, column 15)
              _fnOexpectedType =
                  {-# LINE 122 "./TypeChecking/ParameterizedStatements.ag" #-}
                  Nothing
                  {-# LINE 15935 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  FunTref ann_ _fnIannotatedTree _aliasIannotatedTree
                  {-# LINE 15940 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  FunTref ann_ _fnIfixedUpIdentifiersTree _aliasIfixedUpIdentifiersTree
                  {-# LINE 15945 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  FunTref ann_ _fnIoriginalTree _aliasIoriginalTree
                  {-# LINE 15950 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15955 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15960 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15965 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15970 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15975 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15980 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15985 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIfixedUpIdentifiersTree,_fnIoriginalTree,_fnIuType) =
                  (fn_ _fnOcat _fnOexpectedType _fnOidenv _fnOlib )
              ( _aliasIannotatedTree,_aliasIfixedUpIdentifiersTree,_aliasIoriginalTree) =
                  (alias_ _aliasOcat _aliasOexpectedNumCols _aliasOidenv _aliasOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewLib2,_lhsOoriginalTree,_lhsOtrefIDs)))
sem_TableRef_JoinTref :: Annotation ->
                         T_TableRef  ->
                         Natural ->
                         JoinType ->
                         T_TableRef  ->
                         T_OnExpr  ->
                         T_TableAlias  ->
                         T_TableRef 
sem_TableRef_JoinTref ann_ tbl_ nat_ joinType_ tbl1_ onExpr_ alias_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let __tup3 :: (([(String,[String])],TableRef))
              _lhsOtrefIDs :: ([(String,[String])])
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _newLib :: (Either [TypeError] LocalBindings)
              _onExprOlib :: LocalBindings
              _lhsOnewLib2 :: LocalBindings
              _aliasOexpectedNumCols :: (Maybe Int)
              _lhsOoriginalTree :: TableRef
              _tblOcat :: Catalog
              _tblOidenv :: IDEnv
              _tblOlib :: LocalBindings
              _tbl1Ocat :: Catalog
              _tbl1Oidenv :: IDEnv
              _tbl1Olib :: LocalBindings
              _onExprOcat :: Catalog
              _onExprOidenv :: IDEnv
              _aliasOcat :: Catalog
              _aliasOidenv :: IDEnv
              _aliasOlib :: LocalBindings
              _tblIannotatedTree :: TableRef
              _tblIfixedUpIdentifiersTree :: TableRef
              _tblIlibUpdates :: ([LocalBindingsUpdate])
              _tblInewLib2 :: LocalBindings
              _tblIoriginalTree :: TableRef
              _tblItrefIDs :: ([(String,[String])])
              _tbl1IannotatedTree :: TableRef
              _tbl1IfixedUpIdentifiersTree :: TableRef
              _tbl1IlibUpdates :: ([LocalBindingsUpdate])
              _tbl1InewLib2 :: LocalBindings
              _tbl1IoriginalTree :: TableRef
              _tbl1ItrefIDs :: ([(String,[String])])
              _onExprIannotatedTree :: OnExpr
              _onExprIfixedUpIdentifiersTree :: OnExpr
              _onExprIoriginalTree :: OnExpr
              _aliasIannotatedTree :: TableAlias
              _aliasIfixedUpIdentifiersTree :: TableAlias
              _aliasIoriginalTree :: TableAlias
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 377, column 16)
              __tup3 =
                  {-# LINE 377 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  let (trs,al) = doAlias _aliasIannotatedTree $ _tblItrefIDs ++ _tbl1ItrefIDs
                  in (trs, JoinTref ann_ _tblIfixedUpIdentifiersTree
                                    nat_ joinType_ _tbl1IfixedUpIdentifiersTree
                                    _onExprIfixedUpIdentifiersTree al)
                  {-# LINE 16049 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 377, column 16)
              (_lhsOtrefIDs,_) =
                  {-# LINE 377 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup3
                  {-# LINE 16054 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 377, column 16)
              (_,_lhsOfixedUpIdentifiersTree) =
                  {-# LINE 377 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup3
                  {-# LINE 16059 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 55, column 9)
              _lhsOannotatedTree =
                  {-# LINE 55 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 16064 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 118, column 9)
              _errs =
                  {-# LINE 118 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  fromLeft [] _newLib
                  ++ _joinErrors
                  {-# LINE 16070 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 120, column 9)
              _lhsOlibUpdates =
                  {-# LINE 120 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  if _joinErrors     == []
                  then _libUpdates
                  else []
                  {-# LINE 16077 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 125, column 9)
              _joinErrors =
                  {-# LINE 125 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  fromLeft [] (foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _libUpdates    )
                  {-# LINE 16082 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 126, column 9)
              _libUpdates =
                  {-# LINE 126 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  case (_tblIlibUpdates, _tbl1IlibUpdates) of
                    ([u1], [u2]) -> [LBJoinTref "join" u1 u2 jids
                                                    (case _aliasIoriginalTree of
                                                             NoAlias _ -> Nothing
                                                             TableAlias _ t -> Just t
                                                             FullAlias _ t _ -> Just t)]
                    _ -> []
                  where
                    jids = case (nat_, _onExprIoriginalTree) of
                                (Natural, _) -> Left ()
                                (_,Just (JoinUsing _ s)) -> Right s
                                _ -> Right []
                  {-# LINE 16098 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 140, column 9)
              _newLib =
                  {-# LINE 140 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  case (_tblIlibUpdates, _tbl1IlibUpdates) of
                    ([u1],[u2]) -> lbUpdate _lhsIcat
                                     (LBJoinTref "join" u1 u2 (Right []) Nothing) _lhsIlib
                    _ -> Right _lhsIlib
                  {-# LINE 16106 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 144, column 9)
              _onExprOlib =
                  {-# LINE 144 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  fromRight _lhsIlib _newLib
                  {-# LINE 16111 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 219, column 9)
              _lhsOnewLib2 =
                  {-# LINE 219 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  emptyBindings
                  {-# LINE 16116 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 234, column 9)
              _backTree =
                  {-# LINE 234 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  JoinTref ann_
                             _tblIannotatedTree
                             nat_
                             joinType_
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
                             _aliasIannotatedTree
                  {-# LINE 16127 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 253, column 9)
              _aliasOexpectedNumCols =
                  {-# LINE 253 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  Nothing
                  {-# LINE 16132 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  JoinTref ann_ _tblIannotatedTree nat_ joinType_ _tbl1IannotatedTree _onExprIannotatedTree _aliasIannotatedTree
                  {-# LINE 16137 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  JoinTref ann_ _tblIfixedUpIdentifiersTree nat_ joinType_ _tbl1IfixedUpIdentifiersTree _onExprIfixedUpIdentifiersTree _aliasIfixedUpIdentifiersTree
                  {-# LINE 16142 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  JoinTref ann_ _tblIoriginalTree nat_ joinType_ _tbl1IoriginalTree _onExprIoriginalTree _aliasIoriginalTree
                  {-# LINE 16147 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16152 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16157 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16162 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16167 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1Ocat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16172 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16177 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1Olib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16182 "AstInternal.hs" #-}
              -- copy rule (down)
              _onExprOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16187 "AstInternal.hs" #-}
              -- copy rule (down)
              _onExprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16192 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16197 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16202 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16207 "AstInternal.hs" #-}
              ( _tblIannotatedTree,_tblIfixedUpIdentifiersTree,_tblIlibUpdates,_tblInewLib2,_tblIoriginalTree,_tblItrefIDs) =
                  (tbl_ _tblOcat _tblOidenv _tblOlib )
              ( _tbl1IannotatedTree,_tbl1IfixedUpIdentifiersTree,_tbl1IlibUpdates,_tbl1InewLib2,_tbl1IoriginalTree,_tbl1ItrefIDs) =
                  (tbl1_ _tbl1Ocat _tbl1Oidenv _tbl1Olib )
              ( _onExprIannotatedTree,_onExprIfixedUpIdentifiersTree,_onExprIoriginalTree) =
                  (onExpr_ _onExprOcat _onExprOidenv _onExprOlib )
              ( _aliasIannotatedTree,_aliasIfixedUpIdentifiersTree,_aliasIoriginalTree) =
                  (alias_ _aliasOcat _aliasOexpectedNumCols _aliasOidenv _aliasOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewLib2,_lhsOoriginalTree,_lhsOtrefIDs)))
sem_TableRef_SubTref :: Annotation ->
                        T_QueryExpr  ->
                        T_TableAlias  ->
                        T_TableRef 
sem_TableRef_SubTref ann_ sel_ alias_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let __tup4 :: (([(String,[String])],TableRef))
              _lhsOtrefIDs :: ([(String,[String])])
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOannotatedTree :: TableRef
              _selectAttrs :: (Either [TypeError] [(String,Type)])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOnewLib2 :: LocalBindings
              _aliasOexpectedNumCols :: (Maybe Int)
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOoriginalTree :: TableRef
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _aliasOcat :: Catalog
              _aliasOidenv :: IDEnv
              _aliasOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              _aliasIannotatedTree :: TableAlias
              _aliasIfixedUpIdentifiersTree :: TableAlias
              _aliasIoriginalTree :: TableAlias
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 367, column 15)
              __tup4 =
                  {-# LINE 367 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  let IDEnv x = _selIcidenv
                      (trs,al) = doAlias _aliasIannotatedTree x
                  in (trs, SubTref ann_ _selIfixedUpIdentifiersTree al)
                  {-# LINE 16256 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 367, column 15)
              (_lhsOtrefIDs,_) =
                  {-# LINE 367 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup4
                  {-# LINE 16261 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 367, column 15)
              (_,_lhsOfixedUpIdentifiersTree) =
                  {-# LINE 367 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup4
                  {-# LINE 16266 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 55, column 9)
              _lhsOannotatedTree =
                  {-# LINE 55 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 16271 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 72, column 9)
              _errs =
                  {-# LINE 72 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  case _selectAttrs     of
                          Left e -> e
                          Right _ -> []
                  {-# LINE 16278 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 76, column 9)
              _selectAttrs =
                  {-# LINE 76 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  lmt _selIuType
                  {-# LINE 16283 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 77, column 9)
              _lhsOlibUpdates =
                  {-# LINE 77 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  [LBTref "sub query" (getAlias "" _aliasIoriginalTree)
                                  (fromRight [] _selectAttrs    ) []]
                  {-# LINE 16289 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 219, column 9)
              _lhsOnewLib2 =
                  {-# LINE 219 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  emptyBindings
                  {-# LINE 16294 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 228, column 9)
              _backTree =
                  {-# LINE 228 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  SubTref ann_ _selIannotatedTree _aliasIannotatedTree
                  {-# LINE 16299 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 253, column 9)
              _aliasOexpectedNumCols =
                  {-# LINE 253 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  Nothing
                  {-# LINE 16304 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 182, column 15)
              _selOexpectedTypes =
                  {-# LINE 182 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 16309 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  SubTref ann_ _selIannotatedTree _aliasIannotatedTree
                  {-# LINE 16314 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SubTref ann_ _selIfixedUpIdentifiersTree _aliasIfixedUpIdentifiersTree
                  {-# LINE 16319 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  SubTref ann_ _selIoriginalTree _aliasIoriginalTree
                  {-# LINE 16324 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16329 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16334 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16339 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16344 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16349 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16354 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16359 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
              ( _aliasIannotatedTree,_aliasIfixedUpIdentifiersTree,_aliasIoriginalTree) =
                  (alias_ _aliasOcat _aliasOexpectedNumCols _aliasOidenv _aliasOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewLib2,_lhsOoriginalTree,_lhsOtrefIDs)))
sem_TableRef_Tref :: Annotation ->
                     T_SQIdentifier  ->
                     T_TableAlias  ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_ alias_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let __tup5 :: (([(String,[String])],TableRef))
              _lhsOtrefIDs :: ([(String,[String])])
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOnewLib2 :: LocalBindings
              _aliasOexpectedNumCols :: (Maybe Int)
              _lhsOoriginalTree :: TableRef
              _tblOcat :: Catalog
              _tblOidenv :: IDEnv
              _tblOlib :: LocalBindings
              _aliasOcat :: Catalog
              _aliasOidenv :: IDEnv
              _aliasOlib :: LocalBindings
              _tblIannotatedTree :: SQIdentifier
              _tblIfixedUpIdentifiersTree :: SQIdentifier
              _tblIoriginalTree :: SQIdentifier
              _tblItbAnnotatedTree :: SQIdentifier
              _tblItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _aliasIannotatedTree :: TableAlias
              _aliasIfixedUpIdentifiersTree :: TableAlias
              _aliasIoriginalTree :: TableAlias
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 359, column 12)
              __tup5 =
                  {-# LINE 359 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  let tn = getTName _tblIfixedUpIdentifiersTree
                      ids = case catCompositePublicAttrs _lhsIcat relationComposites tn of
                              Right attrs -> [(tn, map fst attrs)]
                              Left _ -> [(tn,[])]
                      (trs,al) = doAlias _aliasIannotatedTree  ids
                  in (trs,Tref ann_ _tblIfixedUpIdentifiersTree al)
                  {-# LINE 16404 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 359, column 12)
              (_lhsOtrefIDs,_) =
                  {-# LINE 359 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup5
                  {-# LINE 16409 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 359, column 12)
              (_,_lhsOfixedUpIdentifiersTree) =
                  {-# LINE 359 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup5
                  {-# LINE 16414 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 55, column 9)
              _lhsOannotatedTree =
                  {-# LINE 55 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 16419 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 85, column 9)
              _errs =
                  {-# LINE 85 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  []
                  {-# LINE 16424 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 86, column 9)
              _lhsOlibUpdates =
                  {-# LINE 86 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  maybe [] id $ do
                  let n = getTName _tblIannotatedTree
                  (pu,pr) <- _tblItbUType
                  return [LBTref ("tref: " ++ n)
                            (getAlias n _aliasIoriginalTree)
                            pu
                            pr]
                  {-# LINE 16435 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 208, column 9)
              _lhsOnewLib2 =
                  {-# LINE 208 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  createLocalBindings $ do
                  let n = getTName _tblIannotatedTree
                  (pu,pr) <- _tblItbUType
                  let (n,cs) = case _aliasIoriginalTree of
                                 (FullAlias _ n cs) -> (n,cs)
                                 _ -> (n, [])
                  return [(n,zip cs $ map (Just . snd) pu)
                         ,(n,map (second Just) pr)]
                  {-# LINE 16447 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 230, column 9)
              _backTree =
                  {-# LINE 230 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  Tref ann_ _tblItbAnnotatedTree _aliasIannotatedTree
                  {-# LINE 16452 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 247, column 9)
              _aliasOexpectedNumCols =
                  {-# LINE 247 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  do
                  let n = getTName _tblIannotatedTree
                  (pu,_) <- _tblItbUType
                  return $ length pu
                  {-# LINE 16460 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Tref ann_ _tblIannotatedTree _aliasIannotatedTree
                  {-# LINE 16465 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Tref ann_ _tblIfixedUpIdentifiersTree _aliasIfixedUpIdentifiersTree
                  {-# LINE 16470 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Tref ann_ _tblIoriginalTree _aliasIoriginalTree
                  {-# LINE 16475 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16480 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16485 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16490 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16495 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16500 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16505 "AstInternal.hs" #-}
              -- copy rule (down)
              _aliasOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16510 "AstInternal.hs" #-}
              ( _tblIannotatedTree,_tblIfixedUpIdentifiersTree,_tblIoriginalTree,_tblItbAnnotatedTree,_tblItbUType) =
                  (tbl_ _tblOcat _tblOidenv _tblOlib )
              ( _aliasIannotatedTree,_aliasIfixedUpIdentifiersTree,_aliasIoriginalTree) =
                  (alias_ _aliasOcat _aliasOexpectedNumCols _aliasOidenv _aliasOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewLib2,_lhsOoriginalTree,_lhsOtrefIDs)))
-- TableRefList ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         libUpdates           : [LocalBindingsUpdate]
         newLib2              : LocalBindings
         originalTree         : SELF 
         trefIDs              : [(String,[String])]
   alternatives:
      alternative Cons:
         child hd             : TableRef 
         child tl             : TableRefList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type TableRefList  = [(TableRef)]
-- cata
sem_TableRefList :: TableRefList  ->
                    T_TableRefList 
sem_TableRefList list  =
    (Prelude.foldr sem_TableRefList_Cons sem_TableRefList_Nil (Prelude.map sem_TableRef list) )
-- semantic domain
type T_TableRefList  = Catalog ->
                       IDEnv ->
                       LocalBindings ->
                       ( TableRefList,TableRefList,([LocalBindingsUpdate]),LocalBindings,TableRefList,([(String,[String])]))
data Inh_TableRefList  = Inh_TableRefList {cat_Inh_TableRefList :: Catalog,idenv_Inh_TableRefList :: IDEnv,lib_Inh_TableRefList :: LocalBindings}
data Syn_TableRefList  = Syn_TableRefList {annotatedTree_Syn_TableRefList :: TableRefList,fixedUpIdentifiersTree_Syn_TableRefList :: TableRefList,libUpdates_Syn_TableRefList :: [LocalBindingsUpdate],newLib2_Syn_TableRefList :: LocalBindings,originalTree_Syn_TableRefList :: TableRefList,trefIDs_Syn_TableRefList :: [(String,[String])]}
wrap_TableRefList :: T_TableRefList  ->
                     Inh_TableRefList  ->
                     Syn_TableRefList 
wrap_TableRefList sem (Inh_TableRefList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewLib2,_lhsOoriginalTree,_lhsOtrefIDs) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TableRefList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOnewLib2 _lhsOoriginalTree _lhsOtrefIDs ))
sem_TableRefList_Cons :: T_TableRef  ->
                         T_TableRefList  ->
                         T_TableRefList 
sem_TableRefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOtrefIDs :: ([(String,[String])])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOnewLib2 :: LocalBindings
              _lhsOannotatedTree :: TableRefList
              _lhsOfixedUpIdentifiersTree :: TableRefList
              _lhsOoriginalTree :: TableRefList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: TableRef
              _hdIfixedUpIdentifiersTree :: TableRef
              _hdIlibUpdates :: ([LocalBindingsUpdate])
              _hdInewLib2 :: LocalBindings
              _hdIoriginalTree :: TableRef
              _hdItrefIDs :: ([(String,[String])])
              _tlIannotatedTree :: TableRefList
              _tlIfixedUpIdentifiersTree :: TableRefList
              _tlIlibUpdates :: ([LocalBindingsUpdate])
              _tlInewLib2 :: LocalBindings
              _tlIoriginalTree :: TableRefList
              _tlItrefIDs :: ([(String,[String])])
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 355, column 12)
              _lhsOtrefIDs =
                  {-# LINE 355 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _hdItrefIDs ++ _tlItrefIDs
                  {-# LINE 16599 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 61, column 9)
              _lhsOlibUpdates =
                  {-# LINE 61 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  _hdIlibUpdates
                  {-# LINE 16604 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 204, column 9)
              _lhsOnewLib2 =
                  {-# LINE 204 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  _hdInewLib2
                  {-# LINE 16609 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 16614 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 16619 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 16624 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16629 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 16634 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16639 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16644 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16649 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16654 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16659 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16664 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16669 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIlibUpdates,_hdInewLib2,_hdIoriginalTree,_hdItrefIDs) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIlibUpdates,_tlInewLib2,_tlIoriginalTree,_tlItrefIDs) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewLib2,_lhsOoriginalTree,_lhsOtrefIDs)))
sem_TableRefList_Nil :: T_TableRefList 
sem_TableRefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOtrefIDs :: ([(String,[String])])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOnewLib2 :: LocalBindings
              _lhsOannotatedTree :: TableRefList
              _lhsOfixedUpIdentifiersTree :: TableRefList
              _lhsOoriginalTree :: TableRefList
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 356, column 11)
              _lhsOtrefIDs =
                  {-# LINE 356 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 16690 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 59, column 9)
              _lhsOlibUpdates =
                  {-# LINE 59 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  []
                  {-# LINE 16695 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/TableRefs.ag"(line 202, column 9)
              _lhsOnewLib2 =
                  {-# LINE 202 "./TypeChecking/QueryExprs/TableRefs.ag" #-}
                  createLocalBindings $ Just []
                  {-# LINE 16700 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 16705 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 16710 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 16715 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16720 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 16725 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16730 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewLib2,_lhsOoriginalTree,_lhsOtrefIDs)))
-- TypeAttributeDef --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         attrName             : String
         fixedUpIdentifiersTree : SELF 
         namedType            : Maybe Type
         originalTree         : SELF 
   alternatives:
      alternative TypeAttDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
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
type T_TypeAttributeDef  = Catalog ->
                           IDEnv ->
                           LocalBindings ->
                           ( TypeAttributeDef,String,TypeAttributeDef,(Maybe Type),TypeAttributeDef)
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {cat_Inh_TypeAttributeDef :: Catalog,idenv_Inh_TypeAttributeDef :: IDEnv,lib_Inh_TypeAttributeDef :: LocalBindings}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef,attrName_Syn_TypeAttributeDef :: String,fixedUpIdentifiersTree_Syn_TypeAttributeDef :: TypeAttributeDef,namedType_Syn_TypeAttributeDef :: Maybe Type,originalTree_Syn_TypeAttributeDef :: TypeAttributeDef}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TypeAttributeDef _lhsOannotatedTree _lhsOattrName _lhsOfixedUpIdentifiersTree _lhsOnamedType _lhsOoriginalTree ))
sem_TypeAttributeDef_TypeAttDef :: Annotation ->
                                   String ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef ann_ name_ typ_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOattrName :: String
              _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeAttributeDef
              _lhsOfixedUpIdentifiersTree :: TypeAttributeDef
              _lhsOoriginalTree :: TypeAttributeDef
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 37, column 9)
              _lhsOattrName =
                  {-# LINE 37 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  name_
                  {-# LINE 16800 "AstInternal.hs" #-}
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 38, column 9)
              _lhsOnamedType =
                  {-# LINE 38 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  _typInamedType
                  {-# LINE 16805 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIannotatedTree
                  {-# LINE 16810 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  TypeAttDef ann_ name_ _typIfixedUpIdentifiersTree
                  {-# LINE 16815 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIoriginalTree
                  {-# LINE 16820 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16825 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 16830 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16835 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16840 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16845 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16850 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree)))
-- TypeAttributeDefList ----------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         attrs                : [(String, Maybe Type)]
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : TypeAttributeDef 
         child tl             : TypeAttributeDefList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type TypeAttributeDefList  = [(TypeAttributeDef)]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = Catalog ->
                               IDEnv ->
                               LocalBindings ->
                               ( TypeAttributeDefList,([(String, Maybe Type)]),TypeAttributeDefList,TypeAttributeDefList)
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {cat_Inh_TypeAttributeDefList :: Catalog,idenv_Inh_TypeAttributeDefList :: IDEnv,lib_Inh_TypeAttributeDefList :: LocalBindings}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,attrs_Syn_TypeAttributeDefList :: [(String, Maybe Type)],fixedUpIdentifiersTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,originalTree_Syn_TypeAttributeDefList :: TypeAttributeDefList}
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TypeAttributeDefList _lhsOannotatedTree _lhsOattrs _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOattrs :: ([(String, Maybe Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOfixedUpIdentifiersTree :: TypeAttributeDefList
              _lhsOoriginalTree :: TypeAttributeDefList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: TypeAttributeDef
              _hdIattrName :: String
              _hdIfixedUpIdentifiersTree :: TypeAttributeDef
              _hdInamedType :: (Maybe Type)
              _hdIoriginalTree :: TypeAttributeDef
              _tlIannotatedTree :: TypeAttributeDefList
              _tlIattrs :: ([(String, Maybe Type)])
              _tlIfixedUpIdentifiersTree :: TypeAttributeDefList
              _tlIoriginalTree :: TypeAttributeDefList
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 43, column 12)
              _lhsOattrs =
                  {-# LINE 43 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 16930 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 16935 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 16940 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 16945 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16950 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 16955 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16960 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16965 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16970 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16975 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16980 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16985 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16990 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIattrName,_hdIfixedUpIdentifiersTree,_hdInamedType,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIattrs,_tlIfixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOattrs :: ([(String, Maybe Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOfixedUpIdentifiersTree :: TypeAttributeDefList
              _lhsOoriginalTree :: TypeAttributeDefList
              -- "./TypeChecking/Ddl/MiscCreates.ag"(line 44, column 11)
              _lhsOattrs =
                  {-# LINE 44 "./TypeChecking/Ddl/MiscCreates.ag" #-}
                  []
                  {-# LINE 17009 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17014 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 17019 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17024 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17029 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17034 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17039 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- TypeName ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         namedType            : Maybe Type
         originalTree         : SELF 
   alternatives:
      alternative ArrayTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Prec2TypeName:
         child ann            : {Annotation}
         child tn             : {String}
         child prec           : {Integer}
         child prec1          : {Integer}
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative PrecTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         child prec           : {Integer}
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative SetOfTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative SimpleTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data TypeName  = ArrayTypeName (Annotation) (TypeName) 
               | Prec2TypeName (Annotation) (String) (Integer) (Integer) 
               | PrecTypeName (Annotation) (String) (Integer) 
               | SetOfTypeName (Annotation) (TypeName) 
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
                   IDEnv ->
                   LocalBindings ->
                   ( TypeName,TypeName,(Maybe Type),TypeName)
data Inh_TypeName  = Inh_TypeName {cat_Inh_TypeName :: Catalog,idenv_Inh_TypeName :: IDEnv,lib_Inh_TypeName :: LocalBindings}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName,fixedUpIdentifiersTree_Syn_TypeName :: TypeName,namedType_Syn_TypeName :: Maybe Type,originalTree_Syn_TypeName :: TypeName}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TypeName _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnamedType _lhsOoriginalTree ))
sem_TypeName_ArrayTypeName :: Annotation ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName ann_ typ_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeName
              _lhsOfixedUpIdentifiersTree :: TypeName
              _lhsOoriginalTree :: TypeName
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  etmt _tpe
                  {-# LINE 17158 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 17163 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 27, column 9)
              _tpe =
                  {-# LINE 27 "./TypeChecking/Misc.ag" #-}
                  lmt _typInamedType >>=  Right . ArrayType
                  {-# LINE 17168 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 28, column 9)
              _backTree =
                  {-# LINE 28 "./TypeChecking/Misc.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 17173 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 17178 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ArrayTypeName ann_ _typIfixedUpIdentifiersTree
                  {-# LINE 17183 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIoriginalTree
                  {-# LINE 17188 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17193 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17198 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17203 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17208 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17213 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_Prec2TypeName :: Annotation ->
                              String ->
                              Integer ->
                              Integer ->
                              T_TypeName 
sem_TypeName_Prec2TypeName ann_ tn_ prec_ prec1_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeName
              _lhsOfixedUpIdentifiersTree :: TypeName
              _lhsOoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  etmt _tpe
                  {-# LINE 17234 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 17239 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 36, column 9)
              _tpe =
                  {-# LINE 36 "./TypeChecking/Misc.ag" #-}
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
                  {-# LINE 17244 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 37, column 9)
              _backTree =
                  {-# LINE 37 "./TypeChecking/Misc.ag" #-}
                  Prec2TypeName ann_ tn_ prec_ prec1_
                  {-# LINE 17249 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  Prec2TypeName ann_ tn_ prec_ prec1_
                  {-# LINE 17254 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Prec2TypeName ann_ tn_ prec_ prec1_
                  {-# LINE 17259 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  Prec2TypeName ann_ tn_ prec_ prec1_
                  {-# LINE 17264 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17269 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17274 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_PrecTypeName :: Annotation ->
                             String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName ann_ tn_ prec_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeName
              _lhsOfixedUpIdentifiersTree :: TypeName
              _lhsOoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  etmt _tpe
                  {-# LINE 17292 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 17297 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 33, column 9)
              _tpe =
                  {-# LINE 33 "./TypeChecking/Misc.ag" #-}
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
                  {-# LINE 17302 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 34, column 9)
              _backTree =
                  {-# LINE 34 "./TypeChecking/Misc.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 17307 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 17312 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 17317 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 17322 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17327 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17332 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_SetOfTypeName :: Annotation ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName ann_ typ_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeName
              _lhsOfixedUpIdentifiersTree :: TypeName
              _lhsOoriginalTree :: TypeName
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  etmt _tpe
                  {-# LINE 17356 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 17361 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 30, column 9)
              _tpe =
                  {-# LINE 30 "./TypeChecking/Misc.ag" #-}
                  lmt _typInamedType >>=  Right . SetOfType
                  {-# LINE 17366 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 31, column 9)
              _backTree =
                  {-# LINE 31 "./TypeChecking/Misc.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 17371 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 17376 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SetOfTypeName ann_ _typIfixedUpIdentifiersTree
                  {-# LINE 17381 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIoriginalTree
                  {-# LINE 17386 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17391 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17396 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17401 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17406 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17411 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_SimpleTypeName :: Annotation ->
                               String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName ann_ tn_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeName
              _lhsOfixedUpIdentifiersTree :: TypeName
              _lhsOoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  etmt _tpe
                  {-# LINE 17430 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 17435 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 24, column 9)
              _tpe =
                  {-# LINE 24 "./TypeChecking/Misc.ag" #-}
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
                  {-# LINE 17440 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 25, column 9)
              _backTree =
                  {-# LINE 25 "./TypeChecking/Misc.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 17445 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 17450 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 17455 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 17460 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17465 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17470 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOoriginalTree)))
-- TypeNameList ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         namedTypes           : [Maybe Type]
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : TypeName 
         child tl             : TypeNameList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type TypeNameList  = [(TypeName)]
-- cata
sem_TypeNameList :: TypeNameList  ->
                    T_TypeNameList 
sem_TypeNameList list  =
    (Prelude.foldr sem_TypeNameList_Cons sem_TypeNameList_Nil (Prelude.map sem_TypeName list) )
-- semantic domain
type T_TypeNameList  = Catalog ->
                       IDEnv ->
                       LocalBindings ->
                       ( TypeNameList,TypeNameList,([Maybe Type]),TypeNameList)
data Inh_TypeNameList  = Inh_TypeNameList {cat_Inh_TypeNameList :: Catalog,idenv_Inh_TypeNameList :: IDEnv,lib_Inh_TypeNameList :: LocalBindings}
data Syn_TypeNameList  = Syn_TypeNameList {annotatedTree_Syn_TypeNameList :: TypeNameList,fixedUpIdentifiersTree_Syn_TypeNameList :: TypeNameList,namedTypes_Syn_TypeNameList :: [Maybe Type],originalTree_Syn_TypeNameList :: TypeNameList}
wrap_TypeNameList :: T_TypeNameList  ->
                     Inh_TypeNameList  ->
                     Syn_TypeNameList 
wrap_TypeNameList sem (Inh_TypeNameList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedTypes,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TypeNameList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnamedTypes _lhsOoriginalTree ))
sem_TypeNameList_Cons :: T_TypeName  ->
                         T_TypeNameList  ->
                         T_TypeNameList 
sem_TypeNameList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOnamedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: TypeNameList
              _lhsOfixedUpIdentifiersTree :: TypeNameList
              _lhsOoriginalTree :: TypeNameList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: TypeName
              _hdIfixedUpIdentifiersTree :: TypeName
              _hdInamedType :: (Maybe Type)
              _hdIoriginalTree :: TypeName
              _tlIannotatedTree :: TypeNameList
              _tlIfixedUpIdentifiersTree :: TypeNameList
              _tlInamedTypes :: ([Maybe Type])
              _tlIoriginalTree :: TypeNameList
              -- "./TypeChecking/Ddl/Drops.ag"(line 37, column 12)
              _lhsOnamedTypes =
                  {-# LINE 37 "./TypeChecking/Ddl/Drops.ag" #-}
                  _hdInamedType : _tlInamedTypes
                  {-# LINE 17547 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 17552 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 17557 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 17562 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17567 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17572 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17577 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17582 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17587 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17592 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17597 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17602 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17607 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInamedType,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInamedTypes,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedTypes,_lhsOoriginalTree)))
sem_TypeNameList_Nil :: T_TypeNameList 
sem_TypeNameList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOnamedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: TypeNameList
              _lhsOfixedUpIdentifiersTree :: TypeNameList
              _lhsOoriginalTree :: TypeNameList
              -- "./TypeChecking/Ddl/Drops.ag"(line 38, column 11)
              _lhsOnamedTypes =
                  {-# LINE 38 "./TypeChecking/Ddl/Drops.ag" #-}
                  []
                  {-# LINE 17626 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17631 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 17636 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17641 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17646 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17651 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17656 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedTypes,_lhsOoriginalTree)))
-- VarDef ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         def                  : (String,Maybe Type)
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative ParamAlias:
         child ann            : {Annotation}
         child name           : {String}
         child i              : {Integer}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative VarAlias:
         child ann            : {Annotation}
         child name           : {String}
         child aliased        : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative VarDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         child value          : {Maybe ScalarExpr}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data VarDef  = ParamAlias (Annotation) (String) (Integer) 
             | VarAlias (Annotation) (String) (String) 
             | VarDef (Annotation) (String) (TypeName) (Maybe ScalarExpr) 
             deriving ( Data,Eq,Show,Typeable)
-- cata
sem_VarDef :: VarDef  ->
              T_VarDef 
sem_VarDef (ParamAlias _ann _name _i )  =
    (sem_VarDef_ParamAlias _ann _name _i )
sem_VarDef (VarAlias _ann _name _aliased )  =
    (sem_VarDef_VarAlias _ann _name _aliased )
sem_VarDef (VarDef _ann _name _typ _value )  =
    (sem_VarDef_VarDef _ann _name (sem_TypeName _typ ) _value )
-- semantic domain
type T_VarDef  = Catalog ->
                 IDEnv ->
                 LocalBindings ->
                 ( VarDef,((String,Maybe Type)),VarDef,VarDef)
data Inh_VarDef  = Inh_VarDef {cat_Inh_VarDef :: Catalog,idenv_Inh_VarDef :: IDEnv,lib_Inh_VarDef :: LocalBindings}
data Syn_VarDef  = Syn_VarDef {annotatedTree_Syn_VarDef :: VarDef,def_Syn_VarDef :: (String,Maybe Type),fixedUpIdentifiersTree_Syn_VarDef :: VarDef,originalTree_Syn_VarDef :: VarDef}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOdef,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_VarDef _lhsOannotatedTree _lhsOdef _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_VarDef_ParamAlias :: Annotation ->
                         String ->
                         Integer ->
                         T_VarDef 
sem_VarDef_ParamAlias ann_ name_ i_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOdef :: ((String,Maybe Type))
              _lhsOannotatedTree :: VarDef
              _lhsOfixedUpIdentifiersTree :: VarDef
              _lhsOoriginalTree :: VarDef
              -- "./TypeChecking/Plpgsql/Block.ag"(line 14, column 18)
              _lhsOdef =
                  {-# LINE 14 "./TypeChecking/Plpgsql/Block.ag" #-}
                  (name_, Nothing)
                  {-# LINE 17740 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  ParamAlias ann_ name_ i_
                  {-# LINE 17745 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ParamAlias ann_ name_ i_
                  {-# LINE 17750 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  ParamAlias ann_ name_ i_
                  {-# LINE 17755 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17760 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17765 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17770 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdef,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_VarDef_VarAlias :: Annotation ->
                       String ->
                       String ->
                       T_VarDef 
sem_VarDef_VarAlias ann_ name_ aliased_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOdef :: ((String,Maybe Type))
              _lhsOannotatedTree :: VarDef
              _lhsOfixedUpIdentifiersTree :: VarDef
              _lhsOoriginalTree :: VarDef
              -- "./TypeChecking/Plpgsql/Block.ag"(line 13, column 16)
              _lhsOdef =
                  {-# LINE 13 "./TypeChecking/Plpgsql/Block.ag" #-}
                  (name_, Nothing)
                  {-# LINE 17788 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  VarAlias ann_ name_ aliased_
                  {-# LINE 17793 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  VarAlias ann_ name_ aliased_
                  {-# LINE 17798 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  VarAlias ann_ name_ aliased_
                  {-# LINE 17803 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17808 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17813 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17818 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdef,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_VarDef_VarDef :: Annotation ->
                     String ->
                     T_TypeName  ->
                     (Maybe ScalarExpr) ->
                     T_VarDef 
sem_VarDef_VarDef ann_ name_ typ_ value_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOdef :: ((String,Maybe Type))
              _lhsOannotatedTree :: VarDef
              _lhsOfixedUpIdentifiersTree :: VarDef
              _lhsOoriginalTree :: VarDef
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Plpgsql/Block.ag"(line 10, column 14)
              _lhsOdef =
                  {-# LINE 10 "./TypeChecking/Plpgsql/Block.ag" #-}
                  (name_, if _typInamedType == Just (Pseudo Record)
                          then Just (PgRecord Nothing)
                          else _typInamedType)
                  {-# LINE 17846 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIannotatedTree value_
                  {-# LINE 17851 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  VarDef ann_ name_ _typIfixedUpIdentifiersTree value_
                  {-# LINE 17856 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIoriginalTree value_
                  {-# LINE 17861 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17866 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17871 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17876 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17881 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17886 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17891 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOdef,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- VarDefList --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         defs                 : [(String,Maybe Type)]
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : VarDef 
         child tl             : VarDefList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type VarDefList  = [(VarDef)]
-- cata
sem_VarDefList :: VarDefList  ->
                  T_VarDefList 
sem_VarDefList list  =
    (Prelude.foldr sem_VarDefList_Cons sem_VarDefList_Nil (Prelude.map sem_VarDef list) )
-- semantic domain
type T_VarDefList  = Catalog ->
                     IDEnv ->
                     LocalBindings ->
                     ( VarDefList,([(String,Maybe Type)]),VarDefList,VarDefList)
data Inh_VarDefList  = Inh_VarDefList {cat_Inh_VarDefList :: Catalog,idenv_Inh_VarDefList :: IDEnv,lib_Inh_VarDefList :: LocalBindings}
data Syn_VarDefList  = Syn_VarDefList {annotatedTree_Syn_VarDefList :: VarDefList,defs_Syn_VarDefList :: [(String,Maybe Type)],fixedUpIdentifiersTree_Syn_VarDefList :: VarDefList,originalTree_Syn_VarDefList :: VarDefList}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOdefs,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_VarDefList _lhsOannotatedTree _lhsOdefs _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOdefs :: ([(String,Maybe Type)])
              _lhsOannotatedTree :: VarDefList
              _lhsOfixedUpIdentifiersTree :: VarDefList
              _lhsOoriginalTree :: VarDefList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: VarDef
              _hdIdef :: ((String,Maybe Type))
              _hdIfixedUpIdentifiersTree :: VarDef
              _hdIoriginalTree :: VarDef
              _tlIannotatedTree :: VarDefList
              _tlIdefs :: ([(String,Maybe Type)])
              _tlIfixedUpIdentifiersTree :: VarDefList
              _tlIoriginalTree :: VarDefList
              -- "./TypeChecking/Plpgsql/Block.ag"(line 17, column 12)
              _lhsOdefs =
                  {-# LINE 17 "./TypeChecking/Plpgsql/Block.ag" #-}
                  _hdIdef : _tlIdefs
                  {-# LINE 17970 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 17975 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 17980 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 17985 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17990 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17995 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 18000 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 18005 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 18010 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 18015 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 18020 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 18025 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 18030 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIdef,_hdIfixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIdefs,_tlIfixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOdefs :: ([(String,Maybe Type)])
              _lhsOannotatedTree :: VarDefList
              _lhsOfixedUpIdentifiersTree :: VarDefList
              _lhsOoriginalTree :: VarDefList
              -- "./TypeChecking/Plpgsql/Block.ag"(line 18, column 11)
              _lhsOdefs =
                  {-# LINE 18 "./TypeChecking/Plpgsql/Block.ag" #-}
                  []
                  {-# LINE 18049 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 18054 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 18059 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 18064 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 18069 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 18074 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 18079 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- WithQuery ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         catUpdates           : [CatalogUpdate]
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative WithQuery:
         child ann            : {Annotation}
         child name           : {String}
         child colAliases     : {Maybe [String]}
         child ex             : QueryExpr 
         visit 0:
            local tpe         : _
            local backTree    : _
            local attrs       : _
            local catUpdates  : _
            local statementType : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
data WithQuery  = WithQuery (Annotation) (String) (Maybe [String]) (QueryExpr) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_WithQuery :: WithQuery  ->
                 T_WithQuery 
sem_WithQuery (WithQuery _ann _name _colAliases _ex )  =
    (sem_WithQuery_WithQuery _ann _name _colAliases (sem_QueryExpr _ex ) )
-- semantic domain
type T_WithQuery  = Catalog ->
                    IDEnv ->
                    LocalBindings ->
                    ( WithQuery,([CatalogUpdate]),WithQuery,WithQuery)
data Inh_WithQuery  = Inh_WithQuery {cat_Inh_WithQuery :: Catalog,idenv_Inh_WithQuery :: IDEnv,lib_Inh_WithQuery :: LocalBindings}
data Syn_WithQuery  = Syn_WithQuery {annotatedTree_Syn_WithQuery :: WithQuery,catUpdates_Syn_WithQuery :: [CatalogUpdate],fixedUpIdentifiersTree_Syn_WithQuery :: WithQuery,originalTree_Syn_WithQuery :: WithQuery}
wrap_WithQuery :: T_WithQuery  ->
                  Inh_WithQuery  ->
                  Syn_WithQuery 
wrap_WithQuery sem (Inh_WithQuery _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_WithQuery _lhsOannotatedTree _lhsOcatUpdates _lhsOfixedUpIdentifiersTree _lhsOoriginalTree ))
sem_WithQuery_WithQuery :: Annotation ->
                           String ->
                           (Maybe [String]) ->
                           T_QueryExpr  ->
                           T_WithQuery 
sem_WithQuery_WithQuery ann_ name_ colAliases_ ex_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _exOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: WithQuery
              _lhsOfixedUpIdentifiersTree :: WithQuery
              _lhsOoriginalTree :: WithQuery
              _lhsOcatUpdates :: ([CatalogUpdate])
              _exOcat :: Catalog
              _exOidenv :: IDEnv
              _exOlib :: LocalBindings
              _exIannotatedTree :: QueryExpr
              _exIcidenv :: IDEnv
              _exIfixedUpIdentifiersTree :: QueryExpr
              _exIlibUpdates :: ([LocalBindingsUpdate])
              _exIoriginalTree :: QueryExpr
              _exIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 247, column 9)
              _tpe =
                  {-# LINE 247 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 18157 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 248, column 9)
              _backTree =
                  {-# LINE 248 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  WithQuery ann_ name_ colAliases_ _exIannotatedTree
                  {-# LINE 18162 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 249, column 9)
              _attrs =
                  {-# LINE 249 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  maybe [] id $ _exIuType
                  {-# LINE 18167 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 250, column 9)
              _catUpdates =
                  {-# LINE 250 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  [CatCreateView name_ _attrs    ]
                  {-# LINE 18172 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 251, column 9)
              _statementType =
                  {-# LINE 251 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  Nothing
                  {-# LINE 18177 "AstInternal.hs" #-}
              -- "./TypeChecking/ParameterizedStatements.ag"(line 184, column 17)
              _exOexpectedTypes =
                  {-# LINE 184 "./TypeChecking/ParameterizedStatements.ag" #-}
                  []
                  {-# LINE 18182 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  WithQuery ann_ name_ colAliases_ _exIannotatedTree
                  {-# LINE 18187 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  WithQuery ann_ name_ colAliases_ _exIfixedUpIdentifiersTree
                  {-# LINE 18192 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  WithQuery ann_ name_ colAliases_ _exIoriginalTree
                  {-# LINE 18197 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 18202 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 18207 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 18212 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOcatUpdates =
                  {-# LINE 221 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _catUpdates
                  {-# LINE 18217 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 18222 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 18227 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 18232 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIcidenv,_exIfixedUpIdentifiersTree,_exIlibUpdates,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedTypes _exOidenv _exOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree)))
-- WithQueryList -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         catUpdates           : [CatalogUpdate]
         idenv                : IDEnv
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         fixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         producedCat          : Catalog
   alternatives:
      alternative Cons:
         child hd             : WithQuery 
         child tl             : WithQueryList 
         visit 0:
            local newCat      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local newCat      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
-}
type WithQueryList  = [(WithQuery)]
-- cata
sem_WithQueryList :: WithQueryList  ->
                     T_WithQueryList 
sem_WithQueryList list  =
    (Prelude.foldr sem_WithQueryList_Cons sem_WithQueryList_Nil (Prelude.map sem_WithQuery list) )
-- semantic domain
type T_WithQueryList  = Catalog ->
                        ([CatalogUpdate]) ->
                        IDEnv ->
                        LocalBindings ->
                        ( WithQueryList,WithQueryList,WithQueryList,Catalog)
data Inh_WithQueryList  = Inh_WithQueryList {cat_Inh_WithQueryList :: Catalog,catUpdates_Inh_WithQueryList :: [CatalogUpdate],idenv_Inh_WithQueryList :: IDEnv,lib_Inh_WithQueryList :: LocalBindings}
data Syn_WithQueryList  = Syn_WithQueryList {annotatedTree_Syn_WithQueryList :: WithQueryList,fixedUpIdentifiersTree_Syn_WithQueryList :: WithQueryList,originalTree_Syn_WithQueryList :: WithQueryList,producedCat_Syn_WithQueryList :: Catalog}
wrap_WithQueryList :: T_WithQueryList  ->
                      Inh_WithQueryList  ->
                      Syn_WithQueryList 
wrap_WithQueryList sem (Inh_WithQueryList _lhsIcat _lhsIcatUpdates _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat) =
             (sem _lhsIcat _lhsIcatUpdates _lhsIidenv _lhsIlib )
     in  (Syn_WithQueryList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOoriginalTree _lhsOproducedCat ))
sem_WithQueryList_Cons :: T_WithQuery  ->
                          T_WithQueryList  ->
                          T_WithQueryList 
sem_WithQueryList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIcatUpdates
       _lhsIidenv
       _lhsIlib ->
         (let _hdOcat :: Catalog
              _tlOcat :: Catalog
              _lhsOproducedCat :: Catalog
              _tlOcatUpdates :: ([CatalogUpdate])
              _lhsOannotatedTree :: WithQueryList
              _lhsOfixedUpIdentifiersTree :: WithQueryList
              _lhsOoriginalTree :: WithQueryList
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: WithQuery
              _hdIcatUpdates :: ([CatalogUpdate])
              _hdIfixedUpIdentifiersTree :: WithQuery
              _hdIoriginalTree :: WithQuery
              _tlIannotatedTree :: WithQueryList
              _tlIfixedUpIdentifiersTree :: WithQueryList
              _tlIoriginalTree :: WithQueryList
              _tlIproducedCat :: Catalog
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 231, column 9)
              _newCat =
                  {-# LINE 231 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
                  {-# LINE 18317 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 233, column 9)
              _hdOcat =
                  {-# LINE 233 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _newCat
                  {-# LINE 18322 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 234, column 9)
              _tlOcat =
                  {-# LINE 234 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _newCat
                  {-# LINE 18327 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 238, column 9)
              _lhsOproducedCat =
                  {-# LINE 238 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _tlIproducedCat
                  {-# LINE 18332 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 241, column 9)
              _tlOcatUpdates =
                  {-# LINE 241 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _hdIcatUpdates
                  {-# LINE 18337 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 18342 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 18347 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 18352 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 18357 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 18362 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 18367 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 18372 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 18377 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 18382 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 18387 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIcatUpdates,_hdIfixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIoriginalTree,_tlIproducedCat) =
                  (tl_ _tlOcat _tlOcatUpdates _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat)))
sem_WithQueryList_Nil :: T_WithQueryList 
sem_WithQueryList_Nil  =
    (\ _lhsIcat
       _lhsIcatUpdates
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOproducedCat :: Catalog
              _lhsOannotatedTree :: WithQueryList
              _lhsOfixedUpIdentifiersTree :: WithQueryList
              _lhsOoriginalTree :: WithQueryList
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 231, column 9)
              _newCat =
                  {-# LINE 231 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
                  {-# LINE 18407 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryExprs/QueryStatement.ag"(line 243, column 9)
              _lhsOproducedCat =
                  {-# LINE 243 "./TypeChecking/QueryExprs/QueryStatement.ag" #-}
                  _newCat
                  {-# LINE 18412 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 18417 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 18422 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 18427 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 84 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 18432 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 18437 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 90 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 18442 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat)))