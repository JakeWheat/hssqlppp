

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
   ,typeCheckQueryExpression
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

import Data.Generics.PlateData
import Debug.Trace


import Database.HsSqlPpp.AstInternals.TypeType
import Database.HsSqlPpp.AstInternals.TypeChecking.TypeConversion
import Database.HsSqlPpp.AstInternals.AstAnnotation
import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
import Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindings
import Database.HsSqlPpp.Utils.Utils
import Database.HsSqlPpp.AstInternals.TypeChecking.ErrorUtils


{-# LINE 334 "AstInternal.ag" #-}



-- used for schema qualified identifiers
-- should be used in more places in the ast
{-# LINE 117 "AstInternal.hs" #-}

{-# LINE 391 "AstInternal.ag" #-}

data TableAlias = NoAlias
                | TableAlias String --alias:String
                | FullAlias String [String] -- alias:String cols:{[String]}
                  deriving (Show,Eq,Typeable,Data)
{-# LINE 125 "AstInternal.hs" #-}

{-# LINE 401 "AstInternal.ag" #-}

data JoinType = Inner | LeftOuter| RightOuter | FullOuter | Cross
                deriving (Show,Eq,Typeable,Data)
{-# LINE 131 "AstInternal.hs" #-}

{-# LINE 413 "AstInternal.ag" #-}

data CopySource = CopyFilename String
                | Stdin
                  deriving (Show,Eq,Typeable,Data)
{-# LINE 138 "AstInternal.hs" #-}

{-# LINE 462 "AstInternal.ag" #-}

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
{-# LINE 155 "AstInternal.hs" #-}

{-# LINE 491 "AstInternal.ag" #-}

data RaiseType = RNotice | RException | RError
                 deriving (Show,Eq,Typeable,Data)

data CombineType = Except | Union | Intersect | UnionAll
                   deriving (Show,Eq,Typeable,Data)

data Volatility = Volatile | Stable | Immutable
                  deriving (Show,Eq,Typeable,Data)

data Language = Sql | Plpgsql
                deriving (Show,Eq,Typeable,Data)
{-# LINE 170 "AstInternal.hs" #-}

{-# LINE 510 "AstInternal.ag" #-}

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

{-# LINE 201 "AstInternal.hs" #-}

{-# LINE 609 "AstInternal.ag" #-}

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

{-# LINE 246 "AstInternal.hs" #-}

{-# LINE 657 "AstInternal.ag" #-}

data FrameClause = FrameUnboundedPreceding
                 | FrameUnboundedFull
                 | FrameRowsUnboundedPreceding
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 254 "AstInternal.hs" #-}

{-# LINE 824 "AstInternal.ag" #-}

{-
-- | Type check multiple asts, allowing type checking references in
--   later files to definitions in earlier files. This is probably
--   more straightforward if you parse the files then concatenate the
--   statementlists together before type checking rather than using
--   this function
typeCheckMany :: Catalog -> [StatementList] -> [StatementList]
typeCheckMany cat sts =
    annInt cat sts []
    where
      annInt e (s:ss) ress =
          let (e1,res) = typeCheck e s
          in annInt e1 ss (res:ress)
      annInt _ [] ress = reverse ress
-}

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

typeCheckQueryExpression :: Catalog -> QueryExpr -> QueryExpr
typeCheckQueryExpression cat qe =
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

{-# LINE 331 "AstInternal.hs" #-}

{-# LINE 63 "./TypeChecking/Misc.ag" #-}

{-
================================================================================

= some small utils

-}


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
ghc -Wall --make -i/home/jake/wd/gpgpu_stream/jake/src/cuda:/home/jake/wd/gpgpu_stream/jake/src/sqream:/home/jake/wd/gpgpu_stream/jake/src/ra:/home/jake/wd/gpgpu_stream/jake/src/sqldbms:/home/jake/wd/gpgpu_stream/jake/src/tests:/home/jake/wd/gpgpu_stream/jake/src/sql  /home/jake/wd/gpgpu_stream/jake/src/tests/GenerateCudaTests.lhs
getTName :: SQIdentifier -> String
getTName (SQIdentifier _ x@(_:_)) = last x
getTName x = error $ "internal error getName called on: " ++ show x


unwrapLookup :: (String,[String],Type) -> Type
unwrapLookup (_,_,t) = t

{-unwrapStar :: [(String,[String],Type)] -> [(String,Type)]
unwrapStar = map uw
             where
               uw (_,n,t) = (last n, t)-}

allAtts :: ([(String,Type)],[(String,Type)]) -> [(String,Type)]
allAtts (a,b) = a ++ b

{-
================================================================================

proper dodgy:
1st pass is to add inferred types to the tree. This is done only for
expressions in a funcall argument list atm. Then we pull out the
placeholders after they've had this information added. Only the
placeholders in funcall argument lists will have their type inferred
in this way, to be expanded. Insert also does this currently, but in Dml.ag

This should probably be done during the typechecking phase instead,
but probably needs a proper type inferencing algorithm to be used, is
done like this for development expediency.

Trying to follow haskell naming convention (?) - the type that the
node is expected to have as determined by it's parent node is the
expectedType, and the type it claims to have by its own logic and the
types of its child nodes is the inferred type. Confusingly, this means
the inferredType is often the declared type, and not the type that has
been inferred here...

The plan is to have three attributes: inferredType, expectedType and
type, where the type is Just iff the inferredType and expectedType are
the same, or onne of them is Nothing.

-}

getPlaceholderTypes :: Data a => a -> [Maybe Type]
getPlaceholderTypes ex =
    [infType (getAnnotation x) | x <- universeBi ex
                                 ,isPlaceholder x]
    where
      isPlaceholder e = case e of
                          PositionalArg _ _ -> True
                          Placeholder _ -> True
                          _ -> False

{-# LINE 425 "AstInternal.hs" #-}

{-# LINE 161 "./TypeChecking/QueryStatement.ag" #-}


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

{-# LINE 461 "AstInternal.hs" #-}

{-# LINE 208 "./TypeChecking/TableRefs.ag" #-}




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
    NoAlias -> def
    TableAlias t -> t
    FullAlias t _ -> t

{-# LINE 509 "AstInternal.hs" #-}

{-# LINE 18 "./TypeChecking/SelectLists.ag" #-}

{-data SiType = SiType (String,Maybe Type)
            | SiStarType [(String,Maybe Type)]-}
{-# LINE 515 "AstInternal.hs" #-}

{-# LINE 64 "./TypeChecking/SelectLists.ag" #-}

--unwrapSetofs :: [(String,Type)] -> [(String,Type)]
--unwrapSetofs = map (\(n,t) -> (n, unwrapSetof t))

unwrapSetof :: Type -> Type
unwrapSetof (SetOfType u) = u
unwrapSetof v = v

{-# LINE 526 "AstInternal.hs" #-}

{-# LINE 51 "./TypeChecking/CreateTable.ag" #-}

defaultSystemColumns :: [(String,Type)]
defaultSystemColumns = [("tableoid", ScalarType "oid")
                       ,("cmax", ScalarType "cid")
                       ,("xmax", ScalarType "xid")
                       ,("cmin", ScalarType "cid")
                       ,("xmin", ScalarType "xid")
                       ,("ctid", ScalarType "tid")]
{-# LINE 537 "AstInternal.hs" #-}

{-# LINE 31 "./TypeChecking/CreateFunction.ag" #-}

data ParamName = NamedParam Int String
               | UnnamedParam Int
{-# LINE 543 "AstInternal.hs" #-}

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


{-# LINE 585 "AstInternal.hs" #-}

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

{-# LINE 605 "AstInternal.hs" #-}

{-# LINE 297 "./TypeChecking/FixUpIdentifiers.ag" #-}




{-# LINE 612 "AstInternal.hs" #-}

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

{-# LINE 641 "AstInternal.hs" #-}

{-# LINE 401 "./TypeChecking/FixUpIdentifiers.ag" #-}

doAlias :: TableAlias -> [(String,[String])] -> ([(String,[String])],TableAlias)
doAlias NoAlias [] = ([],NoAlias)
doAlias NoAlias cs@((t,_):ts) = if all (==t) $ map fst ts
                                then (cs,FullAlias t $ concatMap snd cs)
                                else (cs,NoAlias)
doAlias (TableAlias a) cs = let cs' = concatMap snd cs
                            in ([(a, cs')], FullAlias a cs')
doAlias f@(FullAlias a cs) _ = ([(a,cs)], f)
{-# LINE 653 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  AddConstraint ann_ _conIannotatedTree
                  {-# LINE 726 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  AddConstraint ann_ _conIfixedUpIdentifiersTree
                  {-# LINE 731 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  AddConstraint ann_ _conIoriginalTree
                  {-# LINE 736 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 741 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 746 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 751 "AstInternal.hs" #-}
              -- copy rule (down)
              _conOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 756 "AstInternal.hs" #-}
              -- copy rule (down)
              _conOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 761 "AstInternal.hs" #-}
              -- copy rule (down)
              _conOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 766 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 376, column 26)
              _defOexpectedType =
                  {-# LINE 376 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 793 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  AlterColumnDefault ann_ nm_ _defIannotatedTree
                  {-# LINE 798 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  AlterColumnDefault ann_ nm_ _defIfixedUpIdentifiersTree
                  {-# LINE 803 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  AlterColumnDefault ann_ nm_ _defIoriginalTree
                  {-# LINE 808 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 813 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 818 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 823 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 828 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 833 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 838 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 913 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 918 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 923 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 928 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 933 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 938 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 943 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 948 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 953 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 958 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 963 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 968 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 986 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 991 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 996 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1001 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1006 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1011 "AstInternal.hs" #-}
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
              -- "./TypeChecking/CreateTable.ag"(line 83, column 9)
              _lhsOattrName =
                  {-# LINE 83 "./TypeChecking/CreateTable.ag" #-}
                  map toLower name_
                  {-# LINE 1098 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 84, column 9)
              _lhsOnamedType =
                  {-# LINE 84 "./TypeChecking/CreateTable.ag" #-}
                  _typInamedType
                  {-# LINE 1103 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 98, column 9)
              _consOlib =
                  {-# LINE 98 "./TypeChecking/CreateTable.ag" #-}
                  either (const _lhsIlib) id $ do
                  t <- lmt _typInamedType
                  lbUpdate _lhsIcat
                           (LBIds "attribute def" Nothing
                                  [(name_, t)]) _lhsIlib
                  {-# LINE 1112 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
                  {-# LINE 1117 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  AttributeDef ann_ name_ _typIfixedUpIdentifiersTree _defIfixedUpIdentifiersTree _consIfixedUpIdentifiersTree
                  {-# LINE 1122 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIoriginalTree _defIoriginalTree _consIoriginalTree
                  {-# LINE 1127 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1132 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1137 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1142 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1147 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1152 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1157 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1162 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1167 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1172 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1177 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1182 "AstInternal.hs" #-}
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
              -- "./TypeChecking/CreateTable.ag"(line 88, column 12)
              _lhsOattrs =
                  {-# LINE 88 "./TypeChecking/CreateTable.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 1266 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1271 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 1276 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 1281 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1286 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1291 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1296 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1301 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1306 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1311 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1316 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1321 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1326 "AstInternal.hs" #-}
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
              -- "./TypeChecking/CreateTable.ag"(line 89, column 11)
              _lhsOattrs =
                  {-# LINE 89 "./TypeChecking/CreateTable.ag" #-}
                  []
                  {-# LINE 1345 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1350 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 1355 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1360 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1365 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1370 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1375 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 196, column 13)
              _lhsOwhenTypes =
                  {-# LINE 196 "./TypeChecking/ScalarExprs.ag" #-}
                  _x1IuType
                  {-# LINE 1451 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 197, column 13)
              _lhsOthenType =
                  {-# LINE 197 "./TypeChecking/ScalarExprs.ag" #-}
                  _x2IuType
                  {-# LINE 1456 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 380, column 13)
              _x1OexpectedTypes =
                  {-# LINE 380 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 1461 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 381, column 13)
              _x2OexpectedType =
                  {-# LINE 381 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 1466 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 1471 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (_x1IfixedUpIdentifiersTree,_x2IfixedUpIdentifiersTree)
                  {-# LINE 1476 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 1481 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1486 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1491 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1496 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1501 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1506 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1511 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1516 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1521 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1526 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 187, column 10)
              _lhsOwhenTypes =
                  {-# LINE 187 "./TypeChecking/ScalarExprs.ag" #-}
                  _hdIwhenTypes : _tlIwhenTypes
                  {-# LINE 1611 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 188, column 10)
              _lhsOthenTypes =
                  {-# LINE 188 "./TypeChecking/ScalarExprs.ag" #-}
                  _hdIthenType : _tlIthenTypes
                  {-# LINE 1616 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1621 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 1626 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 1631 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1636 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1641 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1646 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1651 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1656 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1661 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1666 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1671 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1676 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 189, column 9)
              _lhsOwhenTypes =
                  {-# LINE 189 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 1696 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 190, column 9)
              _lhsOthenTypes =
                  {-# LINE 190 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 1701 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1706 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 1711 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1716 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1721 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1726 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1731 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 384, column 23)
              _exprOexpectedType =
                  {-# LINE 384 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 1835 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CheckConstraint ann_ name_ _exprIannotatedTree
                  {-# LINE 1840 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CheckConstraint ann_ name_ _exprIfixedUpIdentifiersTree
                  {-# LINE 1845 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CheckConstraint ann_ name_ _exprIoriginalTree
                  {-# LINE 1850 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1855 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1860 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1865 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1870 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 1875 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1880 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  PrimaryKeyConstraint ann_ name_ x_
                  {-# LINE 1899 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  PrimaryKeyConstraint ann_ name_ x_
                  {-# LINE 1904 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  PrimaryKeyConstraint ann_ name_ x_
                  {-# LINE 1909 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1914 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1919 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1924 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
                  {-# LINE 1945 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
                  {-# LINE 1950 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
                  {-# LINE 1955 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1960 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 1965 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1970 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  UniqueConstraint ann_ name_ x_
                  {-# LINE 1987 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  UniqueConstraint ann_ name_ x_
                  {-# LINE 1992 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  UniqueConstraint ann_ name_ x_
                  {-# LINE 1997 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2002 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2007 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2012 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 2085 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 2090 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 2095 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2100 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2105 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2110 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2115 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2120 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2125 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2130 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2135 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2140 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 2158 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 2163 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 2168 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2173 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2178 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2183 "AstInternal.hs" #-}
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
                  {-# LINE 2259 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _blkIannotatedTree
                  {-# LINE 2264 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  PlpgsqlFnBody ann_ _blkIfixedUpIdentifiersTree
                  {-# LINE 2269 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _blkIoriginalTree
                  {-# LINE 2274 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2279 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2284 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2289 "AstInternal.hs" #-}
              -- copy rule (down)
              _blkOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2294 "AstInternal.hs" #-}
              -- copy rule (down)
              _blkOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2299 "AstInternal.hs" #-}
              -- copy rule (down)
              _blkOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2304 "AstInternal.hs" #-}
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
                  {-# LINE 2332 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 130, column 9)
              _stsOlibUpdates =
                  {-# LINE 130 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 2337 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIannotatedTree
                  {-# LINE 2342 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SqlFnBody ann_ _stsIfixedUpIdentifiersTree
                  {-# LINE 2347 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIoriginalTree
                  {-# LINE 2352 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2357 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2362 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2367 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2372 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2377 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2382 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 336, column 9)
              _lhsOlistType =
                  {-# LINE 336 "./TypeChecking/ScalarExprs.ag" #-}
                  mapM lmt _exprsIuType >>= resolveResultSetType _lhsIcat
                  {-# LINE 2461 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 453, column 14)
              _exprsOexpectedTypes =
                  {-# LINE 453 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 2466 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  InList ann_ _exprsIannotatedTree
                  {-# LINE 2471 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  InList ann_ _exprsIfixedUpIdentifiersTree
                  {-# LINE 2476 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  InList ann_ _exprsIoriginalTree
                  {-# LINE 2481 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2486 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2491 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2496 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2501 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2506 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2511 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 338, column 9)
              _lhsOlistType =
                  {-# LINE 338 "./TypeChecking/ScalarExprs.ag" #-}
                  do
                  st <- lmt (map snd <$> _selIuType)
                  case length st of
                            0 -> Left [InternalError
                                       "got subquery with no columns? in inselect"]
                            1 -> Right $ head st
                            _ -> Right $ AnonymousRecordType st
                  {-# LINE 2546 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 479, column 16)
              _selOexpectedTypes =
                  {-# LINE 479 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 2551 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  InSelect ann_ _selIannotatedTree
                  {-# LINE 2556 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  InSelect ann_ _selIfixedUpIdentifiersTree
                  {-# LINE 2561 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  InSelect ann_ _selIoriginalTree
                  {-# LINE 2566 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2571 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2576 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2581 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2586 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2591 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2596 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 398, column 14)
              _exprOexpectedType =
                  {-# LINE 398 "./TypeChecking/ScalarExprs.ag" #-}
                  Just typeBool
                  {-# LINE 2673 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  JoinOn ann_ _exprIannotatedTree
                  {-# LINE 2678 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  JoinOn ann_ _exprIfixedUpIdentifiersTree
                  {-# LINE 2683 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  JoinOn ann_ _exprIoriginalTree
                  {-# LINE 2688 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2693 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2698 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2703 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2708 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2713 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2718 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  JoinUsing ann_ x_
                  {-# LINE 2736 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  JoinUsing ann_ x_
                  {-# LINE 2741 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  JoinUsing ann_ x_
                  {-# LINE 2746 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2751 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2756 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2761 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 64, column 9)
              _lhsOannotatedTree =
                  {-# LINE 64 "./TypeChecking/ScalarExprs.ag" #-}
                  let t = _justIuType
                  in if t `elem` [Nothing,Just typeBool]
                     then Just _justIannotatedTree
                     else Just $ addTypeErrors [ExpressionMustBeBool] _justIannotatedTree
                  {-# LINE 2833 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 401, column 12)
              _justOexpectedType =
                  {-# LINE 401 "./TypeChecking/ScalarExprs.ag" #-}
                  Just typeBool
                  {-# LINE 2838 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 2843 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Just _justIfixedUpIdentifiersTree
                  {-# LINE 2848 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 2853 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2858 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2863 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2868 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 2873 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2878 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 2894 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Nothing
                  {-# LINE 2899 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 2904 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 2909 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 2914 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2919 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 57, column 12)
              _lhsOuType =
                  {-# LINE 57 "./TypeChecking/ScalarExprs.ag" #-}
                  _justIuType
                  {-# LINE 2990 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 404, column 12)
              _justOexpectedType =
                  {-# LINE 404 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 2995 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3000 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Just _justIfixedUpIdentifiersTree
                  {-# LINE 3005 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 3010 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3015 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3020 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3025 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3030 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3035 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3040 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 58, column 15)
              _lhsOuType =
                  {-# LINE 58 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 3057 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3062 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Nothing
                  {-# LINE 3067 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3072 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3077 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3082 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3087 "AstInternal.hs" #-}
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
              -- "./TypeChecking/SelectLists.ag"(line 38, column 12)
              _lhsOlistType =
                  {-# LINE 38 "./TypeChecking/SelectLists.ag" #-}
                  _justIlistType
                  {-# LINE 3159 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3164 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Just _justIfixedUpIdentifiersTree
                  {-# LINE 3169 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 3174 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3179 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3184 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3189 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3194 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3199 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3204 "AstInternal.hs" #-}
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
              -- "./TypeChecking/SelectLists.ag"(line 39, column 15)
              _lhsOlistType =
                  {-# LINE 39 "./TypeChecking/SelectLists.ag" #-}
                  []
                  {-# LINE 3221 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3226 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Nothing
                  {-# LINE 3231 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3236 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3241 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3246 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3251 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 3318 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Just _justIfixedUpIdentifiersTree
                  {-# LINE 3323 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 3328 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3333 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3338 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3343 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3348 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3353 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3358 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3374 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Nothing
                  {-# LINE 3379 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 3384 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3389 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3394 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3399 "AstInternal.hs" #-}
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
              -- "./TypeChecking/CreateFunction.ag"(line 45, column 9)
              _lhsOnamedType =
                  {-# LINE 45 "./TypeChecking/CreateFunction.ag" #-}
                  _typInamedType
                  {-# LINE 3482 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 47, column 9)
              _lhsOparamName =
                  {-# LINE 47 "./TypeChecking/CreateFunction.ag" #-}
                  NamedParam _lhsIpos name_
                  {-# LINE 3487 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIannotatedTree
                  {-# LINE 3492 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ParamDef ann_ name_ _typIfixedUpIdentifiersTree
                  {-# LINE 3497 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIoriginalTree
                  {-# LINE 3502 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3507 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3512 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3517 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3522 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3527 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3532 "AstInternal.hs" #-}
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
              -- "./TypeChecking/CreateFunction.ag"(line 45, column 9)
              _lhsOnamedType =
                  {-# LINE 45 "./TypeChecking/CreateFunction.ag" #-}
                  _typInamedType
                  {-# LINE 3560 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 49, column 9)
              _lhsOparamName =
                  {-# LINE 49 "./TypeChecking/CreateFunction.ag" #-}
                  UnnamedParam _lhsIpos
                  {-# LINE 3565 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIannotatedTree
                  {-# LINE 3570 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ParamDefTp ann_ _typIfixedUpIdentifiersTree
                  {-# LINE 3575 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIoriginalTree
                  {-# LINE 3580 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3585 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3590 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3595 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3600 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3605 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3610 "AstInternal.hs" #-}
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
              -- "./TypeChecking/CreateFunction.ag"(line 53, column 13)
              _lhsOparams =
                  {-# LINE 53 "./TypeChecking/CreateFunction.ag" #-}
                  ((_hdIparamName, _hdInamedType) : _tlIparams)
                  {-# LINE 3695 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 54, column 13)
              _hdOpos =
                  {-# LINE 54 "./TypeChecking/CreateFunction.ag" #-}
                  _lhsIpos
                  {-# LINE 3700 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 55, column 13)
              _tlOpos =
                  {-# LINE 55 "./TypeChecking/CreateFunction.ag" #-}
                  _lhsIpos + 1
                  {-# LINE 3705 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 3710 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 3715 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 3720 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3725 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3730 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3735 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3740 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3745 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3750 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3755 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 3760 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3765 "AstInternal.hs" #-}
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
              -- "./TypeChecking/CreateFunction.ag"(line 52, column 12)
              _lhsOparams =
                  {-# LINE 52 "./TypeChecking/CreateFunction.ag" #-}
                  []
                  {-# LINE 3785 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 3790 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 3795 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 3800 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 3805 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 3810 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3815 "AstInternal.hs" #-}
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
            local newLib      : _
            local tpe         : {Et}
            local backTree    : _
            local trefEnv     : _
            local includeCorrelations : _
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
         (let _lhsOannotatedTree :: QueryExpr
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _lhsOuType :: (Maybe [(String,Type)])
              _lhsOcidenv :: IDEnv
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
              -- "./TypeChecking/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/QueryStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 3956 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 115, column 9)
              _lhsOlibUpdates =
                  {-# LINE 115 "./TypeChecking/QueryStatement.ag" #-}
                  []
                  {-# LINE 3961 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 142, column 9)
              _tpe =
                  {-# LINE 142 "./TypeChecking/QueryStatement.ag" #-}
                  do
                  sel1t <- lmt ((SetOfType . CompositeType) <$> _sel1IuType)
                  sel2t <- lmt ((SetOfType . CompositeType) <$> _sel2IuType)
                  typeCheckCombineSelect _lhsIcat sel1t sel2t
                  {-# LINE 3969 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 148, column 9)
              _backTree =
                  {-# LINE 148 "./TypeChecking/QueryStatement.ag" #-}
                  CombineSelect ann_ ctype_
                                _sel1IannotatedTree
                                _sel2IannotatedTree
                  {-# LINE 3976 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 159, column 9)
              _lhsOuType =
                  {-# LINE 159 "./TypeChecking/QueryStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 3981 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 230, column 21)
              _lhsOcidenv =
                  {-# LINE 230 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _sel1Icidenv
                  {-# LINE 3986 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CombineSelect ann_ ctype_ _sel1IannotatedTree _sel2IannotatedTree
                  {-# LINE 3991 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CombineSelect ann_ ctype_ _sel1IfixedUpIdentifiersTree _sel2IfixedUpIdentifiersTree
                  {-# LINE 3996 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CombineSelect ann_ ctype_ _sel1IoriginalTree _sel2IoriginalTree
                  {-# LINE 4001 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4006 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4011 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4016 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1OexpectedTypes =
                  {-# LINE 465 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 4021 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4026 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4031 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4036 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2OexpectedTypes =
                  {-# LINE 465 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 4041 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4046 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4051 "AstInternal.hs" #-}
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
         (let _selGroupByOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: QueryExpr
              _selSelectListOlib :: LocalBindings
              _selWhereOlib :: LocalBindings
              _selGroupByOlib :: LocalBindings
              _selOrderByOlib :: LocalBindings
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _lhsOuType :: (Maybe [(String,Type)])
              _lhsOfixedUpIdentifiersTree :: QueryExpr
              _lhsOcidenv :: IDEnv
              _selSelectListOidenv :: IDEnv
              _selWhereOidenv :: IDEnv
              _selGroupByOidenv :: IDEnv
              _selHavingOidenv :: IDEnv
              _selOrderByOidenv :: IDEnv
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
              -- "./TypeChecking/ScalarExprs.ag"(line 456, column 14)
              _selGroupByOexpectedTypes =
                  {-# LINE 456 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 4141 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/QueryStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 4146 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 98, column 10)
              _newLib =
                  {-# LINE 98 "./TypeChecking/QueryStatement.ag" #-}
                  case foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _selTrefIlibUpdates of
                    Left x -> error $ "selectexpression-select-loc.newlib " ++ show x
                    Right e -> e
                  {-# LINE 4153 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 101, column 10)
              _selSelectListOlib =
                  {-# LINE 101 "./TypeChecking/QueryStatement.ag" #-}
                  _newLib
                  {-# LINE 4158 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 102, column 10)
              _selWhereOlib =
                  {-# LINE 102 "./TypeChecking/QueryStatement.ag" #-}
                  _newLib
                  {-# LINE 4163 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 103, column 10)
              _selGroupByOlib =
                  {-# LINE 103 "./TypeChecking/QueryStatement.ag" #-}
                  _newLib
                  {-# LINE 4168 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 104, column 10)
              _selOrderByOlib =
                  {-# LINE 104 "./TypeChecking/QueryStatement.ag" #-}
                  _newLib
                  {-# LINE 4173 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/QueryStatement.ag" #-}
                  _selSelectListIlibUpdates
                  {-# LINE 4178 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 129, column 9)
              _tpe =
                  {-# LINE 129 "./TypeChecking/QueryStatement.ag" #-}
                  Right $ SetOfType $ CompositeType $ fromMaybe [] $ liftList  _selSelectListIlistType
                  {-# LINE 4183 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 131, column 9)
              _backTree =
                  {-# LINE 131 "./TypeChecking/QueryStatement.ag" #-}
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
                  {-# LINE 4197 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 159, column 9)
              _lhsOuType =
                  {-# LINE 159 "./TypeChecking/QueryStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 4202 "AstInternal.hs" #-}
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
                  {-# LINE 4216 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 229, column 14)
              _lhsOcidenv =
                  {-# LINE 229 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _selSelectListIcidenv
                  {-# LINE 4221 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 270, column 14)
              _trefEnv =
                  {-# LINE 270 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  makeIDEnvP _selTrefItrefIDs
                  {-# LINE 4226 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 271, column 14)
              _includeCorrelations =
                  {-# LINE 271 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  joinIDEnvs _lhsIidenv _trefEnv
                  {-# LINE 4231 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 272, column 14)
              _selSelectListOidenv =
                  {-# LINE 272 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _trefEnv
                  {-# LINE 4236 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 273, column 14)
              _selWhereOidenv =
                  {-# LINE 273 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _includeCorrelations
                  {-# LINE 4241 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 274, column 14)
              _selGroupByOidenv =
                  {-# LINE 274 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _trefEnv
                  {-# LINE 4246 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 275, column 14)
              _selHavingOidenv =
                  {-# LINE 275 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _includeCorrelations
                  {-# LINE 4251 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 276, column 14)
              _selOrderByOidenv =
                  {-# LINE 276 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _trefEnv
                  {-# LINE 4256 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Select ann_ selDistinct_ _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
                  {-# LINE 4261 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Select ann_ selDistinct_ _selSelectListIfixedUpIdentifiersTree _selTrefIfixedUpIdentifiersTree _selWhereIfixedUpIdentifiersTree _selGroupByIfixedUpIdentifiersTree _selHavingIfixedUpIdentifiersTree _selOrderByIfixedUpIdentifiersTree _selLimitIfixedUpIdentifiersTree _selOffsetIfixedUpIdentifiersTree
                  {-# LINE 4266 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Select ann_ selDistinct_ _selSelectListIoriginalTree _selTrefIoriginalTree _selWhereIoriginalTree _selGroupByIoriginalTree _selHavingIoriginalTree _selOrderByIoriginalTree _selLimitIoriginalTree _selOffsetIoriginalTree
                  {-# LINE 4271 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4276 "AstInternal.hs" #-}
              -- copy rule (down)
              _selSelectListOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4281 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4286 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4291 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4296 "AstInternal.hs" #-}
              -- copy rule (down)
              _selWhereOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4301 "AstInternal.hs" #-}
              -- copy rule (down)
              _selGroupByOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4306 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4311 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4316 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOrderByOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4321 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4326 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4331 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4336 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4341 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4346 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4351 "AstInternal.hs" #-}
              ( _selSelectListIannotatedTree,_selSelectListIcidenv,_selSelectListIfixedUpIdentifiersTree,_selSelectListIlibUpdates,_selSelectListIlistType,_selSelectListIoriginalTree) =
                  (selSelectList_ _selSelectListOcat _selSelectListOidenv _selSelectListOlib )
              ( _selTrefIannotatedTree,_selTrefIfixedUpIdentifiersTree,_selTrefIlibUpdates,_selTrefIoriginalTree,_selTrefItrefIDs) =
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
         (let _vllOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: QueryExpr
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _lhsOuType :: (Maybe [(String,Type)])
              _lhsOcidenv :: IDEnv
              _lhsOfixedUpIdentifiersTree :: QueryExpr
              _lhsOoriginalTree :: QueryExpr
              _vllOcat :: Catalog
              _vllOidenv :: IDEnv
              _vllOlib :: LocalBindings
              _vllIannotatedTree :: ScalarExprListList
              _vllIfixedUpIdentifiersTree :: ScalarExprListList
              _vllIoriginalTree :: ScalarExprListList
              _vllIuType :: ([[Maybe Type]])
              -- "./TypeChecking/ScalarExprs.ag"(line 470, column 14)
              _vllOexpectedTypes =
                  {-# LINE 470 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 4396 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/QueryStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 4401 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 115, column 9)
              _lhsOlibUpdates =
                  {-# LINE 115 "./TypeChecking/QueryStatement.ag" #-}
                  []
                  {-# LINE 4406 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 124, column 9)
              _tpe =
                  {-# LINE 124 "./TypeChecking/QueryStatement.ag" #-}
                  typeCheckValuesExpr
                              _lhsIcat
                              _vllIuType
                  {-# LINE 4413 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 127, column 9)
              _backTree =
                  {-# LINE 127 "./TypeChecking/QueryStatement.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 4418 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 159, column 9)
              _lhsOuType =
                  {-# LINE 159 "./TypeChecking/QueryStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 4423 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 231, column 14)
              _lhsOcidenv =
                  {-# LINE 231 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  unimplementedIDEnv
                  {-# LINE 4428 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 4433 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Values ann_ _vllIfixedUpIdentifiersTree
                  {-# LINE 4438 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Values ann_ _vllIoriginalTree
                  {-# LINE 4443 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4448 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4453 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4458 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4463 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4468 "AstInternal.hs" #-}
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
         (let _lhsOannotatedTree :: QueryExpr
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _exOcat :: Catalog
              _withsOcatUpdates :: ([CatalogUpdate])
              _lhsOuType :: (Maybe [(String,Type)])
              _lhsOcidenv :: IDEnv
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
              -- "./TypeChecking/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/QueryStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 4510 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 119, column 9)
              _lhsOlibUpdates =
                  {-# LINE 119 "./TypeChecking/QueryStatement.ag" #-}
                  _exIlibUpdates
                  {-# LINE 4515 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 152, column 9)
              _tpe =
                  {-# LINE 152 "./TypeChecking/QueryStatement.ag" #-}
                  lmt ((SetOfType . CompositeType) <$> _exIuType)
                  {-# LINE 4520 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 153, column 9)
              _backTree =
                  {-# LINE 153 "./TypeChecking/QueryStatement.ag" #-}
                  WithSelect ann_ _withsIannotatedTree _exIannotatedTree
                  {-# LINE 4525 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 154, column 9)
              _exOcat =
                  {-# LINE 154 "./TypeChecking/QueryStatement.ag" #-}
                  _withsIproducedCat
                  {-# LINE 4530 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 155, column 9)
              _withsOcatUpdates =
                  {-# LINE 155 "./TypeChecking/QueryStatement.ag" #-}
                  []
                  {-# LINE 4535 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 159, column 9)
              _lhsOuType =
                  {-# LINE 159 "./TypeChecking/QueryStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 4540 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 232, column 18)
              _lhsOcidenv =
                  {-# LINE 232 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _exIcidenv
                  {-# LINE 4545 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  WithSelect ann_ _withsIannotatedTree _exIannotatedTree
                  {-# LINE 4550 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  WithSelect ann_ _withsIfixedUpIdentifiersTree _exIfixedUpIdentifiersTree
                  {-# LINE 4555 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  WithSelect ann_ _withsIoriginalTree _exIoriginalTree
                  {-# LINE 4560 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4565 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4570 "AstInternal.hs" #-}
              -- copy rule (down)
              _withsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4575 "AstInternal.hs" #-}
              -- copy rule (down)
              _withsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4580 "AstInternal.hs" #-}
              -- copy rule (down)
              _withsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4585 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOexpectedTypes =
                  {-# LINE 465 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 4590 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4595 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4600 "AstInternal.hs" #-}
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
                  {-# LINE 4673 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 108, column 12)
              _statementsOlibUpdates =
                  {-# LINE 108 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 4678 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Root _statementsIannotatedTree
                  {-# LINE 4683 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Root _statementsIfixedUpIdentifiersTree
                  {-# LINE 4688 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Root _statementsIoriginalTree
                  {-# LINE 4693 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4698 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4703 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4708 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOproducedCat =
                  {-# LINE 27 "./TypeChecking/Statements.ag" #-}
                  _statementsIproducedCat
                  {-# LINE 4713 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOproducedLib =
                  {-# LINE 28 "./TypeChecking/Statements.ag" #-}
                  _statementsIproducedLib
                  {-# LINE 4718 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4723 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4728 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4733 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  NotNullConstraint ann_ name_
                  {-# LINE 4847 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  NotNullConstraint ann_ name_
                  {-# LINE 4852 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  NotNullConstraint ann_ name_
                  {-# LINE 4857 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4862 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4867 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4872 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  NullConstraint ann_ name_
                  {-# LINE 4888 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  NullConstraint ann_ name_
                  {-# LINE 4893 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  NullConstraint ann_ name_
                  {-# LINE 4898 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4903 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4908 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4913 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 407, column 26)
              _exprOexpectedType =
                  {-# LINE 407 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 4938 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  RowCheckConstraint ann_ name_ _exprIannotatedTree
                  {-# LINE 4943 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  RowCheckConstraint ann_ name_ _exprIfixedUpIdentifiersTree
                  {-# LINE 4948 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  RowCheckConstraint ann_ name_ _exprIoriginalTree
                  {-# LINE 4953 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4958 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 4963 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4968 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4973 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 4978 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4983 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint ann_ name_
                  {-# LINE 5001 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  RowPrimaryKeyConstraint ann_ name_
                  {-# LINE 5006 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint ann_ name_
                  {-# LINE 5011 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5016 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5021 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5026 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
                  {-# LINE 5046 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
                  {-# LINE 5051 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
                  {-# LINE 5056 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5061 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5066 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5071 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  RowUniqueConstraint ann_ name_
                  {-# LINE 5087 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  RowUniqueConstraint ann_ name_
                  {-# LINE 5092 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  RowUniqueConstraint ann_ name_
                  {-# LINE 5097 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5102 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5107 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5112 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5185 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 5190 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 5195 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5200 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5205 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5210 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5215 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 5220 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5225 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5230 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 5235 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5240 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5258 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 5263 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5268 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5273 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5278 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5283 "AstInternal.hs" #-}
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
            local tbUType     : _
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
         (let _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _lhsOtbAnnotatedTree :: SQIdentifier
              _lhsOannotatedTree :: SQIdentifier
              _lhsOfixedUpIdentifiersTree :: SQIdentifier
              _lhsOoriginalTree :: SQIdentifier
              -- "./TypeChecking/Misc.ag"(line 53, column 9)
              _tbUType =
                  {-# LINE 53 "./TypeChecking/Misc.ag" #-}
                  catCompositeAttrsPair _lhsIcat relationComposites (last is_)
                  {-# LINE 5346 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 54, column 9)
              _lhsOtbUType =
                  {-# LINE 54 "./TypeChecking/Misc.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 5351 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 55, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 55 "./TypeChecking/Misc.ag" #-}
                  updateAnnotation
                    (\a -> a {errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 5357 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 58, column 9)
              _backTree =
                  {-# LINE 58 "./TypeChecking/Misc.ag" #-}
                  SQIdentifier ann_ is_
                  {-# LINE 5362 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  SQIdentifier ann_ is_
                  {-# LINE 5367 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SQIdentifier ann_ is_
                  {-# LINE 5372 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  SQIdentifier ann_ is_
                  {-# LINE 5377 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5382 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5387 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5392 "AstInternal.hs" #-}
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
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Exists:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative NullLit:
         child ann            : {Annotation}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Placeholder:
         child ann            : {Annotation}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
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
            local liftedColumnName : _
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 5762 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 5767 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 5772 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 79, column 19)
              _tpe =
                  {-# LINE 79 "./TypeChecking/ScalarExprs.ag" #-}
                  Right typeBool
                  {-# LINE 5777 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 91, column 9)
              _backTree =
                  {-# LINE 91 "./TypeChecking/ScalarExprs.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 5782 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 5787 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 5792 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 5797 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 5802 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5807 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5812 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 5853 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 5858 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 5863 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 202, column 9)
              _whenTypes =
                  {-# LINE 202 "./TypeChecking/ScalarExprs.ag" #-}
                  _casesIwhenTypes
                  {-# LINE 5868 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 203, column 9)
              _thenTypes =
                  {-# LINE 203 "./TypeChecking/ScalarExprs.ag" #-}
                  _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIuType
                  {-# LINE 5873 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 207, column 9)
              _tpe =
                  {-# LINE 207 "./TypeChecking/ScalarExprs.ag" #-}
                  do
                  wt <- mapM lmt $ concat _whenTypes
                  errorWhen (any (/= typeBool) wt)
                      [WrongTypes typeBool wt]
                  tt <- mapM lmt _thenTypes
                  resolveResultSetType _lhsIcat tt
                  {-# LINE 5883 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 215, column 9)
              _backTree =
                  {-# LINE 215 "./TypeChecking/ScalarExprs.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 5888 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 5893 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 5898 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Case ann_ _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
                  {-# LINE 5903 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Case ann_ _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 5908 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 5913 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5918 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5923 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 5928 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5933 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5938 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 5943 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5948 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6002 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6007 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6012 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 202, column 9)
              _whenTypes =
                  {-# LINE 202 "./TypeChecking/ScalarExprs.ag" #-}
                  _casesIwhenTypes
                  {-# LINE 6017 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 203, column 9)
              _thenTypes =
                  {-# LINE 203 "./TypeChecking/ScalarExprs.ag" #-}
                  _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIuType
                  {-# LINE 6022 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 220, column 9)
              _tpe =
                  {-# LINE 220 "./TypeChecking/ScalarExprs.ag" #-}
                  do
                  wt <- mapM lmt $ concat _whenTypes
                  vt <- lmt _valueIuType
                  _ <- resolveResultSetType _lhsIcat (vt : wt)
                  tt <- mapM lmt _thenTypes
                  resolveResultSetType _lhsIcat tt
                  {-# LINE 6032 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 227, column 9)
              _backTree =
                  {-# LINE 227 "./TypeChecking/ScalarExprs.ag" #-}
                  CaseSimple ann_
                             _valueIannotatedTree
                             _casesIannotatedTree
                             _elsIannotatedTree
                  {-# LINE 6040 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 6045 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 6050 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CaseSimple ann_ _valueIfixedUpIdentifiersTree _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
                  {-# LINE 6055 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIoriginalTree _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 6060 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6065 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6070 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6075 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOexpectedType =
                  {-# LINE 371 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 6080 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6085 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6090 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6095 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6100 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6105 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6110 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6115 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6120 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6167 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6172 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6177 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 108, column 12)
              _tpe =
                  {-# LINE 108 "./TypeChecking/ScalarExprs.ag" #-}
                  lmt _tnInamedType
                  {-# LINE 6182 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 109, column 12)
              _backTree =
                  {-# LINE 109 "./TypeChecking/ScalarExprs.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 6187 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 266, column 7)
              _liftedColumnName =
                  {-# LINE 266 "./TypeChecking/ScalarExprs.ag" #-}
                  case _tnIannotatedTree of
                    SimpleTypeName _ tn -> tn
                    _ -> ""
                  {-# LINE 6194 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 6199 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Cast ann_ _exprIfixedUpIdentifiersTree _tnIfixedUpIdentifiersTree
                  {-# LINE 6204 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Cast ann_ _exprIoriginalTree _tnIoriginalTree
                  {-# LINE 6209 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6214 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6219 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6224 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOexpectedType =
                  {-# LINE 371 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 6229 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6234 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6239 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6244 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6249 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6254 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6293 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6298 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6303 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 6308 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 293, column 9)
              _tpe =
                  {-# LINE 293 "./TypeChecking/ScalarExprs.ag" #-}
                  Right typeBool
                  {-# LINE 6313 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 294, column 9)
              _backTree =
                  {-# LINE 294 "./TypeChecking/ScalarExprs.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 6318 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 477, column 29)
              _selOexpectedTypes =
                  {-# LINE 477 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 6323 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 6328 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Exists ann_ _selIfixedUpIdentifiersTree
                  {-# LINE 6333 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Exists ann_ _selIoriginalTree
                  {-# LINE 6338 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6343 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6348 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6353 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6358 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6363 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Extract ann_ field_ _eIannotatedTree
                  {-# LINE 6392 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Extract ann_ field_ _eIfixedUpIdentifiersTree
                  {-# LINE 6397 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Extract ann_ field_ _eIoriginalTree
                  {-# LINE 6402 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6407 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6412 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6417 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOuType =
                  {-# LINE 34 "./TypeChecking/ScalarExprs.ag" #-}
                  _eIuType
                  {-# LINE 6422 "AstInternal.hs" #-}
              -- copy rule (down)
              _eOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6427 "AstInternal.hs" #-}
              -- copy rule (down)
              _eOexpectedType =
                  {-# LINE 371 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 6432 "AstInternal.hs" #-}
              -- copy rule (down)
              _eOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6437 "AstInternal.hs" #-}
              -- copy rule (down)
              _eOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6442 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6469 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6474 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6479 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 78, column 17)
              _tpe =
                  {-# LINE 78 "./TypeChecking/ScalarExprs.ag" #-}
                  Right typeNumeric
                  {-# LINE 6484 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 89, column 9)
              _backTree =
                  {-# LINE 89 "./TypeChecking/ScalarExprs.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 6489 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 6494 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 6499 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 6504 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 6509 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6514 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6519 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6552 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6557 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 122, column 9)
              __tup1 =
                  {-# LINE 122 "./TypeChecking/ScalarExprs.ag" #-}
                  either (\e -> (Left e, Nothing)) id $ do
                  args <- mapM lmt _argsIuType
                  efp <- findCallMatch _lhsIcat
                                       funName_
                                       args
                  let (_,_,r,_) = efp
                  return (Right r, Just efp)
                  {-# LINE 6568 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 122, column 9)
              (_tpe,_) =
                  {-# LINE 122 "./TypeChecking/ScalarExprs.ag" #-}
                  __tup1
                  {-# LINE 6573 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 122, column 9)
              (_,_prototype) =
                  {-# LINE 123 "./TypeChecking/ScalarExprs.ag" #-}
                  __tup1
                  {-# LINE 6578 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 132, column 9)
              _backTree =
                  {-# LINE 132 "./TypeChecking/ScalarExprs.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 6583 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 260, column 7)
              _liftedColumnName =
                  {-# LINE 260 "./TypeChecking/ScalarExprs.ag" #-}
                  case funName_ of
                    "." -> getName _backTree
                    x | isOperatorName x -> "?column?"
                    _ -> funName_
                  {-# LINE 6591 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 436, column 9)
              _argsOexpectedTypes =
                  {-# LINE 436 "./TypeChecking/ScalarExprs.ag" #-}
                  maybe [] id $
                  case (funName_,_lhsIexpectedType) of
                    ("!rowctor", Just (AnonymousRecordType ts)) -> return $ map Just ts
                    _ -> do
                         (_,t,_,_) <- _prototype
                         return $ map Just t
                  {-# LINE 6601 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 6606 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  FunCall ann_ funName_ _argsIfixedUpIdentifiersTree
                  {-# LINE 6611 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIoriginalTree
                  {-# LINE 6616 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6621 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6626 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6631 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6636 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6641 "AstInternal.hs" #-}
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
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6668 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6673 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6678 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 240, column 9)
              _tpe =
                  {-# LINE 240 "./TypeChecking/ScalarExprs.ag" #-}
                  unwrapLookup <$> lbLookupID _lhsIlib [i_]
                  {-# LINE 6683 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 241, column 9)
              _backTree =
                  {-# LINE 241 "./TypeChecking/ScalarExprs.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 6688 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 131, column 9)
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 131 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  case qualifyID _lhsIidenv i_ of
                    Nothing -> Identifier ann_ i_
                    Just (t,i) -> QIdentifier ann_ (Identifier ann_ t) i
                  {-# LINE 6695 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 6700 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 6705 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 6710 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6715 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6757 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6762 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6767 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 6772 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 321, column 9)
              _tpe =
                  {-# LINE 321 "./TypeChecking/ScalarExprs.ag" #-}
                  do
                  lt <- _listIlistType
                  expt <- lmt _exprIuType
                  _ <- resolveResultSetType _lhsIcat [expt, lt]
                  return typeBool
                  {-# LINE 6781 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 326, column 9)
              _backTree =
                  {-# LINE 326 "./TypeChecking/ScalarExprs.ag" #-}
                  InPredicate ann_
                              _exprIannotatedTree
                              i_
                              _listIannotatedTree
                  {-# LINE 6789 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                  {-# LINE 6794 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  InPredicate ann_ _exprIfixedUpIdentifiersTree i_ _listIfixedUpIdentifiersTree
                  {-# LINE 6799 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  InPredicate ann_ _exprIoriginalTree i_ _listIoriginalTree
                  {-# LINE 6804 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6809 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6814 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6819 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOexpectedType =
                  {-# LINE 371 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 6824 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6829 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6834 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6839 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 6844 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6849 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6878 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6883 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6888 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 76, column 19)
              _tpe =
                  {-# LINE 76 "./TypeChecking/ScalarExprs.ag" #-}
                  Right typeInt
                  {-# LINE 6893 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 85, column 9)
              _backTree =
                  {-# LINE 85 "./TypeChecking/ScalarExprs.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 6898 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 6903 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 6908 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 6913 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 6918 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 6923 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6928 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 6955 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 6960 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 6965 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 96, column 16)
              _tpe =
                  {-# LINE 96 "./TypeChecking/ScalarExprs.ag" #-}
                  Right $ ScalarType "interval"
                  {-# LINE 6970 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 97, column 16)
              _backTree =
                  {-# LINE 97 "./TypeChecking/ScalarExprs.ag" #-}
                  Interval ann_ value_ field_ prec_
                  {-# LINE 6975 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 6980 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Interval ann_ value_ field_ prec_
                  {-# LINE 6985 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Interval ann_ value_ field_ prec_
                  {-# LINE 6990 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Interval ann_ value_ field_ prec_
                  {-# LINE 6995 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7000 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7005 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7040 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7045 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7050 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 155, column 9)
              _tpe =
                  {-# LINE 155 "./TypeChecking/ScalarExprs.ag" #-}
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
                  {-# LINE 7068 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 169, column 9)
              _backTree =
                  {-# LINE 169 "./TypeChecking/ScalarExprs.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIannotatedTree
                  {-# LINE 7073 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 7078 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 444, column 9)
              _argsOexpectedTypes =
                  {-# LINE 444 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 7083 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIannotatedTree
                  {-# LINE 7088 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIfixedUpIdentifiersTree
                  {-# LINE 7093 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIoriginalTree
                  {-# LINE 7098 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7103 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7108 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7113 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7118 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7123 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7149 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7154 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7159 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 16)
              _tpe =
                  {-# LINE 81 "./TypeChecking/ScalarExprs.ag" #-}
                  Right UnknownType
                  {-# LINE 7164 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 93, column 9)
              _backTree =
                  {-# LINE 93 "./TypeChecking/ScalarExprs.ag" #-}
                  NullLit ann_
                  {-# LINE 7169 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 7174 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 7179 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  NullLit ann_
                  {-# LINE 7184 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 7189 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7194 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7199 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7223 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7228 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7233 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 7238 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 287, column 9)
              _tpe =
                  {-# LINE 287 "./TypeChecking/ScalarExprs.ag" #-}
                  Right UnknownType
                  {-# LINE 7243 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 288, column 9)
              _backTree =
                  {-# LINE 288 "./TypeChecking/ScalarExprs.ag" #-}
                  Placeholder ann_
                  {-# LINE 7248 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Placeholder ann_
                  {-# LINE 7253 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Placeholder ann_
                  {-# LINE 7258 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Placeholder ann_
                  {-# LINE 7263 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7268 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7273 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7298 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7303 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7308 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 7313 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 281, column 9)
              _tpe =
                  {-# LINE 281 "./TypeChecking/ScalarExprs.ag" #-}
                  unwrapLookup <$> lbLookupID _lhsIlib ['$':show p_]
                  {-# LINE 7318 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 282, column 9)
              _backTree =
                  {-# LINE 282 "./TypeChecking/ScalarExprs.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 7323 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 7328 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 7333 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 7338 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7343 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7348 "AstInternal.hs" #-}
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
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _qid :: (Maybe String)
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _qualOcat :: Catalog
              _qualOexpectedType :: (Maybe Type)
              _qualOidenv :: IDEnv
              _qualOlib :: LocalBindings
              _qualIannotatedTree :: ScalarExpr
              _qualIfixedUpIdentifiersTree :: ScalarExpr
              _qualIoriginalTree :: ScalarExpr
              _qualIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7383 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7388 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7393 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 243, column 9)
              _tpe =
                  {-# LINE 243 "./TypeChecking/ScalarExprs.ag" #-}
                  case _qid     of
                            Nothing -> byT
                            Just q -> either (const byT) Right $ unwrapLookup <$> lbLookupID _lhsIlib [q,i_]
                  where
                    byT = do
                      (t::Type) <- lmt _qualIuType
                      unwrapLookup <$> lbLookupIDInType _lhsIcat _lhsIlib t i_
                  {-# LINE 7404 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 251, column 9)
              _qid =
                  {-# LINE 251 "./TypeChecking/ScalarExprs.ag" #-}
                  case _backTree     of
                     QIdentifier _ (Identifier _ q) _ -> Just q
                     _ -> Nothing
                  {-# LINE 7411 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 254, column 9)
              _backTree =
                  {-# LINE 254 "./TypeChecking/ScalarExprs.ag" #-}
                  QIdentifier ann_ _qAnnTreeNoUnrec     i_
                  {-# LINE 7416 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 256, column 9)
              _qAnnTreeNoUnrec =
                  {-# LINE 256 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation (\a -> a {errs = []}) _qualIannotatedTree
                  {-# LINE 7421 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 137, column 9)
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 137 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  QIdentifier ann_ _qualIoriginalTree i_
                  {-# LINE 7426 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  QIdentifier ann_ _qualIannotatedTree i_
                  {-# LINE 7431 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  QIdentifier ann_ _qualIfixedUpIdentifiersTree i_
                  {-# LINE 7436 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  QIdentifier ann_ _qualIoriginalTree i_
                  {-# LINE 7441 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7446 "AstInternal.hs" #-}
              -- copy rule (down)
              _qualOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7451 "AstInternal.hs" #-}
              -- copy rule (down)
              _qualOexpectedType =
                  {-# LINE 371 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 7456 "AstInternal.hs" #-}
              -- copy rule (down)
              _qualOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7461 "AstInternal.hs" #-}
              -- copy rule (down)
              _qualOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7466 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7503 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7508 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7513 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 7518 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 305, column 9)
              _tpe =
                  {-# LINE 305 "./TypeChecking/ScalarExprs.ag" #-}
                  do
                  selType <- lmt (map snd <$> _selIuType)
                  case length selType of
                    0 -> Left [InternalError "no columns in scalar subquery?"]
                    1 -> Right $ head selType
                    _ -> Right $ AnonymousRecordType selType
                  {-# LINE 7528 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 313, column 9)
              _backTree =
                  {-# LINE 313 "./TypeChecking/ScalarExprs.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 7533 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 477, column 29)
              _selOexpectedTypes =
                  {-# LINE 477 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 7538 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 7543 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ScalarSubQuery ann_ _selIfixedUpIdentifiersTree
                  {-# LINE 7548 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIoriginalTree
                  {-# LINE 7553 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7558 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7563 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7568 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7573 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7578 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7605 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7610 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7615 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 18)
              _tpe =
                  {-# LINE 77 "./TypeChecking/ScalarExprs.ag" #-}
                  Right UnknownType
                  {-# LINE 7620 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 87, column 9)
              _backTree =
                  {-# LINE 87 "./TypeChecking/ScalarExprs.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 7625 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 7630 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 7635 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 7640 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 7645 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7650 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7655 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7688 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7693 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7698 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 113, column 10)
              _tpe =
                  {-# LINE 113 "./TypeChecking/ScalarExprs.ag" #-}
                  lmt _tnInamedType
                  {-# LINE 7703 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 114, column 10)
              _backTree =
                  {-# LINE 114 "./TypeChecking/ScalarExprs.ag" #-}
                  TypedStringLit ann_ _tnIannotatedTree value_
                  {-# LINE 7708 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 277, column 7)
              _liftedColumnName =
                  {-# LINE 277 "./TypeChecking/ScalarExprs.ag" #-}
                  ""
                  {-# LINE 7713 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  TypedStringLit ann_ _tnIannotatedTree value_
                  {-# LINE 7718 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  TypedStringLit ann_ _tnIfixedUpIdentifiersTree value_
                  {-# LINE 7723 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  TypedStringLit ann_ _tnIoriginalTree value_
                  {-# LINE 7728 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7733 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7738 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7743 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7748 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7753 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 18, column 9)
              _lhsOannotatedTree =
                  {-# LINE 18 "./TypeChecking/ScalarExprs.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 7808 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 32, column 9)
              _prototype =
                  {-# LINE 32 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7813 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOuType =
                  {-# LINE 40 "./TypeChecking/ScalarExprs.ag" #-}
                  etmt _tpe
                  {-# LINE 7818 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 136, column 9)
              _tpe =
                  {-# LINE 136 "./TypeChecking/ScalarExprs.ag" #-}
                  lmt _fnIuType
                  {-# LINE 7823 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 137, column 9)
              _backTree =
                  {-# LINE 137 "./TypeChecking/ScalarExprs.ag" #-}
                  WindowFn ann_
                           _fnIannotatedTree
                           _partitionByIannotatedTree
                           _orderByIannotatedTree
                           dir_
                           frm_
                  {-# LINE 7833 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 270, column 7)
              _liftedColumnName =
                  {-# LINE 270 "./TypeChecking/ScalarExprs.ag" #-}
                  let (FunCall _ fn _) = _fnIannotatedTree
                  in fn
                  {-# LINE 7839 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 446, column 9)
              _partitionByOexpectedTypes =
                  {-# LINE 446 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 7844 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 447, column 9)
              _orderByOexpectedTypes =
                  {-# LINE 447 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 7849 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree dir_ frm_
                  {-# LINE 7854 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  WindowFn ann_ _fnIfixedUpIdentifiersTree _partitionByIfixedUpIdentifiersTree _orderByIfixedUpIdentifiersTree dir_ frm_
                  {-# LINE 7859 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  WindowFn ann_ _fnIoriginalTree _partitionByIoriginalTree _orderByIoriginalTree dir_ frm_
                  {-# LINE 7864 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 7869 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7874 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7879 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOexpectedType =
                  {-# LINE 371 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 7884 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7889 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7894 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7899 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7904 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7909 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7914 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 7919 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7924 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 387, column 13)
              _x1OexpectedType =
                  {-# LINE 387 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 7994 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,x2_)
                  {-# LINE 7999 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (_x1IfixedUpIdentifiersTree,x2_)
                  {-# LINE 8004 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,x2_)
                  {-# LINE 8009 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8014 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8019 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8024 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8029 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8034 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8039 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8114 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 8119 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 8124 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8129 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8134 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8139 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8144 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8149 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8154 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8159 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8164 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8169 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8187 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 8192 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8197 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8202 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8207 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8212 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 43, column 12)
              _lhsOuType =
                  {-# LINE 43 "./TypeChecking/ScalarExprs.ag" #-}
                  _hdIuType : _tlIuType
                  {-# LINE 8294 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 390, column 12)
              _hdOexpectedType =
                  {-# LINE 390 "./TypeChecking/ScalarExprs.ag" #-}
                  case _lhsIexpectedTypes of
                    (t:_) -> t
                    _ -> Nothing
                  {-# LINE 8301 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 393, column 12)
              _tlOexpectedTypes =
                  {-# LINE 393 "./TypeChecking/ScalarExprs.ag" #-}
                  case _lhsIexpectedTypes of
                  (_:ts) -> ts
                  _ -> []
                  {-# LINE 8308 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8313 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 8318 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 8323 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8328 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8333 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8338 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8343 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8348 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8353 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8358 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8363 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8368 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 44, column 11)
              _lhsOuType =
                  {-# LINE 44 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 8388 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8393 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 8398 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8403 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8408 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8413 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8418 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 50, column 12)
              _lhsOuType =
                  {-# LINE 50 "./TypeChecking/ScalarExprs.ag" #-}
                  _hdIuType : _tlIuType
                  {-# LINE 8500 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 473, column 12)
              _hdOexpectedTypes =
                  {-# LINE 473 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 8505 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 474, column 12)
              _tlOexpectedTypes =
                  {-# LINE 474 "./TypeChecking/ScalarExprs.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 8510 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8515 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 8520 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 8525 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8530 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8535 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8540 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8545 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8550 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8555 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8560 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8565 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8570 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 51, column 11)
              _lhsOuType =
                  {-# LINE 51 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 8590 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8595 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 8600 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8605 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8610 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8615 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8620 "AstInternal.hs" #-}
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
         (let _x1OexpectedTypes :: ([Maybe Type])
              _x2OcatUpdates :: ([CatalogUpdate])
              _x2OlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 450, column 13)
              _x1OexpectedTypes =
                  {-# LINE 450 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 8694 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 121, column 9)
              _x2OcatUpdates =
                  {-# LINE 121 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 8699 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 122, column 9)
              _x2OlibUpdates =
                  {-# LINE 122 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 8704 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 8709 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (_x1IfixedUpIdentifiersTree,_x2IfixedUpIdentifiersTree)
                  {-# LINE 8714 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 8719 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8724 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8729 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8734 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8739 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8744 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8749 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8754 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8759 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8764 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8841 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 8846 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 8851 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8856 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8861 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8866 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8871 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8876 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8881 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8886 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 8891 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8896 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8914 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 8919 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8924 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8929 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 8934 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8939 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 429, column 22)
              _exprOexpectedType =
                  {-# LINE 429 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 9002 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ScalarExprRoot _exprIannotatedTree
                  {-# LINE 9007 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ScalarExprRoot _exprIfixedUpIdentifiersTree
                  {-# LINE 9012 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ScalarExprRoot _exprIoriginalTree
                  {-# LINE 9017 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9022 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9027 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9032 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9037 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9042 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9047 "AstInternal.hs" #-}
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
         (let _x1OexpectedType :: (Maybe Type)
              _x2OcatUpdates :: ([CatalogUpdate])
              _x2OlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 432, column 13)
              _x1OexpectedType =
                  {-# LINE 432 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 9123 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 125, column 9)
              _x2OcatUpdates =
                  {-# LINE 125 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9128 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 126, column 9)
              _x2OlibUpdates =
                  {-# LINE 126 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 9133 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 9138 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (_x1IfixedUpIdentifiersTree,_x2IfixedUpIdentifiersTree)
                  {-# LINE 9143 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 9148 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9153 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9158 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9163 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9168 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9173 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9178 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9183 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9188 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9193 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 9270 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 9275 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 9280 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9285 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9290 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9295 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9300 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9305 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9310 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9315 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9320 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9325 "AstInternal.hs" #-}
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
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 9343 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 9348 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 9353 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9358 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9363 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9368 "AstInternal.hs" #-}
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
         (let _exOexpectedType :: (Maybe Type)
              _lhsOitemType :: ((String,Maybe Type))
              _lhsOseIdTree :: ([SelectItem])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 414, column 25)
              _exOexpectedType =
                  {-# LINE 414 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 9448 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 32, column 9)
              _annotatedTree =
                  {-# LINE 32 "./TypeChecking/SelectLists.ag" #-}
                  SelExp ann_ _exIannotatedTree
                  {-# LINE 9453 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 60, column 9)
              _lhsOitemType =
                  {-# LINE 60 "./TypeChecking/SelectLists.ag" #-}
                  ("", Nothing)
                  {-# LINE 9458 "AstInternal.hs" #-}
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
                  {-# LINE 9473 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SelExp ann_ _exIfixedUpIdentifiersTree
                  {-# LINE 9478 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  SelExp ann_ _exIoriginalTree
                  {-# LINE 9483 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9488 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9493 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9498 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9503 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9508 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9513 "AstInternal.hs" #-}
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
         (let _exOexpectedType :: (Maybe Type)
              _lhsOitemType :: ((String,Maybe Type))
              _lhsOseIdTree :: ([SelectItem])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 414, column 25)
              _exOexpectedType =
                  {-# LINE 414 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 9542 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 34, column 9)
              _annotatedTree =
                  {-# LINE 34 "./TypeChecking/SelectLists.ag" #-}
                  SelectItem ann_ _exIannotatedTree name_
                  {-# LINE 9547 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 62, column 9)
              _lhsOitemType =
                  {-# LINE 62 "./TypeChecking/SelectLists.ag" #-}
                  (name_, unwrapSetof `fmap` _exIuType)
                  {-# LINE 9552 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 163, column 18)
              _lhsOseIdTree =
                  {-# LINE 163 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  [SelectItem ann_ _exIfixedUpIdentifiersTree name_]
                  {-# LINE 9557 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SelectItem ann_ _exIfixedUpIdentifiersTree name_
                  {-# LINE 9562 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  SelectItem ann_ _exIoriginalTree name_
                  {-# LINE 9567 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9572 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9577 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9582 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9587 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9592 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9597 "AstInternal.hs" #-}
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
         (let _lhsOlistType :: ([(String,Maybe Type)])
              _lhsOfixedUpIdentifiersTree :: SelectItemList
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
              -- "./TypeChecking/SelectLists.ag"(line 42, column 12)
              _lhsOlistType =
                  {-# LINE 42 "./TypeChecking/SelectLists.ag" #-}
                  _hdIitemType : _tlIlistType
                  {-# LINE 9677 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 141, column 12)
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 141 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _hdIseIdTree ++ _tlIfixedUpIdentifiersTree
                  {-# LINE 9682 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 9687 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 9692 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 9697 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9702 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9707 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9712 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9717 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9722 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9727 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9732 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9737 "AstInternal.hs" #-}
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
         (let _lhsOlistType :: ([(String,Maybe Type)])
              _lhsOfixedUpIdentifiersTree :: SelectItemList
              _lhsOannotatedTree :: SelectItemList
              _lhsOoriginalTree :: SelectItemList
              -- "./TypeChecking/SelectLists.ag"(line 43, column 11)
              _lhsOlistType =
                  {-# LINE 43 "./TypeChecking/SelectLists.ag" #-}
                  []
                  {-# LINE 9756 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 142, column 11)
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 142 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 9761 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 9766 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 9771 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 9776 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9781 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9786 "AstInternal.hs" #-}
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
         (let _intoOexpectedTypes :: ([Maybe Type])
              _lhsOlistType :: ([(String,Maybe Type)])
              _intoFroms :: (E ([(String,Type)],[(String,Type)]))
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: SelectList
              _lhsOcidenv :: IDEnv
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
              -- "./TypeChecking/ScalarExprs.ag"(line 410, column 18)
              _intoOexpectedTypes =
                  {-# LINE 410 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 9869 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 77, column 9)
              _lhsOlistType =
                  {-# LINE 77 "./TypeChecking/SelectLists.ag" #-}
                  _itemsIlistType
                  {-# LINE 9874 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 79, column 9)
              _intoFroms =
                  {-# LINE 79 "./TypeChecking/SelectLists.ag" #-}
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
                  {-# LINE 9888 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 93, column 9)
              _tpe =
                  {-# LINE 93 "./TypeChecking/SelectLists.ag" #-}
                  returnWhen (_intoIoriginalTree == []) () $ do
                  (it,ft) <- _intoFroms
                  checkAssignmentsValid _lhsIcat (map snd ft) (map snd it)
                  {-# LINE 9895 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 98, column 9)
              _lhsOlibUpdates =
                  {-# LINE 98 "./TypeChecking/SelectLists.ag" #-}
                  maybe [] id $ do
                  _ <- etmt _tpe
                  (it,ft) <- etmt _intoFroms
                  return $ case it of
                    [(n,PgRecord _)] -> [LBIds "set record actual fields from select into"
                                               Nothing
                                               [(n,PgRecord $ Just $ CompositeType ft)]]
                    _ -> []
                  {-# LINE 9907 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 131, column 9)
              _lhsOannotatedTree =
                  {-# LINE 131 "./TypeChecking/SelectLists.ag" #-}
                  addTypeErrors (tes _tpe    ) $
                  SelectList ann_
                             _itemsIannotatedTree
                             _intoIannotatedTree
                  {-# LINE 9915 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 124, column 18)
              _lhsOcidenv =
                  {-# LINE 124 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  makeIDEnv "" $ flip map _itemsIfixedUpIdentifiersTree
                                   $ \(SelectItem _ _ n) -> n
                  {-# LINE 9921 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  SelectList ann_ _itemsIannotatedTree _intoIannotatedTree
                  {-# LINE 9926 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SelectList ann_ _itemsIfixedUpIdentifiersTree _intoIfixedUpIdentifiersTree
                  {-# LINE 9931 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  SelectList ann_ _itemsIoriginalTree _intoIoriginalTree
                  {-# LINE 9936 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 9941 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9946 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9951 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9956 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9961 "AstInternal.hs" #-}
              -- copy rule (down)
              _intoOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9966 "AstInternal.hs" #-}
              -- copy rule (down)
              _intoOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 9971 "AstInternal.hs" #-}
              -- copy rule (down)
              _intoOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9976 "AstInternal.hs" #-}
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
                  {-# LINE 10612 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10617 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10622 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  AlterSequence ann_ name_ _ownedByIannotatedTree
                  {-# LINE 10627 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  AlterSequence ann_ name_ _ownedByIfixedUpIdentifiersTree
                  {-# LINE 10632 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  AlterSequence ann_ name_ _ownedByIoriginalTree
                  {-# LINE 10637 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10642 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 10647 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10652 "AstInternal.hs" #-}
              -- copy rule (down)
              _ownedByOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10657 "AstInternal.hs" #-}
              -- copy rule (down)
              _ownedByOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10662 "AstInternal.hs" #-}
              -- copy rule (down)
              _ownedByOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10667 "AstInternal.hs" #-}
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
                  {-# LINE 10695 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10700 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  AlterTable ann_ name_ _actionsIannotatedTree
                  {-# LINE 10705 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  AlterTable ann_ name_ _actionsIfixedUpIdentifiersTree
                  {-# LINE 10710 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  AlterTable ann_ name_ _actionsIoriginalTree
                  {-# LINE 10715 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10720 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 10725 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10730 "AstInternal.hs" #-}
              -- copy rule (down)
              _actionsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10735 "AstInternal.hs" #-}
              -- copy rule (down)
              _actionsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10740 "AstInternal.hs" #-}
              -- copy rule (down)
              _actionsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10745 "AstInternal.hs" #-}
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
         (let _valueOexpectedType :: (Maybe Type)
              _targetOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
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
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 18)
              _valueOexpectedType =
                  {-# LINE 417 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 10786 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 423, column 18)
              _targetOexpectedType =
                  {-# LINE 423 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 10791 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 10799 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 10804 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10809 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10814 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 20, column 9)
              _tpe =
                  {-# LINE 20 "./TypeChecking/Plpgsql.ag" #-}
                  do
                  fromType <- lmt _valueIuType
                  toType <- lmt _targetIuType
                  checkAssignmentValid _lhsIcat fromType toType
                  return $ Pseudo Void
                  {-# LINE 10823 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 26, column 9)
              _backTree =
                  {-# LINE 26 "./TypeChecking/Plpgsql.ag" #-}
                  Assignment ann_ _targetIannotatedTree _valueIannotatedTree
                  {-# LINE 10828 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 27, column 9)
              _catUpdates =
                  {-# LINE 27 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 10833 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 28, column 9)
              _statementType =
                  {-# LINE 28 "./TypeChecking/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 10838 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Assignment ann_ _targetIannotatedTree _valueIannotatedTree
                  {-# LINE 10843 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Assignment ann_ _targetIfixedUpIdentifiersTree _valueIfixedUpIdentifiersTree
                  {-# LINE 10848 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Assignment ann_ _targetIoriginalTree _valueIoriginalTree
                  {-# LINE 10853 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 10858 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10863 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10868 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10873 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10878 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10883 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 10888 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10893 "AstInternal.hs" #-}
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
                  {-# LINE 10935 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 100, column 13)
              _lhsOcatUpdates =
                  {-# LINE 100 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10940 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 101, column 13)
              _stsOcatUpdates =
                  {-# LINE 101 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10945 "AstInternal.hs" #-}
              -- "./TypeChecking/Block.ag"(line 22, column 9)
              _stsOlib =
                  {-# LINE 22 "./TypeChecking/Block.ag" #-}
                  fromRight _lhsIlib $
                  lbUpdate _lhsIcat
                           (LBIds "declarations" lb_ $ mapMaybe lv _varsIdefs)
                           _lhsIlib
                  where
                    lv (_,Nothing) = Nothing
                    lv (s,Just t) = Just (s,t)
                  {-# LINE 10956 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Block ann_ lb_ _varsIannotatedTree _stsIannotatedTree
                  {-# LINE 10961 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Block ann_ lb_ _varsIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
                  {-# LINE 10966 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Block ann_ lb_ _varsIoriginalTree _stsIoriginalTree
                  {-# LINE 10971 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10976 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 10981 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10986 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOlibUpdates =
                  {-# LINE 19 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10991 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10996 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11001 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11006 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11011 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11016 "AstInternal.hs" #-}
              -- copy rule (from local)
              _stsOlibUpdates =
                  {-# LINE 23 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11021 "AstInternal.hs" #-}
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
                  {-# LINE 11061 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11066 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  {-# LINE 134 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11071 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  {-# LINE 135 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11076 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatement ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 11081 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CaseStatement ann_ _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
                  {-# LINE 11086 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatement ann_ _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 11091 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11096 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11101 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11106 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11111 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11116 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11121 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11126 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11131 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11136 "AstInternal.hs" #-}
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
         (let _valOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _elsOcatUpdates :: ([CatalogUpdate])
              _elsOlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 418, column 27)
              _valOexpectedType =
                  {-# LINE 418 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 11185 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  {-# LINE 134 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11200 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  {-# LINE 135 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11205 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatementSimple ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 11210 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CaseStatementSimple ann_ _valIfixedUpIdentifiersTree _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
                  {-# LINE 11215 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatementSimple ann_ _valIoriginalTree _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 11220 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11225 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11230 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11235 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11240 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11245 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11250 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11255 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11260 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11265 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11270 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11275 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11280 "AstInternal.hs" #-}
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
                  {-# LINE 11305 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11310 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ContinueStatement ann_ lb_
                  {-# LINE 11315 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ContinueStatement ann_ lb_
                  {-# LINE 11320 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ContinueStatement ann_ lb_
                  {-# LINE 11325 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11330 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11335 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11340 "AstInternal.hs" #-}
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
                  {-# LINE 11361 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11366 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Copy ann_ table_ targetCols_ source_
                  {-# LINE 11371 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Copy ann_ table_ targetCols_ source_
                  {-# LINE 11376 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Copy ann_ table_ targetCols_ source_
                  {-# LINE 11381 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11386 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11391 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11396 "AstInternal.hs" #-}
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
                  {-# LINE 11415 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11420 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 11425 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 11430 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 11435 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11440 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11445 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11450 "AstInternal.hs" #-}
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
                  {-# LINE 11491 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11496 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11501 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11506 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 65, column 9)
              _tpe =
                  {-# LINE 65 "./TypeChecking/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11511 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 66, column 9)
              _backTree =
                  {-# LINE 66 "./TypeChecking/MiscCreates.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree checkName_ _checkIannotatedTree
                  {-# LINE 11516 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 67, column 9)
              _statementType =
                  {-# LINE 67 "./TypeChecking/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 11521 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 68, column 9)
              _catUpdates =
                  {-# LINE 68 "./TypeChecking/MiscCreates.ag" #-}
                  maybe [] (\t -> [CatCreateDomain (DomainType name_) t]) _typInamedType
                  {-# LINE 11526 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 70, column 9)
              _checkOlib =
                  {-# LINE 70 "./TypeChecking/MiscCreates.ag" #-}
                  either (const _lhsIlib) id $ do
                  nt <- lmt _typInamedType
                  lbUpdate _lhsIcat
                    (LBIds "domain check value" Nothing [("value", nt)])
                    _lhsIlib
                  {-# LINE 11535 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree checkName_ _checkIannotatedTree
                  {-# LINE 11540 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateDomain ann_ name_ _typIfixedUpIdentifiersTree checkName_ _checkIfixedUpIdentifiersTree
                  {-# LINE 11545 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIoriginalTree checkName_ _checkIoriginalTree
                  {-# LINE 11550 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11555 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11560 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11565 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11570 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11575 "AstInternal.hs" #-}
              -- copy rule (down)
              _checkOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11580 "AstInternal.hs" #-}
              -- copy rule (down)
              _checkOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11585 "AstInternal.hs" #-}
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
                  {-# LINE 11641 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11646 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11651 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11656 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 63, column 9)
              _bodyOlib =
                  {-# LINE 63 "./TypeChecking/CreateFunction.ag" #-}
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
                  {-# LINE 11676 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 79, column 9)
              _paramsOpos =
                  {-# LINE 79 "./TypeChecking/CreateFunction.ag" #-}
                  1
                  {-# LINE 11681 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 88, column 9)
              _tpe =
                  {-# LINE 88 "./TypeChecking/CreateFunction.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11686 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 89, column 9)
              _catUpdates =
                  {-# LINE 89 "./TypeChecking/CreateFunction.ag" #-}
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
                  {-# LINE 11701 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 101, column 9)
              _backTree =
                  {-# LINE 101 "./TypeChecking/CreateFunction.ag" #-}
                  CreateFunction ann_
                                 name_
                                 _paramsIannotatedTree
                                 _rettypeIannotatedTree
                                 rep_
                                 lang_
                                 _bodyIannotatedTree
                                 vol_
                  {-# LINE 11713 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 109, column 9)
              _statementType =
                  {-# LINE 109 "./TypeChecking/CreateFunction.ag" #-}
                  Nothing
                  {-# LINE 11718 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 110, column 9)
              _bodyOcat =
                  {-# LINE 110 "./TypeChecking/CreateFunction.ag" #-}
                  _lhsIinProducedCat
                  {-# LINE 11723 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CreateFunction ann_ name_ _paramsIannotatedTree _rettypeIannotatedTree rep_ lang_ _bodyIannotatedTree vol_
                  {-# LINE 11728 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateFunction ann_ name_ _paramsIfixedUpIdentifiersTree _rettypeIfixedUpIdentifiersTree rep_ lang_ _bodyIfixedUpIdentifiersTree vol_
                  {-# LINE 11733 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CreateFunction ann_ name_ _paramsIoriginalTree _rettypeIoriginalTree rep_ lang_ _bodyIoriginalTree vol_
                  {-# LINE 11738 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11743 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11748 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11753 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11758 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11763 "AstInternal.hs" #-}
              -- copy rule (down)
              _rettypeOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11768 "AstInternal.hs" #-}
              -- copy rule (down)
              _rettypeOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11773 "AstInternal.hs" #-}
              -- copy rule (down)
              _rettypeOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11778 "AstInternal.hs" #-}
              -- copy rule (down)
              _bodyOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 11783 "AstInternal.hs" #-}
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
                  {-# LINE 11814 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11819 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11824 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11829 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 78, column 9)
              _tpe =
                  {-# LINE 78 "./TypeChecking/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11834 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 79, column 9)
              _backTree =
                  {-# LINE 79 "./TypeChecking/MiscCreates.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11839 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 80, column 9)
              _statementType =
                  {-# LINE 80 "./TypeChecking/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 11844 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 81, column 9)
              _catUpdates =
                  {-# LINE 81 "./TypeChecking/MiscCreates.ag" #-}
                  [CatCreateFunction FunName "plpgsql_call_handler" [] (Pseudo LanguageHandler) False
                  ,CatCreateFunction FunName "plpgsql_validator" [ScalarType "oid"] (Pseudo Void) False]
                  {-# LINE 11850 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11855 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11860 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11865 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11870 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11875 "AstInternal.hs" #-}
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
                  {-# LINE 11899 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11904 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11909 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
                  {-# LINE 11914 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
                  {-# LINE 11919 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
                  {-# LINE 11924 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11929 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 11934 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11939 "AstInternal.hs" #-}
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
                  {-# LINE 11980 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11985 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11990 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11995 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 31, column 9)
              _tpe =
                  {-# LINE 31 "./TypeChecking/CreateTable.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12000 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 32, column 9)
              _catUpdates =
                  {-# LINE 32 "./TypeChecking/CreateTable.ag" #-}
                  [CatCreateTable name_ _attrs     defaultSystemColumns]
                  {-# LINE 12005 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 35, column 9)
              _attrs =
                  {-# LINE 35 "./TypeChecking/CreateTable.ag" #-}
                  mapMaybe okAt _attsIattrs
                  where
                    okAt (s, Just t) = Just (s,t)
                    okAt (_,Nothing) = Nothing
                  {-# LINE 12013 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 40, column 9)
              _statementType =
                  {-# LINE 40 "./TypeChecking/CreateTable.ag" #-}
                  Nothing
                  {-# LINE 12018 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 41, column 9)
              _backTree =
                  {-# LINE 41 "./TypeChecking/CreateTable.ag" #-}
                  CreateTable ann_
                              name_
                              _attsIannotatedTree
                              _consIannotatedTree
                  {-# LINE 12026 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 45, column 9)
              _consOlib =
                  {-# LINE 45 "./TypeChecking/CreateTable.ag" #-}
                  case lbUpdate _lhsIcat
                         (LBIds "attributedefs" Nothing _attrs    )
                         _lhsIlib of
                     Left x -> error $ "statement-createtable-cons.lib " ++ show x
                     Right e -> e
                  {-# LINE 12035 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
                  {-# LINE 12040 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateTable ann_ name_ _attsIfixedUpIdentifiersTree _consIfixedUpIdentifiersTree
                  {-# LINE 12045 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIoriginalTree _consIoriginalTree
                  {-# LINE 12050 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12055 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12060 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12065 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12070 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12075 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12080 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12085 "AstInternal.hs" #-}
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
         (let _exprOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _attrs :: (Either [TypeError] [(String,Type)])
              _statementType :: (Maybe StatementType)
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
              -- "./TypeChecking/ScalarExprs.ag"(line 481, column 32)
              _exprOexpectedTypes =
                  {-# LINE 481 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 12123 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 12131 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12136 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12141 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12146 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 64, column 9)
              _tpe =
                  {-# LINE 64 "./TypeChecking/CreateTable.ag" #-}
                  CompositeType <$> lmt _exprIuType
                  {-# LINE 12151 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 65, column 9)
              _catUpdates =
                  {-# LINE 65 "./TypeChecking/CreateTable.ag" #-}
                  either (const []) id $ do
                  ats <- _attrs
                  return [CatCreateTable name_ ats defaultSystemColumns]
                  {-# LINE 12158 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 71, column 9)
              _attrs =
                  {-# LINE 71 "./TypeChecking/CreateTable.ag" #-}
                  lmt _exprIuType
                  {-# LINE 12163 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 73, column 9)
              _backTree =
                  {-# LINE 73 "./TypeChecking/CreateTable.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 12168 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 74, column 9)
              _statementType =
                  {-# LINE 74 "./TypeChecking/CreateTable.ag" #-}
                  Nothing
                  {-# LINE 12173 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 12178 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateTableAs ann_ name_ _exprIfixedUpIdentifiersTree
                  {-# LINE 12183 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIoriginalTree
                  {-# LINE 12188 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12193 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12198 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12203 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12208 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12213 "AstInternal.hs" #-}
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
         (let _fnArgsOexpectedTypes :: ([Maybe Type])
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 459, column 21)
              _fnArgsOexpectedTypes =
                  {-# LINE 459 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 12248 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12253 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12258 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIannotatedTree
                  {-# LINE 12263 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIfixedUpIdentifiersTree
                  {-# LINE 12268 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIoriginalTree
                  {-# LINE 12273 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12278 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12283 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12288 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnArgsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12293 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnArgsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12298 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnArgsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12303 "AstInternal.hs" #-}
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
                  {-# LINE 12338 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12343 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12348 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12353 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 48, column 9)
              _tpe =
                  {-# LINE 48 "./TypeChecking/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12358 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 49, column 9)
              _attrs =
                  {-# LINE 49 "./TypeChecking/MiscCreates.ag" #-}
                  mapMaybe okAt _attsIattrs
                  where
                    okAt (s, Just t) = Just (s,t)
                    okAt (_,Nothing) = Nothing
                  {-# LINE 12366 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 53, column 9)
              _backTree =
                  {-# LINE 53 "./TypeChecking/MiscCreates.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 12371 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 54, column 9)
              _statementType =
                  {-# LINE 54 "./TypeChecking/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 12376 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 55, column 9)
              _catUpdates =
                  {-# LINE 55 "./TypeChecking/MiscCreates.ag" #-}
                  [CatCreateComposite name_ _attrs    ]
                  {-# LINE 12381 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 12386 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateType ann_ name_ _attsIfixedUpIdentifiersTree
                  {-# LINE 12391 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIoriginalTree
                  {-# LINE 12396 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12401 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12406 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12411 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12416 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12421 "AstInternal.hs" #-}
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
         (let _exprOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
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
              -- "./TypeChecking/ScalarExprs.ag"(line 481, column 32)
              _exprOexpectedTypes =
                  {-# LINE 481 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 12457 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 12465 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12470 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12475 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12480 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 15, column 9)
              _tpe =
                  {-# LINE 15 "./TypeChecking/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12485 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 16, column 9)
              _backTree =
                  {-# LINE 16 "./TypeChecking/MiscCreates.ag" #-}
                  CreateView ann_ name_ colNames_ _exprIannotatedTree
                  {-# LINE 12490 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 17, column 9)
              _catUpdates =
                  {-# LINE 17 "./TypeChecking/MiscCreates.ag" #-}
                  maybe [] (\a -> [CatCreateView name_ a]) _exprIuType
                  {-# LINE 12495 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 19, column 9)
              _statementType =
                  {-# LINE 19 "./TypeChecking/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 12500 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  CreateView ann_ name_ colNames_ _exprIannotatedTree
                  {-# LINE 12505 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  CreateView ann_ name_ colNames_ _exprIfixedUpIdentifiersTree
                  {-# LINE 12510 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  CreateView ann_ name_ colNames_ _exprIoriginalTree
                  {-# LINE 12515 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12520 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12525 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12530 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12535 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12540 "AstInternal.hs" #-}
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
                  {-# LINE 12599 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12604 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12609 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12614 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 13, column 9)
              _tpe =
                  {-# LINE 13 "./TypeChecking/Delete.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12619 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 14, column 9)
              _statementType =
                  {-# LINE 14 "./TypeChecking/Delete.ag" #-}
                  do
                  pt <- sequence $ getPlaceholderTypes _whrIannotatedTree
                  lt <- liftList _returningIlistType
                  return (pt,lt)
                  {-# LINE 12627 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 19, column 9)
              _backTree =
                  {-# LINE 19 "./TypeChecking/Delete.ag" #-}
                  Delete ann_ _tableItbAnnotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
                  {-# LINE 12632 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 20, column 9)
              _catUpdates =
                  {-# LINE 20 "./TypeChecking/Delete.ag" #-}
                  []
                  {-# LINE 12637 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 22, column 9)
              _lib =
                  {-# LINE 22 "./TypeChecking/Delete.ag" #-}
                  either (const _lhsIlib) id $ do
                  a <- lmt (allAtts <$> _tableItbUType)
                  lbUpdate _lhsIcat (LBIds "delete table attrs" (Just $ getTName _tableIannotatedTree) a) _lhsIlib
                  {-# LINE 12644 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 26, column 9)
              _whrOlib =
                  {-# LINE 26 "./TypeChecking/Delete.ag" #-}
                  _lib
                  {-# LINE 12649 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 27, column 9)
              _returningOlib =
                  {-# LINE 27 "./TypeChecking/Delete.ag" #-}
                  _lib
                  {-# LINE 12654 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Delete ann_ _tableIannotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
                  {-# LINE 12659 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Delete ann_ _tableIfixedUpIdentifiersTree _usingIfixedUpIdentifiersTree _whrIfixedUpIdentifiersTree _returningIfixedUpIdentifiersTree
                  {-# LINE 12664 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Delete ann_ _tableIoriginalTree _usingIoriginalTree _whrIoriginalTree _returningIoriginalTree
                  {-# LINE 12669 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12674 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12679 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12684 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12689 "AstInternal.hs" #-}
              -- copy rule (from local)
              _tableOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 12694 "AstInternal.hs" #-}
              -- copy rule (down)
              _usingOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12699 "AstInternal.hs" #-}
              -- copy rule (down)
              _usingOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12704 "AstInternal.hs" #-}
              -- copy rule (from local)
              _usingOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 12709 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12714 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12719 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12724 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12729 "AstInternal.hs" #-}
              ( _tableIannotatedTree,_tableIfixedUpIdentifiersTree,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType) =
                  (table_ _tableOcat _tableOidenv _tableOlib )
              ( _usingIannotatedTree,_usingIfixedUpIdentifiersTree,_usingIlibUpdates,_usingIoriginalTree,_usingItrefIDs) =
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
                  {-# LINE 12771 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12776 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12781 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12786 "AstInternal.hs" #-}
              -- "./TypeChecking/Drops.ag"(line 10, column 9)
              _tpe =
                  {-# LINE 10 "./TypeChecking/Drops.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12791 "AstInternal.hs" #-}
              -- "./TypeChecking/Drops.ag"(line 11, column 9)
              _backTree =
                  {-# LINE 11 "./TypeChecking/Drops.ag" #-}
                  DropFunction ann_ ifE_ _sigsIannotatedTree cascade_
                  {-# LINE 12796 "AstInternal.hs" #-}
              -- "./TypeChecking/Drops.ag"(line 12, column 9)
              _catUpdates =
                  {-# LINE 12 "./TypeChecking/Drops.ag" #-}
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
                  {-# LINE 12810 "AstInternal.hs" #-}
              -- "./TypeChecking/Drops.ag"(line 23, column 9)
              _statementType =
                  {-# LINE 23 "./TypeChecking/Drops.ag" #-}
                  Nothing
                  {-# LINE 12815 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  DropFunction ann_ ifE_ _sigsIannotatedTree cascade_
                  {-# LINE 12820 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  DropFunction ann_ ifE_ _sigsIfixedUpIdentifiersTree cascade_
                  {-# LINE 12825 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  DropFunction ann_ ifE_ _sigsIoriginalTree cascade_
                  {-# LINE 12830 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12835 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12840 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12845 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12850 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12855 "AstInternal.hs" #-}
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
                  {-# LINE 12879 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12884 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
                  {-# LINE 12889 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
                  {-# LINE 12894 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
                  {-# LINE 12899 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12904 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12909 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12914 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Execute :: Annotation ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 420, column 9)
              _exprOexpectedType =
                  {-# LINE 420 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 12941 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12946 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12951 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Execute ann_ _exprIannotatedTree
                  {-# LINE 12956 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Execute ann_ _exprIfixedUpIdentifiersTree
                  {-# LINE 12961 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Execute ann_ _exprIoriginalTree
                  {-# LINE 12966 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12971 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 12976 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12981 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12986 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 12991 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12996 "AstInternal.hs" #-}
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
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 420, column 9)
              _exprOexpectedType =
                  {-# LINE 420 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 13026 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13031 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13036 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIannotatedTree targets_
                  {-# LINE 13041 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ExecuteInto ann_ _exprIfixedUpIdentifiersTree targets_
                  {-# LINE 13046 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIoriginalTree targets_
                  {-# LINE 13051 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13056 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13061 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13066 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13071 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13076 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13081 "AstInternal.hs" #-}
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
                  {-# LINE 13102 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13107 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ExitStatement ann_ lb_
                  {-# LINE 13112 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ExitStatement ann_ lb_
                  {-# LINE 13117 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ExitStatement ann_ lb_
                  {-# LINE 13122 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13127 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13132 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13137 "AstInternal.hs" #-}
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
         (let _fromOexpectedType :: (Maybe Type)
              _toOexpectedType :: (Maybe Type)
              _varOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _stsOlib :: LocalBindings
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
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
              -- "./TypeChecking/ScalarExprs.ag"(line 421, column 27)
              _fromOexpectedType =
                  {-# LINE 421 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 13197 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 422, column 27)
              _toOexpectedType =
                  {-# LINE 422 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 13202 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 424, column 45)
              _varOexpectedType =
                  {-# LINE 424 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 13207 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 13215 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 13220 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 13225 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13230 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13235 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13240 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 32, column 9)
              _tpe =
                  {-# LINE 32 "./TypeChecking/Plpgsql.ag" #-}
                  do
                  fromType <- lmt _fromIuType
                  toType <- lmt _toIuType
                  errorWhen (fromType /= toType) [FromToTypesNotSame fromType toType]
                  case _varIuType of
                    Just t -> checkAssignmentValid _lhsIcat fromType t
                    Nothing -> return ()
                  return $ Pseudo Void
                  {-# LINE 13252 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 41, column 9)
              _implicitVar =
                  {-# LINE 41 "./TypeChecking/Plpgsql.ag" #-}
                  case _varIannotatedTree of
                      Identifier a i | errs a == [UnrecognisedIdentifier i] -> True
                      _ -> False
                  {-# LINE 13259 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 44, column 9)
              _stsOlib =
                  {-# LINE 44 "./TypeChecking/Plpgsql.ag" #-}
                  if _implicitVar
                  then either (const _lhsIlib) id $ do
                       ft <- lmt _fromIuType
                       lbUpdate _lhsIcat
                          (LBIds "local for loop variable" Nothing [((getName _varIannotatedTree),ft)]) _lhsIlib
                  else _lhsIlib
                  {-# LINE 13269 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 52, column 9)
              _backTree =
                  {-# LINE 52 "./TypeChecking/Plpgsql.ag" #-}
                  let i = if _implicitVar
                          then let (Identifier a i') = _varIannotatedTree
                               in Identifier a { errs = []} i'
                          else _varIannotatedTree
                  in ForIntegerStatement ann_ lb_ i _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 13278 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 58, column 9)
              _catUpdates =
                  {-# LINE 58 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 13283 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 59, column 9)
              _statementType =
                  {-# LINE 59 "./TypeChecking/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 13288 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ForIntegerStatement ann_ lb_ _varIannotatedTree _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 13293 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ForIntegerStatement ann_ lb_ _varIfixedUpIdentifiersTree _fromIfixedUpIdentifiersTree _toIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
                  {-# LINE 13298 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ForIntegerStatement ann_ lb_ _varIoriginalTree _fromIoriginalTree _toIoriginalTree _stsIoriginalTree
                  {-# LINE 13303 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13308 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13313 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13318 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13323 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13328 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13333 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13338 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13343 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13348 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13353 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13358 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13363 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13368 "AstInternal.hs" #-}
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
         (let _varOexpectedType :: (Maybe Type)
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _stsOlib :: LocalBindings
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
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
              -- "./TypeChecking/ScalarExprs.ag"(line 424, column 45)
              _varOexpectedType =
                  {-# LINE 424 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 13429 "AstInternal.hs" #-}
              -- "./TypeChecking/ScalarExprs.ag"(line 483, column 9)
              _selOexpectedTypes =
                  {-# LINE 483 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 13434 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 13442 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 13447 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 13452 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13457 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13462 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13467 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 64, column 9)
              _tpe =
                  {-# LINE 64 "./TypeChecking/Plpgsql.ag" #-}
                  do
                  st <- lmt (CompositeType <$> _selIuType)
                  toType <- lmt _varIuType
                  checkAssignmentValid _lhsIcat st toType
                  return $ Pseudo Void
                  {-# LINE 13476 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 74, column 9)
              _stsOlib =
                  {-# LINE 74 "./TypeChecking/Plpgsql.ag" #-}
                  either (const _lhsIlib) id $ do
                  _ <- _tpe
                  st <- lmt (CompositeType <$> _selIuType)
                  lbUpdate _lhsIcat (LBIds "for loop record type" Nothing [(getName _varIannotatedTree,st)]) _lhsIlib
                  {-# LINE 13484 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 80, column 9)
              _backTree =
                  {-# LINE 80 "./TypeChecking/Plpgsql.ag" #-}
                  ForQueryStatement ann_ lb_ _varIannotatedTree _selIannotatedTree _stsIannotatedTree
                  {-# LINE 13489 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 81, column 9)
              _catUpdates =
                  {-# LINE 81 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 13494 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 82, column 9)
              _statementType =
                  {-# LINE 82 "./TypeChecking/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 13499 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ForQueryStatement ann_ lb_ _varIannotatedTree _selIannotatedTree _stsIannotatedTree
                  {-# LINE 13504 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ForQueryStatement ann_ lb_ _varIfixedUpIdentifiersTree _selIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
                  {-# LINE 13509 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ForQueryStatement ann_ lb_ _varIoriginalTree _selIoriginalTree _stsIoriginalTree
                  {-# LINE 13514 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13519 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13524 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13529 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13534 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13539 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13544 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13549 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13554 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13559 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13564 "AstInternal.hs" #-}
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
                  {-# LINE 13606 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13611 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  {-# LINE 134 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13616 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  {-# LINE 135 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13621 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  If ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 13626 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  If ann_ _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
                  {-# LINE 13631 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  If ann_ _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 13636 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13641 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13646 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13651 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13656 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13661 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13666 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13671 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13676 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13681 "AstInternal.hs" #-}
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
                  {-# LINE 13739 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 13744 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 13749 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13754 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 14, column 9)
              _tpe =
                  {-# LINE 14 "./TypeChecking/Insert.ag" #-}
                  either Left (const $ Right $ Pseudo Void) _columnTypes
                  {-# LINE 13759 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 15, column 9)
              _statementType =
                  {-# LINE 15 "./TypeChecking/Insert.ag" #-}
                  Just (catMaybes $ getPlaceholderTypes _insDataIannotatedTree
                       ,fromMaybe [] $ liftList _returningIlistType)
                  {-# LINE 13765 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 20, column 9)
              _columnTypes =
                  {-# LINE 20 "./TypeChecking/Insert.ag" #-}
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
                  {-# LINE 13783 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 36, column 9)
              _backTree =
                  {-# LINE 36 "./TypeChecking/Insert.ag" #-}
                  Insert ann_ _tableItbAnnotatedTree
                         targetCols_
                         _insDataIannotatedTree
                         _returningIannotatedTree
                  {-# LINE 13791 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 40, column 9)
              _catUpdates =
                  {-# LINE 40 "./TypeChecking/Insert.ag" #-}
                  []
                  {-# LINE 13796 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 41, column 9)
              _insDataOexpectedTypes =
                  {-# LINE 41 "./TypeChecking/Insert.ag" #-}
                  maybe [] id $ do
                  ts <- etmt $ _columnTypes
                  return $ map (Just . snd) ts
                  {-# LINE 13803 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 45, column 9)
              _returningOlib =
                  {-# LINE 45 "./TypeChecking/Insert.ag" #-}
                  either (const _lhsIlib) id $ do
                    atts <- lmt (allAtts <$> _tableItbUType)
                    lbUpdate _lhsIcat (LBIds "insert target table" (Just $ getTName _tableIannotatedTree) atts) _lhsIlib
                  {-# LINE 13810 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Insert ann_ _tableIannotatedTree targetCols_ _insDataIannotatedTree _returningIannotatedTree
                  {-# LINE 13815 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Insert ann_ _tableIfixedUpIdentifiersTree targetCols_ _insDataIfixedUpIdentifiersTree _returningIfixedUpIdentifiersTree
                  {-# LINE 13820 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Insert ann_ _tableIoriginalTree targetCols_ _insDataIoriginalTree _returningIoriginalTree
                  {-# LINE 13825 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13830 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13835 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13840 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13845 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13850 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13855 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13860 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13865 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13870 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13875 "AstInternal.hs" #-}
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
                  {-# LINE 13911 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13916 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13921 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13926 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  LoopStatement ann_ lb_ _stsIannotatedTree
                  {-# LINE 13931 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  LoopStatement ann_ lb_ _stsIfixedUpIdentifiersTree
                  {-# LINE 13936 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  LoopStatement ann_ lb_ _stsIoriginalTree
                  {-# LINE 13941 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13946 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 13951 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13956 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13961 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 13966 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13971 "AstInternal.hs" #-}
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
                  {-# LINE 13992 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13997 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14002 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Notify ann_ name_
                  {-# LINE 14007 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Notify ann_ name_
                  {-# LINE 14012 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Notify ann_ name_
                  {-# LINE 14017 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14022 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14027 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14032 "AstInternal.hs" #-}
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
                  {-# LINE 14050 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14055 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 14060 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  NullStatement ann_
                  {-# LINE 14065 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 14070 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14075 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14080 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14085 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Perform :: Annotation ->
                         T_ScalarExpr  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 420, column 9)
              _exprOexpectedType =
                  {-# LINE 420 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 14112 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14117 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14122 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Perform ann_ _exprIannotatedTree
                  {-# LINE 14127 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Perform ann_ _exprIfixedUpIdentifiersTree
                  {-# LINE 14132 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Perform ann_ _exprIoriginalTree
                  {-# LINE 14137 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14142 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14147 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14152 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14157 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14162 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14167 "AstInternal.hs" #-}
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
         (let _exOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 484, column 22)
              _exOexpectedTypes =
                  {-# LINE 484 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 14201 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 14209 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 14214 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 14219 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 14, column 9)
              _tpe =
                  {-# LINE 14 "./TypeChecking/QueryStatement.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 14224 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 15, column 9)
              _statementType =
                  {-# LINE 15 "./TypeChecking/QueryStatement.ag" #-}
                  do
                  pt <- sequence $ getPlaceholderTypes _exIannotatedTree
                  st <- _exIuType
                  return (pt
                         ,case st of
                            [(_,(Pseudo Void))] -> []
                            t -> t)
                  {-# LINE 14235 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 23, column 9)
              _backTree =
                  {-# LINE 23 "./TypeChecking/QueryStatement.ag" #-}
                  QueryStatement ann_ _exIannotatedTree
                  {-# LINE 14240 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 24, column 9)
              _catUpdates =
                  {-# LINE 24 "./TypeChecking/QueryStatement.ag" #-}
                  []
                  {-# LINE 14245 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 111, column 9)
              _libUpdates =
                  {-# LINE 111 "./TypeChecking/QueryStatement.ag" #-}
                  _exIlibUpdates
                  {-# LINE 14250 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  QueryStatement ann_ _exIannotatedTree
                  {-# LINE 14255 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  QueryStatement ann_ _exIfixedUpIdentifiersTree
                  {-# LINE 14260 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  QueryStatement ann_ _exIoriginalTree
                  {-# LINE 14265 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14270 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14275 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14280 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14285 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14290 "AstInternal.hs" #-}
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
         (let _argsOexpectedTypes :: ([Maybe Type])
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 460, column 13)
              _argsOexpectedTypes =
                  {-# LINE 460 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 14321 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14326 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14331 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Raise ann_ level_ message_ _argsIannotatedTree
                  {-# LINE 14336 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Raise ann_ level_ message_ _argsIfixedUpIdentifiersTree
                  {-# LINE 14341 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Raise ann_ level_ message_ _argsIoriginalTree
                  {-# LINE 14346 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14351 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14356 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14361 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14366 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14371 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14376 "AstInternal.hs" #-}
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
                  {-# LINE 14410 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 14415 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 14420 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14425 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 12, column 9)
              _tpe =
                  {-# LINE 12 "./TypeChecking/Plpgsql.ag" #-}
                  maybe (Right $ Pseudo Void) Right _valueIuType
                  {-# LINE 14430 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 13, column 9)
              _backTree =
                  {-# LINE 13 "./TypeChecking/Plpgsql.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 14435 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 14, column 9)
              _catUpdates =
                  {-# LINE 14 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 14440 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 15, column 9)
              _statementType =
                  {-# LINE 15 "./TypeChecking/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 14445 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 14450 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Return ann_ _valueIfixedUpIdentifiersTree
                  {-# LINE 14455 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Return ann_ _valueIoriginalTree
                  {-# LINE 14460 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14465 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14470 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14475 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14480 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14485 "AstInternal.hs" #-}
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
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 420, column 9)
              _exprOexpectedType =
                  {-# LINE 420 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 14514 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14519 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14524 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIannotatedTree
                  {-# LINE 14529 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ReturnNext ann_ _exprIfixedUpIdentifiersTree
                  {-# LINE 14534 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIoriginalTree
                  {-# LINE 14539 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14544 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14549 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14554 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14559 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14564 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14569 "AstInternal.hs" #-}
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
         (let _selOexpectedTypes :: ([Maybe Type])
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 483, column 9)
              _selOexpectedTypes =
                  {-# LINE 483 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 14600 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14605 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14610 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIannotatedTree
                  {-# LINE 14615 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ReturnQuery ann_ _selIfixedUpIdentifiersTree
                  {-# LINE 14620 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIoriginalTree
                  {-# LINE 14625 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14630 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14635 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14640 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14645 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14650 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14655 "AstInternal.hs" #-}
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
                  {-# LINE 14677 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14682 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14687 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Set ann_ name_ values_
                  {-# LINE 14692 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Set ann_ name_ values_
                  {-# LINE 14697 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Set ann_ name_ values_
                  {-# LINE 14702 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14707 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14712 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14717 "AstInternal.hs" #-}
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
                  {-# LINE 14738 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14743 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Truncate ann_ tables_ restartIdentity_ cascade_
                  {-# LINE 14748 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Truncate ann_ tables_ restartIdentity_ cascade_
                  {-# LINE 14753 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Truncate ann_ tables_ restartIdentity_ cascade_
                  {-# LINE 14758 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14763 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14768 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14773 "AstInternal.hs" #-}
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
         (let _assignsOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _whrOlib :: LocalBindings
              _assignsOlib :: LocalBindings
              _returningOlib :: LocalBindings
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
              _fromListIoriginalTree :: TableRefList
              _fromListItrefIDs :: ([(String,[String])])
              _whrIannotatedTree :: MaybeBoolExpr
              _whrIfixedUpIdentifiersTree :: MaybeBoolExpr
              _whrIoriginalTree :: MaybeBoolExpr
              _returningIannotatedTree :: MaybeSelectList
              _returningIfixedUpIdentifiersTree :: MaybeSelectList
              _returningIlistType :: ([(String,Maybe Type)])
              _returningIoriginalTree :: MaybeSelectList
              -- "./TypeChecking/ScalarExprs.ag"(line 461, column 14)
              _assignsOexpectedTypes =
                  {-# LINE 461 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 14836 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 14844 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 14849 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 14854 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14859 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 13, column 9)
              _tpe =
                  {-# LINE 13 "./TypeChecking/Update.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 14864 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 18, column 9)
              _statementType =
                  {-# LINE 18 "./TypeChecking/Update.ag" #-}
                  do
                  pt <- sequence $ getPlaceholderTypes _assignsIannotatedTree
                                   ++ getPlaceholderTypes _whrIannotatedTree
                  return (pt,fromMaybe [] $ liftList _returningIlistType)
                  {-# LINE 14872 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 24, column 9)
              _backTree =
                  {-# LINE 24 "./TypeChecking/Update.ag" #-}
                  Update ann_
                         _tableItbAnnotatedTree
                         _assignsIannotatedTree
                         _fromListIannotatedTree
                         _whrIannotatedTree
                         _returningIannotatedTree
                  {-# LINE 14882 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 30, column 9)
              _catUpdates =
                  {-# LINE 30 "./TypeChecking/Update.ag" #-}
                  []
                  {-# LINE 14887 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 35, column 9)
              _lib =
                  {-# LINE 35 "./TypeChecking/Update.ag" #-}
                  either (const _lhsIlib) id $ do
                  a <- lmt (allAtts <$> _tableItbUType)
                  lbUpdate _lhsIcat (LBIds "updated table attrs" (Just $ getTName _tableIannotatedTree) a) _lhsIlib
                  {-# LINE 14894 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 39, column 9)
              _whrOlib =
                  {-# LINE 39 "./TypeChecking/Update.ag" #-}
                  _lib
                  {-# LINE 14899 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 40, column 9)
              _assignsOlib =
                  {-# LINE 40 "./TypeChecking/Update.ag" #-}
                  _lib
                  {-# LINE 14904 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 41, column 9)
              _returningOlib =
                  {-# LINE 41 "./TypeChecking/Update.ag" #-}
                  _lib
                  {-# LINE 14909 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Update ann_ _tableIannotatedTree _assignsIannotatedTree _fromListIannotatedTree _whrIannotatedTree _returningIannotatedTree
                  {-# LINE 14914 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Update ann_ _tableIfixedUpIdentifiersTree _assignsIfixedUpIdentifiersTree _fromListIfixedUpIdentifiersTree _whrIfixedUpIdentifiersTree _returningIfixedUpIdentifiersTree
                  {-# LINE 14919 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Update ann_ _tableIoriginalTree _assignsIoriginalTree _fromListIoriginalTree _whrIoriginalTree _returningIoriginalTree
                  {-# LINE 14924 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 14929 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14934 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14939 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14944 "AstInternal.hs" #-}
              -- copy rule (from local)
              _tableOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 14949 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14954 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14959 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromListOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14964 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromListOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14969 "AstInternal.hs" #-}
              -- copy rule (from local)
              _fromListOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 14974 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14979 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14984 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14989 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 14994 "AstInternal.hs" #-}
              ( _tableIannotatedTree,_tableIfixedUpIdentifiersTree,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType) =
                  (table_ _tableOcat _tableOidenv _tableOlib )
              ( _assignsIannotatedTree,_assignsIfixedUpIdentifiersTree,_assignsIoriginalTree,_assignsIuType) =
                  (assigns_ _assignsOcat _assignsOexpectedTypes _assignsOidenv _assignsOlib )
              ( _fromListIannotatedTree,_fromListIfixedUpIdentifiersTree,_fromListIlibUpdates,_fromListIoriginalTree,_fromListItrefIDs) =
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
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
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
              -- "./TypeChecking/ScalarExprs.ag"(line 420, column 9)
              _exprOexpectedType =
                  {-# LINE 420 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 15043 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 15048 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 15053 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 15058 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 15063 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  WhileStatement ann_ lb_ _exprIannotatedTree _stsIannotatedTree
                  {-# LINE 15068 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  WhileStatement ann_ lb_ _exprIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
                  {-# LINE 15073 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  WhileStatement ann_ lb_ _exprIoriginalTree _stsIoriginalTree
                  {-# LINE 15078 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15083 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15088 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15093 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15098 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15103 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15108 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15113 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15118 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15123 "AstInternal.hs" #-}
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
                  {-# LINE 15221 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 57, column 9)
              _newLib =
                  {-# LINE 57 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIlib $ foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _lhsIlibUpdates
                  {-# LINE 15226 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 59, column 9)
              _hdOcat =
                  {-# LINE 59 "./TypeChecking/Statements.ag" #-}
                  _newCat
                  {-# LINE 15231 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _tlOcat =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  _newCat
                  {-# LINE 15236 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 61, column 9)
              _hdOlib =
                  {-# LINE 61 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 15241 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 62, column 9)
              _tlOlib =
                  {-# LINE 62 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 15246 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOproducedCat =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedCat
                  {-# LINE 15251 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 67, column 9)
              _lhsOproducedLib =
                  {-# LINE 67 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedLib
                  {-# LINE 15256 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _tlOcatUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  _hdIcatUpdates
                  {-# LINE 15261 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 71, column 9)
              _tlOlibUpdates =
                  {-# LINE 71 "./TypeChecking/Statements.ag" #-}
                  _hdIlibUpdates
                  {-# LINE 15266 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 97, column 12)
              _hdOinProducedCat =
                  {-# LINE 97 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedCat
                  {-# LINE 15271 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 15276 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 15281 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 15286 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15291 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15296 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15301 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15306 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15311 "AstInternal.hs" #-}
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
                  {-# LINE 15333 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 57, column 9)
              _newLib =
                  {-# LINE 57 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIlib $ foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _lhsIlibUpdates
                  {-# LINE 15338 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 73, column 9)
              _lhsOproducedCat =
                  {-# LINE 73 "./TypeChecking/Statements.ag" #-}
                  _newCat
                  {-# LINE 15343 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 74, column 9)
              _lhsOproducedLib =
                  {-# LINE 74 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 15348 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15353 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 15358 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15363 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15368 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15373 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15378 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Drops.ag"(line 32, column 13)
              _lhsOfnSig =
                  {-# LINE 32 "./TypeChecking/Drops.ag" #-}
                  (x1_, _x2InamedTypes)
                  {-# LINE 15443 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (x1_,_x2IannotatedTree)
                  {-# LINE 15448 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (x1_,_x2IfixedUpIdentifiersTree)
                  {-# LINE 15453 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (x1_,_x2IoriginalTree)
                  {-# LINE 15458 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15463 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15468 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15473 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15478 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15483 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15488 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Drops.ag"(line 27, column 12)
              _lhsOfnSigs =
                  {-# LINE 27 "./TypeChecking/Drops.ag" #-}
                  _hdIfnSig : _tlIfnSigs
                  {-# LINE 15567 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 15572 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 15577 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 15582 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15587 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15592 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15597 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15602 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15607 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15612 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15617 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15622 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15627 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Drops.ag"(line 28, column 11)
              _lhsOfnSigs =
                  {-# LINE 28 "./TypeChecking/Drops.ag" #-}
                  []
                  {-# LINE 15646 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15651 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 15656 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15661 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15666 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 15671 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15676 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSigs,_lhsOoriginalTree)))
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
         originalTree         : SELF 
         trefIDs              : [(String,[String])]
   alternatives:
      alternative FunTref:
         child ann            : {Annotation}
         child fn             : ScalarExpr 
         child alias          : {TableAlias}
         visit 0:
            local errs        : _
            local eqfunIdens  : {Either [TypeError] (String,[(String,Type)])}
            local qfunIdens   : _
            local backTree    : _
            local _tup2       : {([(String,[String])],TableRef)}
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
         child alias          : {TableAlias}
         visit 0:
            local errs        : _
            local joinErrors  : _
            local libUpdates  : _
            local newLib      : {Either [TypeError] LocalBindings}
            local backTree    : _
            local _tup3       : {([(String,[String])],TableRef)}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative SubTref:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         child alias          : {TableAlias}
         visit 0:
            local errs        : _
            local selectAttrs : {Either [TypeError] [(String,Type)]}
            local backTree    : _
            local _tup4       : {([(String,[String])],TableRef)}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local originalTree : _
      alternative Tref:
         child ann            : {Annotation}
         child tbl            : SQIdentifier 
         child alias          : {TableAlias}
         visit 0:
            local errs        : _
            local backTree    : _
            local _tup5       : {([(String,[String])],TableRef)}
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
    (sem_TableRef_FunTref _ann (sem_ScalarExpr _fn ) _alias )
sem_TableRef (JoinTref _ann _tbl _nat _joinType _tbl1 _onExpr _alias )  =
    (sem_TableRef_JoinTref _ann (sem_TableRef _tbl ) _nat _joinType (sem_TableRef _tbl1 ) (sem_OnExpr _onExpr ) _alias )
sem_TableRef (SubTref _ann _sel _alias )  =
    (sem_TableRef_SubTref _ann (sem_QueryExpr _sel ) _alias )
sem_TableRef (Tref _ann _tbl _alias )  =
    (sem_TableRef_Tref _ann (sem_SQIdentifier _tbl ) _alias )
-- semantic domain
type T_TableRef  = Catalog ->
                   IDEnv ->
                   LocalBindings ->
                   ( TableRef,TableRef,([LocalBindingsUpdate]),TableRef,([(String,[String])]))
data Inh_TableRef  = Inh_TableRef {cat_Inh_TableRef :: Catalog,idenv_Inh_TableRef :: IDEnv,lib_Inh_TableRef :: LocalBindings}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef,fixedUpIdentifiersTree_Syn_TableRef :: TableRef,libUpdates_Syn_TableRef :: [LocalBindingsUpdate],originalTree_Syn_TableRef :: TableRef,trefIDs_Syn_TableRef :: [(String,[String])]}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOtrefIDs) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TableRef _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOoriginalTree _lhsOtrefIDs ))
sem_TableRef_FunTref :: Annotation ->
                        T_ScalarExpr  ->
                        TableAlias ->
                        T_TableRef 
sem_TableRef_FunTref ann_ fn_ alias_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _fnOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: TableRef
              _eqfunIdens :: (Either [TypeError] (String,[(String,Type)]))
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              __tup2 :: (([(String,[String])],TableRef))
              _lhsOtrefIDs :: ([(String,[String])])
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOoriginalTree :: TableRef
              _fnOcat :: Catalog
              _fnOidenv :: IDEnv
              _fnOlib :: LocalBindings
              _fnIannotatedTree :: ScalarExpr
              _fnIfixedUpIdentifiersTree :: ScalarExpr
              _fnIoriginalTree :: ScalarExpr
              _fnIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 426, column 15)
              _fnOexpectedType =
                  {-# LINE 426 "./TypeChecking/ScalarExprs.ag" #-}
                  Nothing
                  {-# LINE 15804 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  {-# LINE 91 "./TypeChecking/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 15809 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 164, column 9)
              _errs =
                  {-# LINE 164 "./TypeChecking/TableRefs.ag" #-}
                  case _eqfunIdens of
                    Left e -> e
                    Right _ -> []
                  {-# LINE 15816 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 170, column 9)
              _eqfunIdens =
                  {-# LINE 170 "./TypeChecking/TableRefs.ag" #-}
                  funIdens _lhsIcat (getAlias "" alias_) _fnIannotatedTree _fnIuType
                  {-# LINE 15821 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 171, column 9)
              _lhsOlibUpdates =
                  {-# LINE 171 "./TypeChecking/TableRefs.ag" #-}
                  [LBTref "fn"
                                  (fst _qfunIdens    )
                                  (snd _qfunIdens    )
                                  []]
                  {-# LINE 15829 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 175, column 9)
              _qfunIdens =
                  {-# LINE 175 "./TypeChecking/TableRefs.ag" #-}
                  fromRight ("",[]) _eqfunIdens
                  {-# LINE 15834 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 263, column 9)
              _backTree =
                  {-# LINE 263 "./TypeChecking/TableRefs.ag" #-}
                  FunTref ann_ _fnIannotatedTree alias_
                  {-# LINE 15839 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 372, column 15)
              __tup2 =
                  {-# LINE 372 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  let (FunCall _ f _) = _fnIfixedUpIdentifiersTree
                      (trs,al) = doAlias alias_ [(f,[f])]
                  in (trs,FunTref ann_ _fnIfixedUpIdentifiersTree al)
                  {-# LINE 15846 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 372, column 15)
              (_lhsOtrefIDs,_) =
                  {-# LINE 372 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup2
                  {-# LINE 15851 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 372, column 15)
              (_,_lhsOfixedUpIdentifiersTree) =
                  {-# LINE 372 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup2
                  {-# LINE 15856 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  FunTref ann_ _fnIannotatedTree alias_
                  {-# LINE 15861 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  FunTref ann_ _fnIfixedUpIdentifiersTree alias_
                  {-# LINE 15866 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  FunTref ann_ _fnIoriginalTree alias_
                  {-# LINE 15871 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15876 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15881 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 15886 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15891 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIfixedUpIdentifiersTree,_fnIoriginalTree,_fnIuType) =
                  (fn_ _fnOcat _fnOexpectedType _fnOidenv _fnOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOtrefIDs)))
sem_TableRef_JoinTref :: Annotation ->
                         T_TableRef  ->
                         Natural ->
                         JoinType ->
                         T_TableRef  ->
                         T_OnExpr  ->
                         TableAlias ->
                         T_TableRef 
sem_TableRef_JoinTref ann_ tbl_ nat_ joinType_ tbl1_ onExpr_ alias_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _newLib :: (Either [TypeError] LocalBindings)
              _onExprOlib :: LocalBindings
              __tup3 :: (([(String,[String])],TableRef))
              _lhsOtrefIDs :: ([(String,[String])])
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOoriginalTree :: TableRef
              _tblOcat :: Catalog
              _tblOidenv :: IDEnv
              _tblOlib :: LocalBindings
              _tbl1Ocat :: Catalog
              _tbl1Oidenv :: IDEnv
              _tbl1Olib :: LocalBindings
              _onExprOcat :: Catalog
              _onExprOidenv :: IDEnv
              _tblIannotatedTree :: TableRef
              _tblIfixedUpIdentifiersTree :: TableRef
              _tblIlibUpdates :: ([LocalBindingsUpdate])
              _tblIoriginalTree :: TableRef
              _tblItrefIDs :: ([(String,[String])])
              _tbl1IannotatedTree :: TableRef
              _tbl1IfixedUpIdentifiersTree :: TableRef
              _tbl1IlibUpdates :: ([LocalBindingsUpdate])
              _tbl1IoriginalTree :: TableRef
              _tbl1ItrefIDs :: ([(String,[String])])
              _onExprIannotatedTree :: OnExpr
              _onExprIfixedUpIdentifiersTree :: OnExpr
              _onExprIoriginalTree :: OnExpr
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  {-# LINE 91 "./TypeChecking/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 15940 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 180, column 9)
              _errs =
                  {-# LINE 180 "./TypeChecking/TableRefs.ag" #-}
                  fromLeft [] _newLib
                  ++ _joinErrors
                  {-# LINE 15946 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 182, column 9)
              _lhsOlibUpdates =
                  {-# LINE 182 "./TypeChecking/TableRefs.ag" #-}
                  if _joinErrors     == []
                  then _libUpdates
                  else []
                  {-# LINE 15953 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 187, column 9)
              _joinErrors =
                  {-# LINE 187 "./TypeChecking/TableRefs.ag" #-}
                  fromLeft [] (foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _libUpdates    )
                  {-# LINE 15958 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 188, column 9)
              _libUpdates =
                  {-# LINE 188 "./TypeChecking/TableRefs.ag" #-}
                  case (_tblIlibUpdates, _tbl1IlibUpdates) of
                    ([u1], [u2]) -> [LBJoinTref "join" u1 u2 jids
                                                    (case alias_ of
                                                             NoAlias -> Nothing
                                                             TableAlias t -> Just t
                                                             FullAlias t _ -> Just t)]
                    _ -> []
                  where
                    jids = case (nat_, _onExprIoriginalTree) of
                                (Natural, _) -> Left ()
                                (_,Just (JoinUsing _ s)) -> Right s
                                _ -> Right []
                  {-# LINE 15974 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 202, column 9)
              _newLib =
                  {-# LINE 202 "./TypeChecking/TableRefs.ag" #-}
                  case (_tblIlibUpdates, _tbl1IlibUpdates) of
                    ([u1],[u2]) -> lbUpdate _lhsIcat
                                     (LBJoinTref "join" u1 u2 (Right []) Nothing) _lhsIlib
                    _ -> Right _lhsIlib
                  {-# LINE 15982 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 206, column 9)
              _onExprOlib =
                  {-# LINE 206 "./TypeChecking/TableRefs.ag" #-}
                  fromRight _lhsIlib _newLib
                  {-# LINE 15987 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 265, column 9)
              _backTree =
                  {-# LINE 265 "./TypeChecking/TableRefs.ag" #-}
                  JoinTref ann_
                             _tblIannotatedTree
                             nat_
                             joinType_
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
                             alias_
                  {-# LINE 15998 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 377, column 16)
              __tup3 =
                  {-# LINE 377 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  let (trs,al) = doAlias alias_ $ _tblItrefIDs ++ _tbl1ItrefIDs
                  in (trs, JoinTref ann_ _tblIfixedUpIdentifiersTree
                                    nat_ joinType_ _tbl1IfixedUpIdentifiersTree
                                    _onExprIfixedUpIdentifiersTree al)
                  {-# LINE 16006 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 377, column 16)
              (_lhsOtrefIDs,_) =
                  {-# LINE 377 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup3
                  {-# LINE 16011 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 377, column 16)
              (_,_lhsOfixedUpIdentifiersTree) =
                  {-# LINE 377 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup3
                  {-# LINE 16016 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  JoinTref ann_ _tblIannotatedTree nat_ joinType_ _tbl1IannotatedTree _onExprIannotatedTree alias_
                  {-# LINE 16021 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  JoinTref ann_ _tblIfixedUpIdentifiersTree nat_ joinType_ _tbl1IfixedUpIdentifiersTree _onExprIfixedUpIdentifiersTree alias_
                  {-# LINE 16026 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  JoinTref ann_ _tblIoriginalTree nat_ joinType_ _tbl1IoriginalTree _onExprIoriginalTree alias_
                  {-# LINE 16031 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16036 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16041 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16046 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16051 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1Ocat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16056 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1Oidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16061 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1Olib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16066 "AstInternal.hs" #-}
              -- copy rule (down)
              _onExprOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16071 "AstInternal.hs" #-}
              -- copy rule (down)
              _onExprOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16076 "AstInternal.hs" #-}
              ( _tblIannotatedTree,_tblIfixedUpIdentifiersTree,_tblIlibUpdates,_tblIoriginalTree,_tblItrefIDs) =
                  (tbl_ _tblOcat _tblOidenv _tblOlib )
              ( _tbl1IannotatedTree,_tbl1IfixedUpIdentifiersTree,_tbl1IlibUpdates,_tbl1IoriginalTree,_tbl1ItrefIDs) =
                  (tbl1_ _tbl1Ocat _tbl1Oidenv _tbl1Olib )
              ( _onExprIannotatedTree,_onExprIfixedUpIdentifiersTree,_onExprIoriginalTree) =
                  (onExpr_ _onExprOcat _onExprOidenv _onExprOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOtrefIDs)))
sem_TableRef_SubTref :: Annotation ->
                        T_QueryExpr  ->
                        TableAlias ->
                        T_TableRef 
sem_TableRef_SubTref ann_ sel_ alias_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _selOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: TableRef
              _selectAttrs :: (Either [TypeError] [(String,Type)])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              __tup4 :: (([(String,[String])],TableRef))
              _lhsOtrefIDs :: ([(String,[String])])
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOoriginalTree :: TableRef
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs.ag"(line 486, column 15)
              _selOexpectedTypes =
                  {-# LINE 486 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 16113 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  {-# LINE 91 "./TypeChecking/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 16118 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 134, column 9)
              _errs =
                  {-# LINE 134 "./TypeChecking/TableRefs.ag" #-}
                  case _selectAttrs     of
                          Left e -> e
                          Right _ -> []
                  {-# LINE 16125 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 138, column 9)
              _selectAttrs =
                  {-# LINE 138 "./TypeChecking/TableRefs.ag" #-}
                  lmt _selIuType
                  {-# LINE 16130 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 139, column 9)
              _lhsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/TableRefs.ag" #-}
                  [LBTref "sub query" (getAlias "" alias_)
                                  (fromRight [] _selectAttrs    ) []]
                  {-# LINE 16136 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 259, column 9)
              _backTree =
                  {-# LINE 259 "./TypeChecking/TableRefs.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 16141 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 367, column 15)
              __tup4 =
                  {-# LINE 367 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  let IDEnv x = _selIcidenv
                      (trs,al) = doAlias alias_ x
                  in (trs, SubTref ann_ _selIfixedUpIdentifiersTree al)
                  {-# LINE 16148 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 367, column 15)
              (_lhsOtrefIDs,_) =
                  {-# LINE 367 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup4
                  {-# LINE 16153 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 367, column 15)
              (_,_lhsOfixedUpIdentifiersTree) =
                  {-# LINE 367 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup4
                  {-# LINE 16158 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 16163 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SubTref ann_ _selIfixedUpIdentifiersTree alias_
                  {-# LINE 16168 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  SubTref ann_ _selIoriginalTree alias_
                  {-# LINE 16173 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16178 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16183 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16188 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16193 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOtrefIDs)))
sem_TableRef_Tref :: Annotation ->
                     T_SQIdentifier  ->
                     TableAlias ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_ alias_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              __tup5 :: (([(String,[String])],TableRef))
              _lhsOtrefIDs :: ([(String,[String])])
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOoriginalTree :: TableRef
              _tblOcat :: Catalog
              _tblOidenv :: IDEnv
              _tblOlib :: LocalBindings
              _tblIannotatedTree :: SQIdentifier
              _tblIfixedUpIdentifiersTree :: SQIdentifier
              _tblIoriginalTree :: SQIdentifier
              _tblItbAnnotatedTree :: SQIdentifier
              _tblItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  {-# LINE 91 "./TypeChecking/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 16223 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 147, column 9)
              _errs =
                  {-# LINE 147 "./TypeChecking/TableRefs.ag" #-}
                  []
                  {-# LINE 16228 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 148, column 9)
              _lhsOlibUpdates =
                  {-# LINE 148 "./TypeChecking/TableRefs.ag" #-}
                  maybe [] id $ do
                  let n = getTName _tblIannotatedTree
                  (pu,pr) <- _tblItbUType
                  return [LBTref ("tref: " ++ n)
                            (getAlias n alias_)
                            pu
                            pr]
                  {-# LINE 16239 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 261, column 9)
              _backTree =
                  {-# LINE 261 "./TypeChecking/TableRefs.ag" #-}
                  Tref ann_ _tblItbAnnotatedTree alias_
                  {-# LINE 16244 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 359, column 12)
              __tup5 =
                  {-# LINE 359 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  let tn = getTName _tblIfixedUpIdentifiersTree
                      ids = case catCompositePublicAttrs _lhsIcat relationComposites tn of
                              Right attrs -> [(tn, map fst attrs)]
                              Left _ -> [(tn,[])]
                      (trs,al) = doAlias alias_ ids
                  in (trs,Tref ann_ _tblIfixedUpIdentifiersTree al)
                  {-# LINE 16254 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 359, column 12)
              (_lhsOtrefIDs,_) =
                  {-# LINE 359 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup5
                  {-# LINE 16259 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 359, column 12)
              (_,_lhsOfixedUpIdentifiersTree) =
                  {-# LINE 359 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  __tup5
                  {-# LINE 16264 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Tref ann_ _tblIannotatedTree alias_
                  {-# LINE 16269 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Tref ann_ _tblIfixedUpIdentifiersTree alias_
                  {-# LINE 16274 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Tref ann_ _tblIoriginalTree alias_
                  {-# LINE 16279 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16284 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16289 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16294 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16299 "AstInternal.hs" #-}
              ( _tblIannotatedTree,_tblIfixedUpIdentifiersTree,_tblIoriginalTree,_tblItbAnnotatedTree,_tblItbUType) =
                  (tbl_ _tblOcat _tblOidenv _tblOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOtrefIDs)))
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
                       ( TableRefList,TableRefList,([LocalBindingsUpdate]),TableRefList,([(String,[String])]))
data Inh_TableRefList  = Inh_TableRefList {cat_Inh_TableRefList :: Catalog,idenv_Inh_TableRefList :: IDEnv,lib_Inh_TableRefList :: LocalBindings}
data Syn_TableRefList  = Syn_TableRefList {annotatedTree_Syn_TableRefList :: TableRefList,fixedUpIdentifiersTree_Syn_TableRefList :: TableRefList,libUpdates_Syn_TableRefList :: [LocalBindingsUpdate],originalTree_Syn_TableRefList :: TableRefList,trefIDs_Syn_TableRefList :: [(String,[String])]}
wrap_TableRefList :: T_TableRefList  ->
                     Inh_TableRefList  ->
                     Syn_TableRefList 
wrap_TableRefList sem (Inh_TableRefList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOtrefIDs) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TableRefList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOoriginalTree _lhsOtrefIDs ))
sem_TableRefList_Cons :: T_TableRef  ->
                         T_TableRefList  ->
                         T_TableRefList 
sem_TableRefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOtrefIDs :: ([(String,[String])])
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
              _hdIoriginalTree :: TableRef
              _hdItrefIDs :: ([(String,[String])])
              _tlIannotatedTree :: TableRefList
              _tlIfixedUpIdentifiersTree :: TableRefList
              _tlIlibUpdates :: ([LocalBindingsUpdate])
              _tlIoriginalTree :: TableRefList
              _tlItrefIDs :: ([(String,[String])])
              -- "./TypeChecking/TableRefs.ag"(line 97, column 9)
              _lhsOlibUpdates =
                  {-# LINE 97 "./TypeChecking/TableRefs.ag" #-}
                  _hdIlibUpdates
                  {-# LINE 16382 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 355, column 12)
              _lhsOtrefIDs =
                  {-# LINE 355 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _hdItrefIDs ++ _tlItrefIDs
                  {-# LINE 16387 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 16392 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 16397 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 16402 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16407 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 16412 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16417 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16422 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16427 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16432 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16437 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16442 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16447 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIlibUpdates,_hdIoriginalTree,_hdItrefIDs) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIlibUpdates,_tlIoriginalTree,_tlItrefIDs) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOtrefIDs)))
sem_TableRefList_Nil :: T_TableRefList 
sem_TableRefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOtrefIDs :: ([(String,[String])])
              _lhsOannotatedTree :: TableRefList
              _lhsOfixedUpIdentifiersTree :: TableRefList
              _lhsOoriginalTree :: TableRefList
              -- "./TypeChecking/TableRefs.ag"(line 95, column 9)
              _lhsOlibUpdates =
                  {-# LINE 95 "./TypeChecking/TableRefs.ag" #-}
                  []
                  {-# LINE 16467 "AstInternal.hs" #-}
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 356, column 11)
              _lhsOtrefIDs =
                  {-# LINE 356 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 16472 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 16477 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 16482 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 16487 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16492 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 16497 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16502 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOtrefIDs)))
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
              -- "./TypeChecking/MiscCreates.ag"(line 37, column 9)
              _lhsOattrName =
                  {-# LINE 37 "./TypeChecking/MiscCreates.ag" #-}
                  name_
                  {-# LINE 16572 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 38, column 9)
              _lhsOnamedType =
                  {-# LINE 38 "./TypeChecking/MiscCreates.ag" #-}
                  _typInamedType
                  {-# LINE 16577 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIannotatedTree
                  {-# LINE 16582 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  TypeAttDef ann_ name_ _typIfixedUpIdentifiersTree
                  {-# LINE 16587 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIoriginalTree
                  {-# LINE 16592 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16597 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 16602 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16607 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16612 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16617 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16622 "AstInternal.hs" #-}
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
              -- "./TypeChecking/MiscCreates.ag"(line 43, column 12)
              _lhsOattrs =
                  {-# LINE 43 "./TypeChecking/MiscCreates.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 16702 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 16707 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 16712 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 16717 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16722 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 16727 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16732 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16737 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16742 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16747 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16752 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16757 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16762 "AstInternal.hs" #-}
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
              -- "./TypeChecking/MiscCreates.ag"(line 44, column 11)
              _lhsOattrs =
                  {-# LINE 44 "./TypeChecking/MiscCreates.ag" #-}
                  []
                  {-# LINE 16781 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 16786 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 16791 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 16796 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16801 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 16806 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16811 "AstInternal.hs" #-}
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
                  {-# LINE 16930 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 16935 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 27, column 9)
              _tpe =
                  {-# LINE 27 "./TypeChecking/Misc.ag" #-}
                  lmt _typInamedType >>=  Right . ArrayType
                  {-# LINE 16940 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 28, column 9)
              _backTree =
                  {-# LINE 28 "./TypeChecking/Misc.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 16945 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 16950 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ArrayTypeName ann_ _typIfixedUpIdentifiersTree
                  {-# LINE 16955 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIoriginalTree
                  {-# LINE 16960 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 16965 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16970 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16975 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 16980 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16985 "AstInternal.hs" #-}
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
                  {-# LINE 17006 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 17011 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 36, column 9)
              _tpe =
                  {-# LINE 36 "./TypeChecking/Misc.ag" #-}
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
                  {-# LINE 17016 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 37, column 9)
              _backTree =
                  {-# LINE 37 "./TypeChecking/Misc.ag" #-}
                  Prec2TypeName ann_ tn_ prec_ prec1_
                  {-# LINE 17021 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  Prec2TypeName ann_ tn_ prec_ prec1_
                  {-# LINE 17026 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  Prec2TypeName ann_ tn_ prec_ prec1_
                  {-# LINE 17031 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  Prec2TypeName ann_ tn_ prec_ prec1_
                  {-# LINE 17036 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17041 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17046 "AstInternal.hs" #-}
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
                  {-# LINE 17064 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 17069 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 33, column 9)
              _tpe =
                  {-# LINE 33 "./TypeChecking/Misc.ag" #-}
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
                  {-# LINE 17074 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 34, column 9)
              _backTree =
                  {-# LINE 34 "./TypeChecking/Misc.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 17079 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 17084 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 17089 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 17094 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17099 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17104 "AstInternal.hs" #-}
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
                  {-# LINE 17128 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 17133 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 30, column 9)
              _tpe =
                  {-# LINE 30 "./TypeChecking/Misc.ag" #-}
                  lmt _typInamedType >>=  Right . SetOfType
                  {-# LINE 17138 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 31, column 9)
              _backTree =
                  {-# LINE 31 "./TypeChecking/Misc.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 17143 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 17148 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SetOfTypeName ann_ _typIfixedUpIdentifiersTree
                  {-# LINE 17153 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIoriginalTree
                  {-# LINE 17158 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17163 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17168 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17173 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17178 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17183 "AstInternal.hs" #-}
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
                  {-# LINE 17202 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 17207 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 24, column 9)
              _tpe =
                  {-# LINE 24 "./TypeChecking/Misc.ag" #-}
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
                  {-# LINE 17212 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 25, column 9)
              _backTree =
                  {-# LINE 25 "./TypeChecking/Misc.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 17217 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 17222 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 17227 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 17232 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17237 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17242 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Drops.ag"(line 37, column 12)
              _lhsOnamedTypes =
                  {-# LINE 37 "./TypeChecking/Drops.ag" #-}
                  _hdInamedType : _tlInamedTypes
                  {-# LINE 17319 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 17324 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 17329 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 17334 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17339 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17344 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17349 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17354 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17359 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17364 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17369 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17374 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17379 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Drops.ag"(line 38, column 11)
              _lhsOnamedTypes =
                  {-# LINE 38 "./TypeChecking/Drops.ag" #-}
                  []
                  {-# LINE 17398 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17403 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 17408 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17413 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17418 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17423 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17428 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Block.ag"(line 14, column 18)
              _lhsOdef =
                  {-# LINE 14 "./TypeChecking/Block.ag" #-}
                  (name_, Nothing)
                  {-# LINE 17512 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  ParamAlias ann_ name_ i_
                  {-# LINE 17517 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  ParamAlias ann_ name_ i_
                  {-# LINE 17522 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  ParamAlias ann_ name_ i_
                  {-# LINE 17527 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17532 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17537 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17542 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Block.ag"(line 13, column 16)
              _lhsOdef =
                  {-# LINE 13 "./TypeChecking/Block.ag" #-}
                  (name_, Nothing)
                  {-# LINE 17560 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  VarAlias ann_ name_ aliased_
                  {-# LINE 17565 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  VarAlias ann_ name_ aliased_
                  {-# LINE 17570 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  VarAlias ann_ name_ aliased_
                  {-# LINE 17575 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17580 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17585 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17590 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Block.ag"(line 10, column 14)
              _lhsOdef =
                  {-# LINE 10 "./TypeChecking/Block.ag" #-}
                  (name_, if _typInamedType == Just (Pseudo Record)
                          then Just (PgRecord Nothing)
                          else _typInamedType)
                  {-# LINE 17618 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIannotatedTree value_
                  {-# LINE 17623 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  VarDef ann_ name_ _typIfixedUpIdentifiersTree value_
                  {-# LINE 17628 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIoriginalTree value_
                  {-# LINE 17633 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17638 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17643 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17648 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17653 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17658 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17663 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Block.ag"(line 17, column 12)
              _lhsOdefs =
                  {-# LINE 17 "./TypeChecking/Block.ag" #-}
                  _hdIdef : _tlIdefs
                  {-# LINE 17742 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 17747 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 17752 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 17757 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17762 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17767 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17772 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17777 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17782 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17787 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17792 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17797 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17802 "AstInternal.hs" #-}
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
              -- "./TypeChecking/Block.ag"(line 18, column 11)
              _lhsOdefs =
                  {-# LINE 18 "./TypeChecking/Block.ag" #-}
                  []
                  {-# LINE 17821 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17826 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 17831 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17836 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17841 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17846 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17851 "AstInternal.hs" #-}
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
              -- "./TypeChecking/ScalarExprs.ag"(line 488, column 17)
              _exOexpectedTypes =
                  {-# LINE 488 "./TypeChecking/ScalarExprs.ag" #-}
                  []
                  {-# LINE 17929 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 246, column 9)
              _tpe =
                  {-# LINE 246 "./TypeChecking/QueryStatement.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 17934 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 247, column 9)
              _backTree =
                  {-# LINE 247 "./TypeChecking/QueryStatement.ag" #-}
                  WithQuery ann_ name_ colAliases_ _exIannotatedTree
                  {-# LINE 17939 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 248, column 9)
              _attrs =
                  {-# LINE 248 "./TypeChecking/QueryStatement.ag" #-}
                  maybe [] id $ _exIuType
                  {-# LINE 17944 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 249, column 9)
              _catUpdates =
                  {-# LINE 249 "./TypeChecking/QueryStatement.ag" #-}
                  [CatCreateView name_ _attrs    ]
                  {-# LINE 17949 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 250, column 9)
              _statementType =
                  {-# LINE 250 "./TypeChecking/QueryStatement.ag" #-}
                  Nothing
                  {-# LINE 17954 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  WithQuery ann_ name_ colAliases_ _exIannotatedTree
                  {-# LINE 17959 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  WithQuery ann_ name_ colAliases_ _exIfixedUpIdentifiersTree
                  {-# LINE 17964 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  WithQuery ann_ name_ colAliases_ _exIoriginalTree
                  {-# LINE 17969 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17974 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 17979 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17984 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOcatUpdates =
                  {-# LINE 220 "./TypeChecking/QueryStatement.ag" #-}
                  _catUpdates
                  {-# LINE 17989 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 76 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17994 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 17999 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 18004 "AstInternal.hs" #-}
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
              -- "./TypeChecking/QueryStatement.ag"(line 230, column 9)
              _newCat =
                  {-# LINE 230 "./TypeChecking/QueryStatement.ag" #-}
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
                  {-# LINE 18089 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 232, column 9)
              _hdOcat =
                  {-# LINE 232 "./TypeChecking/QueryStatement.ag" #-}
                  _newCat
                  {-# LINE 18094 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 233, column 9)
              _tlOcat =
                  {-# LINE 233 "./TypeChecking/QueryStatement.ag" #-}
                  _newCat
                  {-# LINE 18099 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 237, column 9)
              _lhsOproducedCat =
                  {-# LINE 237 "./TypeChecking/QueryStatement.ag" #-}
                  _tlIproducedCat
                  {-# LINE 18104 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 240, column 9)
              _tlOcatUpdates =
                  {-# LINE 240 "./TypeChecking/QueryStatement.ag" #-}
                  _hdIcatUpdates
                  {-# LINE 18109 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 18114 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
                  {-# LINE 18119 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 18124 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 18129 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 18134 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 18139 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 18144 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 18149 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOidenv =
                  {-# LINE 258 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _lhsIidenv
                  {-# LINE 18154 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 18159 "AstInternal.hs" #-}
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
              -- "./TypeChecking/QueryStatement.ag"(line 230, column 9)
              _newCat =
                  {-# LINE 230 "./TypeChecking/QueryStatement.ag" #-}
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
                  {-# LINE 18179 "AstInternal.hs" #-}
              -- "./TypeChecking/QueryStatement.ag"(line 242, column 9)
              _lhsOproducedCat =
                  {-# LINE 242 "./TypeChecking/QueryStatement.ag" #-}
                  _newCat
                  {-# LINE 18184 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 18189 "AstInternal.hs" #-}
              -- self rule
              _fixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  []
                  {-# LINE 18194 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 18199 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 77 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 18204 "AstInternal.hs" #-}
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  {-# LINE 312 "./TypeChecking/FixUpIdentifiers.ag" #-}
                  _fixedUpIdentifiersTree
                  {-# LINE 18209 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 83 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 18214 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat)))