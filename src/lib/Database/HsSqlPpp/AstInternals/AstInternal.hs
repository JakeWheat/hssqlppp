

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
   ,fixUpIdentifiers
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




-- used for possibly dot qualified identifiers in e.g. table name
-- references
-- todo: start using this everywhere, lots of places which
-- use scalarexpr or string should be using this
data DQIdentifier = DQIdentifier Annotation [String]
                    deriving (Eq,Show,Data,Typeable)


data TableAlias = NoAlias
                | TableAlias String --alias:String
                | FullAlias String [String] -- alias:String cols:{[String]}
                  deriving (Show,Eq,Typeable,Data)


data JoinType = Inner | LeftOuter| RightOuter | FullOuter | Cross
                deriving (Show,Eq,Typeable,Data)


data CopySource = CopyFilename String
                | Stdin
                  deriving (Show,Eq,Typeable,Data)


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


data RaiseType = RNotice | RException | RError
                 deriving (Show,Eq,Typeable,Data)

data CombineType = Except | Union | Intersect | UnionAll
                   deriving (Show,Eq,Typeable,Data)

data Volatility = Volatile | Stable | Immutable
                  deriving (Show,Eq,Typeable,Data)

data Language = Sql | Plpgsql
                deriving (Show,Eq,Typeable,Data)


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



data FrameClause = FrameUnboundedPreceding
                 | FrameUnboundedFull
                 | FrameRowsUnboundedPreceding
                   deriving (Show,Eq,Typeable,Data)


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
    let t = sem_ScalarExprRoot (ScalarExprRoot ex)
        rt = (annotatedTree_Syn_ScalarExprRoot
              (wrap_ScalarExprRoot t Inh_ScalarExprRoot {cat_Inh_ScalarExprRoot = cat
                                                        ,lib_Inh_ScalarExprRoot = emptyBindings
                                                        ,idenv_Inh_ScalarExprRoot = emptyIDEnv}))
    in case rt of
         ScalarExprRoot e -> e



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

unwrapLookup :: (String,[String],Type) -> Type
unwrapLookup (_,_,t) = t

unwrapStar :: [(String,[String],Type)] -> [(String,Type)]
unwrapStar = map uw
             where
               uw (_,n,t) = (last n, t)

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



{-data SiType = SiType (String,Maybe Type)
            | SiStarType [(String,Maybe Type)]-}


unwrapSetofs :: [(String,Type)] -> [(String,Type)]
unwrapSetofs = map (\(n,t) -> (n, unwrapSetof t))

unwrapSetof :: Type -> Type
unwrapSetof (SetOfType u) = u
unwrapSetof v = v



defaultSystemColumns :: [(String,Type)]
defaultSystemColumns = [("tableoid", ScalarType "oid")
                       ,("cmax", ScalarType "cid")
                       ,("xmax", ScalarType "xid")
                       ,("cmin", ScalarType "cid")
                       ,("xmin", ScalarType "xid")
                       ,("ctid", ScalarType "tid")]


data ParamName = NamedParam Int String
               | UnnamedParam Int



data IDEnv = IDEnv [(String, [String])]
             deriving Show
emptyIDEnv :: IDEnv
emptyIDEnv = IDEnv []

qualifyID :: IDEnv -> String -> Maybe (String,String)
qualifyID (IDEnv env) i =
  q env i
  where
    q [] i = Nothing
    q ((t,cs):es) i =
       if i `elem` cs
       then Just (t,i)
       else q es i

makeIDEnv :: String -- range qualifier
          -> [String] -- attribute names
          -> IDEnv
makeIDEnv t c = IDEnv [(t,c)]

unimplementedIDEnv :: IDEnv
unimplementedIDEnv = IDEnv []

joinIDEnvs :: IDEnv -> IDEnv -> IDEnv
joinIDEnvs (IDEnv a) (IDEnv b) = IDEnv $ a ++ b

{-
emptyIDEnv :: IDEnv
emptyIDEnv = undefined

unimplementedIDEnv :: IDEnv
unimplementedIDEnv = undefined


joinIDEnvs :: IDEnv -> IDEnv -> IDEnv
joinIDEnvs = undefined

aliasIDEnvRangeName :: IDEnv -> String -> IDEnv
aliasIDEnvRangeName = undefined

-}
expandStar :: IDEnv -> Maybe String --qualifier
           -> [(String,String)]
expandStar (IDEnv es) Nothing =
  flip concatMap es $ \(t,cs) -> map (t,) cs
expandStar (IDEnv es) (Just t) =
  maybe [(t,"*")] (map (t,)) $ lookup t es




getTableFields :: Catalog -> ScalarExpr -> (String,[String])
getTableFields c e =
  let tn = getName e
  in case catCompositePublicAttrs c relationComposites tn of
       (Right attrs) -> (tn, map fst attrs)
       (Left _) -> (tn,[])




makeSelExps :: Annotation -> Annotation -> Annotation -> [(String,String)] -> [SelectItem]
makeSelExps sea a0 a1 is =
  flip map is $ \(q,c) -> SelExp sea $ QIdentifier a0 (Identifier a1 q) c







fixUpIdentifiers :: Catalog -> [Statement] -> [Statement]
fixUpIdentifiers cat sts =
    let t = sem_Root (Root sts)
        ta = wrap_Root t Inh_Root {cat_Inh_Root = cat
                                  ,lib_Inh_Root = emptyBindings
                                  ,idenv_Inh_Root = emptyIDEnv}
        tl = fixedUpIdentifiersTree_Syn_Root ta
    in case tl of
         Root r -> r

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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative AddConstraint:
         child ann            : {Annotation}
         child con            : Constraint 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative AlterColumnDefault:
         child ann            : {Annotation}
         child nm             : {String}
         child def            : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                           ( AlterTableAction,AlterTableAction,AlterTableAction,AlterTableAction)
data Inh_AlterTableAction  = Inh_AlterTableAction {cat_Inh_AlterTableAction :: Catalog,idenv_Inh_AlterTableAction :: IDEnv,lib_Inh_AlterTableAction :: LocalBindings}
data Syn_AlterTableAction  = Syn_AlterTableAction {annotatedTree_Syn_AlterTableAction :: AlterTableAction,fixedUpIdentifiersTree_Syn_AlterTableAction :: AlterTableAction,newFixedUpIdentifiersTree_Syn_AlterTableAction :: AlterTableAction,originalTree_Syn_AlterTableAction :: AlterTableAction}
wrap_AlterTableAction :: T_AlterTableAction  ->
                         Inh_AlterTableAction  ->
                         Syn_AlterTableAction 
wrap_AlterTableAction sem (Inh_AlterTableAction _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_AlterTableAction _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_AlterTableAction_AddConstraint :: Annotation ->
                                      T_Constraint  ->
                                      T_AlterTableAction 
sem_AlterTableAction_AddConstraint ann_ con_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: AlterTableAction
              _lhsOfixedUpIdentifiersTree :: AlterTableAction
              _lhsOnewFixedUpIdentifiersTree :: AlterTableAction
              _lhsOoriginalTree :: AlterTableAction
              _conOcat :: Catalog
              _conOidenv :: IDEnv
              _conOlib :: LocalBindings
              _conIannotatedTree :: Constraint
              _conIfixedUpIdentifiersTree :: Constraint
              _conInewFixedUpIdentifiersTree :: Constraint
              _conIoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  AddConstraint ann_ _conIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  AddConstraint ann_ _conIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  AddConstraint ann_ _conInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  AddConstraint ann_ _conIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _conOcat =
                  _lhsIcat
              -- copy rule (down)
              _conOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _conOlib =
                  _lhsIlib
              ( _conIannotatedTree,_conIfixedUpIdentifiersTree,_conInewFixedUpIdentifiersTree,_conIoriginalTree) =
                  (con_ _conOcat _conOidenv _conOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: AlterTableAction
              _lhsOoriginalTree :: AlterTableAction
              _defOcat :: Catalog
              _defOidenv :: IDEnv
              _defOlib :: LocalBindings
              _defIannotatedTree :: ScalarExpr
              _defIfixedUpIdentifiersTree :: ScalarExpr
              _defInewFixedUpIdentifiersTree :: ScalarExpr
              _defIntAnnotatedTree :: ScalarExpr
              _defIntType :: ([(String,Type)])
              _defIoriginalTree :: ScalarExpr
              _defItbAnnotatedTree :: ScalarExpr
              _defItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _defIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 533, column 26)
              _defOexpectedType =
                  Nothing
              -- self rule
              _annotatedTree =
                  AlterColumnDefault ann_ nm_ _defIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  AlterColumnDefault ann_ nm_ _defIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  AlterColumnDefault ann_ nm_ _defInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  AlterColumnDefault ann_ nm_ _defIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _defOcat =
                  _lhsIcat
              -- copy rule (down)
              _defOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _defOlib =
                  _lhsIlib
              ( _defIannotatedTree,_defIfixedUpIdentifiersTree,_defInewFixedUpIdentifiersTree,_defIntAnnotatedTree,_defIntType,_defIoriginalTree,_defItbAnnotatedTree,_defItbUType,_defIuType) =
                  (def_ _defOcat _defOexpectedType _defOidenv _defOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : AlterTableAction 
         child tl             : AlterTableActionList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                               ( AlterTableActionList,AlterTableActionList,AlterTableActionList,AlterTableActionList)
data Inh_AlterTableActionList  = Inh_AlterTableActionList {cat_Inh_AlterTableActionList :: Catalog,idenv_Inh_AlterTableActionList :: IDEnv,lib_Inh_AlterTableActionList :: LocalBindings}
data Syn_AlterTableActionList  = Syn_AlterTableActionList {annotatedTree_Syn_AlterTableActionList :: AlterTableActionList,fixedUpIdentifiersTree_Syn_AlterTableActionList :: AlterTableActionList,newFixedUpIdentifiersTree_Syn_AlterTableActionList :: AlterTableActionList,originalTree_Syn_AlterTableActionList :: AlterTableActionList}
wrap_AlterTableActionList :: T_AlterTableActionList  ->
                             Inh_AlterTableActionList  ->
                             Syn_AlterTableActionList 
wrap_AlterTableActionList sem (Inh_AlterTableActionList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_AlterTableActionList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_AlterTableActionList_Cons :: T_AlterTableAction  ->
                                 T_AlterTableActionList  ->
                                 T_AlterTableActionList 
sem_AlterTableActionList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: AlterTableActionList
              _lhsOfixedUpIdentifiersTree :: AlterTableActionList
              _lhsOnewFixedUpIdentifiersTree :: AlterTableActionList
              _lhsOoriginalTree :: AlterTableActionList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: AlterTableAction
              _hdIfixedUpIdentifiersTree :: AlterTableAction
              _hdInewFixedUpIdentifiersTree :: AlterTableAction
              _hdIoriginalTree :: AlterTableAction
              _tlIannotatedTree :: AlterTableActionList
              _tlIfixedUpIdentifiersTree :: AlterTableActionList
              _tlInewFixedUpIdentifiersTree :: AlterTableActionList
              _tlIoriginalTree :: AlterTableActionList
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_AlterTableActionList_Nil :: T_AlterTableActionList 
sem_AlterTableActionList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: AlterTableActionList
              _lhsOfixedUpIdentifiersTree :: AlterTableActionList
              _lhsOnewFixedUpIdentifiersTree :: AlterTableActionList
              _lhsOoriginalTree :: AlterTableActionList
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
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
            local newFixedUpIdentifiersTree : _
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
                       ( AttributeDef,String,AttributeDef,(Maybe Type),AttributeDef,AttributeDef)
data Inh_AttributeDef  = Inh_AttributeDef {cat_Inh_AttributeDef :: Catalog,idenv_Inh_AttributeDef :: IDEnv,lib_Inh_AttributeDef :: LocalBindings}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef,attrName_Syn_AttributeDef :: String,fixedUpIdentifiersTree_Syn_AttributeDef :: AttributeDef,namedType_Syn_AttributeDef :: Maybe Type,newFixedUpIdentifiersTree_Syn_AttributeDef :: AttributeDef,originalTree_Syn_AttributeDef :: AttributeDef}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_AttributeDef _lhsOannotatedTree _lhsOattrName _lhsOfixedUpIdentifiersTree _lhsOnamedType _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: AttributeDef
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
              _typInewFixedUpIdentifiersTree :: TypeName
              _typIoriginalTree :: TypeName
              _defIannotatedTree :: MaybeScalarExpr
              _defIfixedUpIdentifiersTree :: MaybeScalarExpr
              _defInewFixedUpIdentifiersTree :: MaybeScalarExpr
              _defIoriginalTree :: MaybeScalarExpr
              _defIuType :: (Maybe Type)
              _consIannotatedTree :: RowConstraintList
              _consIfixedUpIdentifiersTree :: RowConstraintList
              _consInewFixedUpIdentifiersTree :: RowConstraintList
              _consIoriginalTree :: RowConstraintList
              -- "./TypeChecking/CreateTable.ag"(line 83, column 9)
              _lhsOattrName =
                  map toLower name_
              -- "./TypeChecking/CreateTable.ag"(line 84, column 9)
              _lhsOnamedType =
                  _typInamedType
              -- "./TypeChecking/CreateTable.ag"(line 98, column 9)
              _consOlib =
                  either (const _lhsIlib) id $ do
                  t <- lmt _typInamedType
                  lbUpdate _lhsIcat
                           (LBIds "attribute def" Nothing
                                  [(name_, t)]) _lhsIlib
              -- self rule
              _annotatedTree =
                  AttributeDef ann_ name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  AttributeDef ann_ name_ _typIfixedUpIdentifiersTree _defIfixedUpIdentifiersTree _consIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  AttributeDef ann_ name_ _typInewFixedUpIdentifiersTree _defInewFixedUpIdentifiersTree _consInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  AttributeDef ann_ name_ _typIoriginalTree _defIoriginalTree _consIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              -- copy rule (down)
              _defOcat =
                  _lhsIcat
              -- copy rule (down)
              _defOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _defOlib =
                  _lhsIlib
              -- copy rule (down)
              _consOcat =
                  _lhsIcat
              -- copy rule (down)
              _consOidenv =
                  _lhsIidenv
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typInewFixedUpIdentifiersTree,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
              ( _defIannotatedTree,_defIfixedUpIdentifiersTree,_defInewFixedUpIdentifiersTree,_defIoriginalTree,_defIuType) =
                  (def_ _defOcat _defOidenv _defOlib )
              ( _consIannotatedTree,_consIfixedUpIdentifiersTree,_consInewFixedUpIdentifiersTree,_consIoriginalTree) =
                  (cons_ _consOcat _consOidenv _consOlib )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : AttributeDef 
         child tl             : AttributeDefList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                           ( AttributeDefList,([(String, Maybe Type)]),AttributeDefList,AttributeDefList,AttributeDefList)
data Inh_AttributeDefList  = Inh_AttributeDefList {cat_Inh_AttributeDefList :: Catalog,idenv_Inh_AttributeDefList :: IDEnv,lib_Inh_AttributeDefList :: LocalBindings}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList,attrs_Syn_AttributeDefList :: [(String, Maybe Type)],fixedUpIdentifiersTree_Syn_AttributeDefList :: AttributeDefList,newFixedUpIdentifiersTree_Syn_AttributeDefList :: AttributeDefList,originalTree_Syn_AttributeDefList :: AttributeDefList}
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_AttributeDefList _lhsOannotatedTree _lhsOattrs _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: AttributeDefList
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
              _hdInewFixedUpIdentifiersTree :: AttributeDef
              _hdIoriginalTree :: AttributeDef
              _tlIannotatedTree :: AttributeDefList
              _tlIattrs :: ([(String, Maybe Type)])
              _tlIfixedUpIdentifiersTree :: AttributeDefList
              _tlInewFixedUpIdentifiersTree :: AttributeDefList
              _tlIoriginalTree :: AttributeDefList
              -- "./TypeChecking/CreateTable.ag"(line 88, column 12)
              _lhsOattrs =
                  (_hdIattrName, _hdInamedType) : _tlIattrs
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIattrName,_hdIfixedUpIdentifiersTree,_hdInamedType,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIattrs,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOattrs :: ([(String, Maybe Type)])
              _lhsOannotatedTree :: AttributeDefList
              _lhsOfixedUpIdentifiersTree :: AttributeDefList
              _lhsOnewFixedUpIdentifiersTree :: AttributeDefList
              _lhsOoriginalTree :: AttributeDefList
              -- "./TypeChecking/CreateTable.ag"(line 89, column 11)
              _lhsOattrs =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
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
            local newFixedUpIdentifiersTree : _
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
                                           ( CaseScalarExprListScalarExprPair,CaseScalarExprListScalarExprPair,CaseScalarExprListScalarExprPair,CaseScalarExprListScalarExprPair,(Maybe Type),([Maybe Type]))
data Inh_CaseScalarExprListScalarExprPair  = Inh_CaseScalarExprListScalarExprPair {cat_Inh_CaseScalarExprListScalarExprPair :: Catalog,idenv_Inh_CaseScalarExprListScalarExprPair :: IDEnv,lib_Inh_CaseScalarExprListScalarExprPair :: LocalBindings}
data Syn_CaseScalarExprListScalarExprPair  = Syn_CaseScalarExprListScalarExprPair {annotatedTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair,fixedUpIdentifiersTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair,newFixedUpIdentifiersTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair,originalTree_Syn_CaseScalarExprListScalarExprPair :: CaseScalarExprListScalarExprPair,thenType_Syn_CaseScalarExprListScalarExprPair :: Maybe Type,whenTypes_Syn_CaseScalarExprListScalarExprPair :: [Maybe Type]}
wrap_CaseScalarExprListScalarExprPair :: T_CaseScalarExprListScalarExprPair  ->
                                         Inh_CaseScalarExprListScalarExprPair  ->
                                         Syn_CaseScalarExprListScalarExprPair 
wrap_CaseScalarExprListScalarExprPair sem (Inh_CaseScalarExprListScalarExprPair _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOthenType,_lhsOwhenTypes) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_CaseScalarExprListScalarExprPair _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOthenType _lhsOwhenTypes ))
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
              _lhsOnewFixedUpIdentifiersTree :: CaseScalarExprListScalarExprPair
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPair
              _x1Ocat :: Catalog
              _x1Oidenv :: IDEnv
              _x1Olib :: LocalBindings
              _x2Ocat :: Catalog
              _x2Oidenv :: IDEnv
              _x2Olib :: LocalBindings
              _x1IannotatedTree :: ScalarExprList
              _x1IfixedUpIdentifiersTree :: ScalarExprList
              _x1InewFixedUpIdentifiersTree :: ScalarExprList
              _x1IoriginalTree :: ScalarExprList
              _x1ItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _x1IuType :: ([Maybe Type])
              _x2IannotatedTree :: ScalarExpr
              _x2IfixedUpIdentifiersTree :: ScalarExpr
              _x2InewFixedUpIdentifiersTree :: ScalarExpr
              _x2IntAnnotatedTree :: ScalarExpr
              _x2IntType :: ([(String,Type)])
              _x2IoriginalTree :: ScalarExpr
              _x2ItbAnnotatedTree :: ScalarExpr
              _x2ItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _x2IuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 293, column 13)
              _lhsOwhenTypes =
                  _x1IuType
              -- "./TypeChecking/ScalarExprs.ag"(line 294, column 13)
              _lhsOthenType =
                  _x2IuType
              -- "./TypeChecking/ScalarExprs.ag"(line 537, column 13)
              _x1OexpectedTypes =
                  []
              -- "./TypeChecking/ScalarExprs.ag"(line 538, column 13)
              _x2OexpectedType =
                  Nothing
              -- self rule
              _annotatedTree =
                  (_x1IannotatedTree,_x2IannotatedTree)
              -- self rule
              _fixedUpIdentifiersTree =
                  (_x1IfixedUpIdentifiersTree,_x2IfixedUpIdentifiersTree)
              -- self rule
              _newFixedUpIdentifiersTree =
                  (_x1InewFixedUpIdentifiersTree,_x2InewFixedUpIdentifiersTree)
              -- self rule
              _originalTree =
                  (_x1IoriginalTree,_x2IoriginalTree)
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _x1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x1Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _x1Olib =
                  _lhsIlib
              -- copy rule (down)
              _x2Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x2Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _x2Olib =
                  _lhsIlib
              ( _x1IannotatedTree,_x1IfixedUpIdentifiersTree,_x1InewFixedUpIdentifiersTree,_x1IoriginalTree,_x1ItbUTypes,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedTypes _x1Oidenv _x1Olib )
              ( _x2IannotatedTree,_x2IfixedUpIdentifiersTree,_x2InewFixedUpIdentifiersTree,_x2IntAnnotatedTree,_x2IntType,_x2IoriginalTree,_x2ItbAnnotatedTree,_x2ItbUType,_x2IuType) =
                  (x2_ _x2Ocat _x2OexpectedType _x2Oidenv _x2Olib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOthenType,_lhsOwhenTypes)))
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
         newFixedUpIdentifiersTree : SELF 
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                                               ( CaseScalarExprListScalarExprPairList,CaseScalarExprListScalarExprPairList,CaseScalarExprListScalarExprPairList,CaseScalarExprListScalarExprPairList,([Maybe Type]),([[Maybe Type]]))
data Inh_CaseScalarExprListScalarExprPairList  = Inh_CaseScalarExprListScalarExprPairList {cat_Inh_CaseScalarExprListScalarExprPairList :: Catalog,idenv_Inh_CaseScalarExprListScalarExprPairList :: IDEnv,lib_Inh_CaseScalarExprListScalarExprPairList :: LocalBindings}
data Syn_CaseScalarExprListScalarExprPairList  = Syn_CaseScalarExprListScalarExprPairList {annotatedTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList,fixedUpIdentifiersTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList,newFixedUpIdentifiersTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList,originalTree_Syn_CaseScalarExprListScalarExprPairList :: CaseScalarExprListScalarExprPairList,thenTypes_Syn_CaseScalarExprListScalarExprPairList :: [Maybe Type],whenTypes_Syn_CaseScalarExprListScalarExprPairList :: [[Maybe Type]]}
wrap_CaseScalarExprListScalarExprPairList :: T_CaseScalarExprListScalarExprPairList  ->
                                             Inh_CaseScalarExprListScalarExprPairList  ->
                                             Syn_CaseScalarExprListScalarExprPairList 
wrap_CaseScalarExprListScalarExprPairList sem (Inh_CaseScalarExprListScalarExprPairList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOwhenTypes) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_CaseScalarExprListScalarExprPairList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOthenTypes _lhsOwhenTypes ))
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
              _lhsOnewFixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPairList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: CaseScalarExprListScalarExprPair
              _hdIfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPair
              _hdInewFixedUpIdentifiersTree :: CaseScalarExprListScalarExprPair
              _hdIoriginalTree :: CaseScalarExprListScalarExprPair
              _hdIthenType :: (Maybe Type)
              _hdIwhenTypes :: ([Maybe Type])
              _tlIannotatedTree :: CaseScalarExprListScalarExprPairList
              _tlIfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _tlInewFixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _tlIoriginalTree :: CaseScalarExprListScalarExprPairList
              _tlIthenTypes :: ([Maybe Type])
              _tlIwhenTypes :: ([[Maybe Type]])
              -- "./TypeChecking/ScalarExprs.ag"(line 284, column 10)
              _lhsOwhenTypes =
                  _hdIwhenTypes : _tlIwhenTypes
              -- "./TypeChecking/ScalarExprs.ag"(line 285, column 10)
              _lhsOthenTypes =
                  _hdIthenType : _tlIthenTypes
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree,_hdIthenType,_hdIwhenTypes) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree,_tlIthenTypes,_tlIwhenTypes) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOwhenTypes)))
sem_CaseScalarExprListScalarExprPairList_Nil :: T_CaseScalarExprListScalarExprPairList 
sem_CaseScalarExprListScalarExprPairList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOwhenTypes :: ([[Maybe Type]])
              _lhsOthenTypes :: ([Maybe Type])
              _lhsOannotatedTree :: CaseScalarExprListScalarExprPairList
              _lhsOfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _lhsOnewFixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _lhsOoriginalTree :: CaseScalarExprListScalarExprPairList
              -- "./TypeChecking/ScalarExprs.ag"(line 286, column 9)
              _lhsOwhenTypes =
                  []
              -- "./TypeChecking/ScalarExprs.ag"(line 287, column 9)
              _lhsOthenTypes =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOwhenTypes)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative CheckConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative PrimaryKeyConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child x              : {[String]}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative UniqueConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child x              : {[String]}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                     ( Constraint,Constraint,Constraint,Constraint)
data Inh_Constraint  = Inh_Constraint {cat_Inh_Constraint :: Catalog,idenv_Inh_Constraint :: IDEnv,lib_Inh_Constraint :: LocalBindings}
data Syn_Constraint  = Syn_Constraint {annotatedTree_Syn_Constraint :: Constraint,fixedUpIdentifiersTree_Syn_Constraint :: Constraint,newFixedUpIdentifiersTree_Syn_Constraint :: Constraint,originalTree_Syn_Constraint :: Constraint}
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_Constraint _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: Constraint
              _lhsOoriginalTree :: Constraint
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 541, column 23)
              _exprOexpectedType =
                  Nothing
              -- self rule
              _annotatedTree =
                  CheckConstraint ann_ name_ _exprIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CheckConstraint ann_ name_ _exprIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CheckConstraint ann_ name_ _exprInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CheckConstraint ann_ name_ _exprIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  PrimaryKeyConstraint ann_ name_ x_
              -- self rule
              _fixedUpIdentifiersTree =
                  PrimaryKeyConstraint ann_ name_ x_
              -- self rule
              _newFixedUpIdentifiersTree =
                  PrimaryKeyConstraint ann_ name_ x_
              -- self rule
              _originalTree =
                  PrimaryKeyConstraint ann_ name_ x_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
              -- self rule
              _fixedUpIdentifiersTree =
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
              -- self rule
              _newFixedUpIdentifiersTree =
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
              -- self rule
              _originalTree =
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  UniqueConstraint ann_ name_ x_
              -- self rule
              _fixedUpIdentifiersTree =
                  UniqueConstraint ann_ name_ x_
              -- self rule
              _newFixedUpIdentifiersTree =
                  UniqueConstraint ann_ name_ x_
              -- self rule
              _originalTree =
                  UniqueConstraint ann_ name_ x_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : Constraint 
         child tl             : ConstraintList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                         ( ConstraintList,ConstraintList,ConstraintList,ConstraintList)
data Inh_ConstraintList  = Inh_ConstraintList {cat_Inh_ConstraintList :: Catalog,idenv_Inh_ConstraintList :: IDEnv,lib_Inh_ConstraintList :: LocalBindings}
data Syn_ConstraintList  = Syn_ConstraintList {annotatedTree_Syn_ConstraintList :: ConstraintList,fixedUpIdentifiersTree_Syn_ConstraintList :: ConstraintList,newFixedUpIdentifiersTree_Syn_ConstraintList :: ConstraintList,originalTree_Syn_ConstraintList :: ConstraintList}
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ConstraintList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ConstraintList
              _lhsOfixedUpIdentifiersTree :: ConstraintList
              _lhsOnewFixedUpIdentifiersTree :: ConstraintList
              _lhsOoriginalTree :: ConstraintList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: Constraint
              _hdIfixedUpIdentifiersTree :: Constraint
              _hdInewFixedUpIdentifiersTree :: Constraint
              _hdIoriginalTree :: Constraint
              _tlIannotatedTree :: ConstraintList
              _tlIfixedUpIdentifiersTree :: ConstraintList
              _tlInewFixedUpIdentifiersTree :: ConstraintList
              _tlIoriginalTree :: ConstraintList
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ConstraintList
              _lhsOfixedUpIdentifiersTree :: ConstraintList
              _lhsOnewFixedUpIdentifiersTree :: ConstraintList
              _lhsOoriginalTree :: ConstraintList
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative PlpgsqlFnBody:
         child ann            : {Annotation}
         child blk            : Statement 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative SqlFnBody:
         child ann            : {Annotation}
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                 ( FnBody,FnBody,FnBody,FnBody)
data Inh_FnBody  = Inh_FnBody {cat_Inh_FnBody :: Catalog,idenv_Inh_FnBody :: IDEnv,lib_Inh_FnBody :: LocalBindings}
data Syn_FnBody  = Syn_FnBody {annotatedTree_Syn_FnBody :: FnBody,fixedUpIdentifiersTree_Syn_FnBody :: FnBody,newFixedUpIdentifiersTree_Syn_FnBody :: FnBody,originalTree_Syn_FnBody :: FnBody}
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_FnBody _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: FnBody
              _lhsOoriginalTree :: FnBody
              _blkOcat :: Catalog
              _blkOidenv :: IDEnv
              _blkOlib :: LocalBindings
              _blkIannotatedTree :: Statement
              _blkIcatUpdates :: ([CatalogUpdate])
              _blkIfixedUpIdentifiersTree :: Statement
              _blkIlibUpdates :: ([LocalBindingsUpdate])
              _blkInewFixedUpIdentifiersTree :: Statement
              _blkIoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 104, column 9)
              _blkOinProducedCat =
                  emptyCatalog
              -- self rule
              _annotatedTree =
                  PlpgsqlFnBody ann_ _blkIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  PlpgsqlFnBody ann_ _blkIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  PlpgsqlFnBody ann_ _blkInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  PlpgsqlFnBody ann_ _blkIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _blkOcat =
                  _lhsIcat
              -- copy rule (down)
              _blkOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _blkOlib =
                  _lhsIlib
              ( _blkIannotatedTree,_blkIcatUpdates,_blkIfixedUpIdentifiersTree,_blkIlibUpdates,_blkInewFixedUpIdentifiersTree,_blkIoriginalTree) =
                  (blk_ _blkOcat _blkOidenv _blkOinProducedCat _blkOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: FnBody
              _lhsOoriginalTree :: FnBody
              _stsOcat :: Catalog
              _stsOidenv :: IDEnv
              _stsOlib :: LocalBindings
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsInewFixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 129, column 9)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 130, column 9)
              _stsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  SqlFnBody ann_ _stsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  SqlFnBody ann_ _stsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  SqlFnBody ann_ _stsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  SqlFnBody ann_ _stsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _stsOlib =
                  _lhsIlib
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsInewFixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative InList:
         child ann            : {Annotation}
         child exprs          : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative InSelect:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                 ( InList,InList,(Either [TypeError] Type),InList,InList)
data Inh_InList  = Inh_InList {cat_Inh_InList :: Catalog,idenv_Inh_InList :: IDEnv,lib_Inh_InList :: LocalBindings}
data Syn_InList  = Syn_InList {annotatedTree_Syn_InList :: InList,fixedUpIdentifiersTree_Syn_InList :: InList,listType_Syn_InList :: Either [TypeError] Type,newFixedUpIdentifiersTree_Syn_InList :: InList,originalTree_Syn_InList :: InList}
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_InList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOlistType _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: InList
              _lhsOoriginalTree :: InList
              _exprsOcat :: Catalog
              _exprsOidenv :: IDEnv
              _exprsOlib :: LocalBindings
              _exprsIannotatedTree :: ScalarExprList
              _exprsIfixedUpIdentifiersTree :: ScalarExprList
              _exprsInewFixedUpIdentifiersTree :: ScalarExprList
              _exprsIoriginalTree :: ScalarExprList
              _exprsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _exprsIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs.ag"(line 497, column 9)
              _lhsOlistType =
                  mapM lmt _exprsIuType >>= resolveResultSetType _lhsIcat
              -- "./TypeChecking/ScalarExprs.ag"(line 612, column 14)
              _exprsOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  InList ann_ _exprsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  InList ann_ _exprsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  InList ann_ _exprsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  InList ann_ _exprsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprsOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprsOlib =
                  _lhsIlib
              ( _exprsIannotatedTree,_exprsIfixedUpIdentifiersTree,_exprsInewFixedUpIdentifiersTree,_exprsIoriginalTree,_exprsItbUTypes,_exprsIuType) =
                  (exprs_ _exprsOcat _exprsOexpectedTypes _exprsOidenv _exprsOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: InList
              _lhsOoriginalTree :: InList
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selInewFixedUpIdentifiersTree :: QueryExpr
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs.ag"(line 499, column 9)
              _lhsOlistType =
                  do
                  st <- lmt (map snd <$> _selIuType)
                  case length st of
                            0 -> Left [InternalError
                                       "got subquery with no columns? in inselect"]
                            1 -> Right $ head st
                            _ -> Right $ AnonymousRecordType st
              -- "./TypeChecking/ScalarExprs.ag"(line 638, column 16)
              _selOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  InSelect ann_ _selIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  InSelect ann_ _selIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  InSelect ann_ _selInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  InSelect ann_ _selIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selInewFixedUpIdentifiersTree,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative JoinOn:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative JoinUsing:
         child ann            : {Annotation}
         child x              : {[String]}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                   ( JoinExpr,JoinExpr,JoinExpr,JoinExpr)
data Inh_JoinExpr  = Inh_JoinExpr {cat_Inh_JoinExpr :: Catalog,idenv_Inh_JoinExpr :: IDEnv,lib_Inh_JoinExpr :: LocalBindings}
data Syn_JoinExpr  = Syn_JoinExpr {annotatedTree_Syn_JoinExpr :: JoinExpr,fixedUpIdentifiersTree_Syn_JoinExpr :: JoinExpr,newFixedUpIdentifiersTree_Syn_JoinExpr :: JoinExpr,originalTree_Syn_JoinExpr :: JoinExpr}
wrap_JoinExpr :: T_JoinExpr  ->
                 Inh_JoinExpr  ->
                 Syn_JoinExpr 
wrap_JoinExpr sem (Inh_JoinExpr _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_JoinExpr _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: JoinExpr
              _lhsOoriginalTree :: JoinExpr
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 555, column 14)
              _exprOexpectedType =
                  Just typeBool
              -- self rule
              _annotatedTree =
                  JoinOn ann_ _exprIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  JoinOn ann_ _exprIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  JoinOn ann_ _exprInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  JoinOn ann_ _exprIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_JoinExpr_JoinUsing :: Annotation ->
                          ([String]) ->
                          T_JoinExpr 
sem_JoinExpr_JoinUsing ann_ x_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: JoinExpr
              _lhsOfixedUpIdentifiersTree :: JoinExpr
              _lhsOnewFixedUpIdentifiersTree :: JoinExpr
              _lhsOoriginalTree :: JoinExpr
              -- self rule
              _annotatedTree =
                  JoinUsing ann_ x_
              -- self rule
              _fixedUpIdentifiersTree =
                  JoinUsing ann_ x_
              -- self rule
              _newFixedUpIdentifiersTree =
                  JoinUsing ann_ x_
              -- self rule
              _originalTree =
                  JoinUsing ann_ x_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                        ( MaybeBoolExpr,MaybeBoolExpr,MaybeBoolExpr,MaybeBoolExpr)
data Inh_MaybeBoolExpr  = Inh_MaybeBoolExpr {cat_Inh_MaybeBoolExpr :: Catalog,idenv_Inh_MaybeBoolExpr :: IDEnv,lib_Inh_MaybeBoolExpr :: LocalBindings}
data Syn_MaybeBoolExpr  = Syn_MaybeBoolExpr {annotatedTree_Syn_MaybeBoolExpr :: MaybeBoolExpr,fixedUpIdentifiersTree_Syn_MaybeBoolExpr :: MaybeBoolExpr,newFixedUpIdentifiersTree_Syn_MaybeBoolExpr :: MaybeBoolExpr,originalTree_Syn_MaybeBoolExpr :: MaybeBoolExpr}
wrap_MaybeBoolExpr :: T_MaybeBoolExpr  ->
                      Inh_MaybeBoolExpr  ->
                      Syn_MaybeBoolExpr 
wrap_MaybeBoolExpr sem (Inh_MaybeBoolExpr _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_MaybeBoolExpr _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_MaybeBoolExpr_Just :: T_ScalarExpr  ->
                          T_MaybeBoolExpr 
sem_MaybeBoolExpr_Just just_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: MaybeBoolExpr
              _justOexpectedType :: (Maybe Type)
              _lhsOfixedUpIdentifiersTree :: MaybeBoolExpr
              _lhsOnewFixedUpIdentifiersTree :: MaybeBoolExpr
              _lhsOoriginalTree :: MaybeBoolExpr
              _justOcat :: Catalog
              _justOidenv :: IDEnv
              _justOlib :: LocalBindings
              _justIannotatedTree :: ScalarExpr
              _justIfixedUpIdentifiersTree :: ScalarExpr
              _justInewFixedUpIdentifiersTree :: ScalarExpr
              _justIntAnnotatedTree :: ScalarExpr
              _justIntType :: ([(String,Type)])
              _justIoriginalTree :: ScalarExpr
              _justItbAnnotatedTree :: ScalarExpr
              _justItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _justIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 131, column 9)
              _lhsOannotatedTree =
                  let t = _justIuType
                  in if t `elem` [Nothing,Just typeBool]
                     then Just _justIannotatedTree
                     else Just $ addTypeErrors [ExpressionMustBeBool] _justIannotatedTree
              -- "./TypeChecking/ScalarExprs.ag"(line 558, column 12)
              _justOexpectedType =
                  Just typeBool
              -- self rule
              _annotatedTree =
                  Just _justIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Just _justIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Just _justInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Just _justIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _justOcat =
                  _lhsIcat
              -- copy rule (down)
              _justOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _justOlib =
                  _lhsIlib
              ( _justIannotatedTree,_justIfixedUpIdentifiersTree,_justInewFixedUpIdentifiersTree,_justIntAnnotatedTree,_justIntType,_justIoriginalTree,_justItbAnnotatedTree,_justItbUType,_justIuType) =
                  (just_ _justOcat _justOexpectedType _justOidenv _justOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_MaybeBoolExpr_Nothing :: T_MaybeBoolExpr 
sem_MaybeBoolExpr_Nothing  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: MaybeBoolExpr
              _lhsOfixedUpIdentifiersTree :: MaybeBoolExpr
              _lhsOnewFixedUpIdentifiersTree :: MaybeBoolExpr
              _lhsOoriginalTree :: MaybeBoolExpr
              -- self rule
              _annotatedTree =
                  Nothing
              -- self rule
              _fixedUpIdentifiersTree =
                  Nothing
              -- self rule
              _newFixedUpIdentifiersTree =
                  Nothing
              -- self rule
              _originalTree =
                  Nothing
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         uType                : Maybe Type
   alternatives:
      alternative Just:
         child just           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                          ( MaybeScalarExpr,MaybeScalarExpr,MaybeScalarExpr,MaybeScalarExpr,(Maybe Type))
data Inh_MaybeScalarExpr  = Inh_MaybeScalarExpr {cat_Inh_MaybeScalarExpr :: Catalog,idenv_Inh_MaybeScalarExpr :: IDEnv,lib_Inh_MaybeScalarExpr :: LocalBindings}
data Syn_MaybeScalarExpr  = Syn_MaybeScalarExpr {annotatedTree_Syn_MaybeScalarExpr :: MaybeScalarExpr,fixedUpIdentifiersTree_Syn_MaybeScalarExpr :: MaybeScalarExpr,newFixedUpIdentifiersTree_Syn_MaybeScalarExpr :: MaybeScalarExpr,originalTree_Syn_MaybeScalarExpr :: MaybeScalarExpr,uType_Syn_MaybeScalarExpr :: Maybe Type}
wrap_MaybeScalarExpr :: T_MaybeScalarExpr  ->
                        Inh_MaybeScalarExpr  ->
                        Syn_MaybeScalarExpr 
wrap_MaybeScalarExpr sem (Inh_MaybeScalarExpr _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_MaybeScalarExpr _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOuType ))
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
              _lhsOnewFixedUpIdentifiersTree :: MaybeScalarExpr
              _lhsOoriginalTree :: MaybeScalarExpr
              _justOcat :: Catalog
              _justOidenv :: IDEnv
              _justOlib :: LocalBindings
              _justIannotatedTree :: ScalarExpr
              _justIfixedUpIdentifiersTree :: ScalarExpr
              _justInewFixedUpIdentifiersTree :: ScalarExpr
              _justIntAnnotatedTree :: ScalarExpr
              _justIntType :: ([(String,Type)])
              _justIoriginalTree :: ScalarExpr
              _justItbAnnotatedTree :: ScalarExpr
              _justItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _justIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 124, column 12)
              _lhsOuType =
                  _justIuType
              -- "./TypeChecking/ScalarExprs.ag"(line 561, column 12)
              _justOexpectedType =
                  Nothing
              -- self rule
              _annotatedTree =
                  Just _justIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Just _justIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Just _justInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Just _justIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _justOcat =
                  _lhsIcat
              -- copy rule (down)
              _justOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _justOlib =
                  _lhsIlib
              ( _justIannotatedTree,_justIfixedUpIdentifiersTree,_justInewFixedUpIdentifiersTree,_justIntAnnotatedTree,_justIntType,_justIoriginalTree,_justItbAnnotatedTree,_justItbUType,_justIuType) =
                  (just_ _justOcat _justOexpectedType _justOidenv _justOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_MaybeScalarExpr_Nothing :: T_MaybeScalarExpr 
sem_MaybeScalarExpr_Nothing  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOuType :: (Maybe Type)
              _lhsOannotatedTree :: MaybeScalarExpr
              _lhsOfixedUpIdentifiersTree :: MaybeScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: MaybeScalarExpr
              _lhsOoriginalTree :: MaybeScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 125, column 15)
              _lhsOuType =
                  Nothing
              -- self rule
              _annotatedTree =
                  Nothing
              -- self rule
              _fixedUpIdentifiersTree =
                  Nothing
              -- self rule
              _newFixedUpIdentifiersTree =
                  Nothing
              -- self rule
              _originalTree =
                  Nothing
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
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
         listType             : [(String,Type)]
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : SelectList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                          ( MaybeSelectList,MaybeSelectList,([(String,Type)]),MaybeSelectList,MaybeSelectList)
data Inh_MaybeSelectList  = Inh_MaybeSelectList {cat_Inh_MaybeSelectList :: Catalog,idenv_Inh_MaybeSelectList :: IDEnv,lib_Inh_MaybeSelectList :: LocalBindings}
data Syn_MaybeSelectList  = Syn_MaybeSelectList {annotatedTree_Syn_MaybeSelectList :: MaybeSelectList,fixedUpIdentifiersTree_Syn_MaybeSelectList :: MaybeSelectList,listType_Syn_MaybeSelectList :: [(String,Type)],newFixedUpIdentifiersTree_Syn_MaybeSelectList :: MaybeSelectList,originalTree_Syn_MaybeSelectList :: MaybeSelectList}
wrap_MaybeSelectList :: T_MaybeSelectList  ->
                        Inh_MaybeSelectList  ->
                        Syn_MaybeSelectList 
wrap_MaybeSelectList sem (Inh_MaybeSelectList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_MaybeSelectList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOlistType _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_MaybeSelectList_Just :: T_SelectList  ->
                            T_MaybeSelectList 
sem_MaybeSelectList_Just just_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlistType :: ([(String,Type)])
              _lhsOannotatedTree :: MaybeSelectList
              _lhsOfixedUpIdentifiersTree :: MaybeSelectList
              _lhsOnewFixedUpIdentifiersTree :: MaybeSelectList
              _lhsOoriginalTree :: MaybeSelectList
              _justOcat :: Catalog
              _justOidenv :: IDEnv
              _justOlib :: LocalBindings
              _justIannotatedTree :: SelectList
              _justIcidenv :: IDEnv
              _justIfixedUpIdentifiersTree :: SelectList
              _justIlibUpdates :: ([LocalBindingsUpdate])
              _justIlistType :: ([(String,Type)])
              _justInewFixedUpIdentifiersTree :: SelectList
              _justIoriginalTree :: SelectList
              -- "./TypeChecking/SelectLists.ag"(line 39, column 12)
              _lhsOlistType =
                  _justIlistType
              -- self rule
              _annotatedTree =
                  Just _justIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Just _justIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Just _justInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Just _justIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _justOcat =
                  _lhsIcat
              -- copy rule (down)
              _justOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _justOlib =
                  _lhsIlib
              ( _justIannotatedTree,_justIcidenv,_justIfixedUpIdentifiersTree,_justIlibUpdates,_justIlistType,_justInewFixedUpIdentifiersTree,_justIoriginalTree) =
                  (just_ _justOcat _justOidenv _justOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_MaybeSelectList_Nothing :: T_MaybeSelectList 
sem_MaybeSelectList_Nothing  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlistType :: ([(String,Type)])
              _lhsOannotatedTree :: MaybeSelectList
              _lhsOfixedUpIdentifiersTree :: MaybeSelectList
              _lhsOnewFixedUpIdentifiersTree :: MaybeSelectList
              _lhsOoriginalTree :: MaybeSelectList
              -- "./TypeChecking/SelectLists.ag"(line 40, column 15)
              _lhsOlistType =
                  []
              -- self rule
              _annotatedTree =
                  Nothing
              -- self rule
              _fixedUpIdentifiersTree =
                  Nothing
              -- self rule
              _newFixedUpIdentifiersTree =
                  Nothing
              -- self rule
              _originalTree =
                  Nothing
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : JoinExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                 ( OnExpr,OnExpr,OnExpr,OnExpr)
data Inh_OnExpr  = Inh_OnExpr {cat_Inh_OnExpr :: Catalog,idenv_Inh_OnExpr :: IDEnv,lib_Inh_OnExpr :: LocalBindings}
data Syn_OnExpr  = Syn_OnExpr {annotatedTree_Syn_OnExpr :: OnExpr,fixedUpIdentifiersTree_Syn_OnExpr :: OnExpr,newFixedUpIdentifiersTree_Syn_OnExpr :: OnExpr,originalTree_Syn_OnExpr :: OnExpr}
wrap_OnExpr :: T_OnExpr  ->
               Inh_OnExpr  ->
               Syn_OnExpr 
wrap_OnExpr sem (Inh_OnExpr _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_OnExpr _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_OnExpr_Just :: T_JoinExpr  ->
                   T_OnExpr 
sem_OnExpr_Just just_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: OnExpr
              _lhsOfixedUpIdentifiersTree :: OnExpr
              _lhsOnewFixedUpIdentifiersTree :: OnExpr
              _lhsOoriginalTree :: OnExpr
              _justOcat :: Catalog
              _justOidenv :: IDEnv
              _justOlib :: LocalBindings
              _justIannotatedTree :: JoinExpr
              _justIfixedUpIdentifiersTree :: JoinExpr
              _justInewFixedUpIdentifiersTree :: JoinExpr
              _justIoriginalTree :: JoinExpr
              -- self rule
              _annotatedTree =
                  Just _justIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Just _justIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Just _justInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Just _justIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _justOcat =
                  _lhsIcat
              -- copy rule (down)
              _justOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _justOlib =
                  _lhsIlib
              ( _justIannotatedTree,_justIfixedUpIdentifiersTree,_justInewFixedUpIdentifiersTree,_justIoriginalTree) =
                  (just_ _justOcat _justOidenv _justOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: OnExpr
              _lhsOfixedUpIdentifiersTree :: OnExpr
              _lhsOnewFixedUpIdentifiersTree :: OnExpr
              _lhsOoriginalTree :: OnExpr
              -- self rule
              _annotatedTree =
                  Nothing
              -- self rule
              _fixedUpIdentifiersTree =
                  Nothing
              -- self rule
              _newFixedUpIdentifiersTree =
                  Nothing
              -- self rule
              _originalTree =
                  Nothing
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative ParamDefTp:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                   ( ParamDef,ParamDef,(Maybe Type),ParamDef,ParamDef,ParamName)
data Inh_ParamDef  = Inh_ParamDef {cat_Inh_ParamDef :: Catalog,idenv_Inh_ParamDef :: IDEnv,lib_Inh_ParamDef :: LocalBindings,pos_Inh_ParamDef :: Int}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef,fixedUpIdentifiersTree_Syn_ParamDef :: ParamDef,namedType_Syn_ParamDef :: Maybe Type,newFixedUpIdentifiersTree_Syn_ParamDef :: ParamDef,originalTree_Syn_ParamDef :: ParamDef,paramName_Syn_ParamDef :: ParamName}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIcat _lhsIidenv _lhsIlib _lhsIpos )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOparamName) =
             (sem _lhsIcat _lhsIidenv _lhsIlib _lhsIpos )
     in  (Syn_ParamDef _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnamedType _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOparamName ))
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
              _lhsOnewFixedUpIdentifiersTree :: ParamDef
              _lhsOoriginalTree :: ParamDef
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typInewFixedUpIdentifiersTree :: TypeName
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/CreateFunction.ag"(line 45, column 9)
              _lhsOnamedType =
                  _typInamedType
              -- "./TypeChecking/CreateFunction.ag"(line 47, column 9)
              _lhsOparamName =
                  NamedParam _lhsIpos name_
              -- self rule
              _annotatedTree =
                  ParamDef ann_ name_ _typIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  ParamDef ann_ name_ _typIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  ParamDef ann_ name_ _typInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  ParamDef ann_ name_ _typIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typInewFixedUpIdentifiersTree,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOparamName)))
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
              _lhsOnewFixedUpIdentifiersTree :: ParamDef
              _lhsOoriginalTree :: ParamDef
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typInewFixedUpIdentifiersTree :: TypeName
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/CreateFunction.ag"(line 45, column 9)
              _lhsOnamedType =
                  _typInamedType
              -- "./TypeChecking/CreateFunction.ag"(line 49, column 9)
              _lhsOparamName =
                  UnnamedParam _lhsIpos
              -- self rule
              _annotatedTree =
                  ParamDefTp ann_ _typIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  ParamDefTp ann_ _typIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  ParamDefTp ann_ _typInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  ParamDefTp ann_ _typIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typInewFixedUpIdentifiersTree,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOparamName)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         params               : [(ParamName, Maybe Type)]
   alternatives:
      alternative Cons:
         child hd             : ParamDef 
         child tl             : ParamDefList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                       ( ParamDefList,ParamDefList,ParamDefList,ParamDefList,([(ParamName, Maybe Type)]))
data Inh_ParamDefList  = Inh_ParamDefList {cat_Inh_ParamDefList :: Catalog,idenv_Inh_ParamDefList :: IDEnv,lib_Inh_ParamDefList :: LocalBindings,pos_Inh_ParamDefList :: Int}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList,fixedUpIdentifiersTree_Syn_ParamDefList :: ParamDefList,newFixedUpIdentifiersTree_Syn_ParamDefList :: ParamDefList,originalTree_Syn_ParamDefList :: ParamDefList,params_Syn_ParamDefList :: [(ParamName, Maybe Type)]}
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIcat _lhsIidenv _lhsIlib _lhsIpos )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOparams) =
             (sem _lhsIcat _lhsIidenv _lhsIlib _lhsIpos )
     in  (Syn_ParamDefList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOparams ))
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
              _lhsOnewFixedUpIdentifiersTree :: ParamDefList
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
              _hdInewFixedUpIdentifiersTree :: ParamDef
              _hdIoriginalTree :: ParamDef
              _hdIparamName :: ParamName
              _tlIannotatedTree :: ParamDefList
              _tlIfixedUpIdentifiersTree :: ParamDefList
              _tlInewFixedUpIdentifiersTree :: ParamDefList
              _tlIoriginalTree :: ParamDefList
              _tlIparams :: ([(ParamName, Maybe Type)])
              -- "./TypeChecking/CreateFunction.ag"(line 53, column 13)
              _lhsOparams =
                  ((_hdIparamName, _hdInamedType) : _tlIparams)
              -- "./TypeChecking/CreateFunction.ag"(line 54, column 13)
              _hdOpos =
                  _lhsIpos
              -- "./TypeChecking/CreateFunction.ag"(line 55, column 13)
              _tlOpos =
                  _lhsIpos + 1
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInamedType,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree,_hdIparamName) =
                  (hd_ _hdOcat _hdOidenv _hdOlib _hdOpos )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree,_tlIparams) =
                  (tl_ _tlOcat _tlOidenv _tlOlib _tlOpos )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOparams)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib
       _lhsIpos ->
         (let _lhsOparams :: ([(ParamName, Maybe Type)])
              _lhsOannotatedTree :: ParamDefList
              _lhsOfixedUpIdentifiersTree :: ParamDefList
              _lhsOnewFixedUpIdentifiersTree :: ParamDefList
              _lhsOoriginalTree :: ParamDefList
              -- "./TypeChecking/CreateFunction.ag"(line 52, column 12)
              _lhsOparams =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOparams)))
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
         newFixedUpIdentifiersTree : SELF 
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
            local newFixedUpIdentifiersTree : _
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
            local includeCorrelations : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Values:
         child ann            : {Annotation}
         child vll            : ScalarExprListList 
         visit 0:
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
                    ( QueryExpr,IDEnv,QueryExpr,([LocalBindingsUpdate]),QueryExpr,QueryExpr,(Maybe [(String,Type)]))
data Inh_QueryExpr  = Inh_QueryExpr {cat_Inh_QueryExpr :: Catalog,expectedTypes_Inh_QueryExpr :: [Maybe Type],idenv_Inh_QueryExpr :: IDEnv,lib_Inh_QueryExpr :: LocalBindings}
data Syn_QueryExpr  = Syn_QueryExpr {annotatedTree_Syn_QueryExpr :: QueryExpr,cidenv_Syn_QueryExpr :: IDEnv,fixedUpIdentifiersTree_Syn_QueryExpr :: QueryExpr,libUpdates_Syn_QueryExpr :: [LocalBindingsUpdate],newFixedUpIdentifiersTree_Syn_QueryExpr :: QueryExpr,originalTree_Syn_QueryExpr :: QueryExpr,uType_Syn_QueryExpr :: Maybe [(String,Type)]}
wrap_QueryExpr :: T_QueryExpr  ->
                  Inh_QueryExpr  ->
                  Syn_QueryExpr 
wrap_QueryExpr sem (Inh_QueryExpr _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )
     in  (Syn_QueryExpr _lhsOannotatedTree _lhsOcidenv _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOuType ))
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
              _lhsOnewFixedUpIdentifiersTree :: QueryExpr
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
              _sel1InewFixedUpIdentifiersTree :: QueryExpr
              _sel1IoriginalTree :: QueryExpr
              _sel1IuType :: (Maybe [(String,Type)])
              _sel2IannotatedTree :: QueryExpr
              _sel2Icidenv :: IDEnv
              _sel2IfixedUpIdentifiersTree :: QueryExpr
              _sel2IlibUpdates :: ([LocalBindingsUpdate])
              _sel2InewFixedUpIdentifiersTree :: QueryExpr
              _sel2IoriginalTree :: QueryExpr
              _sel2IuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/QueryStatement.ag"(line 115, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/QueryStatement.ag"(line 142, column 9)
              _tpe =
                  do
                  sel1t <- lmt ((SetOfType . CompositeType) <$> _sel1IuType)
                  sel2t <- lmt ((SetOfType . CompositeType) <$> _sel2IuType)
                  typeCheckCombineSelect _lhsIcat sel1t sel2t
              -- "./TypeChecking/QueryStatement.ag"(line 148, column 9)
              _backTree =
                  CombineSelect ann_ ctype_
                                _sel1IannotatedTree
                                _sel2IannotatedTree
              -- "./TypeChecking/QueryStatement.ag"(line 159, column 9)
              _lhsOuType =
                  etmt (_tpe     >>= unwrapSetOfComposite)
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 246, column 21)
              _lhsOcidenv =
                  _sel1Icidenv
              -- self rule
              _annotatedTree =
                  CombineSelect ann_ ctype_ _sel1IannotatedTree _sel2IannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CombineSelect ann_ ctype_ _sel1IfixedUpIdentifiersTree _sel2IfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CombineSelect ann_ ctype_ _sel1InewFixedUpIdentifiersTree _sel2InewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CombineSelect ann_ ctype_ _sel1IoriginalTree _sel2IoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _sel1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _sel1OexpectedTypes =
                  _lhsIexpectedTypes
              -- copy rule (down)
              _sel1Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _sel1Olib =
                  _lhsIlib
              -- copy rule (down)
              _sel2Ocat =
                  _lhsIcat
              -- copy rule (down)
              _sel2OexpectedTypes =
                  _lhsIexpectedTypes
              -- copy rule (down)
              _sel2Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _sel2Olib =
                  _lhsIlib
              ( _sel1IannotatedTree,_sel1Icidenv,_sel1IfixedUpIdentifiersTree,_sel1IlibUpdates,_sel1InewFixedUpIdentifiersTree,_sel1IoriginalTree,_sel1IuType) =
                  (sel1_ _sel1Ocat _sel1OexpectedTypes _sel1Oidenv _sel1Olib )
              ( _sel2IannotatedTree,_sel2Icidenv,_sel2IfixedUpIdentifiersTree,_sel2IlibUpdates,_sel2InewFixedUpIdentifiersTree,_sel2IoriginalTree,_sel2IuType) =
                  (sel2_ _sel2Ocat _sel2OexpectedTypes _sel2Oidenv _sel2Olib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
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
              _lhsOnewFixedUpIdentifiersTree :: QueryExpr
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
              _selSelectListIlistType :: ([(String,Type)])
              _selSelectListInewFixedUpIdentifiersTree :: SelectList
              _selSelectListIoriginalTree :: SelectList
              _selTrefIannotatedTree :: TableRefList
              _selTrefIcidenv :: IDEnv
              _selTrefIfixedUpIdentifiersTree :: TableRefList
              _selTrefIlibUpdates :: ([LocalBindingsUpdate])
              _selTrefInewFixedUpIdentifiersTree :: TableRefList
              _selTrefIoriginalTree :: TableRefList
              _selWhereIannotatedTree :: MaybeBoolExpr
              _selWhereIfixedUpIdentifiersTree :: MaybeBoolExpr
              _selWhereInewFixedUpIdentifiersTree :: MaybeBoolExpr
              _selWhereIoriginalTree :: MaybeBoolExpr
              _selGroupByIannotatedTree :: ScalarExprList
              _selGroupByIfixedUpIdentifiersTree :: ScalarExprList
              _selGroupByInewFixedUpIdentifiersTree :: ScalarExprList
              _selGroupByIoriginalTree :: ScalarExprList
              _selGroupByItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _selGroupByIuType :: ([Maybe Type])
              _selHavingIannotatedTree :: MaybeBoolExpr
              _selHavingIfixedUpIdentifiersTree :: MaybeBoolExpr
              _selHavingInewFixedUpIdentifiersTree :: MaybeBoolExpr
              _selHavingIoriginalTree :: MaybeBoolExpr
              _selOrderByIannotatedTree :: ScalarExprDirectionPairList
              _selOrderByIfixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _selOrderByInewFixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _selOrderByIoriginalTree :: ScalarExprDirectionPairList
              _selLimitIannotatedTree :: MaybeScalarExpr
              _selLimitIfixedUpIdentifiersTree :: MaybeScalarExpr
              _selLimitInewFixedUpIdentifiersTree :: MaybeScalarExpr
              _selLimitIoriginalTree :: MaybeScalarExpr
              _selLimitIuType :: (Maybe Type)
              _selOffsetIannotatedTree :: MaybeScalarExpr
              _selOffsetIfixedUpIdentifiersTree :: MaybeScalarExpr
              _selOffsetInewFixedUpIdentifiersTree :: MaybeScalarExpr
              _selOffsetIoriginalTree :: MaybeScalarExpr
              _selOffsetIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 615, column 14)
              _selGroupByOexpectedTypes =
                  []
              -- "./TypeChecking/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/QueryStatement.ag"(line 98, column 10)
              _newLib =
                  case foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _selTrefIlibUpdates of
                    Left x -> error $ "selectexpression-select-loc.newlib " ++ show x
                    Right e -> e
              -- "./TypeChecking/QueryStatement.ag"(line 101, column 10)
              _selSelectListOlib =
                  _newLib
              -- "./TypeChecking/QueryStatement.ag"(line 102, column 10)
              _selWhereOlib =
                  _newLib
              -- "./TypeChecking/QueryStatement.ag"(line 103, column 10)
              _selGroupByOlib =
                  _newLib
              -- "./TypeChecking/QueryStatement.ag"(line 104, column 10)
              _selOrderByOlib =
                  _newLib
              -- "./TypeChecking/QueryStatement.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  _selSelectListIlibUpdates
              -- "./TypeChecking/QueryStatement.ag"(line 129, column 9)
              _tpe =
                  Right $ SetOfType $ CompositeType _selSelectListIlistType
              -- "./TypeChecking/QueryStatement.ag"(line 131, column 9)
              _backTree =
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
              -- "./TypeChecking/QueryStatement.ag"(line 159, column 9)
              _lhsOuType =
                  etmt (_tpe     >>= unwrapSetOfComposite)
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 199, column 9)
              _lhsOfixedUpIdentifiersTree =
                  Select ann_
                         selDistinct_
                         _selSelectListIfixedUpIdentifiersTree
                         _selTrefIfixedUpIdentifiersTree
                         _selWhereInewFixedUpIdentifiersTree
                         _selGroupByInewFixedUpIdentifiersTree
                         _selHavingInewFixedUpIdentifiersTree
                         _selOrderByInewFixedUpIdentifiersTree
                         _selLimitIfixedUpIdentifiersTree
                         _selOffsetIfixedUpIdentifiersTree
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 245, column 14)
              _lhsOcidenv =
                  _selSelectListIcidenv
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 277, column 14)
              _includeCorrelations =
                  joinIDEnvs _lhsIidenv _selTrefIcidenv
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 278, column 14)
              _selSelectListOidenv =
                  _selTrefIcidenv
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 279, column 14)
              _selWhereOidenv =
                  _includeCorrelations
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 280, column 14)
              _selGroupByOidenv =
                  _selTrefIcidenv
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 281, column 14)
              _selHavingOidenv =
                  _includeCorrelations
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 282, column 14)
              _selOrderByOidenv =
                  _selTrefIcidenv
              -- self rule
              _annotatedTree =
                  Select ann_ selDistinct_ _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Select ann_ selDistinct_ _selSelectListIfixedUpIdentifiersTree _selTrefIfixedUpIdentifiersTree _selWhereIfixedUpIdentifiersTree _selGroupByIfixedUpIdentifiersTree _selHavingIfixedUpIdentifiersTree _selOrderByIfixedUpIdentifiersTree _selLimitIfixedUpIdentifiersTree _selOffsetIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Select ann_ selDistinct_ _selSelectListInewFixedUpIdentifiersTree _selTrefInewFixedUpIdentifiersTree _selWhereInewFixedUpIdentifiersTree _selGroupByInewFixedUpIdentifiersTree _selHavingInewFixedUpIdentifiersTree _selOrderByInewFixedUpIdentifiersTree _selLimitInewFixedUpIdentifiersTree _selOffsetInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Select ann_ selDistinct_ _selSelectListIoriginalTree _selTrefIoriginalTree _selWhereIoriginalTree _selGroupByIoriginalTree _selHavingIoriginalTree _selOrderByIoriginalTree _selLimitIoriginalTree _selOffsetIoriginalTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selSelectListOcat =
                  _lhsIcat
              -- copy rule (down)
              _selTrefOcat =
                  _lhsIcat
              -- copy rule (down)
              _selTrefOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _selTrefOlib =
                  _lhsIlib
              -- copy rule (down)
              _selWhereOcat =
                  _lhsIcat
              -- copy rule (down)
              _selGroupByOcat =
                  _lhsIcat
              -- copy rule (down)
              _selHavingOcat =
                  _lhsIcat
              -- copy rule (down)
              _selHavingOlib =
                  _lhsIlib
              -- copy rule (down)
              _selOrderByOcat =
                  _lhsIcat
              -- copy rule (down)
              _selLimitOcat =
                  _lhsIcat
              -- copy rule (down)
              _selLimitOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _selLimitOlib =
                  _lhsIlib
              -- copy rule (down)
              _selOffsetOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOffsetOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _selOffsetOlib =
                  _lhsIlib
              ( _selSelectListIannotatedTree,_selSelectListIcidenv,_selSelectListIfixedUpIdentifiersTree,_selSelectListIlibUpdates,_selSelectListIlistType,_selSelectListInewFixedUpIdentifiersTree,_selSelectListIoriginalTree) =
                  (selSelectList_ _selSelectListOcat _selSelectListOidenv _selSelectListOlib )
              ( _selTrefIannotatedTree,_selTrefIcidenv,_selTrefIfixedUpIdentifiersTree,_selTrefIlibUpdates,_selTrefInewFixedUpIdentifiersTree,_selTrefIoriginalTree) =
                  (selTref_ _selTrefOcat _selTrefOidenv _selTrefOlib )
              ( _selWhereIannotatedTree,_selWhereIfixedUpIdentifiersTree,_selWhereInewFixedUpIdentifiersTree,_selWhereIoriginalTree) =
                  (selWhere_ _selWhereOcat _selWhereOidenv _selWhereOlib )
              ( _selGroupByIannotatedTree,_selGroupByIfixedUpIdentifiersTree,_selGroupByInewFixedUpIdentifiersTree,_selGroupByIoriginalTree,_selGroupByItbUTypes,_selGroupByIuType) =
                  (selGroupBy_ _selGroupByOcat _selGroupByOexpectedTypes _selGroupByOidenv _selGroupByOlib )
              ( _selHavingIannotatedTree,_selHavingIfixedUpIdentifiersTree,_selHavingInewFixedUpIdentifiersTree,_selHavingIoriginalTree) =
                  (selHaving_ _selHavingOcat _selHavingOidenv _selHavingOlib )
              ( _selOrderByIannotatedTree,_selOrderByIfixedUpIdentifiersTree,_selOrderByInewFixedUpIdentifiersTree,_selOrderByIoriginalTree) =
                  (selOrderBy_ _selOrderByOcat _selOrderByOidenv _selOrderByOlib )
              ( _selLimitIannotatedTree,_selLimitIfixedUpIdentifiersTree,_selLimitInewFixedUpIdentifiersTree,_selLimitIoriginalTree,_selLimitIuType) =
                  (selLimit_ _selLimitOcat _selLimitOidenv _selLimitOlib )
              ( _selOffsetIannotatedTree,_selOffsetIfixedUpIdentifiersTree,_selOffsetInewFixedUpIdentifiersTree,_selOffsetIoriginalTree,_selOffsetIuType) =
                  (selOffset_ _selOffsetOcat _selOffsetOidenv _selOffsetOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
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
              _lhsOnewFixedUpIdentifiersTree :: QueryExpr
              _lhsOoriginalTree :: QueryExpr
              _vllOcat :: Catalog
              _vllOidenv :: IDEnv
              _vllOlib :: LocalBindings
              _vllIannotatedTree :: ScalarExprListList
              _vllIfixedUpIdentifiersTree :: ScalarExprListList
              _vllInewFixedUpIdentifiersTree :: ScalarExprListList
              _vllIoriginalTree :: ScalarExprListList
              _vllIuType :: ([[Maybe Type]])
              -- "./TypeChecking/ScalarExprs.ag"(line 629, column 14)
              _vllOexpectedTypes =
                  _lhsIexpectedTypes
              -- "./TypeChecking/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/QueryStatement.ag"(line 115, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/QueryStatement.ag"(line 124, column 9)
              _tpe =
                  typeCheckValuesExpr
                              _lhsIcat
                              _vllIuType
              -- "./TypeChecking/QueryStatement.ag"(line 127, column 9)
              _backTree =
                  Values ann_ _vllIannotatedTree
              -- "./TypeChecking/QueryStatement.ag"(line 159, column 9)
              _lhsOuType =
                  etmt (_tpe     >>= unwrapSetOfComposite)
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 247, column 14)
              _lhsOcidenv =
                  unimplementedIDEnv
              -- self rule
              _annotatedTree =
                  Values ann_ _vllIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Values ann_ _vllIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Values ann_ _vllInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Values ann_ _vllIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _vllOcat =
                  _lhsIcat
              -- copy rule (down)
              _vllOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _vllOlib =
                  _lhsIlib
              ( _vllIannotatedTree,_vllIfixedUpIdentifiersTree,_vllInewFixedUpIdentifiersTree,_vllIoriginalTree,_vllIuType) =
                  (vll_ _vllOcat _vllOexpectedTypes _vllOidenv _vllOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
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
              _lhsOnewFixedUpIdentifiersTree :: QueryExpr
              _lhsOoriginalTree :: QueryExpr
              _withsOcat :: Catalog
              _withsOidenv :: IDEnv
              _withsOlib :: LocalBindings
              _exOexpectedTypes :: ([Maybe Type])
              _exOidenv :: IDEnv
              _exOlib :: LocalBindings
              _withsIannotatedTree :: WithQueryList
              _withsIfixedUpIdentifiersTree :: WithQueryList
              _withsInewFixedUpIdentifiersTree :: WithQueryList
              _withsIoriginalTree :: WithQueryList
              _withsIproducedCat :: Catalog
              _exIannotatedTree :: QueryExpr
              _exIcidenv :: IDEnv
              _exIfixedUpIdentifiersTree :: QueryExpr
              _exIlibUpdates :: ([LocalBindingsUpdate])
              _exInewFixedUpIdentifiersTree :: QueryExpr
              _exIoriginalTree :: QueryExpr
              _exIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/QueryStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/QueryStatement.ag"(line 119, column 9)
              _lhsOlibUpdates =
                  _exIlibUpdates
              -- "./TypeChecking/QueryStatement.ag"(line 152, column 9)
              _tpe =
                  lmt ((SetOfType . CompositeType) <$> _exIuType)
              -- "./TypeChecking/QueryStatement.ag"(line 153, column 9)
              _backTree =
                  WithSelect ann_ _withsIannotatedTree _exIannotatedTree
              -- "./TypeChecking/QueryStatement.ag"(line 154, column 9)
              _exOcat =
                  _withsIproducedCat
              -- "./TypeChecking/QueryStatement.ag"(line 155, column 9)
              _withsOcatUpdates =
                  []
              -- "./TypeChecking/QueryStatement.ag"(line 159, column 9)
              _lhsOuType =
                  etmt (_tpe     >>= unwrapSetOfComposite)
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 248, column 18)
              _lhsOcidenv =
                  _exIcidenv
              -- self rule
              _annotatedTree =
                  WithSelect ann_ _withsIannotatedTree _exIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  WithSelect ann_ _withsIfixedUpIdentifiersTree _exIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  WithSelect ann_ _withsInewFixedUpIdentifiersTree _exInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  WithSelect ann_ _withsIoriginalTree _exIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _withsOcat =
                  _lhsIcat
              -- copy rule (down)
              _withsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _withsOlib =
                  _lhsIlib
              -- copy rule (down)
              _exOexpectedTypes =
                  _lhsIexpectedTypes
              -- copy rule (down)
              _exOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exOlib =
                  _lhsIlib
              ( _withsIannotatedTree,_withsIfixedUpIdentifiersTree,_withsInewFixedUpIdentifiersTree,_withsIoriginalTree,_withsIproducedCat) =
                  (withs_ _withsOcat _withsOcatUpdates _withsOidenv _withsOlib )
              ( _exIannotatedTree,_exIcidenv,_exIfixedUpIdentifiersTree,_exIlibUpdates,_exInewFixedUpIdentifiersTree,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedTypes _exOidenv _exOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         producedCat          : Catalog
         producedLib          : LocalBindings
   alternatives:
      alternative Root:
         child statements     : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
               ( Root,Root,Root,Root,Catalog,LocalBindings)
data Inh_Root  = Inh_Root {cat_Inh_Root :: Catalog,idenv_Inh_Root :: IDEnv,lib_Inh_Root :: LocalBindings}
data Syn_Root  = Syn_Root {annotatedTree_Syn_Root :: Root,fixedUpIdentifiersTree_Syn_Root :: Root,newFixedUpIdentifiersTree_Syn_Root :: Root,originalTree_Syn_Root :: Root,producedCat_Syn_Root :: Catalog,producedLib_Syn_Root :: LocalBindings}
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_Root _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOproducedCat _lhsOproducedLib ))
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
              _lhsOnewFixedUpIdentifiersTree :: Root
              _lhsOoriginalTree :: Root
              _lhsOproducedCat :: Catalog
              _lhsOproducedLib :: LocalBindings
              _statementsOcat :: Catalog
              _statementsOidenv :: IDEnv
              _statementsOlib :: LocalBindings
              _statementsIannotatedTree :: StatementList
              _statementsIfixedUpIdentifiersTree :: StatementList
              _statementsInewFixedUpIdentifiersTree :: StatementList
              _statementsIoriginalTree :: StatementList
              _statementsIproducedCat :: Catalog
              _statementsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 107, column 12)
              _statementsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 108, column 12)
              _statementsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Root _statementsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Root _statementsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Root _statementsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Root _statementsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (up)
              _lhsOproducedCat =
                  _statementsIproducedCat
              -- copy rule (up)
              _lhsOproducedLib =
                  _statementsIproducedLib
              -- copy rule (down)
              _statementsOcat =
                  _lhsIcat
              -- copy rule (down)
              _statementsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _statementsOlib =
                  _lhsIlib
              ( _statementsIannotatedTree,_statementsIfixedUpIdentifiersTree,_statementsInewFixedUpIdentifiersTree,_statementsIoriginalTree,_statementsIproducedCat,_statementsIproducedLib) =
                  (statements_ _statementsOcat _statementsOcatUpdates _statementsOidenv _statementsOlib _statementsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative NotNullConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative NullConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative RowCheckConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative RowPrimaryKeyConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative RowUniqueConstraint:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                        ( RowConstraint,RowConstraint,RowConstraint,RowConstraint)
data Inh_RowConstraint  = Inh_RowConstraint {cat_Inh_RowConstraint :: Catalog,idenv_Inh_RowConstraint :: IDEnv,lib_Inh_RowConstraint :: LocalBindings}
data Syn_RowConstraint  = Syn_RowConstraint {annotatedTree_Syn_RowConstraint :: RowConstraint,fixedUpIdentifiersTree_Syn_RowConstraint :: RowConstraint,newFixedUpIdentifiersTree_Syn_RowConstraint :: RowConstraint,originalTree_Syn_RowConstraint :: RowConstraint}
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_RowConstraint _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_RowConstraint_NotNullConstraint :: Annotation ->
                                       String ->
                                       T_RowConstraint 
sem_RowConstraint_NotNullConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOfixedUpIdentifiersTree :: RowConstraint
              _lhsOnewFixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  NotNullConstraint ann_ name_
              -- self rule
              _fixedUpIdentifiersTree =
                  NotNullConstraint ann_ name_
              -- self rule
              _newFixedUpIdentifiersTree =
                  NotNullConstraint ann_ name_
              -- self rule
              _originalTree =
                  NotNullConstraint ann_ name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_RowConstraint_NullConstraint :: Annotation ->
                                    String ->
                                    T_RowConstraint 
sem_RowConstraint_NullConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOfixedUpIdentifiersTree :: RowConstraint
              _lhsOnewFixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  NullConstraint ann_ name_
              -- self rule
              _fixedUpIdentifiersTree =
                  NullConstraint ann_ name_
              -- self rule
              _newFixedUpIdentifiersTree =
                  NullConstraint ann_ name_
              -- self rule
              _originalTree =
                  NullConstraint ann_ name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 564, column 26)
              _exprOexpectedType =
                  Nothing
              -- self rule
              _annotatedTree =
                  RowCheckConstraint ann_ name_ _exprIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  RowCheckConstraint ann_ name_ _exprIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  RowCheckConstraint ann_ name_ _exprInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  RowCheckConstraint ann_ name_ _exprIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_RowConstraint_RowPrimaryKeyConstraint :: Annotation ->
                                             String ->
                                             T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOfixedUpIdentifiersTree :: RowConstraint
              _lhsOnewFixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  RowPrimaryKeyConstraint ann_ name_
              -- self rule
              _fixedUpIdentifiersTree =
                  RowPrimaryKeyConstraint ann_ name_
              -- self rule
              _newFixedUpIdentifiersTree =
                  RowPrimaryKeyConstraint ann_ name_
              -- self rule
              _originalTree =
                  RowPrimaryKeyConstraint ann_ name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
              -- self rule
              _fixedUpIdentifiersTree =
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
              -- self rule
              _newFixedUpIdentifiersTree =
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
              -- self rule
              _originalTree =
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_RowConstraint_RowUniqueConstraint :: Annotation ->
                                         String ->
                                         T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOfixedUpIdentifiersTree :: RowConstraint
              _lhsOnewFixedUpIdentifiersTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  RowUniqueConstraint ann_ name_
              -- self rule
              _fixedUpIdentifiersTree =
                  RowUniqueConstraint ann_ name_
              -- self rule
              _newFixedUpIdentifiersTree =
                  RowUniqueConstraint ann_ name_
              -- self rule
              _originalTree =
                  RowUniqueConstraint ann_ name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : RowConstraint 
         child tl             : RowConstraintList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                            ( RowConstraintList,RowConstraintList,RowConstraintList,RowConstraintList)
data Inh_RowConstraintList  = Inh_RowConstraintList {cat_Inh_RowConstraintList :: Catalog,idenv_Inh_RowConstraintList :: IDEnv,lib_Inh_RowConstraintList :: LocalBindings}
data Syn_RowConstraintList  = Syn_RowConstraintList {annotatedTree_Syn_RowConstraintList :: RowConstraintList,fixedUpIdentifiersTree_Syn_RowConstraintList :: RowConstraintList,newFixedUpIdentifiersTree_Syn_RowConstraintList :: RowConstraintList,originalTree_Syn_RowConstraintList :: RowConstraintList}
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_RowConstraintList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraintList
              _lhsOfixedUpIdentifiersTree :: RowConstraintList
              _lhsOnewFixedUpIdentifiersTree :: RowConstraintList
              _lhsOoriginalTree :: RowConstraintList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: RowConstraint
              _hdIfixedUpIdentifiersTree :: RowConstraint
              _hdInewFixedUpIdentifiersTree :: RowConstraint
              _hdIoriginalTree :: RowConstraint
              _tlIannotatedTree :: RowConstraintList
              _tlIfixedUpIdentifiersTree :: RowConstraintList
              _tlInewFixedUpIdentifiersTree :: RowConstraintList
              _tlIoriginalTree :: RowConstraintList
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: RowConstraintList
              _lhsOfixedUpIdentifiersTree :: RowConstraintList
              _lhsOnewFixedUpIdentifiersTree :: RowConstraintList
              _lhsOoriginalTree :: RowConstraintList
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         ntAnnotatedTree      : ScalarExpr 
         ntType               : [(String,Type)]
         originalTree         : SELF 
         tbAnnotatedTree      : ScalarExpr 
         tbUType              : Maybe ([(String,Type)],[(String,Type)])
         uType                : Maybe Type
   alternatives:
      alternative BooleanLit:
         child ann            : {Annotation}
         child b              : {Bool}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Case:
         child ann            : {Annotation}
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local whenTypes   : _
            local thenTypes   : _
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative CaseSimple:
         child ann            : {Annotation}
         child value          : ScalarExpr 
         child cases          : CaseScalarExprListScalarExprPairList 
         child els            : MaybeScalarExpr 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local whenTypes   : _
            local thenTypes   : _
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Cast:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         child tn             : TypeName 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Exists:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local liftedColumnName : _
            local ntType      : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Extract:
         child ann            : {Annotation}
         child field          : {ExtractField}
         child e              : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative FloatLit:
         child ann            : {Annotation}
         child d              : {Double}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative FunCall:
         child ann            : {Annotation}
         child funName        : {String}
         child args           : ScalarExprList 
         visit 0:
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local _tup1       : _
            local tpe         : {Et}
            local prototype   : _
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Identifier:
         child ann            : {Annotation}
         child i              : {String}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local ntType      : {E [(String,Type)]}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative InPredicate:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         child i              : {Bool}
         child list           : InList 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local liftedColumnName : _
            local ntType      : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative IntegerLit:
         child ann            : {Annotation}
         child i              : {Integer}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Interval:
         child ann            : {Annotation}
         child value          : {String}
         child field          : {IntervalField}
         child prec           : {Maybe Int}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative LiftOperator:
         child ann            : {Annotation}
         child oper           : {String}
         child flav           : {LiftFlavour}
         child args           : ScalarExprList 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative NullLit:
         child ann            : {Annotation}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Placeholder:
         child ann            : {Annotation}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local liftedColumnName : _
            local ntType      : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative PositionalArg:
         child ann            : {Annotation}
         child p              : {Integer}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local liftedColumnName : _
            local ntType      : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative QIdentifier:
         child ann            : {Annotation}
         child qual           : ScalarExpr 
         child i              : {String}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local qid         : {Maybe String}
            local backTree    : _
            local qAnnTreeNoUnrec : _
            local ntType      : {E [(String,Type)]}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative ScalarSubQuery:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local liftedColumnName : _
            local ntType      : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative StringLit:
         child ann            : {Annotation}
         child value          : {String}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative TypedStringLit:
         child ann            : {Annotation}
         child tn             : TypeName 
         child value          : {String}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                     ( ScalarExpr,ScalarExpr,ScalarExpr,ScalarExpr,([(String,Type)]),ScalarExpr,ScalarExpr,(Maybe ([(String,Type)],[(String,Type)])),(Maybe Type))
data Inh_ScalarExpr  = Inh_ScalarExpr {cat_Inh_ScalarExpr :: Catalog,expectedType_Inh_ScalarExpr :: Maybe Type,idenv_Inh_ScalarExpr :: IDEnv,lib_Inh_ScalarExpr :: LocalBindings}
data Syn_ScalarExpr  = Syn_ScalarExpr {annotatedTree_Syn_ScalarExpr :: ScalarExpr,fixedUpIdentifiersTree_Syn_ScalarExpr :: ScalarExpr,newFixedUpIdentifiersTree_Syn_ScalarExpr :: ScalarExpr,ntAnnotatedTree_Syn_ScalarExpr :: ScalarExpr,ntType_Syn_ScalarExpr :: [(String,Type)],originalTree_Syn_ScalarExpr :: ScalarExpr,tbAnnotatedTree_Syn_ScalarExpr :: ScalarExpr,tbUType_Syn_ScalarExpr :: Maybe ([(String,Type)],[(String,Type)]),uType_Syn_ScalarExpr :: Maybe Type}
wrap_ScalarExpr :: T_ScalarExpr  ->
                   Inh_ScalarExpr  ->
                   Syn_ScalarExpr 
wrap_ScalarExpr sem (Inh_ScalarExpr _lhsIcat _lhsIexpectedType _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedType _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExpr _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOntAnnotatedTree _lhsOntType _lhsOoriginalTree _lhsOtbAnnotatedTree _lhsOtbUType _lhsOuType ))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 176, column 19)
              _tpe =
                  Right typeBool
              -- "./TypeChecking/ScalarExprs.ag"(line 188, column 9)
              _backTree =
                  BooleanLit ann_ b_
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- self rule
              _annotatedTree =
                  BooleanLit ann_ b_
              -- self rule
              _fixedUpIdentifiersTree =
                  BooleanLit ann_ b_
              -- self rule
              _newFixedUpIdentifiersTree =
                  BooleanLit ann_ b_
              -- self rule
              _originalTree =
                  BooleanLit ann_ b_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _casesOcat :: Catalog
              _casesOidenv :: IDEnv
              _casesOlib :: LocalBindings
              _elsOcat :: Catalog
              _elsOidenv :: IDEnv
              _elsOlib :: LocalBindings
              _casesIannotatedTree :: CaseScalarExprListScalarExprPairList
              _casesIfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _casesInewFixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _casesIoriginalTree :: CaseScalarExprListScalarExprPairList
              _casesIthenTypes :: ([Maybe Type])
              _casesIwhenTypes :: ([[Maybe Type]])
              _elsIannotatedTree :: MaybeScalarExpr
              _elsIfixedUpIdentifiersTree :: MaybeScalarExpr
              _elsInewFixedUpIdentifiersTree :: MaybeScalarExpr
              _elsIoriginalTree :: MaybeScalarExpr
              _elsIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 299, column 9)
              _whenTypes =
                  _casesIwhenTypes
              -- "./TypeChecking/ScalarExprs.ag"(line 300, column 9)
              _thenTypes =
                  _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIuType
              -- "./TypeChecking/ScalarExprs.ag"(line 304, column 9)
              _tpe =
                  do
                  wt <- mapM lmt $ concat _whenTypes
                  errorWhen (any (/= typeBool) wt)
                      [WrongTypes typeBool wt]
                  tt <- mapM lmt _thenTypes
                  resolveResultSetType _lhsIcat tt
              -- "./TypeChecking/ScalarExprs.ag"(line 312, column 9)
              _backTree =
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- self rule
              _annotatedTree =
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Case ann_ _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Case ann_ _casesInewFixedUpIdentifiersTree _elsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Case ann_ _casesIoriginalTree _elsIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _casesOcat =
                  _lhsIcat
              -- copy rule (down)
              _casesOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _casesOlib =
                  _lhsIlib
              -- copy rule (down)
              _elsOcat =
                  _lhsIcat
              -- copy rule (down)
              _elsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _elsOlib =
                  _lhsIlib
              ( _casesIannotatedTree,_casesIfixedUpIdentifiersTree,_casesInewFixedUpIdentifiersTree,_casesIoriginalTree,_casesIthenTypes,_casesIwhenTypes) =
                  (cases_ _casesOcat _casesOidenv _casesOlib )
              ( _elsIannotatedTree,_elsIfixedUpIdentifiersTree,_elsInewFixedUpIdentifiersTree,_elsIoriginalTree,_elsIuType) =
                  (els_ _elsOcat _elsOidenv _elsOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
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
              _valueInewFixedUpIdentifiersTree :: ScalarExpr
              _valueIntAnnotatedTree :: ScalarExpr
              _valueIntType :: ([(String,Type)])
              _valueIoriginalTree :: ScalarExpr
              _valueItbAnnotatedTree :: ScalarExpr
              _valueItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _valueIuType :: (Maybe Type)
              _casesIannotatedTree :: CaseScalarExprListScalarExprPairList
              _casesIfixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _casesInewFixedUpIdentifiersTree :: CaseScalarExprListScalarExprPairList
              _casesIoriginalTree :: CaseScalarExprListScalarExprPairList
              _casesIthenTypes :: ([Maybe Type])
              _casesIwhenTypes :: ([[Maybe Type]])
              _elsIannotatedTree :: MaybeScalarExpr
              _elsIfixedUpIdentifiersTree :: MaybeScalarExpr
              _elsInewFixedUpIdentifiersTree :: MaybeScalarExpr
              _elsIoriginalTree :: MaybeScalarExpr
              _elsIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 299, column 9)
              _whenTypes =
                  _casesIwhenTypes
              -- "./TypeChecking/ScalarExprs.ag"(line 300, column 9)
              _thenTypes =
                  _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIuType
              -- "./TypeChecking/ScalarExprs.ag"(line 317, column 9)
              _tpe =
                  do
                  wt <- mapM lmt $ concat _whenTypes
                  vt <- lmt _valueIuType
                  _ <- resolveResultSetType _lhsIcat (vt : wt)
                  tt <- mapM lmt _thenTypes
                  resolveResultSetType _lhsIcat tt
              -- "./TypeChecking/ScalarExprs.ag"(line 324, column 9)
              _backTree =
                  CaseSimple ann_
                             _valueIannotatedTree
                             _casesIannotatedTree
                             _elsIannotatedTree
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- self rule
              _annotatedTree =
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CaseSimple ann_ _valueIfixedUpIdentifiersTree _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CaseSimple ann_ _valueInewFixedUpIdentifiersTree _casesInewFixedUpIdentifiersTree _elsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CaseSimple ann_ _valueIoriginalTree _casesIoriginalTree _elsIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _valueOcat =
                  _lhsIcat
              -- copy rule (down)
              _valueOexpectedType =
                  _lhsIexpectedType
              -- copy rule (down)
              _valueOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _valueOlib =
                  _lhsIlib
              -- copy rule (down)
              _casesOcat =
                  _lhsIcat
              -- copy rule (down)
              _casesOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _casesOlib =
                  _lhsIlib
              -- copy rule (down)
              _elsOcat =
                  _lhsIcat
              -- copy rule (down)
              _elsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _elsOlib =
                  _lhsIlib
              ( _valueIannotatedTree,_valueIfixedUpIdentifiersTree,_valueInewFixedUpIdentifiersTree,_valueIntAnnotatedTree,_valueIntType,_valueIoriginalTree,_valueItbAnnotatedTree,_valueItbUType,_valueIuType) =
                  (value_ _valueOcat _valueOexpectedType _valueOidenv _valueOlib )
              ( _casesIannotatedTree,_casesIfixedUpIdentifiersTree,_casesInewFixedUpIdentifiersTree,_casesIoriginalTree,_casesIthenTypes,_casesIwhenTypes) =
                  (cases_ _casesOcat _casesOidenv _casesOlib )
              ( _elsIannotatedTree,_elsIfixedUpIdentifiersTree,_elsInewFixedUpIdentifiersTree,_elsIoriginalTree,_elsIuType) =
                  (els_ _elsOcat _elsOidenv _elsOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
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
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              _tnIannotatedTree :: TypeName
              _tnIfixedUpIdentifiersTree :: TypeName
              _tnInamedType :: (Maybe Type)
              _tnInewFixedUpIdentifiersTree :: TypeName
              _tnIoriginalTree :: TypeName
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 205, column 12)
              _tpe =
                  lmt _tnInamedType
              -- "./TypeChecking/ScalarExprs.ag"(line 206, column 12)
              _backTree =
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
              -- "./TypeChecking/ScalarExprs.ag"(line 400, column 7)
              _liftedColumnName =
                  case _tnIannotatedTree of
                    SimpleTypeName _ tn -> tn
                    _ -> ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- self rule
              _annotatedTree =
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Cast ann_ _exprIfixedUpIdentifiersTree _tnIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Cast ann_ _exprInewFixedUpIdentifiersTree _tnInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Cast ann_ _exprIoriginalTree _tnIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOexpectedType =
                  _lhsIexpectedType
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _tnOcat =
                  _lhsIcat
              -- copy rule (down)
              _tnOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tnOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
              ( _tnIannotatedTree,_tnIfixedUpIdentifiersTree,_tnInamedType,_tnInewFixedUpIdentifiersTree,_tnIoriginalTree) =
                  (tn_ _tnOcat _tnOidenv _tnOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selInewFixedUpIdentifiersTree :: QueryExpr
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/ScalarExprs.ag"(line 454, column 9)
              _tpe =
                  Right typeBool
              -- "./TypeChecking/ScalarExprs.ag"(line 455, column 9)
              _backTree =
                  Exists ann_ _selIannotatedTree
              -- "./TypeChecking/ScalarExprs.ag"(line 636, column 29)
              _selOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  Exists ann_ _selIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Exists ann_ _selIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Exists ann_ _selInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Exists ann_ _selIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selInewFixedUpIdentifiersTree,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _eOcat :: Catalog
              _eOexpectedType :: (Maybe Type)
              _eOidenv :: IDEnv
              _eOlib :: LocalBindings
              _eIannotatedTree :: ScalarExpr
              _eIfixedUpIdentifiersTree :: ScalarExpr
              _eInewFixedUpIdentifiersTree :: ScalarExpr
              _eIntAnnotatedTree :: ScalarExpr
              _eIntType :: ([(String,Type)])
              _eIoriginalTree :: ScalarExpr
              _eItbAnnotatedTree :: ScalarExpr
              _eItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _eIuType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  Extract ann_ field_ _eIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Extract ann_ field_ _eIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Extract ann_ field_ _eInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Extract ann_ field_ _eIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (up)
              _lhsOntAnnotatedTree =
                  _eIntAnnotatedTree
              -- copy rule (up)
              _lhsOntType =
                  _eIntType
              -- copy rule (up)
              _lhsOtbAnnotatedTree =
                  _eItbAnnotatedTree
              -- copy rule (up)
              _lhsOtbUType =
                  _eItbUType
              -- copy rule (up)
              _lhsOuType =
                  _eIuType
              -- copy rule (down)
              _eOcat =
                  _lhsIcat
              -- copy rule (down)
              _eOexpectedType =
                  _lhsIexpectedType
              -- copy rule (down)
              _eOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _eOlib =
                  _lhsIlib
              ( _eIannotatedTree,_eIfixedUpIdentifiersTree,_eInewFixedUpIdentifiersTree,_eIntAnnotatedTree,_eIntType,_eIoriginalTree,_eItbAnnotatedTree,_eItbUType,_eIuType) =
                  (e_ _eOcat _eOexpectedType _eOidenv _eOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 175, column 17)
              _tpe =
                  Right typeNumeric
              -- "./TypeChecking/ScalarExprs.ag"(line 186, column 9)
              _backTree =
                  FloatLit ann_ d_
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- self rule
              _annotatedTree =
                  FloatLit ann_ d_
              -- self rule
              _fixedUpIdentifiersTree =
                  FloatLit ann_ d_
              -- self rule
              _newFixedUpIdentifiersTree =
                  FloatLit ann_ d_
              -- self rule
              _originalTree =
                  FloatLit ann_ d_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _argsOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _argsOcat :: Catalog
              _argsOidenv :: IDEnv
              _argsOlib :: LocalBindings
              _argsIannotatedTree :: ScalarExprList
              _argsIfixedUpIdentifiersTree :: ScalarExprList
              _argsInewFixedUpIdentifiersTree :: ScalarExprList
              _argsIoriginalTree :: ScalarExprList
              _argsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _argsIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 88, column 9)
              _tbUType =
                  case (funName_,_argsItbUTypes) of
                   (".", [_,Just t]) -> Right t
                   _ -> Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 219, column 9)
              __tup1 =
                  either (\e -> (Left e, Nothing)) id $ do
                  args <- mapM lmt _argsIuType
                  efp <- findCallMatch _lhsIcat
                                       funName_
                                       args
                  let (_,_,r,_) = efp
                  return (Right r, Just efp)
              -- "./TypeChecking/ScalarExprs.ag"(line 219, column 9)
              (_tpe,_) =
                  __tup1
              -- "./TypeChecking/ScalarExprs.ag"(line 219, column 9)
              (_,_prototype) =
                  __tup1
              -- "./TypeChecking/ScalarExprs.ag"(line 229, column 9)
              _backTree =
                  FunCall ann_ funName_ _argsIannotatedTree
              -- "./TypeChecking/ScalarExprs.ag"(line 394, column 7)
              _liftedColumnName =
                  case funName_ of
                    "." -> getName _backTree
                    x | isOperatorName x -> "?column?"
                    _ -> funName_
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/ScalarExprs.ag"(line 595, column 9)
              _argsOexpectedTypes =
                  maybe [] id $
                  case (funName_,_lhsIexpectedType) of
                    ("!rowctor", Just (AnonymousRecordType ts)) -> return $ map Just ts
                    _ -> do
                         (_,t,_,_) <- _prototype
                         return $ map Just t
              -- self rule
              _annotatedTree =
                  FunCall ann_ funName_ _argsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  FunCall ann_ funName_ _argsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  FunCall ann_ funName_ _argsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  FunCall ann_ funName_ _argsIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _argsOcat =
                  _lhsIcat
              -- copy rule (down)
              _argsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _argsOlib =
                  _lhsIlib
              ( _argsIannotatedTree,_argsIfixedUpIdentifiersTree,_argsInewFixedUpIdentifiersTree,_argsIoriginalTree,_argsItbUTypes,_argsIuType) =
                  (args_ _argsOcat _argsOexpectedTypes _argsOidenv _argsOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _ntType :: (E [(String,Type)])
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 92, column 9)
              _tbUType =
                  catCompositeAttrsPair _lhsIcat relationComposites i_
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 357, column 9)
              _tpe =
                  unwrapLookup <$> lbLookupID _lhsIlib [i_]
              -- "./TypeChecking/ScalarExprs.ag"(line 358, column 9)
              _backTree =
                  Identifier ann_ i_
              -- "./TypeChecking/ScalarExprs.ag"(line 361, column 9)
              _ntType =
                  if i_ == "*"
                  then unwrapStar <$> lbExpandStar _lhsIlib ""
                  else (\t -> [(i_, t)]) <$> unwrapLookup <$> lbLookupID _lhsIlib [i_]
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 156, column 9)
              _lhsOnewFixedUpIdentifiersTree =
                  case qualifyID _lhsIidenv i_ of
                    Nothing -> Identifier ann_ i_
                    Just (t,i) -> QIdentifier ann_ (Identifier ann_ t) i
              -- self rule
              _annotatedTree =
                  Identifier ann_ i_
              -- self rule
              _fixedUpIdentifiersTree =
                  Identifier ann_ i_
              -- self rule
              _newFixedUpIdentifiersTree =
                  Identifier ann_ i_
              -- self rule
              _originalTree =
                  Identifier ann_ i_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
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
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              _listIannotatedTree :: InList
              _listIfixedUpIdentifiersTree :: InList
              _listIlistType :: (Either [TypeError] Type)
              _listInewFixedUpIdentifiersTree :: InList
              _listIoriginalTree :: InList
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/ScalarExprs.ag"(line 482, column 9)
              _tpe =
                  do
                  lt <- _listIlistType
                  expt <- lmt _exprIuType
                  _ <- resolveResultSetType _lhsIcat [expt, lt]
                  return typeBool
              -- "./TypeChecking/ScalarExprs.ag"(line 487, column 9)
              _backTree =
                  InPredicate ann_
                              _exprIannotatedTree
                              i_
                              _listIannotatedTree
              -- self rule
              _annotatedTree =
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  InPredicate ann_ _exprIfixedUpIdentifiersTree i_ _listIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  InPredicate ann_ _exprInewFixedUpIdentifiersTree i_ _listInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  InPredicate ann_ _exprIoriginalTree i_ _listIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOexpectedType =
                  _lhsIexpectedType
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _listOcat =
                  _lhsIcat
              -- copy rule (down)
              _listOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _listOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
              ( _listIannotatedTree,_listIfixedUpIdentifiersTree,_listIlistType,_listInewFixedUpIdentifiersTree,_listIoriginalTree) =
                  (list_ _listOcat _listOidenv _listOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 173, column 19)
              _tpe =
                  Right typeInt
              -- "./TypeChecking/ScalarExprs.ag"(line 182, column 9)
              _backTree =
                  IntegerLit ann_ i_
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- self rule
              _annotatedTree =
                  IntegerLit ann_ i_
              -- self rule
              _fixedUpIdentifiersTree =
                  IntegerLit ann_ i_
              -- self rule
              _newFixedUpIdentifiersTree =
                  IntegerLit ann_ i_
              -- self rule
              _originalTree =
                  IntegerLit ann_ i_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 193, column 16)
              _tpe =
                  Right $ ScalarType "interval"
              -- "./TypeChecking/ScalarExprs.ag"(line 194, column 16)
              _backTree =
                  Interval ann_ value_ field_ prec_
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- self rule
              _annotatedTree =
                  Interval ann_ value_ field_ prec_
              -- self rule
              _fixedUpIdentifiersTree =
                  Interval ann_ value_ field_ prec_
              -- self rule
              _newFixedUpIdentifiersTree =
                  Interval ann_ value_ field_ prec_
              -- self rule
              _originalTree =
                  Interval ann_ value_ field_ prec_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _argsOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _argsOcat :: Catalog
              _argsOidenv :: IDEnv
              _argsOlib :: LocalBindings
              _argsIannotatedTree :: ScalarExprList
              _argsIfixedUpIdentifiersTree :: ScalarExprList
              _argsInewFixedUpIdentifiersTree :: ScalarExprList
              _argsIoriginalTree :: ScalarExprList
              _argsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _argsIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 252, column 9)
              _tpe =
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
              -- "./TypeChecking/ScalarExprs.ag"(line 266, column 9)
              _backTree =
                  LiftOperator ann_ oper_ flav_ _argsIannotatedTree
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/ScalarExprs.ag"(line 603, column 9)
              _argsOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  LiftOperator ann_ oper_ flav_ _argsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  LiftOperator ann_ oper_ flav_ _argsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  LiftOperator ann_ oper_ flav_ _argsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  LiftOperator ann_ oper_ flav_ _argsIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _argsOcat =
                  _lhsIcat
              -- copy rule (down)
              _argsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _argsOlib =
                  _lhsIlib
              ( _argsIannotatedTree,_argsIfixedUpIdentifiersTree,_argsInewFixedUpIdentifiersTree,_argsIoriginalTree,_argsItbUTypes,_argsIuType) =
                  (args_ _argsOcat _argsOexpectedTypes _argsOidenv _argsOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_ScalarExpr_NullLit :: Annotation ->
                          T_ScalarExpr 
sem_ScalarExpr_NullLit ann_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 178, column 16)
              _tpe =
                  Right UnknownType
              -- "./TypeChecking/ScalarExprs.ag"(line 190, column 9)
              _backTree =
                  NullLit ann_
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- self rule
              _annotatedTree =
                  NullLit ann_
              -- self rule
              _fixedUpIdentifiersTree =
                  NullLit ann_
              -- self rule
              _newFixedUpIdentifiersTree =
                  NullLit ann_
              -- self rule
              _originalTree =
                  NullLit ann_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_ScalarExpr_Placeholder :: Annotation ->
                              T_ScalarExpr 
sem_ScalarExpr_Placeholder ann_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExpr
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/ScalarExprs.ag"(line 448, column 9)
              _tpe =
                  Right UnknownType
              -- "./TypeChecking/ScalarExprs.ag"(line 449, column 9)
              _backTree =
                  Placeholder ann_
              -- self rule
              _annotatedTree =
                  Placeholder ann_
              -- self rule
              _fixedUpIdentifiersTree =
                  Placeholder ann_
              -- self rule
              _newFixedUpIdentifiersTree =
                  Placeholder ann_
              -- self rule
              _originalTree =
                  Placeholder ann_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/ScalarExprs.ag"(line 442, column 9)
              _tpe =
                  unwrapLookup <$> lbLookupID _lhsIlib ['$':show p_]
              -- "./TypeChecking/ScalarExprs.ag"(line 443, column 9)
              _backTree =
                  PositionalArg ann_ p_
              -- self rule
              _annotatedTree =
                  PositionalArg ann_ p_
              -- self rule
              _fixedUpIdentifiersTree =
                  PositionalArg ann_ p_
              -- self rule
              _newFixedUpIdentifiersTree =
                  PositionalArg ann_ p_
              -- self rule
              _originalTree =
                  PositionalArg ann_ p_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _qid :: (Maybe String)
              _ntType :: (E [(String,Type)])
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _qualOcat :: Catalog
              _qualOexpectedType :: (Maybe Type)
              _qualOidenv :: IDEnv
              _qualOlib :: LocalBindings
              _qualIannotatedTree :: ScalarExpr
              _qualIfixedUpIdentifiersTree :: ScalarExpr
              _qualInewFixedUpIdentifiersTree :: ScalarExpr
              _qualIntAnnotatedTree :: ScalarExpr
              _qualIntType :: ([(String,Type)])
              _qualIoriginalTree :: ScalarExpr
              _qualItbAnnotatedTree :: ScalarExpr
              _qualItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _qualIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 92, column 9)
              _tbUType =
                  catCompositeAttrsPair _lhsIcat relationComposites i_
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 366, column 9)
              _tpe =
                  let x = case _qid     of
                            Nothing -> byT
                            Just q -> either (const byT) Right $ unwrapLookup <$> lbLookupID _lhsIlib [q,i_]
                  in trace ("tpe: " ++ show x) x
                  where
                    byT = do
                      (t::Type) <- lmt _qualIuType
                      unwrapLookup <$> lbLookupIDInType _lhsIcat _lhsIlib t i_
              -- "./TypeChecking/ScalarExprs.ag"(line 375, column 9)
              _qid =
                  case _backTree     of
                     QIdentifier _ (Identifier _ q) _ -> Just q
                     _ -> Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 378, column 9)
              _backTree =
                  QIdentifier ann_ _qAnnTreeNoUnrec     i_
              -- "./TypeChecking/ScalarExprs.ag"(line 380, column 9)
              _qAnnTreeNoUnrec =
                  updateAnnotation (\a -> a {errs = []}) _qualIannotatedTree
              -- "./TypeChecking/ScalarExprs.ag"(line 383, column 9)
              _ntType =
                  case _qid     of
                    Nothing -> undefined
                    Just q ->
                        if i_ == "*"
                        then unwrapStar <$> lbExpandStar _lhsIlib q
                        else (\t -> [(i_, t)]) <$> unwrapLookup <$> lbLookupID _lhsIlib [q,i_]
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 162, column 9)
              _lhsOnewFixedUpIdentifiersTree =
                  QIdentifier ann_ _qualIfixedUpIdentifiersTree i_
              -- self rule
              _annotatedTree =
                  QIdentifier ann_ _qualIannotatedTree i_
              -- self rule
              _fixedUpIdentifiersTree =
                  QIdentifier ann_ _qualIfixedUpIdentifiersTree i_
              -- self rule
              _newFixedUpIdentifiersTree =
                  QIdentifier ann_ _qualInewFixedUpIdentifiersTree i_
              -- self rule
              _originalTree =
                  QIdentifier ann_ _qualIoriginalTree i_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _qualOcat =
                  _lhsIcat
              -- copy rule (down)
              _qualOexpectedType =
                  _lhsIexpectedType
              -- copy rule (down)
              _qualOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _qualOlib =
                  _lhsIlib
              ( _qualIannotatedTree,_qualIfixedUpIdentifiersTree,_qualInewFixedUpIdentifiersTree,_qualIntAnnotatedTree,_qualIntType,_qualIoriginalTree,_qualItbAnnotatedTree,_qualItbUType,_qualIuType) =
                  (qual_ _qualOcat _qualOexpectedType _qualOidenv _qualOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selInewFixedUpIdentifiersTree :: QueryExpr
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/ScalarExprs.ag"(line 466, column 9)
              _tpe =
                  do
                  selType <- lmt (map snd <$> _selIuType)
                  case length selType of
                    0 -> Left [InternalError "no columns in scalar subquery?"]
                    1 -> Right $ head selType
                    _ -> Right $ AnonymousRecordType selType
              -- "./TypeChecking/ScalarExprs.ag"(line 474, column 9)
              _backTree =
                  ScalarSubQuery ann_ _selIannotatedTree
              -- "./TypeChecking/ScalarExprs.ag"(line 636, column 29)
              _selOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  ScalarSubQuery ann_ _selIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  ScalarSubQuery ann_ _selIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  ScalarSubQuery ann_ _selInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  ScalarSubQuery ann_ _selIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selInewFixedUpIdentifiersTree,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 174, column 18)
              _tpe =
                  Right UnknownType
              -- "./TypeChecking/ScalarExprs.ag"(line 184, column 9)
              _backTree =
                  StringLit ann_ value_
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- self rule
              _annotatedTree =
                  StringLit ann_ value_
              -- self rule
              _fixedUpIdentifiersTree =
                  StringLit ann_ value_
              -- self rule
              _newFixedUpIdentifiersTree =
                  StringLit ann_ value_
              -- self rule
              _originalTree =
                  StringLit ann_ value_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
              _lhsOoriginalTree :: ScalarExpr
              _tnOcat :: Catalog
              _tnOidenv :: IDEnv
              _tnOlib :: LocalBindings
              _tnIannotatedTree :: TypeName
              _tnIfixedUpIdentifiersTree :: TypeName
              _tnInamedType :: (Maybe Type)
              _tnInewFixedUpIdentifiersTree :: TypeName
              _tnIoriginalTree :: TypeName
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 210, column 10)
              _tpe =
                  lmt _tnInamedType
              -- "./TypeChecking/ScalarExprs.ag"(line 211, column 10)
              _backTree =
                  TypedStringLit ann_ _tnIannotatedTree value_
              -- "./TypeChecking/ScalarExprs.ag"(line 411, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- self rule
              _annotatedTree =
                  TypedStringLit ann_ _tnIannotatedTree value_
              -- self rule
              _fixedUpIdentifiersTree =
                  TypedStringLit ann_ _tnIfixedUpIdentifiersTree value_
              -- self rule
              _newFixedUpIdentifiersTree =
                  TypedStringLit ann_ _tnInewFixedUpIdentifiersTree value_
              -- self rule
              _originalTree =
                  TypedStringLit ann_ _tnIoriginalTree value_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tnOcat =
                  _lhsIcat
              -- copy rule (down)
              _tnOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tnOlib =
                  _lhsIlib
              ( _tnIannotatedTree,_tnIfixedUpIdentifiersTree,_tnInamedType,_tnInewFixedUpIdentifiersTree,_tnIoriginalTree) =
                  (tn_ _tnOcat _tnOidenv _tnOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
              _lhsOntAnnotatedTree :: ScalarExpr
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: ScalarExpr
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _partitionByOexpectedTypes :: ([Maybe Type])
              _orderByOexpectedTypes :: ([Maybe Type])
              _lhsOfixedUpIdentifiersTree :: ScalarExpr
              _lhsOnewFixedUpIdentifiersTree :: ScalarExpr
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
              _fnInewFixedUpIdentifiersTree :: ScalarExpr
              _fnIntAnnotatedTree :: ScalarExpr
              _fnIntType :: ([(String,Type)])
              _fnIoriginalTree :: ScalarExpr
              _fnItbAnnotatedTree :: ScalarExpr
              _fnItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _fnIuType :: (Maybe Type)
              _partitionByIannotatedTree :: ScalarExprList
              _partitionByIfixedUpIdentifiersTree :: ScalarExprList
              _partitionByInewFixedUpIdentifiersTree :: ScalarExprList
              _partitionByIoriginalTree :: ScalarExprList
              _partitionByItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _partitionByIuType :: ([Maybe Type])
              _orderByIannotatedTree :: ScalarExprList
              _orderByIfixedUpIdentifiersTree :: ScalarExprList
              _orderByInewFixedUpIdentifiersTree :: ScalarExprList
              _orderByIoriginalTree :: ScalarExprList
              _orderByItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _orderByIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs.ag"(line 15, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 29, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 37, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 40, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/ScalarExprs.ag"(line 77, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/ScalarExprs.ag"(line 81, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/ScalarExprs.ag"(line 86, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/ScalarExprs.ag"(line 107, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/ScalarExprs.ag"(line 233, column 9)
              _tpe =
                  lmt _fnIuType
              -- "./TypeChecking/ScalarExprs.ag"(line 234, column 9)
              _backTree =
                  WindowFn ann_
                           _fnIannotatedTree
                           _partitionByIannotatedTree
                           _orderByIannotatedTree
                           dir_
                           frm_
              -- "./TypeChecking/ScalarExprs.ag"(line 404, column 7)
              _liftedColumnName =
                  let (FunCall _ fn _) = _fnIannotatedTree
                  in fn
              -- "./TypeChecking/ScalarExprs.ag"(line 417, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/ScalarExprs.ag"(line 605, column 9)
              _partitionByOexpectedTypes =
                  []
              -- "./TypeChecking/ScalarExprs.ag"(line 606, column 9)
              _orderByOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree dir_ frm_
              -- self rule
              _fixedUpIdentifiersTree =
                  WindowFn ann_ _fnIfixedUpIdentifiersTree _partitionByIfixedUpIdentifiersTree _orderByIfixedUpIdentifiersTree dir_ frm_
              -- self rule
              _newFixedUpIdentifiersTree =
                  WindowFn ann_ _fnInewFixedUpIdentifiersTree _partitionByInewFixedUpIdentifiersTree _orderByInewFixedUpIdentifiersTree dir_ frm_
              -- self rule
              _originalTree =
                  WindowFn ann_ _fnIoriginalTree _partitionByIoriginalTree _orderByIoriginalTree dir_ frm_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _fnOcat =
                  _lhsIcat
              -- copy rule (down)
              _fnOexpectedType =
                  _lhsIexpectedType
              -- copy rule (down)
              _fnOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _fnOlib =
                  _lhsIlib
              -- copy rule (down)
              _partitionByOcat =
                  _lhsIcat
              -- copy rule (down)
              _partitionByOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _partitionByOlib =
                  _lhsIlib
              -- copy rule (down)
              _orderByOcat =
                  _lhsIcat
              -- copy rule (down)
              _orderByOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _orderByOlib =
                  _lhsIlib
              ( _fnIannotatedTree,_fnIfixedUpIdentifiersTree,_fnInewFixedUpIdentifiersTree,_fnIntAnnotatedTree,_fnIntType,_fnIoriginalTree,_fnItbAnnotatedTree,_fnItbUType,_fnIuType) =
                  (fn_ _fnOcat _fnOexpectedType _fnOidenv _fnOlib )
              ( _partitionByIannotatedTree,_partitionByIfixedUpIdentifiersTree,_partitionByInewFixedUpIdentifiersTree,_partitionByIoriginalTree,_partitionByItbUTypes,_partitionByIuType) =
                  (partitionBy_ _partitionByOcat _partitionByOexpectedTypes _partitionByOidenv _partitionByOlib )
              ( _orderByIannotatedTree,_orderByIfixedUpIdentifiersTree,_orderByInewFixedUpIdentifiersTree,_orderByIoriginalTree,_orderByItbUTypes,_orderByIuType) =
                  (orderBy_ _orderByOcat _orderByOexpectedTypes _orderByOidenv _orderByOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExpr 
         child x2             : {Direction}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                                  ( ScalarExprDirectionPair,ScalarExprDirectionPair,ScalarExprDirectionPair,ScalarExprDirectionPair)
data Inh_ScalarExprDirectionPair  = Inh_ScalarExprDirectionPair {cat_Inh_ScalarExprDirectionPair :: Catalog,idenv_Inh_ScalarExprDirectionPair :: IDEnv,lib_Inh_ScalarExprDirectionPair :: LocalBindings}
data Syn_ScalarExprDirectionPair  = Syn_ScalarExprDirectionPair {annotatedTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair,fixedUpIdentifiersTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair,newFixedUpIdentifiersTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair,originalTree_Syn_ScalarExprDirectionPair :: ScalarExprDirectionPair}
wrap_ScalarExprDirectionPair :: T_ScalarExprDirectionPair  ->
                                Inh_ScalarExprDirectionPair  ->
                                Syn_ScalarExprDirectionPair 
wrap_ScalarExprDirectionPair sem (Inh_ScalarExprDirectionPair _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprDirectionPair _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprDirectionPair
              _lhsOoriginalTree :: ScalarExprDirectionPair
              _x1Ocat :: Catalog
              _x1Oidenv :: IDEnv
              _x1Olib :: LocalBindings
              _x1IannotatedTree :: ScalarExpr
              _x1IfixedUpIdentifiersTree :: ScalarExpr
              _x1InewFixedUpIdentifiersTree :: ScalarExpr
              _x1IntAnnotatedTree :: ScalarExpr
              _x1IntType :: ([(String,Type)])
              _x1IoriginalTree :: ScalarExpr
              _x1ItbAnnotatedTree :: ScalarExpr
              _x1ItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _x1IuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 544, column 13)
              _x1OexpectedType =
                  Nothing
              -- self rule
              _annotatedTree =
                  (_x1IannotatedTree,x2_)
              -- self rule
              _fixedUpIdentifiersTree =
                  (_x1IfixedUpIdentifiersTree,x2_)
              -- self rule
              _newFixedUpIdentifiersTree =
                  (_x1InewFixedUpIdentifiersTree,x2_)
              -- self rule
              _originalTree =
                  (_x1IoriginalTree,x2_)
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _x1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x1Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _x1Olib =
                  _lhsIlib
              ( _x1IannotatedTree,_x1IfixedUpIdentifiersTree,_x1InewFixedUpIdentifiersTree,_x1IntAnnotatedTree,_x1IntType,_x1IoriginalTree,_x1ItbAnnotatedTree,_x1ItbUType,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedType _x1Oidenv _x1Olib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExprDirectionPair 
         child tl             : ScalarExprDirectionPairList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                                      ( ScalarExprDirectionPairList,ScalarExprDirectionPairList,ScalarExprDirectionPairList,ScalarExprDirectionPairList)
data Inh_ScalarExprDirectionPairList  = Inh_ScalarExprDirectionPairList {cat_Inh_ScalarExprDirectionPairList :: Catalog,idenv_Inh_ScalarExprDirectionPairList :: IDEnv,lib_Inh_ScalarExprDirectionPairList :: LocalBindings}
data Syn_ScalarExprDirectionPairList  = Syn_ScalarExprDirectionPairList {annotatedTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList,fixedUpIdentifiersTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList,newFixedUpIdentifiersTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList,originalTree_Syn_ScalarExprDirectionPairList :: ScalarExprDirectionPairList}
wrap_ScalarExprDirectionPairList :: T_ScalarExprDirectionPairList  ->
                                    Inh_ScalarExprDirectionPairList  ->
                                    Syn_ScalarExprDirectionPairList 
wrap_ScalarExprDirectionPairList sem (Inh_ScalarExprDirectionPairList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprDirectionPairList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ScalarExprDirectionPairList_Cons :: T_ScalarExprDirectionPair  ->
                                        T_ScalarExprDirectionPairList  ->
                                        T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprDirectionPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _lhsOoriginalTree :: ScalarExprDirectionPairList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ScalarExprDirectionPair
              _hdIfixedUpIdentifiersTree :: ScalarExprDirectionPair
              _hdInewFixedUpIdentifiersTree :: ScalarExprDirectionPair
              _hdIoriginalTree :: ScalarExprDirectionPair
              _tlIannotatedTree :: ScalarExprDirectionPairList
              _tlIfixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _tlInewFixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _tlIoriginalTree :: ScalarExprDirectionPairList
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_ScalarExprDirectionPairList_Nil :: T_ScalarExprDirectionPairList 
sem_ScalarExprDirectionPairList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprDirectionPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprDirectionPairList
              _lhsOoriginalTree :: ScalarExprDirectionPairList
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         tbUTypes             : [Maybe ([(String,Type)],[(String,Type)])]
         uType                : [Maybe Type]
   alternatives:
      alternative Cons:
         child hd             : ScalarExpr 
         child tl             : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                         ( ScalarExprList,ScalarExprList,ScalarExprList,ScalarExprList,([Maybe ([(String,Type)],[(String,Type)])]),([Maybe Type]))
data Inh_ScalarExprList  = Inh_ScalarExprList {cat_Inh_ScalarExprList :: Catalog,expectedTypes_Inh_ScalarExprList :: [Maybe Type],idenv_Inh_ScalarExprList :: IDEnv,lib_Inh_ScalarExprList :: LocalBindings}
data Syn_ScalarExprList  = Syn_ScalarExprList {annotatedTree_Syn_ScalarExprList :: ScalarExprList,fixedUpIdentifiersTree_Syn_ScalarExprList :: ScalarExprList,newFixedUpIdentifiersTree_Syn_ScalarExprList :: ScalarExprList,originalTree_Syn_ScalarExprList :: ScalarExprList,tbUTypes_Syn_ScalarExprList :: [Maybe ([(String,Type)],[(String,Type)])],uType_Syn_ScalarExprList :: [Maybe Type]}
wrap_ScalarExprList :: T_ScalarExprList  ->
                       Inh_ScalarExprList  ->
                       Syn_ScalarExprList 
wrap_ScalarExprList sem (Inh_ScalarExprList _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOtbUTypes,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOtbUTypes _lhsOuType ))
sem_ScalarExprList_Cons :: T_ScalarExpr  ->
                           T_ScalarExprList  ->
                           T_ScalarExprList 
sem_ScalarExprList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOtbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _lhsOuType :: ([Maybe Type])
              _hdOexpectedType :: (Maybe Type)
              _tlOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: ScalarExprList
              _lhsOfixedUpIdentifiersTree :: ScalarExprList
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprList
              _lhsOoriginalTree :: ScalarExprList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ScalarExpr
              _hdIfixedUpIdentifiersTree :: ScalarExpr
              _hdInewFixedUpIdentifiersTree :: ScalarExpr
              _hdIntAnnotatedTree :: ScalarExpr
              _hdIntType :: ([(String,Type)])
              _hdIoriginalTree :: ScalarExpr
              _hdItbAnnotatedTree :: ScalarExpr
              _hdItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _hdIuType :: (Maybe Type)
              _tlIannotatedTree :: ScalarExprList
              _tlIfixedUpIdentifiersTree :: ScalarExprList
              _tlInewFixedUpIdentifiersTree :: ScalarExprList
              _tlIoriginalTree :: ScalarExprList
              _tlItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _tlIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs.ag"(line 95, column 12)
              _lhsOtbUTypes =
                  _hdItbUType : _tlItbUTypes
              -- "./TypeChecking/ScalarExprs.ag"(line 110, column 12)
              _lhsOuType =
                  _hdIuType : _tlIuType
              -- "./TypeChecking/ScalarExprs.ag"(line 547, column 12)
              _hdOexpectedType =
                  case _lhsIexpectedTypes of
                    (t:_) -> t
                    _ -> Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 550, column 12)
              _tlOexpectedTypes =
                  case _lhsIexpectedTypes of
                  (_:ts) -> ts
                  _ -> []
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIntAnnotatedTree,_hdIntType,_hdIoriginalTree,_hdItbAnnotatedTree,_hdItbUType,_hdIuType) =
                  (hd_ _hdOcat _hdOexpectedType _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree,_tlItbUTypes,_tlIuType) =
                  (tl_ _tlOcat _tlOexpectedTypes _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOtbUTypes,_lhsOuType)))
sem_ScalarExprList_Nil :: T_ScalarExprList 
sem_ScalarExprList_Nil  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOtbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _lhsOuType :: ([Maybe Type])
              _lhsOannotatedTree :: ScalarExprList
              _lhsOfixedUpIdentifiersTree :: ScalarExprList
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprList
              _lhsOoriginalTree :: ScalarExprList
              -- "./TypeChecking/ScalarExprs.ag"(line 96, column 11)
              _lhsOtbUTypes =
                  []
              -- "./TypeChecking/ScalarExprs.ag"(line 111, column 11)
              _lhsOuType =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOtbUTypes,_lhsOuType)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         uType                : [[Maybe Type]]
   alternatives:
      alternative Cons:
         child hd             : ScalarExprList 
         child tl             : ScalarExprListList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                             ( ScalarExprListList,ScalarExprListList,ScalarExprListList,ScalarExprListList,([[Maybe Type]]))
data Inh_ScalarExprListList  = Inh_ScalarExprListList {cat_Inh_ScalarExprListList :: Catalog,expectedTypes_Inh_ScalarExprListList :: [Maybe Type],idenv_Inh_ScalarExprListList :: IDEnv,lib_Inh_ScalarExprListList :: LocalBindings}
data Syn_ScalarExprListList  = Syn_ScalarExprListList {annotatedTree_Syn_ScalarExprListList :: ScalarExprListList,fixedUpIdentifiersTree_Syn_ScalarExprListList :: ScalarExprListList,newFixedUpIdentifiersTree_Syn_ScalarExprListList :: ScalarExprListList,originalTree_Syn_ScalarExprListList :: ScalarExprListList,uType_Syn_ScalarExprListList :: [[Maybe Type]]}
wrap_ScalarExprListList :: T_ScalarExprListList  ->
                           Inh_ScalarExprListList  ->
                           Syn_ScalarExprListList 
wrap_ScalarExprListList sem (Inh_ScalarExprListList _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedTypes _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprListList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOuType ))
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
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprListList
              _lhsOoriginalTree :: ScalarExprListList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ScalarExprList
              _hdIfixedUpIdentifiersTree :: ScalarExprList
              _hdInewFixedUpIdentifiersTree :: ScalarExprList
              _hdIoriginalTree :: ScalarExprList
              _hdItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _hdIuType :: ([Maybe Type])
              _tlIannotatedTree :: ScalarExprListList
              _tlIfixedUpIdentifiersTree :: ScalarExprListList
              _tlInewFixedUpIdentifiersTree :: ScalarExprListList
              _tlIoriginalTree :: ScalarExprListList
              _tlIuType :: ([[Maybe Type]])
              -- "./TypeChecking/ScalarExprs.ag"(line 117, column 12)
              _lhsOuType =
                  _hdIuType : _tlIuType
              -- "./TypeChecking/ScalarExprs.ag"(line 632, column 12)
              _hdOexpectedTypes =
                  _lhsIexpectedTypes
              -- "./TypeChecking/ScalarExprs.ag"(line 633, column 12)
              _tlOexpectedTypes =
                  _lhsIexpectedTypes
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree,_hdItbUTypes,_hdIuType) =
                  (hd_ _hdOcat _hdOexpectedTypes _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree,_tlIuType) =
                  (tl_ _tlOcat _tlOexpectedTypes _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
sem_ScalarExprListList_Nil :: T_ScalarExprListList 
sem_ScalarExprListList_Nil  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOuType :: ([[Maybe Type]])
              _lhsOannotatedTree :: ScalarExprListList
              _lhsOfixedUpIdentifiersTree :: ScalarExprListList
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprListList
              _lhsOoriginalTree :: ScalarExprListList
              -- "./TypeChecking/ScalarExprs.ag"(line 118, column 11)
              _lhsOuType =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOuType)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExprList 
         child x2             : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                                          ( ScalarExprListStatementListPair,ScalarExprListStatementListPair,ScalarExprListStatementListPair,ScalarExprListStatementListPair)
data Inh_ScalarExprListStatementListPair  = Inh_ScalarExprListStatementListPair {cat_Inh_ScalarExprListStatementListPair :: Catalog,idenv_Inh_ScalarExprListStatementListPair :: IDEnv,lib_Inh_ScalarExprListStatementListPair :: LocalBindings}
data Syn_ScalarExprListStatementListPair  = Syn_ScalarExprListStatementListPair {annotatedTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair,fixedUpIdentifiersTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair,newFixedUpIdentifiersTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair,originalTree_Syn_ScalarExprListStatementListPair :: ScalarExprListStatementListPair}
wrap_ScalarExprListStatementListPair :: T_ScalarExprListStatementListPair  ->
                                        Inh_ScalarExprListStatementListPair  ->
                                        Syn_ScalarExprListStatementListPair 
wrap_ScalarExprListStatementListPair sem (Inh_ScalarExprListStatementListPair _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprListStatementListPair _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprListStatementListPair
              _lhsOoriginalTree :: ScalarExprListStatementListPair
              _x1Ocat :: Catalog
              _x1Oidenv :: IDEnv
              _x1Olib :: LocalBindings
              _x2Ocat :: Catalog
              _x2Oidenv :: IDEnv
              _x2Olib :: LocalBindings
              _x1IannotatedTree :: ScalarExprList
              _x1IfixedUpIdentifiersTree :: ScalarExprList
              _x1InewFixedUpIdentifiersTree :: ScalarExprList
              _x1IoriginalTree :: ScalarExprList
              _x1ItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _x1IuType :: ([Maybe Type])
              _x2IannotatedTree :: StatementList
              _x2IfixedUpIdentifiersTree :: StatementList
              _x2InewFixedUpIdentifiersTree :: StatementList
              _x2IoriginalTree :: StatementList
              _x2IproducedCat :: Catalog
              _x2IproducedLib :: LocalBindings
              -- "./TypeChecking/ScalarExprs.ag"(line 609, column 13)
              _x1OexpectedTypes =
                  []
              -- "./TypeChecking/Statements.ag"(line 121, column 9)
              _x2OcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 122, column 9)
              _x2OlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  (_x1IannotatedTree,_x2IannotatedTree)
              -- self rule
              _fixedUpIdentifiersTree =
                  (_x1IfixedUpIdentifiersTree,_x2IfixedUpIdentifiersTree)
              -- self rule
              _newFixedUpIdentifiersTree =
                  (_x1InewFixedUpIdentifiersTree,_x2InewFixedUpIdentifiersTree)
              -- self rule
              _originalTree =
                  (_x1IoriginalTree,_x2IoriginalTree)
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _x1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x1Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _x1Olib =
                  _lhsIlib
              -- copy rule (down)
              _x2Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x2Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _x2Olib =
                  _lhsIlib
              ( _x1IannotatedTree,_x1IfixedUpIdentifiersTree,_x1InewFixedUpIdentifiersTree,_x1IoriginalTree,_x1ItbUTypes,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedTypes _x1Oidenv _x1Olib )
              ( _x2IannotatedTree,_x2IfixedUpIdentifiersTree,_x2InewFixedUpIdentifiersTree,_x2IoriginalTree,_x2IproducedCat,_x2IproducedLib) =
                  (x2_ _x2Ocat _x2OcatUpdates _x2Oidenv _x2Olib _x2OlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExprListStatementListPair 
         child tl             : ScalarExprListStatementListPairList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                                              ( ScalarExprListStatementListPairList,ScalarExprListStatementListPairList,ScalarExprListStatementListPairList,ScalarExprListStatementListPairList)
data Inh_ScalarExprListStatementListPairList  = Inh_ScalarExprListStatementListPairList {cat_Inh_ScalarExprListStatementListPairList :: Catalog,idenv_Inh_ScalarExprListStatementListPairList :: IDEnv,lib_Inh_ScalarExprListStatementListPairList :: LocalBindings}
data Syn_ScalarExprListStatementListPairList  = Syn_ScalarExprListStatementListPairList {annotatedTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList,fixedUpIdentifiersTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList,newFixedUpIdentifiersTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList,originalTree_Syn_ScalarExprListStatementListPairList :: ScalarExprListStatementListPairList}
wrap_ScalarExprListStatementListPairList :: T_ScalarExprListStatementListPairList  ->
                                            Inh_ScalarExprListStatementListPairList  ->
                                            Syn_ScalarExprListStatementListPairList 
wrap_ScalarExprListStatementListPairList sem (Inh_ScalarExprListStatementListPairList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprListStatementListPairList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ScalarExprListStatementListPairList_Cons :: T_ScalarExprListStatementListPair  ->
                                                T_ScalarExprListStatementListPairList  ->
                                                T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprListStatementListPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _lhsOoriginalTree :: ScalarExprListStatementListPairList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ScalarExprListStatementListPair
              _hdIfixedUpIdentifiersTree :: ScalarExprListStatementListPair
              _hdInewFixedUpIdentifiersTree :: ScalarExprListStatementListPair
              _hdIoriginalTree :: ScalarExprListStatementListPair
              _tlIannotatedTree :: ScalarExprListStatementListPairList
              _tlIfixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _tlInewFixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _tlIoriginalTree :: ScalarExprListStatementListPairList
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_ScalarExprListStatementListPairList_Nil :: T_ScalarExprListStatementListPairList 
sem_ScalarExprListStatementListPairList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprListStatementListPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _lhsOoriginalTree :: ScalarExprListStatementListPairList
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
-- ScalarExprRoot ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         idenv                : IDEnv
         lib                  : LocalBindings
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
                         ( ScalarExprRoot,ScalarExprRoot)
data Inh_ScalarExprRoot  = Inh_ScalarExprRoot {cat_Inh_ScalarExprRoot :: Catalog,idenv_Inh_ScalarExprRoot :: IDEnv,lib_Inh_ScalarExprRoot :: LocalBindings}
data Syn_ScalarExprRoot  = Syn_ScalarExprRoot {annotatedTree_Syn_ScalarExprRoot :: ScalarExprRoot,originalTree_Syn_ScalarExprRoot :: ScalarExprRoot}
wrap_ScalarExprRoot :: T_ScalarExprRoot  ->
                       Inh_ScalarExprRoot  ->
                       Syn_ScalarExprRoot 
wrap_ScalarExprRoot sem (Inh_ScalarExprRoot _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprRoot _lhsOannotatedTree _lhsOoriginalTree ))
sem_ScalarExprRoot_ScalarExprRoot :: T_ScalarExpr  ->
                                     T_ScalarExprRoot 
sem_ScalarExprRoot_ScalarExprRoot expr_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: ScalarExprRoot
              _lhsOoriginalTree :: ScalarExprRoot
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 588, column 22)
              _exprOexpectedType =
                  Nothing
              -- self rule
              _annotatedTree =
                  ScalarExprRoot _exprIannotatedTree
              -- self rule
              _originalTree =
                  ScalarExprRoot _exprIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExpr 
         child x2             : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                                      ( ScalarExprStatementListPair,ScalarExprStatementListPair,ScalarExprStatementListPair,ScalarExprStatementListPair)
data Inh_ScalarExprStatementListPair  = Inh_ScalarExprStatementListPair {cat_Inh_ScalarExprStatementListPair :: Catalog,idenv_Inh_ScalarExprStatementListPair :: IDEnv,lib_Inh_ScalarExprStatementListPair :: LocalBindings}
data Syn_ScalarExprStatementListPair  = Syn_ScalarExprStatementListPair {annotatedTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair,fixedUpIdentifiersTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair,newFixedUpIdentifiersTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair,originalTree_Syn_ScalarExprStatementListPair :: ScalarExprStatementListPair}
wrap_ScalarExprStatementListPair :: T_ScalarExprStatementListPair  ->
                                    Inh_ScalarExprStatementListPair  ->
                                    Syn_ScalarExprStatementListPair 
wrap_ScalarExprStatementListPair sem (Inh_ScalarExprStatementListPair _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprStatementListPair _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprStatementListPair
              _lhsOoriginalTree :: ScalarExprStatementListPair
              _x1Ocat :: Catalog
              _x1Oidenv :: IDEnv
              _x1Olib :: LocalBindings
              _x2Ocat :: Catalog
              _x2Oidenv :: IDEnv
              _x2Olib :: LocalBindings
              _x1IannotatedTree :: ScalarExpr
              _x1IfixedUpIdentifiersTree :: ScalarExpr
              _x1InewFixedUpIdentifiersTree :: ScalarExpr
              _x1IntAnnotatedTree :: ScalarExpr
              _x1IntType :: ([(String,Type)])
              _x1IoriginalTree :: ScalarExpr
              _x1ItbAnnotatedTree :: ScalarExpr
              _x1ItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _x1IuType :: (Maybe Type)
              _x2IannotatedTree :: StatementList
              _x2IfixedUpIdentifiersTree :: StatementList
              _x2InewFixedUpIdentifiersTree :: StatementList
              _x2IoriginalTree :: StatementList
              _x2IproducedCat :: Catalog
              _x2IproducedLib :: LocalBindings
              -- "./TypeChecking/ScalarExprs.ag"(line 591, column 13)
              _x1OexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 125, column 9)
              _x2OcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 126, column 9)
              _x2OlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  (_x1IannotatedTree,_x2IannotatedTree)
              -- self rule
              _fixedUpIdentifiersTree =
                  (_x1IfixedUpIdentifiersTree,_x2IfixedUpIdentifiersTree)
              -- self rule
              _newFixedUpIdentifiersTree =
                  (_x1InewFixedUpIdentifiersTree,_x2InewFixedUpIdentifiersTree)
              -- self rule
              _originalTree =
                  (_x1IoriginalTree,_x2IoriginalTree)
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _x1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x1Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _x1Olib =
                  _lhsIlib
              -- copy rule (down)
              _x2Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x2Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _x2Olib =
                  _lhsIlib
              ( _x1IannotatedTree,_x1IfixedUpIdentifiersTree,_x1InewFixedUpIdentifiersTree,_x1IntAnnotatedTree,_x1IntType,_x1IoriginalTree,_x1ItbAnnotatedTree,_x1ItbUType,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedType _x1Oidenv _x1Olib )
              ( _x2IannotatedTree,_x2IfixedUpIdentifiersTree,_x2InewFixedUpIdentifiersTree,_x2IoriginalTree,_x2IproducedCat,_x2IproducedLib) =
                  (x2_ _x2Ocat _x2OcatUpdates _x2Oidenv _x2Olib _x2OlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExprStatementListPair 
         child tl             : ScalarExprStatementListPairList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                                          ( ScalarExprStatementListPairList,ScalarExprStatementListPairList,ScalarExprStatementListPairList,ScalarExprStatementListPairList)
data Inh_ScalarExprStatementListPairList  = Inh_ScalarExprStatementListPairList {cat_Inh_ScalarExprStatementListPairList :: Catalog,idenv_Inh_ScalarExprStatementListPairList :: IDEnv,lib_Inh_ScalarExprStatementListPairList :: LocalBindings}
data Syn_ScalarExprStatementListPairList  = Syn_ScalarExprStatementListPairList {annotatedTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList,fixedUpIdentifiersTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList,newFixedUpIdentifiersTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList,originalTree_Syn_ScalarExprStatementListPairList :: ScalarExprStatementListPairList}
wrap_ScalarExprStatementListPairList :: T_ScalarExprStatementListPairList  ->
                                        Inh_ScalarExprStatementListPairList  ->
                                        Syn_ScalarExprStatementListPairList 
wrap_ScalarExprStatementListPairList sem (Inh_ScalarExprStatementListPairList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_ScalarExprStatementListPairList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_ScalarExprStatementListPairList_Cons :: T_ScalarExprStatementListPair  ->
                                            T_ScalarExprStatementListPairList  ->
                                            T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprStatementListPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _lhsOoriginalTree :: ScalarExprStatementListPairList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: ScalarExprStatementListPair
              _hdIfixedUpIdentifiersTree :: ScalarExprStatementListPair
              _hdInewFixedUpIdentifiersTree :: ScalarExprStatementListPair
              _hdIoriginalTree :: ScalarExprStatementListPair
              _tlIannotatedTree :: ScalarExprStatementListPairList
              _tlIfixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _tlInewFixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _tlIoriginalTree :: ScalarExprStatementListPairList
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_ScalarExprStatementListPairList_Nil :: T_ScalarExprStatementListPairList 
sem_ScalarExprStatementListPairList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOannotatedTree :: ScalarExprStatementListPairList
              _lhsOfixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _lhsOnewFixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _lhsOoriginalTree :: ScalarExprStatementListPairList
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         itemType             : [(String,Type)]
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
         seIdTree             : [SelectItem]
   alternatives:
      alternative SelExp:
         child ann            : {Annotation}
         child ex             : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative SelectItem:
         child ann            : {Annotation}
         child ex             : ScalarExpr 
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                     ( SelectItem,SelectItem,([(String,Type)]),SelectItem,SelectItem,([SelectItem]))
data Inh_SelectItem  = Inh_SelectItem {cat_Inh_SelectItem :: Catalog,idenv_Inh_SelectItem :: IDEnv,lib_Inh_SelectItem :: LocalBindings}
data Syn_SelectItem  = Syn_SelectItem {annotatedTree_Syn_SelectItem :: SelectItem,fixedUpIdentifiersTree_Syn_SelectItem :: SelectItem,itemType_Syn_SelectItem :: [(String,Type)],newFixedUpIdentifiersTree_Syn_SelectItem :: SelectItem,originalTree_Syn_SelectItem :: SelectItem,seIdTree_Syn_SelectItem :: [SelectItem]}
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOitemType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOseIdTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_SelectItem _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOitemType _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOseIdTree ))
sem_SelectItem_SelExp :: Annotation ->
                         T_ScalarExpr  ->
                         T_SelectItem 
sem_SelectItem_SelExp ann_ ex_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _exOexpectedType :: (Maybe Type)
              _lhsOitemType :: ([(String,Type)])
              _lhsOseIdTree :: ([SelectItem])
              _lhsOannotatedTree :: SelectItem
              _lhsOfixedUpIdentifiersTree :: SelectItem
              _lhsOnewFixedUpIdentifiersTree :: SelectItem
              _lhsOoriginalTree :: SelectItem
              _exOcat :: Catalog
              _exOidenv :: IDEnv
              _exOlib :: LocalBindings
              _exIannotatedTree :: ScalarExpr
              _exIfixedUpIdentifiersTree :: ScalarExpr
              _exInewFixedUpIdentifiersTree :: ScalarExpr
              _exIntAnnotatedTree :: ScalarExpr
              _exIntType :: ([(String,Type)])
              _exIoriginalTree :: ScalarExpr
              _exItbAnnotatedTree :: ScalarExpr
              _exItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 571, column 25)
              _exOexpectedType =
                  Nothing
              -- "./TypeChecking/SelectLists.ag"(line 33, column 9)
              _annotatedTree =
                  SelExp ann_ _exIntAnnotatedTree
              -- "./TypeChecking/SelectLists.ag"(line 61, column 9)
              _lhsOitemType =
                  unwrapSetofs _exIntType
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 175, column 14)
              _lhsOseIdTree =
                  case _exIfixedUpIdentifiersTree of
                    x@(Identifier a "*") -> let s = expandStar _lhsIidenv Nothing
                                            in if null s
                                               then [SelExp ann_ _exIfixedUpIdentifiersTree]
                                               else makeSelExps ann_ a a s
                    x@(QIdentifier a0 (Identifier a1 q) "*") ->
                       let s = expandStar _lhsIidenv $ Just q
                       in if null s
                          then [SelExp ann_ _exIfixedUpIdentifiersTree]
                          else makeSelExps ann_ a0 a1 s
                    x -> [SelExp ann_ _exInewFixedUpIdentifiersTree]
              -- self rule
              _fixedUpIdentifiersTree =
                  SelExp ann_ _exIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  SelExp ann_ _exInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  SelExp ann_ _exIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exOcat =
                  _lhsIcat
              -- copy rule (down)
              _exOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exOlib =
                  _lhsIlib
              ( _exIannotatedTree,_exIfixedUpIdentifiersTree,_exInewFixedUpIdentifiersTree,_exIntAnnotatedTree,_exIntType,_exIoriginalTree,_exItbAnnotatedTree,_exItbUType,_exIuType) =
                  (ex_ _exOcat _exOexpectedType _exOidenv _exOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOitemType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOseIdTree)))
sem_SelectItem_SelectItem :: Annotation ->
                             T_ScalarExpr  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem ann_ ex_ name_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _exOexpectedType :: (Maybe Type)
              _lhsOitemType :: ([(String,Type)])
              _lhsOseIdTree :: ([SelectItem])
              _lhsOannotatedTree :: SelectItem
              _lhsOfixedUpIdentifiersTree :: SelectItem
              _lhsOnewFixedUpIdentifiersTree :: SelectItem
              _lhsOoriginalTree :: SelectItem
              _exOcat :: Catalog
              _exOidenv :: IDEnv
              _exOlib :: LocalBindings
              _exIannotatedTree :: ScalarExpr
              _exIfixedUpIdentifiersTree :: ScalarExpr
              _exInewFixedUpIdentifiersTree :: ScalarExpr
              _exIntAnnotatedTree :: ScalarExpr
              _exIntType :: ([(String,Type)])
              _exIoriginalTree :: ScalarExpr
              _exItbAnnotatedTree :: ScalarExpr
              _exItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 571, column 25)
              _exOexpectedType =
                  Nothing
              -- "./TypeChecking/SelectLists.ag"(line 35, column 9)
              _annotatedTree =
                  SelectItem ann_ _exIannotatedTree name_
              -- "./TypeChecking/SelectLists.ag"(line 63, column 9)
              _lhsOitemType =
                  case _exIntType of
                    [(_,t)] -> [(name_, unwrapSetof t)]
                    _ -> []
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 188, column 18)
              _lhsOseIdTree =
                  [SelectItem ann_ _exInewFixedUpIdentifiersTree name_]
              -- self rule
              _fixedUpIdentifiersTree =
                  SelectItem ann_ _exIfixedUpIdentifiersTree name_
              -- self rule
              _newFixedUpIdentifiersTree =
                  SelectItem ann_ _exInewFixedUpIdentifiersTree name_
              -- self rule
              _originalTree =
                  SelectItem ann_ _exIoriginalTree name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exOcat =
                  _lhsIcat
              -- copy rule (down)
              _exOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exOlib =
                  _lhsIlib
              ( _exIannotatedTree,_exIfixedUpIdentifiersTree,_exInewFixedUpIdentifiersTree,_exIntAnnotatedTree,_exIntType,_exIoriginalTree,_exItbAnnotatedTree,_exItbUType,_exIuType) =
                  (ex_ _exOcat _exOexpectedType _exOidenv _exOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOitemType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOseIdTree)))
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
         listType             : [(String,Type)]
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : SelectItem 
         child tl             : SelectItemList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                         ( SelectItemList,SelectItemList,([(String,Type)]),SelectItemList,SelectItemList)
data Inh_SelectItemList  = Inh_SelectItemList {cat_Inh_SelectItemList :: Catalog,idenv_Inh_SelectItemList :: IDEnv,lib_Inh_SelectItemList :: LocalBindings}
data Syn_SelectItemList  = Syn_SelectItemList {annotatedTree_Syn_SelectItemList :: SelectItemList,fixedUpIdentifiersTree_Syn_SelectItemList :: SelectItemList,listType_Syn_SelectItemList :: [(String,Type)],newFixedUpIdentifiersTree_Syn_SelectItemList :: SelectItemList,originalTree_Syn_SelectItemList :: SelectItemList}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_SelectItemList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOlistType _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlistType :: ([(String,Type)])
              _lhsOfixedUpIdentifiersTree :: SelectItemList
              _lhsOannotatedTree :: SelectItemList
              _lhsOnewFixedUpIdentifiersTree :: SelectItemList
              _lhsOoriginalTree :: SelectItemList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: SelectItem
              _hdIfixedUpIdentifiersTree :: SelectItem
              _hdIitemType :: ([(String,Type)])
              _hdInewFixedUpIdentifiersTree :: SelectItem
              _hdIoriginalTree :: SelectItem
              _hdIseIdTree :: ([SelectItem])
              _tlIannotatedTree :: SelectItemList
              _tlIfixedUpIdentifiersTree :: SelectItemList
              _tlIlistType :: ([(String,Type)])
              _tlInewFixedUpIdentifiersTree :: SelectItemList
              _tlIoriginalTree :: SelectItemList
              -- "./TypeChecking/SelectLists.ag"(line 43, column 12)
              _lhsOlistType =
                  _hdIitemType ++ _tlIlistType
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 166, column 12)
              _lhsOfixedUpIdentifiersTree =
                  _hdIseIdTree ++ _tlIfixedUpIdentifiersTree
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIitemType,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree,_hdIseIdTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIlistType,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlistType :: ([(String,Type)])
              _lhsOfixedUpIdentifiersTree :: SelectItemList
              _lhsOannotatedTree :: SelectItemList
              _lhsOnewFixedUpIdentifiersTree :: SelectItemList
              _lhsOoriginalTree :: SelectItemList
              -- "./TypeChecking/SelectLists.ag"(line 44, column 11)
              _lhsOlistType =
                  []
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 167, column 11)
              _lhsOfixedUpIdentifiersTree =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         listType             : [(String,Type)]
         newFixedUpIdentifiersTree : SELF 
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
            local newFixedUpIdentifiersTree : _
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
                     ( SelectList,IDEnv,SelectList,([LocalBindingsUpdate]),([(String,Type)]),SelectList,SelectList)
data Inh_SelectList  = Inh_SelectList {cat_Inh_SelectList :: Catalog,idenv_Inh_SelectList :: IDEnv,lib_Inh_SelectList :: LocalBindings}
data Syn_SelectList  = Syn_SelectList {annotatedTree_Syn_SelectList :: SelectList,cidenv_Syn_SelectList :: IDEnv,fixedUpIdentifiersTree_Syn_SelectList :: SelectList,libUpdates_Syn_SelectList :: [LocalBindingsUpdate],listType_Syn_SelectList :: [(String,Type)],newFixedUpIdentifiersTree_Syn_SelectList :: SelectList,originalTree_Syn_SelectList :: SelectList}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_SelectList _lhsOannotatedTree _lhsOcidenv _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOlistType _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_SelectList_SelectList :: Annotation ->
                             T_SelectItemList  ->
                             T_ScalarExprList  ->
                             T_SelectList 
sem_SelectList_SelectList ann_ items_ into_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _intoOexpectedTypes :: ([Maybe Type])
              _lhsOlistType :: ([(String,Type)])
              _intoFroms :: (E ([(String,Type)],[(String,Type)]))
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: SelectList
              _lhsOcidenv :: IDEnv
              _lhsOfixedUpIdentifiersTree :: SelectList
              _lhsOnewFixedUpIdentifiersTree :: SelectList
              _lhsOoriginalTree :: SelectList
              _itemsOcat :: Catalog
              _itemsOidenv :: IDEnv
              _itemsOlib :: LocalBindings
              _intoOcat :: Catalog
              _intoOidenv :: IDEnv
              _intoOlib :: LocalBindings
              _itemsIannotatedTree :: SelectItemList
              _itemsIfixedUpIdentifiersTree :: SelectItemList
              _itemsIlistType :: ([(String,Type)])
              _itemsInewFixedUpIdentifiersTree :: SelectItemList
              _itemsIoriginalTree :: SelectItemList
              _intoIannotatedTree :: ScalarExprList
              _intoIfixedUpIdentifiersTree :: ScalarExprList
              _intoInewFixedUpIdentifiersTree :: ScalarExprList
              _intoIoriginalTree :: ScalarExprList
              _intoItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _intoIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs.ag"(line 567, column 18)
              _intoOexpectedTypes =
                  []
              -- "./TypeChecking/SelectLists.ag"(line 80, column 9)
              _lhsOlistType =
                  _itemsIlistType
              -- "./TypeChecking/SelectLists.ag"(line 82, column 9)
              _intoFroms =
                  returnWhen (_intoIoriginalTree == []) ([],[]) $ do
                  it <- lmt intoTypes
                  let ft = _itemsIlistType
                  return (it,ft)
                  where
                    intoTypes :: Maybe [(String,Type)]
                    intoTypes = do
                                ts <- sequence _intoIuType
                                let ns = map getName _intoIoriginalTree
                                return $ zip ns ts
              -- "./TypeChecking/SelectLists.ag"(line 96, column 9)
              _tpe =
                  returnWhen (_intoIoriginalTree == []) () $ do
                  (it,ft) <- _intoFroms
                  checkAssignmentsValid _lhsIcat (map snd ft) (map snd it)
              -- "./TypeChecking/SelectLists.ag"(line 101, column 9)
              _lhsOlibUpdates =
                  maybe [] id $ do
                  _ <- etmt _tpe
                  (it,ft) <- etmt _intoFroms
                  return $ case it of
                    [(n,PgRecord _)] -> [LBIds "set record actual fields from select into"
                                               Nothing
                                               [(n,PgRecord $ Just $ CompositeType ft)]]
                    _ -> []
              -- "./TypeChecking/SelectLists.ag"(line 134, column 9)
              _lhsOannotatedTree =
                  addTypeErrors (tes _tpe    ) $
                  SelectList ann_
                             _itemsIannotatedTree
                             _intoIannotatedTree
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 141, column 18)
              _lhsOcidenv =
                  unimplementedIDEnv
              -- self rule
              _annotatedTree =
                  SelectList ann_ _itemsIannotatedTree _intoIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  SelectList ann_ _itemsIfixedUpIdentifiersTree _intoIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  SelectList ann_ _itemsInewFixedUpIdentifiersTree _intoInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  SelectList ann_ _itemsIoriginalTree _intoIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _itemsOcat =
                  _lhsIcat
              -- copy rule (down)
              _itemsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _itemsOlib =
                  _lhsIlib
              -- copy rule (down)
              _intoOcat =
                  _lhsIcat
              -- copy rule (down)
              _intoOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _intoOlib =
                  _lhsIlib
              ( _itemsIannotatedTree,_itemsIfixedUpIdentifiersTree,_itemsIlistType,_itemsInewFixedUpIdentifiersTree,_itemsIoriginalTree) =
                  (items_ _itemsOcat _itemsOidenv _itemsOlib )
              ( _intoIannotatedTree,_intoIfixedUpIdentifiersTree,_intoInewFixedUpIdentifiersTree,_intoIoriginalTree,_intoItbUTypes,_intoIuType) =
                  (into_ _intoOcat _intoOexpectedTypes _intoOidenv _intoOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOlistType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative AlterSequence:
         child ann            : {Annotation}
         child name           : {String}
         child ownedBy        : ScalarExpr 
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative AlterTable:
         child ann            : {Annotation}
         child name           : {String}
         child actions        : AlterTableActionList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative CaseStatement:
         child ann            : {Annotation}
         child cases          : ScalarExprListStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative CaseStatementSimple:
         child ann            : {Annotation}
         child val            : ScalarExpr 
         child cases          : ScalarExprListStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative ContinueStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Copy:
         child ann            : {Annotation}
         child table          : {String}
         child targetCols     : {[String]}
         child source         : {CopySource}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative CopyData:
         child ann            : {Annotation}
         child insData        : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative CreateView:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : QueryExpr 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Delete:
         child ann            : {Annotation}
         child table          : ScalarExpr 
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Execute:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative ExecuteInto:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         child targets        : {[String]}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative ExitStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative If:
         child ann            : {Annotation}
         child cases          : ScalarExprStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Insert:
         child ann            : {Annotation}
         child table          : ScalarExpr 
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative LoopStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Notify:
         child ann            : {Annotation}
         child name           : {String}
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative NullStatement:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Perform:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Raise:
         child ann            : {Annotation}
         child level          : {RaiseType}
         child message        : {String}
         child args           : ScalarExprList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative ReturnNext:
         child ann            : {Annotation}
         child expr           : ScalarExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative ReturnQuery:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Set:
         child ann            : {Annotation}
         child name           : {String}
         child values         : {[SetValue]}
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Truncate:
         child ann            : {Annotation}
         child tables         : {[String]}
         child restartIdentity : {RestartIdentity}
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Update:
         child ann            : {Annotation}
         child table          : ScalarExpr 
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative WhileStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child expr           : ScalarExpr 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
-}
data Statement  = AlterSequence (Annotation) (String) (ScalarExpr) 
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
                | CreateView (Annotation) (String) (QueryExpr) 
                | Delete (Annotation) (ScalarExpr) (TableRefList) (MaybeBoolExpr) (MaybeSelectList) 
                | DropFunction (Annotation) (IfExists) (StringTypeNameListPairList) (Cascade) 
                | DropSomething (Annotation) (DropType) (IfExists) ([String]) (Cascade) 
                | Execute (Annotation) (ScalarExpr) 
                | ExecuteInto (Annotation) (ScalarExpr) ([String]) 
                | ExitStatement (Annotation) (Maybe String) 
                | ForIntegerStatement (Annotation) (Maybe String) (ScalarExpr) (ScalarExpr) (ScalarExpr) (StatementList) 
                | ForQueryStatement (Annotation) (Maybe String) (ScalarExpr) (QueryExpr) (StatementList) 
                | If (Annotation) (ScalarExprStatementListPairList) (StatementList) 
                | Insert (Annotation) (ScalarExpr) ([String]) (QueryExpr) (MaybeSelectList) 
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
                | Update (Annotation) (ScalarExpr) (ScalarExprList) (TableRefList) (MaybeBoolExpr) (MaybeSelectList) 
                | WhileStatement (Annotation) (Maybe String) (ScalarExpr) (StatementList) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (AlterSequence _ann _name _ownedBy )  =
    (sem_Statement_AlterSequence _ann _name (sem_ScalarExpr _ownedBy ) )
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
sem_Statement (CreateView _ann _name _expr )  =
    (sem_Statement_CreateView _ann _name (sem_QueryExpr _expr ) )
sem_Statement (Delete _ann _table _using _whr _returning )  =
    (sem_Statement_Delete _ann (sem_ScalarExpr _table ) (sem_TableRefList _using ) (sem_MaybeBoolExpr _whr ) (sem_MaybeSelectList _returning ) )
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
    (sem_Statement_Insert _ann (sem_ScalarExpr _table ) _targetCols (sem_QueryExpr _insData ) (sem_MaybeSelectList _returning ) )
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
    (sem_Statement_Update _ann (sem_ScalarExpr _table ) (sem_ScalarExprList _assigns ) (sem_TableRefList _fromList ) (sem_MaybeBoolExpr _whr ) (sem_MaybeSelectList _returning ) )
sem_Statement (WhileStatement _ann _lb _expr _sts )  =
    (sem_Statement_WhileStatement _ann _lb (sem_ScalarExpr _expr ) (sem_StatementList _sts ) )
-- semantic domain
type T_Statement  = Catalog ->
                    IDEnv ->
                    Catalog ->
                    LocalBindings ->
                    ( Statement,([CatalogUpdate]),Statement,([LocalBindingsUpdate]),Statement,Statement)
data Inh_Statement  = Inh_Statement {cat_Inh_Statement :: Catalog,idenv_Inh_Statement :: IDEnv,inProducedCat_Inh_Statement :: Catalog,lib_Inh_Statement :: LocalBindings}
data Syn_Statement  = Syn_Statement {annotatedTree_Syn_Statement :: Statement,catUpdates_Syn_Statement :: [CatalogUpdate],fixedUpIdentifiersTree_Syn_Statement :: Statement,libUpdates_Syn_Statement :: [LocalBindingsUpdate],newFixedUpIdentifiersTree_Syn_Statement :: Statement,originalTree_Syn_Statement :: Statement}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIcat _lhsIidenv _lhsIinProducedCat _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIinProducedCat _lhsIlib )
     in  (Syn_Statement _lhsOannotatedTree _lhsOcatUpdates _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_Statement_AlterSequence :: Annotation ->
                               String ->
                               T_ScalarExpr  ->
                               T_Statement 
sem_Statement_AlterSequence ann_ name_ ownedBy_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _ownedByOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _ownedByOcat :: Catalog
              _ownedByOidenv :: IDEnv
              _ownedByOlib :: LocalBindings
              _ownedByIannotatedTree :: ScalarExpr
              _ownedByIfixedUpIdentifiersTree :: ScalarExpr
              _ownedByInewFixedUpIdentifiersTree :: ScalarExpr
              _ownedByIntAnnotatedTree :: ScalarExpr
              _ownedByIntType :: ([(String,Type)])
              _ownedByIoriginalTree :: ScalarExpr
              _ownedByItbAnnotatedTree :: ScalarExpr
              _ownedByItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _ownedByIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 580, column 21)
              _ownedByOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  AlterSequence ann_ name_ _ownedByIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  AlterSequence ann_ name_ _ownedByIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  AlterSequence ann_ name_ _ownedByInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  AlterSequence ann_ name_ _ownedByIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _ownedByOcat =
                  _lhsIcat
              -- copy rule (down)
              _ownedByOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _ownedByOlib =
                  _lhsIlib
              ( _ownedByIannotatedTree,_ownedByIfixedUpIdentifiersTree,_ownedByInewFixedUpIdentifiersTree,_ownedByIntAnnotatedTree,_ownedByIntType,_ownedByIoriginalTree,_ownedByItbAnnotatedTree,_ownedByItbUType,_ownedByIuType) =
                  (ownedBy_ _ownedByOcat _ownedByOexpectedType _ownedByOidenv _ownedByOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _actionsOcat :: Catalog
              _actionsOidenv :: IDEnv
              _actionsOlib :: LocalBindings
              _actionsIannotatedTree :: AlterTableActionList
              _actionsIfixedUpIdentifiersTree :: AlterTableActionList
              _actionsInewFixedUpIdentifiersTree :: AlterTableActionList
              _actionsIoriginalTree :: AlterTableActionList
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  AlterTable ann_ name_ _actionsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  AlterTable ann_ name_ _actionsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  AlterTable ann_ name_ _actionsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  AlterTable ann_ name_ _actionsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _actionsOcat =
                  _lhsIcat
              -- copy rule (down)
              _actionsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _actionsOlib =
                  _lhsIlib
              ( _actionsIannotatedTree,_actionsIfixedUpIdentifiersTree,_actionsInewFixedUpIdentifiersTree,_actionsIoriginalTree) =
                  (actions_ _actionsOcat _actionsOidenv _actionsOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _targetOcat :: Catalog
              _targetOidenv :: IDEnv
              _targetOlib :: LocalBindings
              _valueOcat :: Catalog
              _valueOidenv :: IDEnv
              _valueOlib :: LocalBindings
              _targetIannotatedTree :: ScalarExpr
              _targetIfixedUpIdentifiersTree :: ScalarExpr
              _targetInewFixedUpIdentifiersTree :: ScalarExpr
              _targetIntAnnotatedTree :: ScalarExpr
              _targetIntType :: ([(String,Type)])
              _targetIoriginalTree :: ScalarExpr
              _targetItbAnnotatedTree :: ScalarExpr
              _targetItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _targetIuType :: (Maybe Type)
              _valueIannotatedTree :: ScalarExpr
              _valueIfixedUpIdentifiersTree :: ScalarExpr
              _valueInewFixedUpIdentifiersTree :: ScalarExpr
              _valueIntAnnotatedTree :: ScalarExpr
              _valueIntType :: ([(String,Type)])
              _valueIoriginalTree :: ScalarExpr
              _valueItbAnnotatedTree :: ScalarExpr
              _valueItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _valueIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 574, column 18)
              _valueOexpectedType =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 581, column 18)
              _targetOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 20, column 9)
              _tpe =
                  do
                  fromType <- lmt _valueIuType
                  toType <- lmt _targetIuType
                  checkAssignmentValid _lhsIcat fromType toType
                  return $ Pseudo Void
              -- "./TypeChecking/Plpgsql.ag"(line 26, column 9)
              _backTree =
                  Assignment ann_ _targetIannotatedTree _valueIannotatedTree
              -- "./TypeChecking/Plpgsql.ag"(line 27, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 28, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  Assignment ann_ _targetIannotatedTree _valueIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Assignment ann_ _targetIfixedUpIdentifiersTree _valueIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Assignment ann_ _targetInewFixedUpIdentifiersTree _valueInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Assignment ann_ _targetIoriginalTree _valueIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _targetOcat =
                  _lhsIcat
              -- copy rule (down)
              _targetOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _targetOlib =
                  _lhsIlib
              -- copy rule (down)
              _valueOcat =
                  _lhsIcat
              -- copy rule (down)
              _valueOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _valueOlib =
                  _lhsIlib
              ( _targetIannotatedTree,_targetIfixedUpIdentifiersTree,_targetInewFixedUpIdentifiersTree,_targetIntAnnotatedTree,_targetIntType,_targetIoriginalTree,_targetItbAnnotatedTree,_targetItbUType,_targetIuType) =
                  (target_ _targetOcat _targetOexpectedType _targetOidenv _targetOlib )
              ( _valueIannotatedTree,_valueIfixedUpIdentifiersTree,_valueInewFixedUpIdentifiersTree,_valueIntAnnotatedTree,_valueIntType,_valueIoriginalTree,_valueItbAnnotatedTree,_valueItbUType,_valueIuType) =
                  (value_ _valueOcat _valueOexpectedType _valueOidenv _valueOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
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
              _varsInewFixedUpIdentifiersTree :: VarDefList
              _varsIoriginalTree :: VarDefList
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsInewFixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 100, column 13)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 101, column 13)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Block.ag"(line 22, column 9)
              _stsOlib =
                  fromRight _lhsIlib $
                  lbUpdate _lhsIcat
                           (LBIds "declarations" lb_ $ mapMaybe lv _varsIdefs)
                           _lhsIlib
                  where
                    lv (_,Nothing) = Nothing
                    lv (s,Just t) = Just (s,t)
              -- self rule
              _annotatedTree =
                  Block ann_ lb_ _varsIannotatedTree _stsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Block ann_ lb_ _varsIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Block ann_ lb_ _varsInewFixedUpIdentifiersTree _stsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Block ann_ lb_ _varsIoriginalTree _stsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (from local)
              _lhsOlibUpdates =
                  _libUpdates
              -- copy rule (down)
              _varsOcat =
                  _lhsIcat
              -- copy rule (down)
              _varsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _varsOlib =
                  _lhsIlib
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOidenv =
                  _lhsIidenv
              -- copy rule (from local)
              _stsOlibUpdates =
                  _libUpdates
              ( _varsIannotatedTree,_varsIdefs,_varsIfixedUpIdentifiersTree,_varsInewFixedUpIdentifiersTree,_varsIoriginalTree) =
                  (vars_ _varsOcat _varsOidenv _varsOlib )
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsInewFixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _casesOcat :: Catalog
              _casesOidenv :: IDEnv
              _casesOlib :: LocalBindings
              _elsOcat :: Catalog
              _elsOidenv :: IDEnv
              _elsOlib :: LocalBindings
              _casesIannotatedTree :: ScalarExprListStatementListPairList
              _casesIfixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _casesInewFixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _casesIoriginalTree :: ScalarExprListStatementListPairList
              _elsIannotatedTree :: StatementList
              _elsIfixedUpIdentifiersTree :: StatementList
              _elsInewFixedUpIdentifiersTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedCat :: Catalog
              _elsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  CaseStatement ann_ _casesIannotatedTree _elsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CaseStatement ann_ _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CaseStatement ann_ _casesInewFixedUpIdentifiersTree _elsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CaseStatement ann_ _casesIoriginalTree _elsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _casesOcat =
                  _lhsIcat
              -- copy rule (down)
              _casesOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _casesOlib =
                  _lhsIlib
              -- copy rule (down)
              _elsOcat =
                  _lhsIcat
              -- copy rule (down)
              _elsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _elsOlib =
                  _lhsIlib
              ( _casesIannotatedTree,_casesIfixedUpIdentifiersTree,_casesInewFixedUpIdentifiersTree,_casesIoriginalTree) =
                  (cases_ _casesOcat _casesOidenv _casesOlib )
              ( _elsIannotatedTree,_elsIfixedUpIdentifiersTree,_elsInewFixedUpIdentifiersTree,_elsIoriginalTree,_elsIproducedCat,_elsIproducedLib) =
                  (els_ _elsOcat _elsOcatUpdates _elsOidenv _elsOlib _elsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
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
              _valInewFixedUpIdentifiersTree :: ScalarExpr
              _valIntAnnotatedTree :: ScalarExpr
              _valIntType :: ([(String,Type)])
              _valIoriginalTree :: ScalarExpr
              _valItbAnnotatedTree :: ScalarExpr
              _valItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _valIuType :: (Maybe Type)
              _casesIannotatedTree :: ScalarExprListStatementListPairList
              _casesIfixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _casesInewFixedUpIdentifiersTree :: ScalarExprListStatementListPairList
              _casesIoriginalTree :: ScalarExprListStatementListPairList
              _elsIannotatedTree :: StatementList
              _elsIfixedUpIdentifiersTree :: StatementList
              _elsInewFixedUpIdentifiersTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedCat :: Catalog
              _elsIproducedLib :: LocalBindings
              -- "./TypeChecking/ScalarExprs.ag"(line 575, column 27)
              _valOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  CaseStatementSimple ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CaseStatementSimple ann_ _valIfixedUpIdentifiersTree _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CaseStatementSimple ann_ _valInewFixedUpIdentifiersTree _casesInewFixedUpIdentifiersTree _elsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CaseStatementSimple ann_ _valIoriginalTree _casesIoriginalTree _elsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _valOcat =
                  _lhsIcat
              -- copy rule (down)
              _valOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _valOlib =
                  _lhsIlib
              -- copy rule (down)
              _casesOcat =
                  _lhsIcat
              -- copy rule (down)
              _casesOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _casesOlib =
                  _lhsIlib
              -- copy rule (down)
              _elsOcat =
                  _lhsIcat
              -- copy rule (down)
              _elsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _elsOlib =
                  _lhsIlib
              ( _valIannotatedTree,_valIfixedUpIdentifiersTree,_valInewFixedUpIdentifiersTree,_valIntAnnotatedTree,_valIntType,_valIoriginalTree,_valItbAnnotatedTree,_valItbUType,_valIuType) =
                  (val_ _valOcat _valOexpectedType _valOidenv _valOlib )
              ( _casesIannotatedTree,_casesIfixedUpIdentifiersTree,_casesInewFixedUpIdentifiersTree,_casesIoriginalTree) =
                  (cases_ _casesOcat _casesOidenv _casesOlib )
              ( _elsIannotatedTree,_elsIfixedUpIdentifiersTree,_elsInewFixedUpIdentifiersTree,_elsIoriginalTree,_elsIproducedCat,_elsIproducedLib) =
                  (els_ _elsOcat _elsOcatUpdates _elsOidenv _elsOlib _elsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  ContinueStatement ann_ lb_
              -- self rule
              _fixedUpIdentifiersTree =
                  ContinueStatement ann_ lb_
              -- self rule
              _newFixedUpIdentifiersTree =
                  ContinueStatement ann_ lb_
              -- self rule
              _originalTree =
                  ContinueStatement ann_ lb_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Copy ann_ table_ targetCols_ source_
              -- self rule
              _fixedUpIdentifiersTree =
                  Copy ann_ table_ targetCols_ source_
              -- self rule
              _newFixedUpIdentifiersTree =
                  Copy ann_ table_ targetCols_ source_
              -- self rule
              _originalTree =
                  Copy ann_ table_ targetCols_ source_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  CopyData ann_ insData_
              -- self rule
              _fixedUpIdentifiersTree =
                  CopyData ann_ insData_
              -- self rule
              _newFixedUpIdentifiersTree =
                  CopyData ann_ insData_
              -- self rule
              _originalTree =
                  CopyData ann_ insData_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _checkOcat :: Catalog
              _checkOidenv :: IDEnv
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typInewFixedUpIdentifiersTree :: TypeName
              _typIoriginalTree :: TypeName
              _checkIannotatedTree :: MaybeBoolExpr
              _checkIfixedUpIdentifiersTree :: MaybeBoolExpr
              _checkInewFixedUpIdentifiersTree :: MaybeBoolExpr
              _checkIoriginalTree :: MaybeBoolExpr
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/MiscCreates.ag"(line 65, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/MiscCreates.ag"(line 66, column 9)
              _backTree =
                  CreateDomain ann_ name_ _typIannotatedTree checkName_ _checkIannotatedTree
              -- "./TypeChecking/MiscCreates.ag"(line 67, column 9)
              _statementType =
                  Nothing
              -- "./TypeChecking/MiscCreates.ag"(line 68, column 9)
              _catUpdates =
                  maybe [] (\t -> [CatCreateDomain (DomainType name_) t]) _typInamedType
              -- "./TypeChecking/MiscCreates.ag"(line 70, column 9)
              _checkOlib =
                  either (const _lhsIlib) id $ do
                  nt <- lmt _typInamedType
                  lbUpdate _lhsIcat
                    (LBIds "domain check value" Nothing [("value", nt)])
                    _lhsIlib
              -- self rule
              _annotatedTree =
                  CreateDomain ann_ name_ _typIannotatedTree checkName_ _checkIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CreateDomain ann_ name_ _typIfixedUpIdentifiersTree checkName_ _checkIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CreateDomain ann_ name_ _typInewFixedUpIdentifiersTree checkName_ _checkInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CreateDomain ann_ name_ _typIoriginalTree checkName_ _checkIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              -- copy rule (down)
              _checkOcat =
                  _lhsIcat
              -- copy rule (down)
              _checkOidenv =
                  _lhsIidenv
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typInewFixedUpIdentifiersTree,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
              ( _checkIannotatedTree,_checkIfixedUpIdentifiersTree,_checkInewFixedUpIdentifiersTree,_checkIoriginalTree) =
                  (check_ _checkOcat _checkOidenv _checkOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
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
              _paramsInewFixedUpIdentifiersTree :: ParamDefList
              _paramsIoriginalTree :: ParamDefList
              _paramsIparams :: ([(ParamName, Maybe Type)])
              _rettypeIannotatedTree :: TypeName
              _rettypeIfixedUpIdentifiersTree :: TypeName
              _rettypeInamedType :: (Maybe Type)
              _rettypeInewFixedUpIdentifiersTree :: TypeName
              _rettypeIoriginalTree :: TypeName
              _bodyIannotatedTree :: FnBody
              _bodyIfixedUpIdentifiersTree :: FnBody
              _bodyInewFixedUpIdentifiersTree :: FnBody
              _bodyIoriginalTree :: FnBody
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/CreateFunction.ag"(line 63, column 9)
              _bodyOlib =
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
              -- "./TypeChecking/CreateFunction.ag"(line 79, column 9)
              _paramsOpos =
                  1
              -- "./TypeChecking/CreateFunction.ag"(line 88, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/CreateFunction.ag"(line 89, column 9)
              _catUpdates =
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
              -- "./TypeChecking/CreateFunction.ag"(line 101, column 9)
              _backTree =
                  CreateFunction ann_
                                 name_
                                 _paramsIannotatedTree
                                 _rettypeIannotatedTree
                                 rep_
                                 lang_
                                 _bodyIannotatedTree
                                 vol_
              -- "./TypeChecking/CreateFunction.ag"(line 109, column 9)
              _statementType =
                  Nothing
              -- "./TypeChecking/CreateFunction.ag"(line 110, column 9)
              _bodyOcat =
                  _lhsIinProducedCat
              -- self rule
              _annotatedTree =
                  CreateFunction ann_ name_ _paramsIannotatedTree _rettypeIannotatedTree rep_ lang_ _bodyIannotatedTree vol_
              -- self rule
              _fixedUpIdentifiersTree =
                  CreateFunction ann_ name_ _paramsIfixedUpIdentifiersTree _rettypeIfixedUpIdentifiersTree rep_ lang_ _bodyIfixedUpIdentifiersTree vol_
              -- self rule
              _newFixedUpIdentifiersTree =
                  CreateFunction ann_ name_ _paramsInewFixedUpIdentifiersTree _rettypeInewFixedUpIdentifiersTree rep_ lang_ _bodyInewFixedUpIdentifiersTree vol_
              -- self rule
              _originalTree =
                  CreateFunction ann_ name_ _paramsIoriginalTree _rettypeIoriginalTree rep_ lang_ _bodyIoriginalTree vol_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _paramsOcat =
                  _lhsIcat
              -- copy rule (down)
              _paramsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _paramsOlib =
                  _lhsIlib
              -- copy rule (down)
              _rettypeOcat =
                  _lhsIcat
              -- copy rule (down)
              _rettypeOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _rettypeOlib =
                  _lhsIlib
              -- copy rule (down)
              _bodyOidenv =
                  _lhsIidenv
              ( _paramsIannotatedTree,_paramsIfixedUpIdentifiersTree,_paramsInewFixedUpIdentifiersTree,_paramsIoriginalTree,_paramsIparams) =
                  (params_ _paramsOcat _paramsOidenv _paramsOlib _paramsOpos )
              ( _rettypeIannotatedTree,_rettypeIfixedUpIdentifiersTree,_rettypeInamedType,_rettypeInewFixedUpIdentifiersTree,_rettypeIoriginalTree) =
                  (rettype_ _rettypeOcat _rettypeOidenv _rettypeOlib )
              ( _bodyIannotatedTree,_bodyIfixedUpIdentifiersTree,_bodyInewFixedUpIdentifiersTree,_bodyIoriginalTree) =
                  (body_ _bodyOcat _bodyOidenv _bodyOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/MiscCreates.ag"(line 78, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/MiscCreates.ag"(line 79, column 9)
              _backTree =
                  CreateLanguage ann_ name_
              -- "./TypeChecking/MiscCreates.ag"(line 80, column 9)
              _statementType =
                  Nothing
              -- "./TypeChecking/MiscCreates.ag"(line 81, column 9)
              _catUpdates =
                  [CatCreateFunction FunName "plpgsql_call_handler" [] (Pseudo LanguageHandler) False
                  ,CatCreateFunction FunName "plpgsql_validator" [ScalarType "oid"] (Pseudo Void) False]
              -- self rule
              _annotatedTree =
                  CreateLanguage ann_ name_
              -- self rule
              _fixedUpIdentifiersTree =
                  CreateLanguage ann_ name_
              -- self rule
              _newFixedUpIdentifiersTree =
                  CreateLanguage ann_ name_
              -- self rule
              _originalTree =
                  CreateLanguage ann_ name_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
              -- self rule
              _fixedUpIdentifiersTree =
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
              -- self rule
              _newFixedUpIdentifiersTree =
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
              -- self rule
              _originalTree =
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _attsOcat :: Catalog
              _attsOidenv :: IDEnv
              _attsOlib :: LocalBindings
              _consOcat :: Catalog
              _consOidenv :: IDEnv
              _attsIannotatedTree :: AttributeDefList
              _attsIattrs :: ([(String, Maybe Type)])
              _attsIfixedUpIdentifiersTree :: AttributeDefList
              _attsInewFixedUpIdentifiersTree :: AttributeDefList
              _attsIoriginalTree :: AttributeDefList
              _consIannotatedTree :: ConstraintList
              _consIfixedUpIdentifiersTree :: ConstraintList
              _consInewFixedUpIdentifiersTree :: ConstraintList
              _consIoriginalTree :: ConstraintList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/CreateTable.ag"(line 31, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/CreateTable.ag"(line 32, column 9)
              _catUpdates =
                  [CatCreateTable name_ _attrs     defaultSystemColumns]
              -- "./TypeChecking/CreateTable.ag"(line 35, column 9)
              _attrs =
                  mapMaybe okAt _attsIattrs
                  where
                    okAt (s, Just t) = Just (s,t)
                    okAt (_,Nothing) = Nothing
              -- "./TypeChecking/CreateTable.ag"(line 40, column 9)
              _statementType =
                  Nothing
              -- "./TypeChecking/CreateTable.ag"(line 41, column 9)
              _backTree =
                  CreateTable ann_
                              name_
                              _attsIannotatedTree
                              _consIannotatedTree
              -- "./TypeChecking/CreateTable.ag"(line 45, column 9)
              _consOlib =
                  case lbUpdate _lhsIcat
                         (LBIds "attributedefs" Nothing _attrs    )
                         _lhsIlib of
                     Left x -> error $ "statement-createtable-cons.lib " ++ show x
                     Right e -> e
              -- self rule
              _annotatedTree =
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CreateTable ann_ name_ _attsIfixedUpIdentifiersTree _consIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CreateTable ann_ name_ _attsInewFixedUpIdentifiersTree _consInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CreateTable ann_ name_ _attsIoriginalTree _consIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _attsOcat =
                  _lhsIcat
              -- copy rule (down)
              _attsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _attsOlib =
                  _lhsIlib
              -- copy rule (down)
              _consOcat =
                  _lhsIcat
              -- copy rule (down)
              _consOidenv =
                  _lhsIidenv
              ( _attsIannotatedTree,_attsIattrs,_attsIfixedUpIdentifiersTree,_attsInewFixedUpIdentifiersTree,_attsIoriginalTree) =
                  (atts_ _attsOcat _attsOidenv _attsOlib )
              ( _consIannotatedTree,_consIfixedUpIdentifiersTree,_consInewFixedUpIdentifiersTree,_consIoriginalTree) =
                  (cons_ _consOcat _consOidenv _consOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: QueryExpr
              _exprIcidenv :: IDEnv
              _exprIfixedUpIdentifiersTree :: QueryExpr
              _exprIlibUpdates :: ([LocalBindingsUpdate])
              _exprInewFixedUpIdentifiersTree :: QueryExpr
              _exprIoriginalTree :: QueryExpr
              _exprIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs.ag"(line 640, column 32)
              _exprOexpectedTypes =
                  []
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/CreateTable.ag"(line 64, column 9)
              _tpe =
                  CompositeType <$> lmt _exprIuType
              -- "./TypeChecking/CreateTable.ag"(line 65, column 9)
              _catUpdates =
                  either (const []) id $ do
                  ats <- _attrs
                  return [CatCreateTable name_ ats defaultSystemColumns]
              -- "./TypeChecking/CreateTable.ag"(line 71, column 9)
              _attrs =
                  lmt _exprIuType
              -- "./TypeChecking/CreateTable.ag"(line 73, column 9)
              _backTree =
                  CreateTableAs ann_ name_ _exprIannotatedTree
              -- "./TypeChecking/CreateTable.ag"(line 74, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  CreateTableAs ann_ name_ _exprIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CreateTableAs ann_ name_ _exprIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CreateTableAs ann_ name_ _exprInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CreateTableAs ann_ name_ _exprIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIcidenv,_exprIfixedUpIdentifiersTree,_exprIlibUpdates,_exprInewFixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedTypes _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _fnArgsOcat :: Catalog
              _fnArgsOidenv :: IDEnv
              _fnArgsOlib :: LocalBindings
              _fnArgsIannotatedTree :: ScalarExprList
              _fnArgsIfixedUpIdentifiersTree :: ScalarExprList
              _fnArgsInewFixedUpIdentifiersTree :: ScalarExprList
              _fnArgsIoriginalTree :: ScalarExprList
              _fnArgsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _fnArgsIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs.ag"(line 618, column 21)
              _fnArgsOexpectedTypes =
                  []
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _fnArgsOcat =
                  _lhsIcat
              -- copy rule (down)
              _fnArgsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _fnArgsOlib =
                  _lhsIlib
              ( _fnArgsIannotatedTree,_fnArgsIfixedUpIdentifiersTree,_fnArgsInewFixedUpIdentifiersTree,_fnArgsIoriginalTree,_fnArgsItbUTypes,_fnArgsIuType) =
                  (fnArgs_ _fnArgsOcat _fnArgsOexpectedTypes _fnArgsOidenv _fnArgsOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _attsOcat :: Catalog
              _attsOidenv :: IDEnv
              _attsOlib :: LocalBindings
              _attsIannotatedTree :: TypeAttributeDefList
              _attsIattrs :: ([(String, Maybe Type)])
              _attsIfixedUpIdentifiersTree :: TypeAttributeDefList
              _attsInewFixedUpIdentifiersTree :: TypeAttributeDefList
              _attsIoriginalTree :: TypeAttributeDefList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/MiscCreates.ag"(line 48, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/MiscCreates.ag"(line 49, column 9)
              _attrs =
                  mapMaybe okAt _attsIattrs
                  where
                    okAt (s, Just t) = Just (s,t)
                    okAt (_,Nothing) = Nothing
              -- "./TypeChecking/MiscCreates.ag"(line 53, column 9)
              _backTree =
                  CreateType ann_ name_ _attsIannotatedTree
              -- "./TypeChecking/MiscCreates.ag"(line 54, column 9)
              _statementType =
                  Nothing
              -- "./TypeChecking/MiscCreates.ag"(line 55, column 9)
              _catUpdates =
                  [CatCreateComposite name_ _attrs    ]
              -- self rule
              _annotatedTree =
                  CreateType ann_ name_ _attsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CreateType ann_ name_ _attsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CreateType ann_ name_ _attsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CreateType ann_ name_ _attsIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _attsOcat =
                  _lhsIcat
              -- copy rule (down)
              _attsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _attsOlib =
                  _lhsIlib
              ( _attsIannotatedTree,_attsIattrs,_attsIfixedUpIdentifiersTree,_attsInewFixedUpIdentifiersTree,_attsIoriginalTree) =
                  (atts_ _attsOcat _attsOidenv _attsOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_Statement_CreateView :: Annotation ->
                            String ->
                            T_QueryExpr  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ expr_  =
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: QueryExpr
              _exprIcidenv :: IDEnv
              _exprIfixedUpIdentifiersTree :: QueryExpr
              _exprIlibUpdates :: ([LocalBindingsUpdate])
              _exprInewFixedUpIdentifiersTree :: QueryExpr
              _exprIoriginalTree :: QueryExpr
              _exprIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs.ag"(line 640, column 32)
              _exprOexpectedTypes =
                  []
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/MiscCreates.ag"(line 15, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/MiscCreates.ag"(line 16, column 9)
              _backTree =
                  CreateView ann_ name_ _exprIannotatedTree
              -- "./TypeChecking/MiscCreates.ag"(line 17, column 9)
              _catUpdates =
                  maybe [] (\a -> [CatCreateView name_ a]) _exprIuType
              -- "./TypeChecking/MiscCreates.ag"(line 19, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  CreateView ann_ name_ _exprIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  CreateView ann_ name_ _exprIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  CreateView ann_ name_ _exprInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  CreateView ann_ name_ _exprIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIcidenv,_exprIfixedUpIdentifiersTree,_exprIlibUpdates,_exprInewFixedUpIdentifiersTree,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedTypes _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_Statement_Delete :: Annotation ->
                        T_ScalarExpr  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Delete ann_ table_ using_ whr_ returning_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _tableOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _whrOlib :: LocalBindings
              _returningOlib :: LocalBindings
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOnewFixedUpIdentifiersTree :: Statement
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
              _tableIannotatedTree :: ScalarExpr
              _tableIfixedUpIdentifiersTree :: ScalarExpr
              _tableInewFixedUpIdentifiersTree :: ScalarExpr
              _tableIntAnnotatedTree :: ScalarExpr
              _tableIntType :: ([(String,Type)])
              _tableIoriginalTree :: ScalarExpr
              _tableItbAnnotatedTree :: ScalarExpr
              _tableItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tableIuType :: (Maybe Type)
              _usingIannotatedTree :: TableRefList
              _usingIcidenv :: IDEnv
              _usingIfixedUpIdentifiersTree :: TableRefList
              _usingIlibUpdates :: ([LocalBindingsUpdate])
              _usingInewFixedUpIdentifiersTree :: TableRefList
              _usingIoriginalTree :: TableRefList
              _whrIannotatedTree :: MaybeBoolExpr
              _whrIfixedUpIdentifiersTree :: MaybeBoolExpr
              _whrInewFixedUpIdentifiersTree :: MaybeBoolExpr
              _whrIoriginalTree :: MaybeBoolExpr
              _returningIannotatedTree :: MaybeSelectList
              _returningIfixedUpIdentifiersTree :: MaybeSelectList
              _returningIlistType :: ([(String,Type)])
              _returningInewFixedUpIdentifiersTree :: MaybeSelectList
              _returningIoriginalTree :: MaybeSelectList
              -- "./TypeChecking/ScalarExprs.ag"(line 651, column 28)
              _tableOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Delete.ag"(line 13, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/Delete.ag"(line 14, column 9)
              _statementType =
                  do
                  pt <- sequence $ getPlaceholderTypes _whrIannotatedTree
                  return (pt,_returningIlistType)
              -- "./TypeChecking/Delete.ag"(line 18, column 9)
              _backTree =
                  Delete ann_ _tableItbAnnotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
              -- "./TypeChecking/Delete.ag"(line 19, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/Delete.ag"(line 21, column 9)
              _lib =
                  either (const _lhsIlib) id $ do
                  a <- lmt (allAtts <$> _tableItbUType)
                  lbUpdate _lhsIcat (LBIds "delete table attrs" (Just $ getName _tableIannotatedTree) a) _lhsIlib
              -- "./TypeChecking/Delete.ag"(line 25, column 9)
              _whrOlib =
                  _lib
              -- "./TypeChecking/Delete.ag"(line 26, column 9)
              _returningOlib =
                  _lib
              -- self rule
              _annotatedTree =
                  Delete ann_ _tableIannotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Delete ann_ _tableIfixedUpIdentifiersTree _usingIfixedUpIdentifiersTree _whrIfixedUpIdentifiersTree _returningIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Delete ann_ _tableInewFixedUpIdentifiersTree _usingInewFixedUpIdentifiersTree _whrInewFixedUpIdentifiersTree _returningInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Delete ann_ _tableIoriginalTree _usingIoriginalTree _whrIoriginalTree _returningIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tableOcat =
                  _lhsIcat
              -- copy rule (down)
              _tableOidenv =
                  _lhsIidenv
              -- copy rule (from local)
              _tableOlib =
                  _lib
              -- copy rule (down)
              _usingOcat =
                  _lhsIcat
              -- copy rule (down)
              _usingOidenv =
                  _lhsIidenv
              -- copy rule (from local)
              _usingOlib =
                  _lib
              -- copy rule (down)
              _whrOcat =
                  _lhsIcat
              -- copy rule (down)
              _whrOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _returningOcat =
                  _lhsIcat
              -- copy rule (down)
              _returningOidenv =
                  _lhsIidenv
              ( _tableIannotatedTree,_tableIfixedUpIdentifiersTree,_tableInewFixedUpIdentifiersTree,_tableIntAnnotatedTree,_tableIntType,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType,_tableIuType) =
                  (table_ _tableOcat _tableOexpectedType _tableOidenv _tableOlib )
              ( _usingIannotatedTree,_usingIcidenv,_usingIfixedUpIdentifiersTree,_usingIlibUpdates,_usingInewFixedUpIdentifiersTree,_usingIoriginalTree) =
                  (using_ _usingOcat _usingOidenv _usingOlib )
              ( _whrIannotatedTree,_whrIfixedUpIdentifiersTree,_whrInewFixedUpIdentifiersTree,_whrIoriginalTree) =
                  (whr_ _whrOcat _whrOidenv _whrOlib )
              ( _returningIannotatedTree,_returningIfixedUpIdentifiersTree,_returningIlistType,_returningInewFixedUpIdentifiersTree,_returningIoriginalTree) =
                  (returning_ _returningOcat _returningOidenv _returningOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _sigsOcat :: Catalog
              _sigsOidenv :: IDEnv
              _sigsOlib :: LocalBindings
              _sigsIannotatedTree :: StringTypeNameListPairList
              _sigsIfixedUpIdentifiersTree :: StringTypeNameListPairList
              _sigsIfnSigs :: ([(String,[Maybe Type])])
              _sigsInewFixedUpIdentifiersTree :: StringTypeNameListPairList
              _sigsIoriginalTree :: StringTypeNameListPairList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Drops.ag"(line 10, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/Drops.ag"(line 11, column 9)
              _backTree =
                  DropFunction ann_ ifE_ _sigsIannotatedTree cascade_
              -- "./TypeChecking/Drops.ag"(line 12, column 9)
              _catUpdates =
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
              -- "./TypeChecking/Drops.ag"(line 23, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  DropFunction ann_ ifE_ _sigsIannotatedTree cascade_
              -- self rule
              _fixedUpIdentifiersTree =
                  DropFunction ann_ ifE_ _sigsIfixedUpIdentifiersTree cascade_
              -- self rule
              _newFixedUpIdentifiersTree =
                  DropFunction ann_ ifE_ _sigsInewFixedUpIdentifiersTree cascade_
              -- self rule
              _originalTree =
                  DropFunction ann_ ifE_ _sigsIoriginalTree cascade_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _sigsOcat =
                  _lhsIcat
              -- copy rule (down)
              _sigsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _sigsOlib =
                  _lhsIlib
              ( _sigsIannotatedTree,_sigsIfixedUpIdentifiersTree,_sigsIfnSigs,_sigsInewFixedUpIdentifiersTree,_sigsIoriginalTree) =
                  (sigs_ _sigsOcat _sigsOidenv _sigsOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
              -- self rule
              _fixedUpIdentifiersTree =
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
              -- self rule
              _newFixedUpIdentifiersTree =
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
              -- self rule
              _originalTree =
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 577, column 9)
              _exprOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Execute ann_ _exprIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Execute ann_ _exprIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Execute ann_ _exprInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Execute ann_ _exprIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 577, column 9)
              _exprOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  ExecuteInto ann_ _exprIannotatedTree targets_
              -- self rule
              _fixedUpIdentifiersTree =
                  ExecuteInto ann_ _exprIfixedUpIdentifiersTree targets_
              -- self rule
              _newFixedUpIdentifiersTree =
                  ExecuteInto ann_ _exprInewFixedUpIdentifiersTree targets_
              -- self rule
              _originalTree =
                  ExecuteInto ann_ _exprIoriginalTree targets_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  ExitStatement ann_ lb_
              -- self rule
              _fixedUpIdentifiersTree =
                  ExitStatement ann_ lb_
              -- self rule
              _newFixedUpIdentifiersTree =
                  ExitStatement ann_ lb_
              -- self rule
              _originalTree =
                  ExitStatement ann_ lb_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
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
              _varInewFixedUpIdentifiersTree :: ScalarExpr
              _varIntAnnotatedTree :: ScalarExpr
              _varIntType :: ([(String,Type)])
              _varIoriginalTree :: ScalarExpr
              _varItbAnnotatedTree :: ScalarExpr
              _varItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _varIuType :: (Maybe Type)
              _fromIannotatedTree :: ScalarExpr
              _fromIfixedUpIdentifiersTree :: ScalarExpr
              _fromInewFixedUpIdentifiersTree :: ScalarExpr
              _fromIntAnnotatedTree :: ScalarExpr
              _fromIntType :: ([(String,Type)])
              _fromIoriginalTree :: ScalarExpr
              _fromItbAnnotatedTree :: ScalarExpr
              _fromItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _fromIuType :: (Maybe Type)
              _toIannotatedTree :: ScalarExpr
              _toIfixedUpIdentifiersTree :: ScalarExpr
              _toInewFixedUpIdentifiersTree :: ScalarExpr
              _toIntAnnotatedTree :: ScalarExpr
              _toIntType :: ([(String,Type)])
              _toIoriginalTree :: ScalarExpr
              _toItbAnnotatedTree :: ScalarExpr
              _toItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _toIuType :: (Maybe Type)
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsInewFixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/ScalarExprs.ag"(line 578, column 27)
              _fromOexpectedType =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 579, column 27)
              _toOexpectedType =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 582, column 45)
              _varOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 32, column 9)
              _tpe =
                  do
                  fromType <- lmt _fromIuType
                  toType <- lmt _toIuType
                  errorWhen (fromType /= toType) [FromToTypesNotSame fromType toType]
                  case _varIuType of
                    Just t -> checkAssignmentValid _lhsIcat fromType t
                    Nothing -> return ()
                  return $ Pseudo Void
              -- "./TypeChecking/Plpgsql.ag"(line 41, column 9)
              _implicitVar =
                  case _varIannotatedTree of
                      Identifier a i | errs a == [UnrecognisedIdentifier i] -> True
                      _ -> False
              -- "./TypeChecking/Plpgsql.ag"(line 44, column 9)
              _stsOlib =
                  if _implicitVar
                  then either (const _lhsIlib) id $ do
                       ft <- lmt _fromIuType
                       lbUpdate _lhsIcat
                          (LBIds "local for loop variable" Nothing [((getName _varIannotatedTree),ft)]) _lhsIlib
                  else _lhsIlib
              -- "./TypeChecking/Plpgsql.ag"(line 52, column 9)
              _backTree =
                  let i = if _implicitVar
                          then let (Identifier a i) = _varIannotatedTree
                               in Identifier a { errs = []} i
                          else _varIannotatedTree
                  in ForIntegerStatement ann_ lb_ i _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
              -- "./TypeChecking/Plpgsql.ag"(line 58, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 59, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  ForIntegerStatement ann_ lb_ _varIannotatedTree _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  ForIntegerStatement ann_ lb_ _varIfixedUpIdentifiersTree _fromIfixedUpIdentifiersTree _toIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  ForIntegerStatement ann_ lb_ _varInewFixedUpIdentifiersTree _fromInewFixedUpIdentifiersTree _toInewFixedUpIdentifiersTree _stsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  ForIntegerStatement ann_ lb_ _varIoriginalTree _fromIoriginalTree _toIoriginalTree _stsIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _varOcat =
                  _lhsIcat
              -- copy rule (down)
              _varOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _varOlib =
                  _lhsIlib
              -- copy rule (down)
              _fromOcat =
                  _lhsIcat
              -- copy rule (down)
              _fromOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _fromOlib =
                  _lhsIlib
              -- copy rule (down)
              _toOcat =
                  _lhsIcat
              -- copy rule (down)
              _toOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _toOlib =
                  _lhsIlib
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOidenv =
                  _lhsIidenv
              ( _varIannotatedTree,_varIfixedUpIdentifiersTree,_varInewFixedUpIdentifiersTree,_varIntAnnotatedTree,_varIntType,_varIoriginalTree,_varItbAnnotatedTree,_varItbUType,_varIuType) =
                  (var_ _varOcat _varOexpectedType _varOidenv _varOlib )
              ( _fromIannotatedTree,_fromIfixedUpIdentifiersTree,_fromInewFixedUpIdentifiersTree,_fromIntAnnotatedTree,_fromIntType,_fromIoriginalTree,_fromItbAnnotatedTree,_fromItbUType,_fromIuType) =
                  (from_ _fromOcat _fromOexpectedType _fromOidenv _fromOlib )
              ( _toIannotatedTree,_toIfixedUpIdentifiersTree,_toInewFixedUpIdentifiersTree,_toIntAnnotatedTree,_toIntType,_toIoriginalTree,_toItbAnnotatedTree,_toItbUType,_toIuType) =
                  (to_ _toOcat _toOexpectedType _toOidenv _toOlib )
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsInewFixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
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
              _varInewFixedUpIdentifiersTree :: ScalarExpr
              _varIntAnnotatedTree :: ScalarExpr
              _varIntType :: ([(String,Type)])
              _varIoriginalTree :: ScalarExpr
              _varItbAnnotatedTree :: ScalarExpr
              _varItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _varIuType :: (Maybe Type)
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selInewFixedUpIdentifiersTree :: QueryExpr
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsInewFixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/ScalarExprs.ag"(line 582, column 45)
              _varOexpectedType =
                  Nothing
              -- "./TypeChecking/ScalarExprs.ag"(line 642, column 9)
              _selOexpectedTypes =
                  []
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 64, column 9)
              _tpe =
                  do
                  st <- lmt (CompositeType <$> _selIuType)
                  toType <- lmt _varIuType
                  checkAssignmentValid _lhsIcat st toType
                  return $ Pseudo Void
              -- "./TypeChecking/Plpgsql.ag"(line 74, column 9)
              _stsOlib =
                  either (const _lhsIlib) id $ do
                  _ <- _tpe
                  st <- lmt (CompositeType <$> _selIuType)
                  lbUpdate _lhsIcat (LBIds "for loop record type" Nothing [(getName _varIannotatedTree,st)]) _lhsIlib
              -- "./TypeChecking/Plpgsql.ag"(line 80, column 9)
              _backTree =
                  ForQueryStatement ann_ lb_ _varIannotatedTree _selIannotatedTree _stsIannotatedTree
              -- "./TypeChecking/Plpgsql.ag"(line 81, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 82, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  ForQueryStatement ann_ lb_ _varIannotatedTree _selIannotatedTree _stsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  ForQueryStatement ann_ lb_ _varIfixedUpIdentifiersTree _selIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  ForQueryStatement ann_ lb_ _varInewFixedUpIdentifiersTree _selInewFixedUpIdentifiersTree _stsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  ForQueryStatement ann_ lb_ _varIoriginalTree _selIoriginalTree _stsIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _varOcat =
                  _lhsIcat
              -- copy rule (down)
              _varOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _varOlib =
                  _lhsIlib
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOidenv =
                  _lhsIidenv
              ( _varIannotatedTree,_varIfixedUpIdentifiersTree,_varInewFixedUpIdentifiersTree,_varIntAnnotatedTree,_varIntType,_varIoriginalTree,_varItbAnnotatedTree,_varItbUType,_varIuType) =
                  (var_ _varOcat _varOexpectedType _varOidenv _varOlib )
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selInewFixedUpIdentifiersTree,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsInewFixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _casesOcat :: Catalog
              _casesOidenv :: IDEnv
              _casesOlib :: LocalBindings
              _elsOcat :: Catalog
              _elsOidenv :: IDEnv
              _elsOlib :: LocalBindings
              _casesIannotatedTree :: ScalarExprStatementListPairList
              _casesIfixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _casesInewFixedUpIdentifiersTree :: ScalarExprStatementListPairList
              _casesIoriginalTree :: ScalarExprStatementListPairList
              _elsIannotatedTree :: StatementList
              _elsIfixedUpIdentifiersTree :: StatementList
              _elsInewFixedUpIdentifiersTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedCat :: Catalog
              _elsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  If ann_ _casesIannotatedTree _elsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  If ann_ _casesIfixedUpIdentifiersTree _elsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  If ann_ _casesInewFixedUpIdentifiersTree _elsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  If ann_ _casesIoriginalTree _elsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _casesOcat =
                  _lhsIcat
              -- copy rule (down)
              _casesOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _casesOlib =
                  _lhsIlib
              -- copy rule (down)
              _elsOcat =
                  _lhsIcat
              -- copy rule (down)
              _elsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _elsOlib =
                  _lhsIlib
              ( _casesIannotatedTree,_casesIfixedUpIdentifiersTree,_casesInewFixedUpIdentifiersTree,_casesIoriginalTree) =
                  (cases_ _casesOcat _casesOidenv _casesOlib )
              ( _elsIannotatedTree,_elsIfixedUpIdentifiersTree,_elsInewFixedUpIdentifiersTree,_elsIoriginalTree,_elsIproducedCat,_elsIproducedLib) =
                  (els_ _elsOcat _elsOcatUpdates _elsOidenv _elsOlib _elsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_Statement_Insert :: Annotation ->
                        T_ScalarExpr  ->
                        ([String]) ->
                        T_QueryExpr  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIinProducedCat
       _lhsIlib ->
         (let _tableOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _columnTypes :: (Either [TypeError] [(String,Type)])
              _catUpdates :: ([CatalogUpdate])
              _insDataOexpectedTypes :: ([Maybe Type])
              _returningOlib :: LocalBindings
              _lhsOfixedUpIdentifiersTree :: Statement
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _tableOcat :: Catalog
              _tableOidenv :: IDEnv
              _tableOlib :: LocalBindings
              _insDataOcat :: Catalog
              _insDataOidenv :: IDEnv
              _insDataOlib :: LocalBindings
              _returningOcat :: Catalog
              _returningOidenv :: IDEnv
              _tableIannotatedTree :: ScalarExpr
              _tableIfixedUpIdentifiersTree :: ScalarExpr
              _tableInewFixedUpIdentifiersTree :: ScalarExpr
              _tableIntAnnotatedTree :: ScalarExpr
              _tableIntType :: ([(String,Type)])
              _tableIoriginalTree :: ScalarExpr
              _tableItbAnnotatedTree :: ScalarExpr
              _tableItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tableIuType :: (Maybe Type)
              _insDataIannotatedTree :: QueryExpr
              _insDataIcidenv :: IDEnv
              _insDataIfixedUpIdentifiersTree :: QueryExpr
              _insDataIlibUpdates :: ([LocalBindingsUpdate])
              _insDataInewFixedUpIdentifiersTree :: QueryExpr
              _insDataIoriginalTree :: QueryExpr
              _insDataIuType :: (Maybe [(String,Type)])
              _returningIannotatedTree :: MaybeSelectList
              _returningIfixedUpIdentifiersTree :: MaybeSelectList
              _returningIlistType :: ([(String,Type)])
              _returningInewFixedUpIdentifiersTree :: MaybeSelectList
              _returningIoriginalTree :: MaybeSelectList
              -- "./TypeChecking/ScalarExprs.ag"(line 651, column 28)
              _tableOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Insert.ag"(line 14, column 9)
              _tpe =
                  either Left (const $ Right $ Pseudo Void) _columnTypes
              -- "./TypeChecking/Insert.ag"(line 15, column 9)
              _statementType =
                  Just (catMaybes $ getPlaceholderTypes _insDataIannotatedTree
                       ,_returningIlistType)
              -- "./TypeChecking/Insert.ag"(line 20, column 9)
              _columnTypes =
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
              -- "./TypeChecking/Insert.ag"(line 36, column 9)
              _backTree =
                  Insert ann_ _tableItbAnnotatedTree
                         targetCols_
                         _insDataIannotatedTree
                         _returningIannotatedTree
              -- "./TypeChecking/Insert.ag"(line 40, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/Insert.ag"(line 41, column 9)
              _insDataOexpectedTypes =
                  maybe [] id $ do
                  ts <- etmt $ _columnTypes
                  return $ map (Just . snd) ts
              -- "./TypeChecking/Insert.ag"(line 45, column 9)
              _returningOlib =
                  either (const _lhsIlib) id $ do
                    atts <- lmt (allAtts <$> _tableItbUType)
                    lbUpdate _lhsIcat (LBIds "insert target table" (Just $ getName _tableIannotatedTree) atts) _lhsIlib
              -- self rule
              _annotatedTree =
                  Insert ann_ _tableIannotatedTree targetCols_ _insDataIannotatedTree _returningIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Insert ann_ _tableIfixedUpIdentifiersTree targetCols_ _insDataIfixedUpIdentifiersTree _returningIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Insert ann_ _tableInewFixedUpIdentifiersTree targetCols_ _insDataInewFixedUpIdentifiersTree _returningInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Insert ann_ _tableIoriginalTree targetCols_ _insDataIoriginalTree _returningIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tableOcat =
                  _lhsIcat
              -- copy rule (down)
              _tableOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tableOlib =
                  _lhsIlib
              -- copy rule (down)
              _insDataOcat =
                  _lhsIcat
              -- copy rule (down)
              _insDataOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _insDataOlib =
                  _lhsIlib
              -- copy rule (down)
              _returningOcat =
                  _lhsIcat
              -- copy rule (down)
              _returningOidenv =
                  _lhsIidenv
              ( _tableIannotatedTree,_tableIfixedUpIdentifiersTree,_tableInewFixedUpIdentifiersTree,_tableIntAnnotatedTree,_tableIntType,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType,_tableIuType) =
                  (table_ _tableOcat _tableOexpectedType _tableOidenv _tableOlib )
              ( _insDataIannotatedTree,_insDataIcidenv,_insDataIfixedUpIdentifiersTree,_insDataIlibUpdates,_insDataInewFixedUpIdentifiersTree,_insDataIoriginalTree,_insDataIuType) =
                  (insData_ _insDataOcat _insDataOexpectedTypes _insDataOidenv _insDataOlib )
              ( _returningIannotatedTree,_returningIfixedUpIdentifiersTree,_returningIlistType,_returningInewFixedUpIdentifiersTree,_returningIoriginalTree) =
                  (returning_ _returningOcat _returningOidenv _returningOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _stsOcat :: Catalog
              _stsOidenv :: IDEnv
              _stsOlib :: LocalBindings
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsInewFixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  LoopStatement ann_ lb_ _stsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  LoopStatement ann_ lb_ _stsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  LoopStatement ann_ lb_ _stsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  LoopStatement ann_ lb_ _stsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _stsOlib =
                  _lhsIlib
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsInewFixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Notify ann_ name_
              -- self rule
              _fixedUpIdentifiersTree =
                  Notify ann_ name_
              -- self rule
              _newFixedUpIdentifiersTree =
                  Notify ann_ name_
              -- self rule
              _originalTree =
                  Notify ann_ name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  NullStatement ann_
              -- self rule
              _fixedUpIdentifiersTree =
                  NullStatement ann_
              -- self rule
              _newFixedUpIdentifiersTree =
                  NullStatement ann_
              -- self rule
              _originalTree =
                  NullStatement ann_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 577, column 9)
              _exprOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Perform ann_ _exprIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Perform ann_ _exprIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Perform ann_ _exprInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Perform ann_ _exprIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exOcat :: Catalog
              _exOidenv :: IDEnv
              _exOlib :: LocalBindings
              _exIannotatedTree :: QueryExpr
              _exIcidenv :: IDEnv
              _exIfixedUpIdentifiersTree :: QueryExpr
              _exIlibUpdates :: ([LocalBindingsUpdate])
              _exInewFixedUpIdentifiersTree :: QueryExpr
              _exIoriginalTree :: QueryExpr
              _exIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs.ag"(line 643, column 22)
              _exOexpectedTypes =
                  []
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/QueryStatement.ag"(line 14, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/QueryStatement.ag"(line 15, column 9)
              _statementType =
                  do
                  pt <- sequence $ getPlaceholderTypes _exIannotatedTree
                  st <- _exIuType
                  return (pt
                         ,case st of
                            [(_,(Pseudo Void))] -> []
                            t -> t)
              -- "./TypeChecking/QueryStatement.ag"(line 23, column 9)
              _backTree =
                  QueryStatement ann_ _exIannotatedTree
              -- "./TypeChecking/QueryStatement.ag"(line 24, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/QueryStatement.ag"(line 111, column 9)
              _libUpdates =
                  _exIlibUpdates
              -- self rule
              _annotatedTree =
                  QueryStatement ann_ _exIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  QueryStatement ann_ _exIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  QueryStatement ann_ _exInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  QueryStatement ann_ _exIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exOcat =
                  _lhsIcat
              -- copy rule (down)
              _exOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exOlib =
                  _lhsIlib
              ( _exIannotatedTree,_exIcidenv,_exIfixedUpIdentifiersTree,_exIlibUpdates,_exInewFixedUpIdentifiersTree,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedTypes _exOidenv _exOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _argsOcat :: Catalog
              _argsOidenv :: IDEnv
              _argsOlib :: LocalBindings
              _argsIannotatedTree :: ScalarExprList
              _argsIfixedUpIdentifiersTree :: ScalarExprList
              _argsInewFixedUpIdentifiersTree :: ScalarExprList
              _argsIoriginalTree :: ScalarExprList
              _argsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _argsIuType :: ([Maybe Type])
              -- "./TypeChecking/ScalarExprs.ag"(line 619, column 13)
              _argsOexpectedTypes =
                  []
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Raise ann_ level_ message_ _argsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Raise ann_ level_ message_ _argsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Raise ann_ level_ message_ _argsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Raise ann_ level_ message_ _argsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _argsOcat =
                  _lhsIcat
              -- copy rule (down)
              _argsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _argsOlib =
                  _lhsIlib
              ( _argsIannotatedTree,_argsIfixedUpIdentifiersTree,_argsInewFixedUpIdentifiersTree,_argsIoriginalTree,_argsItbUTypes,_argsIuType) =
                  (args_ _argsOcat _argsOexpectedTypes _argsOidenv _argsOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _valueOcat :: Catalog
              _valueOidenv :: IDEnv
              _valueOlib :: LocalBindings
              _valueIannotatedTree :: MaybeScalarExpr
              _valueIfixedUpIdentifiersTree :: MaybeScalarExpr
              _valueInewFixedUpIdentifiersTree :: MaybeScalarExpr
              _valueIoriginalTree :: MaybeScalarExpr
              _valueIuType :: (Maybe Type)
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 12, column 9)
              _tpe =
                  maybe (Right $ Pseudo Void) Right _valueIuType
              -- "./TypeChecking/Plpgsql.ag"(line 13, column 9)
              _backTree =
                  Return ann_ _valueIannotatedTree
              -- "./TypeChecking/Plpgsql.ag"(line 14, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 15, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  Return ann_ _valueIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Return ann_ _valueIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Return ann_ _valueInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Return ann_ _valueIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _valueOcat =
                  _lhsIcat
              -- copy rule (down)
              _valueOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _valueOlib =
                  _lhsIlib
              ( _valueIannotatedTree,_valueIfixedUpIdentifiersTree,_valueInewFixedUpIdentifiersTree,_valueIoriginalTree,_valueIuType) =
                  (value_ _valueOcat _valueOidenv _valueOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 577, column 9)
              _exprOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  ReturnNext ann_ _exprIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  ReturnNext ann_ _exprIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  ReturnNext ann_ _exprInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  ReturnNext ann_ _exprIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selInewFixedUpIdentifiersTree :: QueryExpr
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs.ag"(line 642, column 9)
              _selOexpectedTypes =
                  []
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  ReturnQuery ann_ _selIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  ReturnQuery ann_ _selIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  ReturnQuery ann_ _selInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  ReturnQuery ann_ _selIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selInewFixedUpIdentifiersTree,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Set ann_ name_ values_
              -- self rule
              _fixedUpIdentifiersTree =
                  Set ann_ name_ values_
              -- self rule
              _newFixedUpIdentifiersTree =
                  Set ann_ name_ values_
              -- self rule
              _originalTree =
                  Set ann_ name_ values_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Truncate ann_ tables_ restartIdentity_ cascade_
              -- self rule
              _fixedUpIdentifiersTree =
                  Truncate ann_ tables_ restartIdentity_ cascade_
              -- self rule
              _newFixedUpIdentifiersTree =
                  Truncate ann_ tables_ restartIdentity_ cascade_
              -- self rule
              _originalTree =
                  Truncate ann_ tables_ restartIdentity_ cascade_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_Statement_Update :: Annotation ->
                        T_ScalarExpr  ->
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
              _tableOexpectedType :: (Maybe Type)
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
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
              _tableIannotatedTree :: ScalarExpr
              _tableIfixedUpIdentifiersTree :: ScalarExpr
              _tableInewFixedUpIdentifiersTree :: ScalarExpr
              _tableIntAnnotatedTree :: ScalarExpr
              _tableIntType :: ([(String,Type)])
              _tableIoriginalTree :: ScalarExpr
              _tableItbAnnotatedTree :: ScalarExpr
              _tableItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tableIuType :: (Maybe Type)
              _assignsIannotatedTree :: ScalarExprList
              _assignsIfixedUpIdentifiersTree :: ScalarExprList
              _assignsInewFixedUpIdentifiersTree :: ScalarExprList
              _assignsIoriginalTree :: ScalarExprList
              _assignsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _assignsIuType :: ([Maybe Type])
              _fromListIannotatedTree :: TableRefList
              _fromListIcidenv :: IDEnv
              _fromListIfixedUpIdentifiersTree :: TableRefList
              _fromListIlibUpdates :: ([LocalBindingsUpdate])
              _fromListInewFixedUpIdentifiersTree :: TableRefList
              _fromListIoriginalTree :: TableRefList
              _whrIannotatedTree :: MaybeBoolExpr
              _whrIfixedUpIdentifiersTree :: MaybeBoolExpr
              _whrInewFixedUpIdentifiersTree :: MaybeBoolExpr
              _whrIoriginalTree :: MaybeBoolExpr
              _returningIannotatedTree :: MaybeSelectList
              _returningIfixedUpIdentifiersTree :: MaybeSelectList
              _returningIlistType :: ([(String,Type)])
              _returningInewFixedUpIdentifiersTree :: MaybeSelectList
              _returningIoriginalTree :: MaybeSelectList
              -- "./TypeChecking/ScalarExprs.ag"(line 620, column 14)
              _assignsOexpectedTypes =
                  []
              -- "./TypeChecking/ScalarExprs.ag"(line 651, column 28)
              _tableOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Update.ag"(line 13, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/Update.ag"(line 18, column 9)
              _statementType =
                  do
                  pt <- sequence $ getPlaceholderTypes _assignsIannotatedTree
                                   ++ getPlaceholderTypes _whrIannotatedTree
                  return (pt,_returningIlistType)
              -- "./TypeChecking/Update.ag"(line 24, column 9)
              _backTree =
                  Update ann_
                         _tableItbAnnotatedTree
                         _assignsIannotatedTree
                         _fromListIannotatedTree
                         _whrIannotatedTree
                         _returningIannotatedTree
              -- "./TypeChecking/Update.ag"(line 30, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/Update.ag"(line 35, column 9)
              _lib =
                  either (const _lhsIlib) id $ do
                  a <- lmt (allAtts <$> _tableItbUType)
                  lbUpdate _lhsIcat (LBIds "updated table attrs" (Just $ getName _tableIannotatedTree) a) _lhsIlib
              -- "./TypeChecking/Update.ag"(line 39, column 9)
              _whrOlib =
                  _lib
              -- "./TypeChecking/Update.ag"(line 40, column 9)
              _assignsOlib =
                  _lib
              -- "./TypeChecking/Update.ag"(line 41, column 9)
              _returningOlib =
                  _lib
              -- self rule
              _annotatedTree =
                  Update ann_ _tableIannotatedTree _assignsIannotatedTree _fromListIannotatedTree _whrIannotatedTree _returningIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  Update ann_ _tableIfixedUpIdentifiersTree _assignsIfixedUpIdentifiersTree _fromListIfixedUpIdentifiersTree _whrIfixedUpIdentifiersTree _returningIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  Update ann_ _tableInewFixedUpIdentifiersTree _assignsInewFixedUpIdentifiersTree _fromListInewFixedUpIdentifiersTree _whrInewFixedUpIdentifiersTree _returningInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  Update ann_ _tableIoriginalTree _assignsIoriginalTree _fromListIoriginalTree _whrIoriginalTree _returningIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tableOcat =
                  _lhsIcat
              -- copy rule (down)
              _tableOidenv =
                  _lhsIidenv
              -- copy rule (from local)
              _tableOlib =
                  _lib
              -- copy rule (down)
              _assignsOcat =
                  _lhsIcat
              -- copy rule (down)
              _assignsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _fromListOcat =
                  _lhsIcat
              -- copy rule (down)
              _fromListOidenv =
                  _lhsIidenv
              -- copy rule (from local)
              _fromListOlib =
                  _lib
              -- copy rule (down)
              _whrOcat =
                  _lhsIcat
              -- copy rule (down)
              _whrOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _returningOcat =
                  _lhsIcat
              -- copy rule (down)
              _returningOidenv =
                  _lhsIidenv
              ( _tableIannotatedTree,_tableIfixedUpIdentifiersTree,_tableInewFixedUpIdentifiersTree,_tableIntAnnotatedTree,_tableIntType,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType,_tableIuType) =
                  (table_ _tableOcat _tableOexpectedType _tableOidenv _tableOlib )
              ( _assignsIannotatedTree,_assignsIfixedUpIdentifiersTree,_assignsInewFixedUpIdentifiersTree,_assignsIoriginalTree,_assignsItbUTypes,_assignsIuType) =
                  (assigns_ _assignsOcat _assignsOexpectedTypes _assignsOidenv _assignsOlib )
              ( _fromListIannotatedTree,_fromListIcidenv,_fromListIfixedUpIdentifiersTree,_fromListIlibUpdates,_fromListInewFixedUpIdentifiersTree,_fromListIoriginalTree) =
                  (fromList_ _fromListOcat _fromListOidenv _fromListOlib )
              ( _whrIannotatedTree,_whrIfixedUpIdentifiersTree,_whrInewFixedUpIdentifiersTree,_whrIoriginalTree) =
                  (whr_ _whrOcat _whrOidenv _whrOlib )
              ( _returningIannotatedTree,_returningIfixedUpIdentifiersTree,_returningIlistType,_returningInewFixedUpIdentifiersTree,_returningIoriginalTree) =
                  (returning_ _returningOcat _returningOidenv _returningOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOidenv :: IDEnv
              _exprOlib :: LocalBindings
              _stsOcat :: Catalog
              _stsOidenv :: IDEnv
              _stsOlib :: LocalBindings
              _exprIannotatedTree :: ScalarExpr
              _exprIfixedUpIdentifiersTree :: ScalarExpr
              _exprInewFixedUpIdentifiersTree :: ScalarExpr
              _exprIntAnnotatedTree :: ScalarExpr
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: ScalarExpr
              _exprItbAnnotatedTree :: ScalarExpr
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              _stsIannotatedTree :: StatementList
              _stsIfixedUpIdentifiersTree :: StatementList
              _stsInewFixedUpIdentifiersTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/ScalarExprs.ag"(line 577, column 9)
              _exprOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  WhileStatement ann_ lb_ _exprIannotatedTree _stsIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  WhileStatement ann_ lb_ _exprIfixedUpIdentifiersTree _stsIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  WhileStatement ann_ lb_ _exprInewFixedUpIdentifiersTree _stsInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  WhileStatement ann_ lb_ _exprIoriginalTree _stsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _stsOlib =
                  _lhsIlib
              ( _exprIannotatedTree,_exprIfixedUpIdentifiersTree,_exprInewFixedUpIdentifiersTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOidenv _exprOlib )
              ( _stsIannotatedTree,_stsIfixedUpIdentifiersTree,_stsInewFixedUpIdentifiersTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOidenv _stsOlib _stsOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local newCat      : _
            local newLib      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                        ( StatementList,StatementList,StatementList,StatementList,Catalog,LocalBindings)
data Inh_StatementList  = Inh_StatementList {cat_Inh_StatementList :: Catalog,catUpdates_Inh_StatementList :: [CatalogUpdate],idenv_Inh_StatementList :: IDEnv,lib_Inh_StatementList :: LocalBindings,libUpdates_Inh_StatementList :: [LocalBindingsUpdate]}
data Syn_StatementList  = Syn_StatementList {annotatedTree_Syn_StatementList :: StatementList,fixedUpIdentifiersTree_Syn_StatementList :: StatementList,newFixedUpIdentifiersTree_Syn_StatementList :: StatementList,originalTree_Syn_StatementList :: StatementList,producedCat_Syn_StatementList :: Catalog,producedLib_Syn_StatementList :: LocalBindings}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIcat _lhsIcatUpdates _lhsIidenv _lhsIlib _lhsIlibUpdates )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib) =
             (sem _lhsIcat _lhsIcatUpdates _lhsIidenv _lhsIlib _lhsIlibUpdates )
     in  (Syn_StatementList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOproducedCat _lhsOproducedLib ))
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
              _lhsOnewFixedUpIdentifiersTree :: StatementList
              _lhsOoriginalTree :: StatementList
              _hdOidenv :: IDEnv
              _tlOidenv :: IDEnv
              _hdIannotatedTree :: Statement
              _hdIcatUpdates :: ([CatalogUpdate])
              _hdIfixedUpIdentifiersTree :: Statement
              _hdIlibUpdates :: ([LocalBindingsUpdate])
              _hdInewFixedUpIdentifiersTree :: Statement
              _hdIoriginalTree :: Statement
              _tlIannotatedTree :: StatementList
              _tlIfixedUpIdentifiersTree :: StatementList
              _tlInewFixedUpIdentifiersTree :: StatementList
              _tlIoriginalTree :: StatementList
              _tlIproducedCat :: Catalog
              _tlIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 56, column 9)
              _newCat =
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
              -- "./TypeChecking/Statements.ag"(line 57, column 9)
              _newLib =
                  fromRight _lhsIlib $ foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _lhsIlibUpdates
              -- "./TypeChecking/Statements.ag"(line 59, column 9)
              _hdOcat =
                  _newCat
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _tlOcat =
                  _newCat
              -- "./TypeChecking/Statements.ag"(line 61, column 9)
              _hdOlib =
                  _newLib
              -- "./TypeChecking/Statements.ag"(line 62, column 9)
              _tlOlib =
                  _newLib
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOproducedCat =
                  _tlIproducedCat
              -- "./TypeChecking/Statements.ag"(line 67, column 9)
              _lhsOproducedLib =
                  _tlIproducedLib
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _tlOcatUpdates =
                  _hdIcatUpdates
              -- "./TypeChecking/Statements.ag"(line 71, column 9)
              _tlOlibUpdates =
                  _hdIlibUpdates
              -- "./TypeChecking/Statements.ag"(line 97, column 12)
              _hdOinProducedCat =
                  _tlIproducedCat
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              ( _hdIannotatedTree,_hdIcatUpdates,_hdIfixedUpIdentifiersTree,_hdIlibUpdates,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOinProducedCat _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree,_tlIproducedCat,_tlIproducedLib) =
                  (tl_ _tlOcat _tlOcatUpdates _tlOidenv _tlOlib _tlOlibUpdates )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib)))
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
              _lhsOnewFixedUpIdentifiersTree :: StatementList
              _lhsOoriginalTree :: StatementList
              -- "./TypeChecking/Statements.ag"(line 56, column 9)
              _newCat =
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
              -- "./TypeChecking/Statements.ag"(line 57, column 9)
              _newLib =
                  fromRight _lhsIlib $ foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _lhsIlibUpdates
              -- "./TypeChecking/Statements.ag"(line 73, column 9)
              _lhsOproducedCat =
                  _newCat
              -- "./TypeChecking/Statements.ag"(line 74, column 9)
              _lhsOproducedLib =
                  _newLib
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : {String}
         child x2             : TypeNameList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                                 ( StringTypeNameListPair,StringTypeNameListPair,((String,[Maybe Type])),StringTypeNameListPair,StringTypeNameListPair)
data Inh_StringTypeNameListPair  = Inh_StringTypeNameListPair {cat_Inh_StringTypeNameListPair :: Catalog,idenv_Inh_StringTypeNameListPair :: IDEnv,lib_Inh_StringTypeNameListPair :: LocalBindings}
data Syn_StringTypeNameListPair  = Syn_StringTypeNameListPair {annotatedTree_Syn_StringTypeNameListPair :: StringTypeNameListPair,fixedUpIdentifiersTree_Syn_StringTypeNameListPair :: StringTypeNameListPair,fnSig_Syn_StringTypeNameListPair :: (String,[Maybe Type]),newFixedUpIdentifiersTree_Syn_StringTypeNameListPair :: StringTypeNameListPair,originalTree_Syn_StringTypeNameListPair :: StringTypeNameListPair}
wrap_StringTypeNameListPair :: T_StringTypeNameListPair  ->
                               Inh_StringTypeNameListPair  ->
                               Syn_StringTypeNameListPair 
wrap_StringTypeNameListPair sem (Inh_StringTypeNameListPair _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSig,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_StringTypeNameListPair _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOfnSig _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: StringTypeNameListPair
              _lhsOoriginalTree :: StringTypeNameListPair
              _x2Ocat :: Catalog
              _x2Oidenv :: IDEnv
              _x2Olib :: LocalBindings
              _x2IannotatedTree :: TypeNameList
              _x2IfixedUpIdentifiersTree :: TypeNameList
              _x2InamedTypes :: ([Maybe Type])
              _x2InewFixedUpIdentifiersTree :: TypeNameList
              _x2IoriginalTree :: TypeNameList
              -- "./TypeChecking/Drops.ag"(line 32, column 13)
              _lhsOfnSig =
                  (x1_, _x2InamedTypes)
              -- self rule
              _annotatedTree =
                  (x1_,_x2IannotatedTree)
              -- self rule
              _fixedUpIdentifiersTree =
                  (x1_,_x2IfixedUpIdentifiersTree)
              -- self rule
              _newFixedUpIdentifiersTree =
                  (x1_,_x2InewFixedUpIdentifiersTree)
              -- self rule
              _originalTree =
                  (x1_,_x2IoriginalTree)
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _x2Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x2Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _x2Olib =
                  _lhsIlib
              ( _x2IannotatedTree,_x2IfixedUpIdentifiersTree,_x2InamedTypes,_x2InewFixedUpIdentifiersTree,_x2IoriginalTree) =
                  (x2_ _x2Ocat _x2Oidenv _x2Olib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSig,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : StringTypeNameListPair 
         child tl             : StringTypeNameListPairList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                                     ( StringTypeNameListPairList,StringTypeNameListPairList,([(String,[Maybe Type])]),StringTypeNameListPairList,StringTypeNameListPairList)
data Inh_StringTypeNameListPairList  = Inh_StringTypeNameListPairList {cat_Inh_StringTypeNameListPairList :: Catalog,idenv_Inh_StringTypeNameListPairList :: IDEnv,lib_Inh_StringTypeNameListPairList :: LocalBindings}
data Syn_StringTypeNameListPairList  = Syn_StringTypeNameListPairList {annotatedTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList,fixedUpIdentifiersTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList,fnSigs_Syn_StringTypeNameListPairList :: [(String,[Maybe Type])],newFixedUpIdentifiersTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList,originalTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList}
wrap_StringTypeNameListPairList :: T_StringTypeNameListPairList  ->
                                   Inh_StringTypeNameListPairList  ->
                                   Syn_StringTypeNameListPairList 
wrap_StringTypeNameListPairList sem (Inh_StringTypeNameListPairList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSigs,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_StringTypeNameListPairList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOfnSigs _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: StringTypeNameListPairList
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
              _hdInewFixedUpIdentifiersTree :: StringTypeNameListPair
              _hdIoriginalTree :: StringTypeNameListPair
              _tlIannotatedTree :: StringTypeNameListPairList
              _tlIfixedUpIdentifiersTree :: StringTypeNameListPairList
              _tlIfnSigs :: ([(String,[Maybe Type])])
              _tlInewFixedUpIdentifiersTree :: StringTypeNameListPairList
              _tlIoriginalTree :: StringTypeNameListPairList
              -- "./TypeChecking/Drops.ag"(line 27, column 12)
              _lhsOfnSigs =
                  _hdIfnSig : _tlIfnSigs
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdIfnSig,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlIfnSigs,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSigs,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_StringTypeNameListPairList_Nil :: T_StringTypeNameListPairList 
sem_StringTypeNameListPairList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOfnSigs :: ([(String,[Maybe Type])])
              _lhsOannotatedTree :: StringTypeNameListPairList
              _lhsOfixedUpIdentifiersTree :: StringTypeNameListPairList
              _lhsOnewFixedUpIdentifiersTree :: StringTypeNameListPairList
              _lhsOoriginalTree :: StringTypeNameListPairList
              -- "./TypeChecking/Drops.ag"(line 28, column 11)
              _lhsOfnSigs =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOfnSigs,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
-- TableRef ----------------------------------------------------
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
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
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative SubTref:
         child ann            : {Annotation}
         child sel            : QueryExpr 
         child alias          : {TableAlias}
         visit 0:
            local errs        : _
            local selectAttrs : {Either [TypeError] [(String,Type)]}
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Tref:
         child ann            : {Annotation}
         child tbl            : ScalarExpr 
         child alias          : {TableAlias}
         visit 0:
            local errs        : _
            local backTree    : _
            local fields      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
-}
data TableRef  = FunTref (Annotation) (ScalarExpr) (TableAlias) 
               | JoinTref (Annotation) (TableRef) (Natural) (JoinType) (TableRef) (OnExpr) (TableAlias) 
               | SubTref (Annotation) (QueryExpr) (TableAlias) 
               | Tref (Annotation) (ScalarExpr) (TableAlias) 
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
    (sem_TableRef_Tref _ann (sem_ScalarExpr _tbl ) _alias )
-- semantic domain
type T_TableRef  = Catalog ->
                   IDEnv ->
                   LocalBindings ->
                   ( TableRef,IDEnv,TableRef,([LocalBindingsUpdate]),TableRef,TableRef)
data Inh_TableRef  = Inh_TableRef {cat_Inh_TableRef :: Catalog,idenv_Inh_TableRef :: IDEnv,lib_Inh_TableRef :: LocalBindings}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef,cidenv_Syn_TableRef :: IDEnv,fixedUpIdentifiersTree_Syn_TableRef :: TableRef,libUpdates_Syn_TableRef :: [LocalBindingsUpdate],newFixedUpIdentifiersTree_Syn_TableRef :: TableRef,originalTree_Syn_TableRef :: TableRef}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TableRef _lhsOannotatedTree _lhsOcidenv _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOcidenv :: IDEnv
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOnewFixedUpIdentifiersTree :: TableRef
              _lhsOoriginalTree :: TableRef
              _fnOcat :: Catalog
              _fnOidenv :: IDEnv
              _fnOlib :: LocalBindings
              _fnIannotatedTree :: ScalarExpr
              _fnIfixedUpIdentifiersTree :: ScalarExpr
              _fnInewFixedUpIdentifiersTree :: ScalarExpr
              _fnIntAnnotatedTree :: ScalarExpr
              _fnIntType :: ([(String,Type)])
              _fnIoriginalTree :: ScalarExpr
              _fnItbAnnotatedTree :: ScalarExpr
              _fnItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _fnIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 584, column 15)
              _fnOexpectedType =
                  Nothing
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  addTypeErrors _errs     _backTree
              -- "./TypeChecking/TableRefs.ag"(line 164, column 9)
              _errs =
                  case _eqfunIdens of
                    Left e -> e
                    Right _ -> []
              -- "./TypeChecking/TableRefs.ag"(line 170, column 9)
              _eqfunIdens =
                  funIdens _lhsIcat (getAlias "" alias_) _fnIannotatedTree _fnIuType
              -- "./TypeChecking/TableRefs.ag"(line 171, column 9)
              _lhsOlibUpdates =
                  [LBTref "fn"
                                  (fst _qfunIdens    )
                                  (snd _qfunIdens    )
                                  []]
              -- "./TypeChecking/TableRefs.ag"(line 175, column 9)
              _qfunIdens =
                  fromRight ("",[]) _eqfunIdens
              -- "./TypeChecking/TableRefs.ag"(line 263, column 9)
              _backTree =
                  FunTref ann_ _fnIannotatedTree alias_
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 138, column 32)
              _lhsOcidenv =
                  unimplementedIDEnv
              -- self rule
              _annotatedTree =
                  FunTref ann_ _fnIannotatedTree alias_
              -- self rule
              _fixedUpIdentifiersTree =
                  FunTref ann_ _fnIfixedUpIdentifiersTree alias_
              -- self rule
              _newFixedUpIdentifiersTree =
                  FunTref ann_ _fnInewFixedUpIdentifiersTree alias_
              -- self rule
              _originalTree =
                  FunTref ann_ _fnIoriginalTree alias_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _fnOcat =
                  _lhsIcat
              -- copy rule (down)
              _fnOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _fnOlib =
                  _lhsIlib
              ( _fnIannotatedTree,_fnIfixedUpIdentifiersTree,_fnInewFixedUpIdentifiersTree,_fnIntAnnotatedTree,_fnIntType,_fnIoriginalTree,_fnItbAnnotatedTree,_fnItbUType,_fnIuType) =
                  (fn_ _fnOcat _fnOexpectedType _fnOidenv _fnOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOcidenv :: IDEnv
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOnewFixedUpIdentifiersTree :: TableRef
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
              _tblIcidenv :: IDEnv
              _tblIfixedUpIdentifiersTree :: TableRef
              _tblIlibUpdates :: ([LocalBindingsUpdate])
              _tblInewFixedUpIdentifiersTree :: TableRef
              _tblIoriginalTree :: TableRef
              _tbl1IannotatedTree :: TableRef
              _tbl1Icidenv :: IDEnv
              _tbl1IfixedUpIdentifiersTree :: TableRef
              _tbl1IlibUpdates :: ([LocalBindingsUpdate])
              _tbl1InewFixedUpIdentifiersTree :: TableRef
              _tbl1IoriginalTree :: TableRef
              _onExprIannotatedTree :: OnExpr
              _onExprIfixedUpIdentifiersTree :: OnExpr
              _onExprInewFixedUpIdentifiersTree :: OnExpr
              _onExprIoriginalTree :: OnExpr
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  addTypeErrors _errs     _backTree
              -- "./TypeChecking/TableRefs.ag"(line 180, column 9)
              _errs =
                  fromLeft [] _newLib
                  ++ _joinErrors
              -- "./TypeChecking/TableRefs.ag"(line 182, column 9)
              _lhsOlibUpdates =
                  if _joinErrors     == []
                  then _libUpdates
                  else []
              -- "./TypeChecking/TableRefs.ag"(line 187, column 9)
              _joinErrors =
                  fromLeft [] (foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _libUpdates    )
              -- "./TypeChecking/TableRefs.ag"(line 188, column 9)
              _libUpdates =
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
              -- "./TypeChecking/TableRefs.ag"(line 202, column 9)
              _newLib =
                  case (_tblIlibUpdates, _tbl1IlibUpdates) of
                    ([u1],[u2]) -> lbUpdate _lhsIcat
                                     (LBJoinTref "join" u1 u2 (Right []) Nothing) _lhsIlib
                    _ -> Right _lhsIlib
              -- "./TypeChecking/TableRefs.ag"(line 206, column 9)
              _onExprOlib =
                  fromRight _lhsIlib _newLib
              -- "./TypeChecking/TableRefs.ag"(line 265, column 9)
              _backTree =
                  JoinTref ann_
                             _tblIannotatedTree
                             nat_
                             joinType_
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
                             alias_
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 138, column 32)
              _lhsOcidenv =
                  unimplementedIDEnv
              -- self rule
              _annotatedTree =
                  JoinTref ann_ _tblIannotatedTree nat_ joinType_ _tbl1IannotatedTree _onExprIannotatedTree alias_
              -- self rule
              _fixedUpIdentifiersTree =
                  JoinTref ann_ _tblIfixedUpIdentifiersTree nat_ joinType_ _tbl1IfixedUpIdentifiersTree _onExprIfixedUpIdentifiersTree alias_
              -- self rule
              _newFixedUpIdentifiersTree =
                  JoinTref ann_ _tblInewFixedUpIdentifiersTree nat_ joinType_ _tbl1InewFixedUpIdentifiersTree _onExprInewFixedUpIdentifiersTree alias_
              -- self rule
              _originalTree =
                  JoinTref ann_ _tblIoriginalTree nat_ joinType_ _tbl1IoriginalTree _onExprIoriginalTree alias_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tblOcat =
                  _lhsIcat
              -- copy rule (down)
              _tblOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tblOlib =
                  _lhsIlib
              -- copy rule (down)
              _tbl1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _tbl1Oidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tbl1Olib =
                  _lhsIlib
              -- copy rule (down)
              _onExprOcat =
                  _lhsIcat
              -- copy rule (down)
              _onExprOidenv =
                  _lhsIidenv
              ( _tblIannotatedTree,_tblIcidenv,_tblIfixedUpIdentifiersTree,_tblIlibUpdates,_tblInewFixedUpIdentifiersTree,_tblIoriginalTree) =
                  (tbl_ _tblOcat _tblOidenv _tblOlib )
              ( _tbl1IannotatedTree,_tbl1Icidenv,_tbl1IfixedUpIdentifiersTree,_tbl1IlibUpdates,_tbl1InewFixedUpIdentifiersTree,_tbl1IoriginalTree) =
                  (tbl1_ _tbl1Ocat _tbl1Oidenv _tbl1Olib )
              ( _onExprIannotatedTree,_onExprIfixedUpIdentifiersTree,_onExprInewFixedUpIdentifiersTree,_onExprIoriginalTree) =
                  (onExpr_ _onExprOcat _onExprOidenv _onExprOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOcidenv :: IDEnv
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOnewFixedUpIdentifiersTree :: TableRef
              _lhsOoriginalTree :: TableRef
              _selOcat :: Catalog
              _selOidenv :: IDEnv
              _selOlib :: LocalBindings
              _selIannotatedTree :: QueryExpr
              _selIcidenv :: IDEnv
              _selIfixedUpIdentifiersTree :: QueryExpr
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selInewFixedUpIdentifiersTree :: QueryExpr
              _selIoriginalTree :: QueryExpr
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs.ag"(line 645, column 15)
              _selOexpectedTypes =
                  []
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  addTypeErrors _errs     _backTree
              -- "./TypeChecking/TableRefs.ag"(line 134, column 9)
              _errs =
                  case _selectAttrs     of
                          Left e -> e
                          Right _ -> []
              -- "./TypeChecking/TableRefs.ag"(line 138, column 9)
              _selectAttrs =
                  lmt _selIuType
              -- "./TypeChecking/TableRefs.ag"(line 139, column 9)
              _lhsOlibUpdates =
                  [LBTref "sub query" (getAlias "" alias_)
                                  (fromRight [] _selectAttrs    ) []]
              -- "./TypeChecking/TableRefs.ag"(line 259, column 9)
              _backTree =
                  SubTref ann_ _selIannotatedTree alias_
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 138, column 32)
              _lhsOcidenv =
                  unimplementedIDEnv
              -- self rule
              _annotatedTree =
                  SubTref ann_ _selIannotatedTree alias_
              -- self rule
              _fixedUpIdentifiersTree =
                  SubTref ann_ _selIfixedUpIdentifiersTree alias_
              -- self rule
              _newFixedUpIdentifiersTree =
                  SubTref ann_ _selInewFixedUpIdentifiersTree alias_
              -- self rule
              _originalTree =
                  SubTref ann_ _selIoriginalTree alias_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              ( _selIannotatedTree,_selIcidenv,_selIfixedUpIdentifiersTree,_selIlibUpdates,_selInewFixedUpIdentifiersTree,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOidenv _selOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_TableRef_Tref :: Annotation ->
                     T_ScalarExpr  ->
                     TableAlias ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_ alias_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _tblOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOcidenv :: IDEnv
              _lhsOfixedUpIdentifiersTree :: TableRef
              _lhsOnewFixedUpIdentifiersTree :: TableRef
              _lhsOoriginalTree :: TableRef
              _tblOcat :: Catalog
              _tblOidenv :: IDEnv
              _tblOlib :: LocalBindings
              _tblIannotatedTree :: ScalarExpr
              _tblIfixedUpIdentifiersTree :: ScalarExpr
              _tblInewFixedUpIdentifiersTree :: ScalarExpr
              _tblIntAnnotatedTree :: ScalarExpr
              _tblIntType :: ([(String,Type)])
              _tblIoriginalTree :: ScalarExpr
              _tblItbAnnotatedTree :: ScalarExpr
              _tblItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tblIuType :: (Maybe Type)
              -- "./TypeChecking/ScalarExprs.ag"(line 585, column 12)
              _tblOexpectedType =
                  Nothing
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  addTypeErrors _errs     _backTree
              -- "./TypeChecking/TableRefs.ag"(line 147, column 9)
              _errs =
                  []
              -- "./TypeChecking/TableRefs.ag"(line 148, column 9)
              _lhsOlibUpdates =
                  maybe [] id $ do
                  let n = getName _tblIannotatedTree
                  (pu,pr) <- _tblItbUType
                  return [LBTref ("tref: " ++ n)
                            (getAlias n alias_)
                            pu
                            pr]
              -- "./TypeChecking/TableRefs.ag"(line 261, column 9)
              _backTree =
                  Tref ann_ _tblItbAnnotatedTree alias_
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 136, column 12)
              _fields =
                  getTableFields _lhsIcat _tblIoriginalTree
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 137, column 12)
              _lhsOcidenv =
                  uncurry makeIDEnv $ _fields
              -- self rule
              _annotatedTree =
                  Tref ann_ _tblIannotatedTree alias_
              -- self rule
              _fixedUpIdentifiersTree =
                  Tref ann_ _tblIfixedUpIdentifiersTree alias_
              -- self rule
              _newFixedUpIdentifiersTree =
                  Tref ann_ _tblInewFixedUpIdentifiersTree alias_
              -- self rule
              _originalTree =
                  Tref ann_ _tblIoriginalTree alias_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tblOcat =
                  _lhsIcat
              -- copy rule (down)
              _tblOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tblOlib =
                  _lhsIlib
              ( _tblIannotatedTree,_tblIfixedUpIdentifiersTree,_tblInewFixedUpIdentifiersTree,_tblIntAnnotatedTree,_tblIntType,_tblIoriginalTree,_tblItbAnnotatedTree,_tblItbUType,_tblIuType) =
                  (tbl_ _tblOcat _tblOexpectedType _tblOidenv _tblOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
-- TableRefList ------------------------------------------------
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : TableRef 
         child tl             : TableRefList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                       ( TableRefList,IDEnv,TableRefList,([LocalBindingsUpdate]),TableRefList,TableRefList)
data Inh_TableRefList  = Inh_TableRefList {cat_Inh_TableRefList :: Catalog,idenv_Inh_TableRefList :: IDEnv,lib_Inh_TableRefList :: LocalBindings}
data Syn_TableRefList  = Syn_TableRefList {annotatedTree_Syn_TableRefList :: TableRefList,cidenv_Syn_TableRefList :: IDEnv,fixedUpIdentifiersTree_Syn_TableRefList :: TableRefList,libUpdates_Syn_TableRefList :: [LocalBindingsUpdate],newFixedUpIdentifiersTree_Syn_TableRefList :: TableRefList,originalTree_Syn_TableRefList :: TableRefList}
wrap_TableRefList :: T_TableRefList  ->
                     Inh_TableRefList  ->
                     Syn_TableRefList 
wrap_TableRefList sem (Inh_TableRefList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TableRefList _lhsOannotatedTree _lhsOcidenv _lhsOfixedUpIdentifiersTree _lhsOlibUpdates _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_TableRefList_Cons :: T_TableRef  ->
                         T_TableRefList  ->
                         T_TableRefList 
sem_TableRefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOcidenv :: IDEnv
              _lhsOannotatedTree :: TableRefList
              _lhsOfixedUpIdentifiersTree :: TableRefList
              _lhsOnewFixedUpIdentifiersTree :: TableRefList
              _lhsOoriginalTree :: TableRefList
              _hdOcat :: Catalog
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOcat :: Catalog
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: TableRef
              _hdIcidenv :: IDEnv
              _hdIfixedUpIdentifiersTree :: TableRef
              _hdIlibUpdates :: ([LocalBindingsUpdate])
              _hdInewFixedUpIdentifiersTree :: TableRef
              _hdIoriginalTree :: TableRef
              _tlIannotatedTree :: TableRefList
              _tlIcidenv :: IDEnv
              _tlIfixedUpIdentifiersTree :: TableRefList
              _tlIlibUpdates :: ([LocalBindingsUpdate])
              _tlInewFixedUpIdentifiersTree :: TableRefList
              _tlIoriginalTree :: TableRefList
              -- "./TypeChecking/TableRefs.ag"(line 97, column 9)
              _lhsOlibUpdates =
                  _hdIlibUpdates
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 241, column 12)
              _lhsOcidenv =
                  joinIDEnvs _hdIcidenv _tlIcidenv
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIcidenv,_hdIfixedUpIdentifiersTree,_hdIlibUpdates,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIcidenv,_tlIfixedUpIdentifiersTree,_tlIlibUpdates,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_TableRefList_Nil :: T_TableRefList 
sem_TableRefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOcidenv :: IDEnv
              _lhsOannotatedTree :: TableRefList
              _lhsOfixedUpIdentifiersTree :: TableRefList
              _lhsOnewFixedUpIdentifiersTree :: TableRefList
              _lhsOoriginalTree :: TableRefList
              -- "./TypeChecking/TableRefs.ag"(line 95, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/FixUpIdentifiers.ag"(line 242, column 11)
              _lhsOcidenv =
                  emptyIDEnv
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOcidenv,_lhsOfixedUpIdentifiersTree,_lhsOlibUpdates,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative TypeAttDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                           ( TypeAttributeDef,String,TypeAttributeDef,(Maybe Type),TypeAttributeDef,TypeAttributeDef)
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {cat_Inh_TypeAttributeDef :: Catalog,idenv_Inh_TypeAttributeDef :: IDEnv,lib_Inh_TypeAttributeDef :: LocalBindings}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef,attrName_Syn_TypeAttributeDef :: String,fixedUpIdentifiersTree_Syn_TypeAttributeDef :: TypeAttributeDef,namedType_Syn_TypeAttributeDef :: Maybe Type,newFixedUpIdentifiersTree_Syn_TypeAttributeDef :: TypeAttributeDef,originalTree_Syn_TypeAttributeDef :: TypeAttributeDef}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TypeAttributeDef _lhsOannotatedTree _lhsOattrName _lhsOfixedUpIdentifiersTree _lhsOnamedType _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: TypeAttributeDef
              _lhsOoriginalTree :: TypeAttributeDef
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typInewFixedUpIdentifiersTree :: TypeName
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/MiscCreates.ag"(line 37, column 9)
              _lhsOattrName =
                  name_
              -- "./TypeChecking/MiscCreates.ag"(line 38, column 9)
              _lhsOnamedType =
                  _typInamedType
              -- self rule
              _annotatedTree =
                  TypeAttDef ann_ name_ _typIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  TypeAttDef ann_ name_ _typIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  TypeAttDef ann_ name_ _typInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  TypeAttDef ann_ name_ _typIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typInewFixedUpIdentifiersTree,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : TypeAttributeDef 
         child tl             : TypeAttributeDefList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                               ( TypeAttributeDefList,([(String, Maybe Type)]),TypeAttributeDefList,TypeAttributeDefList,TypeAttributeDefList)
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {cat_Inh_TypeAttributeDefList :: Catalog,idenv_Inh_TypeAttributeDefList :: IDEnv,lib_Inh_TypeAttributeDefList :: LocalBindings}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,attrs_Syn_TypeAttributeDefList :: [(String, Maybe Type)],fixedUpIdentifiersTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,newFixedUpIdentifiersTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,originalTree_Syn_TypeAttributeDefList :: TypeAttributeDefList}
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TypeAttributeDefList _lhsOannotatedTree _lhsOattrs _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: TypeAttributeDefList
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
              _hdInewFixedUpIdentifiersTree :: TypeAttributeDef
              _hdIoriginalTree :: TypeAttributeDef
              _tlIannotatedTree :: TypeAttributeDefList
              _tlIattrs :: ([(String, Maybe Type)])
              _tlIfixedUpIdentifiersTree :: TypeAttributeDefList
              _tlInewFixedUpIdentifiersTree :: TypeAttributeDefList
              _tlIoriginalTree :: TypeAttributeDefList
              -- "./TypeChecking/MiscCreates.ag"(line 43, column 12)
              _lhsOattrs =
                  (_hdIattrName, _hdInamedType) : _tlIattrs
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIattrName,_hdIfixedUpIdentifiersTree,_hdInamedType,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIattrs,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOattrs :: ([(String, Maybe Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOfixedUpIdentifiersTree :: TypeAttributeDefList
              _lhsOnewFixedUpIdentifiersTree :: TypeAttributeDefList
              _lhsOoriginalTree :: TypeAttributeDefList
              -- "./TypeChecking/MiscCreates.ag"(line 44, column 11)
              _lhsOattrs =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative SetOfTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative SimpleTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                   ( TypeName,TypeName,(Maybe Type),TypeName,TypeName)
data Inh_TypeName  = Inh_TypeName {cat_Inh_TypeName :: Catalog,idenv_Inh_TypeName :: IDEnv,lib_Inh_TypeName :: LocalBindings}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName,fixedUpIdentifiersTree_Syn_TypeName :: TypeName,namedType_Syn_TypeName :: Maybe Type,newFixedUpIdentifiersTree_Syn_TypeName :: TypeName,originalTree_Syn_TypeName :: TypeName}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TypeName _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnamedType _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: TypeName
              _lhsOoriginalTree :: TypeName
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typInewFixedUpIdentifiersTree :: TypeName
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  etmt _tpe
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  addTypeErrors (tes _tpe    ) _backTree
              -- "./TypeChecking/Misc.ag"(line 27, column 9)
              _tpe =
                  lmt _typInamedType >>=  Right . ArrayType
              -- "./TypeChecking/Misc.ag"(line 28, column 9)
              _backTree =
                  ArrayTypeName ann_ _typIannotatedTree
              -- self rule
              _annotatedTree =
                  ArrayTypeName ann_ _typIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  ArrayTypeName ann_ _typIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  ArrayTypeName ann_ _typInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  ArrayTypeName ann_ _typIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typInewFixedUpIdentifiersTree,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: TypeName
              _lhsOoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  etmt _tpe
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  addTypeErrors (tes _tpe    ) _backTree
              -- "./TypeChecking/Misc.ag"(line 36, column 9)
              _tpe =
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
              -- "./TypeChecking/Misc.ag"(line 37, column 9)
              _backTree =
                  Prec2TypeName ann_ tn_ prec_ prec1_
              -- self rule
              _annotatedTree =
                  Prec2TypeName ann_ tn_ prec_ prec1_
              -- self rule
              _fixedUpIdentifiersTree =
                  Prec2TypeName ann_ tn_ prec_ prec1_
              -- self rule
              _newFixedUpIdentifiersTree =
                  Prec2TypeName ann_ tn_ prec_ prec1_
              -- self rule
              _originalTree =
                  Prec2TypeName ann_ tn_ prec_ prec1_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: TypeName
              _lhsOoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  etmt _tpe
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  addTypeErrors (tes _tpe    ) _backTree
              -- "./TypeChecking/Misc.ag"(line 33, column 9)
              _tpe =
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
              -- "./TypeChecking/Misc.ag"(line 34, column 9)
              _backTree =
                  PrecTypeName ann_ tn_ prec_
              -- self rule
              _annotatedTree =
                  PrecTypeName ann_ tn_ prec_
              -- self rule
              _fixedUpIdentifiersTree =
                  PrecTypeName ann_ tn_ prec_
              -- self rule
              _newFixedUpIdentifiersTree =
                  PrecTypeName ann_ tn_ prec_
              -- self rule
              _originalTree =
                  PrecTypeName ann_ tn_ prec_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: TypeName
              _lhsOoriginalTree :: TypeName
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typInewFixedUpIdentifiersTree :: TypeName
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  etmt _tpe
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  addTypeErrors (tes _tpe    ) _backTree
              -- "./TypeChecking/Misc.ag"(line 30, column 9)
              _tpe =
                  lmt _typInamedType >>=  Right . SetOfType
              -- "./TypeChecking/Misc.ag"(line 31, column 9)
              _backTree =
                  SetOfTypeName ann_ _typIannotatedTree
              -- self rule
              _annotatedTree =
                  SetOfTypeName ann_ _typIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  SetOfTypeName ann_ _typIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  SetOfTypeName ann_ _typInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  SetOfTypeName ann_ _typIoriginalTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typInewFixedUpIdentifiersTree,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: TypeName
              _lhsOoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  etmt _tpe
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  addTypeErrors (tes _tpe    ) _backTree
              -- "./TypeChecking/Misc.ag"(line 24, column 9)
              _tpe =
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
              -- "./TypeChecking/Misc.ag"(line 25, column 9)
              _backTree =
                  SimpleTypeName ann_ tn_
              -- self rule
              _annotatedTree =
                  SimpleTypeName ann_ tn_
              -- self rule
              _fixedUpIdentifiersTree =
                  SimpleTypeName ann_ tn_
              -- self rule
              _newFixedUpIdentifiersTree =
                  SimpleTypeName ann_ tn_
              -- self rule
              _originalTree =
                  SimpleTypeName ann_ tn_
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedType,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : TypeName 
         child tl             : TypeNameList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                       ( TypeNameList,TypeNameList,([Maybe Type]),TypeNameList,TypeNameList)
data Inh_TypeNameList  = Inh_TypeNameList {cat_Inh_TypeNameList :: Catalog,idenv_Inh_TypeNameList :: IDEnv,lib_Inh_TypeNameList :: LocalBindings}
data Syn_TypeNameList  = Syn_TypeNameList {annotatedTree_Syn_TypeNameList :: TypeNameList,fixedUpIdentifiersTree_Syn_TypeNameList :: TypeNameList,namedTypes_Syn_TypeNameList :: [Maybe Type],newFixedUpIdentifiersTree_Syn_TypeNameList :: TypeNameList,originalTree_Syn_TypeNameList :: TypeNameList}
wrap_TypeNameList :: T_TypeNameList  ->
                     Inh_TypeNameList  ->
                     Syn_TypeNameList 
wrap_TypeNameList sem (Inh_TypeNameList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedTypes,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_TypeNameList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnamedTypes _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: TypeNameList
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
              _hdInewFixedUpIdentifiersTree :: TypeName
              _hdIoriginalTree :: TypeName
              _tlIannotatedTree :: TypeNameList
              _tlIfixedUpIdentifiersTree :: TypeNameList
              _tlInamedTypes :: ([Maybe Type])
              _tlInewFixedUpIdentifiersTree :: TypeNameList
              _tlIoriginalTree :: TypeNameList
              -- "./TypeChecking/Drops.ag"(line 37, column 12)
              _lhsOnamedTypes =
                  _hdInamedType : _tlInamedTypes
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIfixedUpIdentifiersTree,_hdInamedType,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInamedTypes,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedTypes,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_TypeNameList_Nil :: T_TypeNameList 
sem_TypeNameList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOnamedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: TypeNameList
              _lhsOfixedUpIdentifiersTree :: TypeNameList
              _lhsOnewFixedUpIdentifiersTree :: TypeNameList
              _lhsOoriginalTree :: TypeNameList
              -- "./TypeChecking/Drops.ag"(line 38, column 11)
              _lhsOnamedTypes =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnamedTypes,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative ParamAlias:
         child ann            : {Annotation}
         child name           : {String}
         child i              : {Integer}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative VarAlias:
         child ann            : {Annotation}
         child name           : {String}
         child aliased        : {String}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative VarDef:
         child ann            : {Annotation}
         child name           : {String}
         child typ            : TypeName 
         child value          : {Maybe ScalarExpr}
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                 ( VarDef,((String,Maybe Type)),VarDef,VarDef,VarDef)
data Inh_VarDef  = Inh_VarDef {cat_Inh_VarDef :: Catalog,idenv_Inh_VarDef :: IDEnv,lib_Inh_VarDef :: LocalBindings}
data Syn_VarDef  = Syn_VarDef {annotatedTree_Syn_VarDef :: VarDef,def_Syn_VarDef :: (String,Maybe Type),fixedUpIdentifiersTree_Syn_VarDef :: VarDef,newFixedUpIdentifiersTree_Syn_VarDef :: VarDef,originalTree_Syn_VarDef :: VarDef}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOdef,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_VarDef _lhsOannotatedTree _lhsOdef _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: VarDef
              _lhsOoriginalTree :: VarDef
              -- "./TypeChecking/Block.ag"(line 14, column 18)
              _lhsOdef =
                  (name_, Nothing)
              -- self rule
              _annotatedTree =
                  ParamAlias ann_ name_ i_
              -- self rule
              _fixedUpIdentifiersTree =
                  ParamAlias ann_ name_ i_
              -- self rule
              _newFixedUpIdentifiersTree =
                  ParamAlias ann_ name_ i_
              -- self rule
              _originalTree =
                  ParamAlias ann_ name_ i_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOdef,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: VarDef
              _lhsOoriginalTree :: VarDef
              -- "./TypeChecking/Block.ag"(line 13, column 16)
              _lhsOdef =
                  (name_, Nothing)
              -- self rule
              _annotatedTree =
                  VarAlias ann_ name_ aliased_
              -- self rule
              _fixedUpIdentifiersTree =
                  VarAlias ann_ name_ aliased_
              -- self rule
              _newFixedUpIdentifiersTree =
                  VarAlias ann_ name_ aliased_
              -- self rule
              _originalTree =
                  VarAlias ann_ name_ aliased_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOdef,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
              _lhsOnewFixedUpIdentifiersTree :: VarDef
              _lhsOoriginalTree :: VarDef
              _typOcat :: Catalog
              _typOidenv :: IDEnv
              _typOlib :: LocalBindings
              _typIannotatedTree :: TypeName
              _typIfixedUpIdentifiersTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typInewFixedUpIdentifiersTree :: TypeName
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Block.ag"(line 10, column 14)
              _lhsOdef =
                  (name_, if _typInamedType == Just (Pseudo Record)
                          then Just (PgRecord Nothing)
                          else _typInamedType)
              -- self rule
              _annotatedTree =
                  VarDef ann_ name_ _typIannotatedTree value_
              -- self rule
              _fixedUpIdentifiersTree =
                  VarDef ann_ name_ _typIfixedUpIdentifiersTree value_
              -- self rule
              _newFixedUpIdentifiersTree =
                  VarDef ann_ name_ _typInewFixedUpIdentifiersTree value_
              -- self rule
              _originalTree =
                  VarDef ann_ name_ _typIoriginalTree value_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              ( _typIannotatedTree,_typIfixedUpIdentifiersTree,_typInamedType,_typInewFixedUpIdentifiersTree,_typIoriginalTree) =
                  (typ_ _typOcat _typOidenv _typOlib )
          in  ( _lhsOannotatedTree,_lhsOdef,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : VarDef 
         child tl             : VarDefList 
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                     ( VarDefList,([(String,Maybe Type)]),VarDefList,VarDefList,VarDefList)
data Inh_VarDefList  = Inh_VarDefList {cat_Inh_VarDefList :: Catalog,idenv_Inh_VarDefList :: IDEnv,lib_Inh_VarDefList :: LocalBindings}
data Syn_VarDefList  = Syn_VarDefList {annotatedTree_Syn_VarDefList :: VarDefList,defs_Syn_VarDefList :: [(String,Maybe Type)],fixedUpIdentifiersTree_Syn_VarDefList :: VarDefList,newFixedUpIdentifiersTree_Syn_VarDefList :: VarDefList,originalTree_Syn_VarDefList :: VarDefList}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOdefs,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_VarDefList _lhsOannotatedTree _lhsOdefs _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
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
              _lhsOnewFixedUpIdentifiersTree :: VarDefList
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
              _hdInewFixedUpIdentifiersTree :: VarDef
              _hdIoriginalTree :: VarDef
              _tlIannotatedTree :: VarDefList
              _tlIdefs :: ([(String,Maybe Type)])
              _tlIfixedUpIdentifiersTree :: VarDefList
              _tlInewFixedUpIdentifiersTree :: VarDefList
              _tlIoriginalTree :: VarDefList
              -- "./TypeChecking/Block.ag"(line 17, column 12)
              _lhsOdefs =
                  _hdIdef : _tlIdefs
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIdef,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIdefs,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOdefs :: ([(String,Maybe Type)])
              _lhsOannotatedTree :: VarDefList
              _lhsOfixedUpIdentifiersTree :: VarDefList
              _lhsOnewFixedUpIdentifiersTree :: VarDefList
              _lhsOoriginalTree :: VarDefList
              -- "./TypeChecking/Block.ag"(line 18, column 11)
              _lhsOdefs =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
         originalTree         : SELF 
   alternatives:
      alternative WithQuery:
         child ann            : {Annotation}
         child name           : {String}
         child ex             : QueryExpr 
         visit 0:
            local tpe         : _
            local backTree    : _
            local attrs       : _
            local catUpdates  : _
            local statementType : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
            local originalTree : _
-}
data WithQuery  = WithQuery (Annotation) (String) (QueryExpr) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_WithQuery :: WithQuery  ->
                 T_WithQuery 
sem_WithQuery (WithQuery _ann _name _ex )  =
    (sem_WithQuery_WithQuery _ann _name (sem_QueryExpr _ex ) )
-- semantic domain
type T_WithQuery  = Catalog ->
                    IDEnv ->
                    LocalBindings ->
                    ( WithQuery,([CatalogUpdate]),WithQuery,WithQuery,WithQuery)
data Inh_WithQuery  = Inh_WithQuery {cat_Inh_WithQuery :: Catalog,idenv_Inh_WithQuery :: IDEnv,lib_Inh_WithQuery :: LocalBindings}
data Syn_WithQuery  = Syn_WithQuery {annotatedTree_Syn_WithQuery :: WithQuery,catUpdates_Syn_WithQuery :: [CatalogUpdate],fixedUpIdentifiersTree_Syn_WithQuery :: WithQuery,newFixedUpIdentifiersTree_Syn_WithQuery :: WithQuery,originalTree_Syn_WithQuery :: WithQuery}
wrap_WithQuery :: T_WithQuery  ->
                  Inh_WithQuery  ->
                  Syn_WithQuery 
wrap_WithQuery sem (Inh_WithQuery _lhsIcat _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIidenv _lhsIlib )
     in  (Syn_WithQuery _lhsOannotatedTree _lhsOcatUpdates _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree ))
sem_WithQuery_WithQuery :: Annotation ->
                           String ->
                           T_QueryExpr  ->
                           T_WithQuery 
sem_WithQuery_WithQuery ann_ name_ ex_  =
    (\ _lhsIcat
       _lhsIidenv
       _lhsIlib ->
         (let _exOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: WithQuery
              _lhsOfixedUpIdentifiersTree :: WithQuery
              _lhsOnewFixedUpIdentifiersTree :: WithQuery
              _lhsOoriginalTree :: WithQuery
              _lhsOcatUpdates :: ([CatalogUpdate])
              _exOcat :: Catalog
              _exOidenv :: IDEnv
              _exOlib :: LocalBindings
              _exIannotatedTree :: QueryExpr
              _exIcidenv :: IDEnv
              _exIfixedUpIdentifiersTree :: QueryExpr
              _exIlibUpdates :: ([LocalBindingsUpdate])
              _exInewFixedUpIdentifiersTree :: QueryExpr
              _exIoriginalTree :: QueryExpr
              _exIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/ScalarExprs.ag"(line 647, column 17)
              _exOexpectedTypes =
                  []
              -- "./TypeChecking/QueryStatement.ag"(line 246, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/QueryStatement.ag"(line 247, column 9)
              _backTree =
                  WithQuery ann_ name_ _exIannotatedTree
              -- "./TypeChecking/QueryStatement.ag"(line 248, column 9)
              _attrs =
                  maybe [] id $ _exIuType
              -- "./TypeChecking/QueryStatement.ag"(line 249, column 9)
              _catUpdates =
                  [CatCreateView name_ _attrs    ]
              -- "./TypeChecking/QueryStatement.ag"(line 250, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  WithQuery ann_ name_ _exIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  WithQuery ann_ name_ _exIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  WithQuery ann_ name_ _exInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  WithQuery ann_ name_ _exIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (from local)
              _lhsOcatUpdates =
                  _catUpdates
              -- copy rule (down)
              _exOcat =
                  _lhsIcat
              -- copy rule (down)
              _exOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _exOlib =
                  _lhsIlib
              ( _exIannotatedTree,_exIcidenv,_exIfixedUpIdentifiersTree,_exIlibUpdates,_exInewFixedUpIdentifiersTree,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedTypes _exOidenv _exOlib )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree)))
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
         newFixedUpIdentifiersTree : SELF 
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
            local newFixedUpIdentifiersTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local newCat      : _
            local annotatedTree : _
            local fixedUpIdentifiersTree : _
            local newFixedUpIdentifiersTree : _
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
                        ( WithQueryList,WithQueryList,WithQueryList,WithQueryList,Catalog)
data Inh_WithQueryList  = Inh_WithQueryList {cat_Inh_WithQueryList :: Catalog,catUpdates_Inh_WithQueryList :: [CatalogUpdate],idenv_Inh_WithQueryList :: IDEnv,lib_Inh_WithQueryList :: LocalBindings}
data Syn_WithQueryList  = Syn_WithQueryList {annotatedTree_Syn_WithQueryList :: WithQueryList,fixedUpIdentifiersTree_Syn_WithQueryList :: WithQueryList,newFixedUpIdentifiersTree_Syn_WithQueryList :: WithQueryList,originalTree_Syn_WithQueryList :: WithQueryList,producedCat_Syn_WithQueryList :: Catalog}
wrap_WithQueryList :: T_WithQueryList  ->
                      Inh_WithQueryList  ->
                      Syn_WithQueryList 
wrap_WithQueryList sem (Inh_WithQueryList _lhsIcat _lhsIcatUpdates _lhsIidenv _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat) =
             (sem _lhsIcat _lhsIcatUpdates _lhsIidenv _lhsIlib )
     in  (Syn_WithQueryList _lhsOannotatedTree _lhsOfixedUpIdentifiersTree _lhsOnewFixedUpIdentifiersTree _lhsOoriginalTree _lhsOproducedCat ))
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
              _lhsOnewFixedUpIdentifiersTree :: WithQueryList
              _lhsOoriginalTree :: WithQueryList
              _hdOidenv :: IDEnv
              _hdOlib :: LocalBindings
              _tlOidenv :: IDEnv
              _tlOlib :: LocalBindings
              _hdIannotatedTree :: WithQuery
              _hdIcatUpdates :: ([CatalogUpdate])
              _hdIfixedUpIdentifiersTree :: WithQuery
              _hdInewFixedUpIdentifiersTree :: WithQuery
              _hdIoriginalTree :: WithQuery
              _tlIannotatedTree :: WithQueryList
              _tlIfixedUpIdentifiersTree :: WithQueryList
              _tlInewFixedUpIdentifiersTree :: WithQueryList
              _tlIoriginalTree :: WithQueryList
              _tlIproducedCat :: Catalog
              -- "./TypeChecking/QueryStatement.ag"(line 230, column 9)
              _newCat =
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
              -- "./TypeChecking/QueryStatement.ag"(line 232, column 9)
              _hdOcat =
                  _newCat
              -- "./TypeChecking/QueryStatement.ag"(line 233, column 9)
              _tlOcat =
                  _newCat
              -- "./TypeChecking/QueryStatement.ag"(line 237, column 9)
              _lhsOproducedCat =
                  _tlIproducedCat
              -- "./TypeChecking/QueryStatement.ag"(line 240, column 9)
              _tlOcatUpdates =
                  _hdIcatUpdates
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _fixedUpIdentifiersTree =
                  (:) _hdIfixedUpIdentifiersTree _tlIfixedUpIdentifiersTree
              -- self rule
              _newFixedUpIdentifiersTree =
                  (:) _hdInewFixedUpIdentifiersTree _tlInewFixedUpIdentifiersTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOidenv =
                  _lhsIidenv
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              ( _hdIannotatedTree,_hdIcatUpdates,_hdIfixedUpIdentifiersTree,_hdInewFixedUpIdentifiersTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOidenv _hdOlib )
              ( _tlIannotatedTree,_tlIfixedUpIdentifiersTree,_tlInewFixedUpIdentifiersTree,_tlIoriginalTree,_tlIproducedCat) =
                  (tl_ _tlOcat _tlOcatUpdates _tlOidenv _tlOlib )
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat)))
sem_WithQueryList_Nil :: T_WithQueryList 
sem_WithQueryList_Nil  =
    (\ _lhsIcat
       _lhsIcatUpdates
       _lhsIidenv
       _lhsIlib ->
         (let _lhsOproducedCat :: Catalog
              _lhsOannotatedTree :: WithQueryList
              _lhsOfixedUpIdentifiersTree :: WithQueryList
              _lhsOnewFixedUpIdentifiersTree :: WithQueryList
              _lhsOoriginalTree :: WithQueryList
              -- "./TypeChecking/QueryStatement.ag"(line 230, column 9)
              _newCat =
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
              -- "./TypeChecking/QueryStatement.ag"(line 242, column 9)
              _lhsOproducedCat =
                  _newCat
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _fixedUpIdentifiersTree =
                  []
              -- self rule
              _newFixedUpIdentifiersTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOfixedUpIdentifiersTree =
                  _fixedUpIdentifiersTree
              -- self rule
              _lhsOnewFixedUpIdentifiersTree =
                  _newFixedUpIdentifiersTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOfixedUpIdentifiersTree,_lhsOnewFixedUpIdentifiersTree,_lhsOoriginalTree,_lhsOproducedCat)))