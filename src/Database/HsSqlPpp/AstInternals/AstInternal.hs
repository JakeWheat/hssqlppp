

-- UUAGC 0.9.19 (AstInternal.ag)
module Database.HsSqlPpp.AstInternals.AstInternal(
    -- {-# LANGUAGE DeriveDataTypeable,RankNTypes,ScopedTypeVariables #-}
    --from the ag files:
    --ast nodes
    Statement (..)
   ,SelectExpression (..)
   ,WithQueryList
   ,WithQuery(..)
   ,FnBody (..)
   --,SetClause (..)
   ,TableRef (..)
   ,TableAlias(..)
   ,JoinExpression (..)
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
   ,Expression (..)
   ,FrameClause(..)
   ,InList (..)
   ,LiftFlavour(..)
   ,TriggerWhen(..)
   ,TriggerEvent(..)
   ,TriggerFire(..)
   ,StatementList
   ,ExpressionListStatementListPairList
   ,ExpressionListStatementListPair
   ,ExpressionList
   ,ParamDefList
   ,AttributeDefList
   ,ConstraintList
   ,TypeAttributeDefList
   ,TypeNameList
   ,StringTypeNameListPair
   ,StringTypeNameListPairList
   ,ExpressionStatementListPairList
   --,SetClauseList
   ,CaseExpressionListExpressionPairList
   ,MaybeExpression
   ,TableRefList
   ,ExpressionListList
   ,SelectItemList
   ,OnExpr
   ,RowConstraintList
   ,VarDefList
   ,ExpressionStatementListPair
   ,CaseExpressionListExpressionPair
   ,ExpressionDirectionPair
   ,ExpressionDirectionPairList
   ,MaybeBoolExpression
   ,MaybeSelectList
   ,SetValue(..)
   ,AlterTableActionList
   -- typechecking
   ,typeCheck
   ,typeCheckPS
   ,typeCheckExpression
) where

import Data.Maybe
import Data.List
import Control.Applicative
import Data.Generics
import Data.Char
import Control.Monad.State

import Data.Generics.PlateData

import Database.HsSqlPpp.AstInternals.TypeType
import Database.HsSqlPpp.AstInternals.TypeChecking.TypeConversion
import Database.HsSqlPpp.AstInternals.AstAnnotation
import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
import Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindings
import Database.HsSqlPpp.Utils.Utils
import Database.HsSqlPpp.AstInternals.TypeChecking.ErrorUtils


{-# LINE 367 "AstInternal.ag" #-}

data TableAlias = NoAlias
                | TableAlias String --alias:String
                | FullAlias String [String] -- alias:String cols:{[String]}
                  deriving (Show,Eq,Typeable,Data)
{-# LINE 108 "AstInternal.hs" #-}

{-# LINE 377 "AstInternal.ag" #-}

data JoinType = Inner | LeftOuter| RightOuter | FullOuter | Cross
                deriving (Show,Eq,Typeable,Data)
{-# LINE 114 "AstInternal.hs" #-}

{-# LINE 389 "AstInternal.ag" #-}

data CopySource = CopyFilename String
                | Stdin
                  deriving (Show,Eq,Typeable,Data)
{-# LINE 121 "AstInternal.hs" #-}

{-# LINE 438 "AstInternal.ag" #-}

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
{-# LINE 138 "AstInternal.hs" #-}

{-# LINE 461 "AstInternal.ag" #-}

data RaiseType = RNotice | RException | RError
                 deriving (Show,Eq,Typeable,Data)

data CombineType = Except | Union | Intersect | UnionAll
                   deriving (Show,Eq,Typeable,Data)

data Volatility = Volatile | Stable | Immutable
                  deriving (Show,Eq,Typeable,Data)

data Language = Sql | Plpgsql
                deriving (Show,Eq,Typeable,Data)
{-# LINE 153 "AstInternal.hs" #-}

{-# LINE 479 "AstInternal.ag" #-}

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
{-# LINE 183 "AstInternal.hs" #-}

{-# LINE 564 "AstInternal.ag" #-}

data LiftFlavour = LiftAny | LiftAll
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 189 "AstInternal.hs" #-}

{-# LINE 573 "AstInternal.ag" #-}

data FrameClause = FrameUnboundedPreceding
                 | FrameUnboundedFull
                 | FrameRowsUnboundedPreceding
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 197 "AstInternal.hs" #-}

{-# LINE 740 "AstInternal.ag" #-}

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
typeCheck :: Catalog -> StatementList -> (Catalog,StatementList)
typeCheck cat sts =
    let t = sem_Root (Root (fixupImplicitJoins sts))
        ta = wrap_Root t Inh_Root {cat_Inh_Root = cat
                                  ,lib_Inh_Root = emptyBindings}
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
typeCheckPS :: Catalog -> Statement -> Either String Statement
typeCheckPS cat st =
    case st of
      SelectStatement _ _ -> tc
      Insert _ _ _ _ _ -> tc
      Update _ _ _ _ _ _ -> tc
      Delete _ _ _ _ _ -> tc
      _ -> Left "requires select, update, insert or delete statement"
    where
      tc = let t = sem_Root (Root (fixupImplicitJoins [st]))
               ta = wrap_Root t Inh_Root {cat_Inh_Root = cat
                                         ,lib_Inh_Root = emptyBindings}
               tl = annotatedTree_Syn_Root ta
               --cat1 = producedCat_Syn_Root ta
           in case tl of
                Root [st1] -> Right st1
                _ -> error "impossible happened in typeCheckPS!"


-- | Testing utility, mainly used to check an expression for type errors
-- or to get its type.
typeCheckExpression :: Catalog -> Expression -> Expression
typeCheckExpression cat ex =
    let t = sem_ExpressionRoot (ExpressionRoot (fixupImplicitJoins ex))
        rt = (annotatedTree_Syn_ExpressionRoot
              (wrap_ExpressionRoot t Inh_ExpressionRoot {cat_Inh_ExpressionRoot = cat
                                                        ,lib_Inh_ExpressionRoot = emptyBindings}))
    in case rt of
         ExpressionRoot e -> e

{-
bit of a hack, to avoid rewriting the tableref type checking to be
able to do implicit joins, we just convert them in to the equivalent
explicit join
-}

fixupImplicitJoins :: Data a => a -> a
fixupImplicitJoins =
    transformBi $ \x ->
            case x of
              -- alter asts to change implicit joins into explicit joins
              Select an dis sl trs@(_:_:_) whr grp hav od lim off
                  -> Select an dis sl [convTrefs trs] whr grp hav od lim off
              x1 -> x1
    where
      convTrefs (tr:tr1:trs) = JoinedTref emptyAnnotation tr Unnatural Cross (convTrefs (tr1:trs)) Nothing NoAlias
      convTrefs (tr:[]) = tr
      convTrefs _ = error "failed doing implicit join fixup hack"

{-# LINE 285 "AstInternal.hs" #-}

{-# LINE 44 "./TypeChecking/Misc.ag" #-}

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
getName :: Expression -> String
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

{-# LINE 374 "AstInternal.hs" #-}

{-# LINE 161 "./TypeChecking/SelectStatement.ag" #-}


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

{-# LINE 410 "AstInternal.hs" #-}

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
funIdens :: Catalog -> String -> Expression -> Maybe Type -> Either [TypeError] (String,[(String,Type)])
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

{-# LINE 458 "AstInternal.hs" #-}

{-# LINE 18 "./TypeChecking/SelectLists.ag" #-}

{-data SiType = SiType (String,Maybe Type)
            | SiStarType [(String,Maybe Type)]-}
{-# LINE 464 "AstInternal.hs" #-}

{-# LINE 67 "./TypeChecking/SelectLists.ag" #-}

unwrapSetofs :: [(String,Type)] -> [(String,Type)]
unwrapSetofs = map (\(n,t) -> (n, unwrapSetof t))

unwrapSetof :: Type -> Type
unwrapSetof (SetOfType u) = u
unwrapSetof v = v

{-# LINE 475 "AstInternal.hs" #-}

{-# LINE 51 "./TypeChecking/CreateTable.ag" #-}

defaultSystemColumns :: [(String,Type)]
defaultSystemColumns = [("tableoid", ScalarType "oid")
                       ,("cmax", ScalarType "cid")
                       ,("xmax", ScalarType "xid")
                       ,("cmin", ScalarType "cid")
                       ,("xmin", ScalarType "xid")
                       ,("ctid", ScalarType "tid")]
{-# LINE 486 "AstInternal.hs" #-}

{-# LINE 31 "./TypeChecking/CreateFunction.ag" #-}

data ParamName = NamedParam Int String
               | UnnamedParam Int
{-# LINE 492 "AstInternal.hs" #-}
-- AlterTableAction --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
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
         child nm             : {String}
         child def            : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data AlterTableAction  = AddConstraint (Annotation) (Constraint) 
                       | AlterColumnDefault (Annotation) (String) (Expression) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_AlterTableAction :: AlterTableAction  ->
                        T_AlterTableAction 
sem_AlterTableAction (AddConstraint _ann _con )  =
    (sem_AlterTableAction_AddConstraint _ann (sem_Constraint _con ) )
sem_AlterTableAction (AlterColumnDefault _ann _nm _def )  =
    (sem_AlterTableAction_AlterColumnDefault _ann _nm (sem_Expression _def ) )
-- semantic domain
type T_AlterTableAction  = Catalog ->
                           LocalBindings ->
                           (Maybe LocalBindings) ->
                           ([Maybe LocalBindings]) ->
                           ( AlterTableAction,AlterTableAction)
data Inh_AlterTableAction  = Inh_AlterTableAction {cat_Inh_AlterTableAction :: Catalog,lib_Inh_AlterTableAction :: LocalBindings,overrideLib_Inh_AlterTableAction :: Maybe LocalBindings,overrideLibs_Inh_AlterTableAction :: [Maybe LocalBindings]}
data Syn_AlterTableAction  = Syn_AlterTableAction {annotatedTree_Syn_AlterTableAction :: AlterTableAction,originalTree_Syn_AlterTableAction :: AlterTableAction}
wrap_AlterTableAction :: T_AlterTableAction  ->
                         Inh_AlterTableAction  ->
                         Syn_AlterTableAction 
wrap_AlterTableAction sem (Inh_AlterTableAction _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_AlterTableAction _lhsOannotatedTree _lhsOoriginalTree ))
sem_AlterTableAction_AddConstraint :: Annotation ->
                                      T_Constraint  ->
                                      T_AlterTableAction 
sem_AlterTableAction_AddConstraint ann_ con_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: AlterTableAction
              _lhsOoriginalTree :: AlterTableAction
              _conOcat :: Catalog
              _conOlib :: LocalBindings
              _conOoverrideLib :: (Maybe LocalBindings)
              _conOoverrideLibs :: ([Maybe LocalBindings])
              _conIannotatedTree :: Constraint
              _conIoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  AddConstraint ann_ _conIannotatedTree
                  {-# LINE 564 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  AddConstraint ann_ _conIoriginalTree
                  {-# LINE 569 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 574 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 579 "AstInternal.hs" #-}
              -- copy rule (down)
              _conOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 584 "AstInternal.hs" #-}
              -- copy rule (down)
              _conOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 589 "AstInternal.hs" #-}
              -- copy rule (down)
              _conOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 594 "AstInternal.hs" #-}
              -- copy rule (down)
              _conOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 599 "AstInternal.hs" #-}
              ( _conIannotatedTree,_conIoriginalTree) =
                  (con_ _conOcat _conOlib _conOoverrideLib _conOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_AlterTableAction_AlterColumnDefault :: Annotation ->
                                           String ->
                                           T_Expression  ->
                                           T_AlterTableAction 
sem_AlterTableAction_AlterColumnDefault ann_ nm_ def_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _defOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: AlterTableAction
              _lhsOoriginalTree :: AlterTableAction
              _defOcat :: Catalog
              _defOlib :: LocalBindings
              _defOoverrideLib :: (Maybe LocalBindings)
              _defOoverrideLibs :: ([Maybe LocalBindings])
              _defIannotatedTree :: Expression
              _defIntAnnotatedTree :: Expression
              _defIntType :: ([(String,Type)])
              _defIoriginalTree :: Expression
              _defItbAnnotatedTree :: Expression
              _defItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _defIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 459, column 26)
              _defOexpectedType =
                  {-# LINE 459 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 630 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  AlterColumnDefault ann_ nm_ _defIannotatedTree
                  {-# LINE 635 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  AlterColumnDefault ann_ nm_ _defIoriginalTree
                  {-# LINE 640 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 645 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 650 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 655 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 660 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 665 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 670 "AstInternal.hs" #-}
              ( _defIannotatedTree,_defIntAnnotatedTree,_defIntType,_defIoriginalTree,_defItbAnnotatedTree,_defItbUType,_defIuType) =
                  (def_ _defOcat _defOexpectedType _defOlib _defOoverrideLib _defOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- AlterTableActionList ----------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
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
type AlterTableActionList  = [(AlterTableAction)]
-- cata
sem_AlterTableActionList :: AlterTableActionList  ->
                            T_AlterTableActionList 
sem_AlterTableActionList list  =
    (Prelude.foldr sem_AlterTableActionList_Cons sem_AlterTableActionList_Nil (Prelude.map sem_AlterTableAction list) )
-- semantic domain
type T_AlterTableActionList  = Catalog ->
                               LocalBindings ->
                               (Maybe LocalBindings) ->
                               ([Maybe LocalBindings]) ->
                               ( AlterTableActionList,AlterTableActionList)
data Inh_AlterTableActionList  = Inh_AlterTableActionList {cat_Inh_AlterTableActionList :: Catalog,lib_Inh_AlterTableActionList :: LocalBindings,overrideLib_Inh_AlterTableActionList :: Maybe LocalBindings,overrideLibs_Inh_AlterTableActionList :: [Maybe LocalBindings]}
data Syn_AlterTableActionList  = Syn_AlterTableActionList {annotatedTree_Syn_AlterTableActionList :: AlterTableActionList,originalTree_Syn_AlterTableActionList :: AlterTableActionList}
wrap_AlterTableActionList :: T_AlterTableActionList  ->
                             Inh_AlterTableActionList  ->
                             Syn_AlterTableActionList 
wrap_AlterTableActionList sem (Inh_AlterTableActionList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_AlterTableActionList _lhsOannotatedTree _lhsOoriginalTree ))
sem_AlterTableActionList_Cons :: T_AlterTableAction  ->
                                 T_AlterTableActionList  ->
                                 T_AlterTableActionList 
sem_AlterTableActionList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: AlterTableActionList
              _lhsOoriginalTree :: AlterTableActionList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: AlterTableAction
              _hdIoriginalTree :: AlterTableAction
              _tlIannotatedTree :: AlterTableActionList
              _tlIoriginalTree :: AlterTableActionList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 744 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 749 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 754 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 759 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 764 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 769 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 774 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 779 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 784 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 789 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 794 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 799 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_AlterTableActionList_Nil :: T_AlterTableActionList 
sem_AlterTableActionList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: AlterTableActionList
              _lhsOoriginalTree :: AlterTableActionList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 817 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 822 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 827 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 832 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- AttributeDef ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         attrName             : String
         namedType            : Maybe Type
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
type T_AttributeDef  = Catalog ->
                       LocalBindings ->
                       (Maybe LocalBindings) ->
                       ([Maybe LocalBindings]) ->
                       ( AttributeDef,String,(Maybe Type),AttributeDef)
data Inh_AttributeDef  = Inh_AttributeDef {cat_Inh_AttributeDef :: Catalog,lib_Inh_AttributeDef :: LocalBindings,overrideLib_Inh_AttributeDef :: Maybe LocalBindings,overrideLibs_Inh_AttributeDef :: [Maybe LocalBindings]}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef,attrName_Syn_AttributeDef :: String,namedType_Syn_AttributeDef :: Maybe Type,originalTree_Syn_AttributeDef :: AttributeDef}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_AttributeDef _lhsOannotatedTree _lhsOattrName _lhsOnamedType _lhsOoriginalTree ))
sem_AttributeDef_AttributeDef :: Annotation ->
                                 String ->
                                 T_TypeName  ->
                                 T_MaybeExpression  ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef ann_ name_ typ_ def_ cons_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOattrName :: String
              _lhsOnamedType :: (Maybe Type)
              _consOlib :: LocalBindings
              _lhsOannotatedTree :: AttributeDef
              _lhsOoriginalTree :: AttributeDef
              _typOcat :: Catalog
              _typOlib :: LocalBindings
              _typOoverrideLib :: (Maybe LocalBindings)
              _typOoverrideLibs :: ([Maybe LocalBindings])
              _defOcat :: Catalog
              _defOlib :: LocalBindings
              _defOoverrideLib :: (Maybe LocalBindings)
              _defOoverrideLibs :: ([Maybe LocalBindings])
              _consOcat :: Catalog
              _consOoverrideLib :: (Maybe LocalBindings)
              _consOoverrideLibs :: ([Maybe LocalBindings])
              _typIannotatedTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              _defIannotatedTree :: MaybeExpression
              _defIoriginalTree :: MaybeExpression
              _defIuType :: (Maybe Type)
              _consIannotatedTree :: RowConstraintList
              _consIoriginalTree :: RowConstraintList
              -- "./TypeChecking/CreateTable.ag"(line 83, column 9)
              _lhsOattrName =
                  {-# LINE 83 "./TypeChecking/CreateTable.ag" #-}
                  map toLower name_
                  {-# LINE 919 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 84, column 9)
              _lhsOnamedType =
                  {-# LINE 84 "./TypeChecking/CreateTable.ag" #-}
                  _typInamedType
                  {-# LINE 924 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 98, column 9)
              _consOlib =
                  {-# LINE 98 "./TypeChecking/CreateTable.ag" #-}
                  either (const _lhsIlib) id $ do
                  t <- lmt _typInamedType
                  lbUpdate _lhsIcat
                           (LBIds "attribute def" Nothing
                                  [(name_, t)]) _lhsIlib
                  {-# LINE 933 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIannotatedTree _defIannotatedTree _consIannotatedTree
                  {-# LINE 938 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  AttributeDef ann_ name_ _typIoriginalTree _defIoriginalTree _consIoriginalTree
                  {-# LINE 943 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 948 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 953 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 958 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 963 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 968 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 973 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 978 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 983 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 988 "AstInternal.hs" #-}
              -- copy rule (down)
              _defOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 993 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 998 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 1003 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 1008 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOlib _typOoverrideLib _typOoverrideLibs )
              ( _defIannotatedTree,_defIoriginalTree,_defIuType) =
                  (def_ _defOcat _defOlib _defOoverrideLib _defOoverrideLibs )
              ( _consIannotatedTree,_consIoriginalTree) =
                  (cons_ _consOcat _consOlib _consOoverrideLib _consOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType,_lhsOoriginalTree)))
-- AttributeDefList --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         attrs                : [(String, Maybe Type)]
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
type T_AttributeDefList  = Catalog ->
                           LocalBindings ->
                           (Maybe LocalBindings) ->
                           ([Maybe LocalBindings]) ->
                           ( AttributeDefList,([(String, Maybe Type)]),AttributeDefList)
data Inh_AttributeDefList  = Inh_AttributeDefList {cat_Inh_AttributeDefList :: Catalog,lib_Inh_AttributeDefList :: LocalBindings,overrideLib_Inh_AttributeDefList :: Maybe LocalBindings,overrideLibs_Inh_AttributeDefList :: [Maybe LocalBindings]}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList,attrs_Syn_AttributeDefList :: [(String, Maybe Type)],originalTree_Syn_AttributeDefList :: AttributeDefList}
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_AttributeDefList _lhsOannotatedTree _lhsOattrs _lhsOoriginalTree ))
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOattrs :: ([(String, Maybe Type)])
              _lhsOannotatedTree :: AttributeDefList
              _lhsOoriginalTree :: AttributeDefList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: AttributeDef
              _hdIattrName :: String
              _hdInamedType :: (Maybe Type)
              _hdIoriginalTree :: AttributeDef
              _tlIannotatedTree :: AttributeDefList
              _tlIattrs :: ([(String, Maybe Type)])
              _tlIoriginalTree :: AttributeDefList
              -- "./TypeChecking/CreateTable.ag"(line 88, column 12)
              _lhsOattrs =
                  {-# LINE 88 "./TypeChecking/CreateTable.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 1091 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1096 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 1101 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1106 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1111 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1116 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1121 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 1126 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 1131 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1136 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1141 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 1146 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 1151 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIattrName,_hdInamedType,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIattrs,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOattrs :: ([(String, Maybe Type)])
              _lhsOannotatedTree :: AttributeDefList
              _lhsOoriginalTree :: AttributeDefList
              -- "./TypeChecking/CreateTable.ag"(line 89, column 11)
              _lhsOattrs =
                  {-# LINE 89 "./TypeChecking/CreateTable.ag" #-}
                  []
                  {-# LINE 1170 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1175 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1180 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1185 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1190 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree)))
-- CaseExpressionListExpressionPair ----------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         thenType             : Maybe Type
         whenTypes            : [Maybe Type]
   alternatives:
      alternative Tuple:
         child x1             : ExpressionList 
         child x2             : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type CaseExpressionListExpressionPair  = ( (ExpressionList),(Expression))
-- cata
sem_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair  ->
                                        T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair ( x1,x2)  =
    (sem_CaseExpressionListExpressionPair_Tuple (sem_ExpressionList x1 ) (sem_Expression x2 ) )
-- semantic domain
type T_CaseExpressionListExpressionPair  = Catalog ->
                                           LocalBindings ->
                                           (Maybe LocalBindings) ->
                                           ([Maybe LocalBindings]) ->
                                           ( CaseExpressionListExpressionPair,CaseExpressionListExpressionPair,(Maybe Type),([Maybe Type]))
data Inh_CaseExpressionListExpressionPair  = Inh_CaseExpressionListExpressionPair {cat_Inh_CaseExpressionListExpressionPair :: Catalog,lib_Inh_CaseExpressionListExpressionPair :: LocalBindings,overrideLib_Inh_CaseExpressionListExpressionPair :: Maybe LocalBindings,overrideLibs_Inh_CaseExpressionListExpressionPair :: [Maybe LocalBindings]}
data Syn_CaseExpressionListExpressionPair  = Syn_CaseExpressionListExpressionPair {annotatedTree_Syn_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair,originalTree_Syn_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair,thenType_Syn_CaseExpressionListExpressionPair :: Maybe Type,whenTypes_Syn_CaseExpressionListExpressionPair :: [Maybe Type]}
wrap_CaseExpressionListExpressionPair :: T_CaseExpressionListExpressionPair  ->
                                         Inh_CaseExpressionListExpressionPair  ->
                                         Syn_CaseExpressionListExpressionPair 
wrap_CaseExpressionListExpressionPair sem (Inh_CaseExpressionListExpressionPair _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOthenType,_lhsOwhenTypes) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_CaseExpressionListExpressionPair _lhsOannotatedTree _lhsOoriginalTree _lhsOthenType _lhsOwhenTypes ))
sem_CaseExpressionListExpressionPair_Tuple :: T_ExpressionList  ->
                                              T_Expression  ->
                                              T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOwhenTypes :: ([Maybe Type])
              _lhsOthenType :: (Maybe Type)
              _x1OexpectedTypes :: ([Maybe Type])
              _x2OexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: CaseExpressionListExpressionPair
              _lhsOoriginalTree :: CaseExpressionListExpressionPair
              _x1Ocat :: Catalog
              _x1Olib :: LocalBindings
              _x1OoverrideLib :: (Maybe LocalBindings)
              _x1OoverrideLibs :: ([Maybe LocalBindings])
              _x2Ocat :: Catalog
              _x2Olib :: LocalBindings
              _x2OoverrideLib :: (Maybe LocalBindings)
              _x2OoverrideLibs :: ([Maybe LocalBindings])
              _x1IannotatedTree :: ExpressionList
              _x1IoriginalTree :: ExpressionList
              _x1ItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _x1IuType :: ([Maybe Type])
              _x2IannotatedTree :: Expression
              _x2IntAnnotatedTree :: Expression
              _x2IntType :: ([(String,Type)])
              _x2IoriginalTree :: Expression
              _x2ItbAnnotatedTree :: Expression
              _x2ItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _x2IuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 259, column 13)
              _lhsOwhenTypes =
                  {-# LINE 259 "./TypeChecking/Expressions.ag" #-}
                  _x1IuType
                  {-# LINE 1271 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 260, column 13)
              _lhsOthenType =
                  {-# LINE 260 "./TypeChecking/Expressions.ag" #-}
                  _x2IuType
                  {-# LINE 1276 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 463, column 13)
              _x1OexpectedTypes =
                  {-# LINE 463 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 1281 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 464, column 13)
              _x2OexpectedType =
                  {-# LINE 464 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 1286 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 1291 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 1296 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1301 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1306 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1311 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1316 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 1321 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 1326 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1331 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1336 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 1341 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 1346 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1IoriginalTree,_x1ItbUTypes,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedTypes _x1Olib _x1OoverrideLib _x1OoverrideLibs )
              ( _x2IannotatedTree,_x2IntAnnotatedTree,_x2IntType,_x2IoriginalTree,_x2ItbAnnotatedTree,_x2ItbUType,_x2IuType) =
                  (x2_ _x2Ocat _x2OexpectedType _x2Olib _x2OoverrideLib _x2OoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOthenType,_lhsOwhenTypes)))
-- CaseExpressionListExpressionPairList ------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         thenTypes            : [Maybe Type]
         whenTypes            : [[Maybe Type]]
   alternatives:
      alternative Cons:
         child hd             : CaseExpressionListExpressionPair 
         child tl             : CaseExpressionListExpressionPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type CaseExpressionListExpressionPairList  = [(CaseExpressionListExpressionPair)]
-- cata
sem_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList  ->
                                            T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList list  =
    (Prelude.foldr sem_CaseExpressionListExpressionPairList_Cons sem_CaseExpressionListExpressionPairList_Nil (Prelude.map sem_CaseExpressionListExpressionPair list) )
-- semantic domain
type T_CaseExpressionListExpressionPairList  = Catalog ->
                                               LocalBindings ->
                                               (Maybe LocalBindings) ->
                                               ([Maybe LocalBindings]) ->
                                               ( CaseExpressionListExpressionPairList,CaseExpressionListExpressionPairList,([Maybe Type]),([[Maybe Type]]))
data Inh_CaseExpressionListExpressionPairList  = Inh_CaseExpressionListExpressionPairList {cat_Inh_CaseExpressionListExpressionPairList :: Catalog,lib_Inh_CaseExpressionListExpressionPairList :: LocalBindings,overrideLib_Inh_CaseExpressionListExpressionPairList :: Maybe LocalBindings,overrideLibs_Inh_CaseExpressionListExpressionPairList :: [Maybe LocalBindings]}
data Syn_CaseExpressionListExpressionPairList  = Syn_CaseExpressionListExpressionPairList {annotatedTree_Syn_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList,originalTree_Syn_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList,thenTypes_Syn_CaseExpressionListExpressionPairList :: [Maybe Type],whenTypes_Syn_CaseExpressionListExpressionPairList :: [[Maybe Type]]}
wrap_CaseExpressionListExpressionPairList :: T_CaseExpressionListExpressionPairList  ->
                                             Inh_CaseExpressionListExpressionPairList  ->
                                             Syn_CaseExpressionListExpressionPairList 
wrap_CaseExpressionListExpressionPairList sem (Inh_CaseExpressionListExpressionPairList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOwhenTypes) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_CaseExpressionListExpressionPairList _lhsOannotatedTree _lhsOoriginalTree _lhsOthenTypes _lhsOwhenTypes ))
sem_CaseExpressionListExpressionPairList_Cons :: T_CaseExpressionListExpressionPair  ->
                                                 T_CaseExpressionListExpressionPairList  ->
                                                 T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOwhenTypes :: ([[Maybe Type]])
              _lhsOthenTypes :: ([Maybe Type])
              _lhsOannotatedTree :: CaseExpressionListExpressionPairList
              _lhsOoriginalTree :: CaseExpressionListExpressionPairList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: CaseExpressionListExpressionPair
              _hdIoriginalTree :: CaseExpressionListExpressionPair
              _hdIthenType :: (Maybe Type)
              _hdIwhenTypes :: ([Maybe Type])
              _tlIannotatedTree :: CaseExpressionListExpressionPairList
              _tlIoriginalTree :: CaseExpressionListExpressionPairList
              _tlIthenTypes :: ([Maybe Type])
              _tlIwhenTypes :: ([[Maybe Type]])
              -- "./TypeChecking/Expressions.ag"(line 250, column 10)
              _lhsOwhenTypes =
                  {-# LINE 250 "./TypeChecking/Expressions.ag" #-}
                  _hdIwhenTypes : _tlIwhenTypes
                  {-# LINE 1430 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 251, column 10)
              _lhsOthenTypes =
                  {-# LINE 251 "./TypeChecking/Expressions.ag" #-}
                  _hdIthenType : _tlIthenTypes
                  {-# LINE 1435 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1440 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 1445 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1450 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1455 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1460 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1465 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 1470 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 1475 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1480 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1485 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 1490 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 1495 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIoriginalTree,_hdIthenType,_hdIwhenTypes) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIthenTypes,_tlIwhenTypes) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOwhenTypes)))
sem_CaseExpressionListExpressionPairList_Nil :: T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOwhenTypes :: ([[Maybe Type]])
              _lhsOthenTypes :: ([Maybe Type])
              _lhsOannotatedTree :: CaseExpressionListExpressionPairList
              _lhsOoriginalTree :: CaseExpressionListExpressionPairList
              -- "./TypeChecking/Expressions.ag"(line 252, column 9)
              _lhsOwhenTypes =
                  {-# LINE 252 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 1515 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 253, column 9)
              _lhsOthenTypes =
                  {-# LINE 253 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 1520 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1525 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1530 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1535 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1540 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOthenTypes,_lhsOwhenTypes)))
-- Constraint --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative CheckConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative PrimaryKeyConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child x              : {[String]}
         visit 0:
            local annotatedTree : _
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
            local originalTree : _
      alternative UniqueConstraint:
         child ann            : {Annotation}
         child name           : {String}
         child x              : {[String]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data Constraint  = CheckConstraint (Annotation) (String) (Expression) 
                 | PrimaryKeyConstraint (Annotation) (String) ([String]) 
                 | ReferenceConstraint (Annotation) (String) ([String]) (String) ([String]) (Cascade) (Cascade) 
                 | UniqueConstraint (Annotation) (String) ([String]) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Constraint :: Constraint  ->
                  T_Constraint 
sem_Constraint (CheckConstraint _ann _name _expr )  =
    (sem_Constraint_CheckConstraint _ann _name (sem_Expression _expr ) )
sem_Constraint (PrimaryKeyConstraint _ann _name _x )  =
    (sem_Constraint_PrimaryKeyConstraint _ann _name _x )
sem_Constraint (ReferenceConstraint _ann _name _atts _table _tableAtts _onUpdate _onDelete )  =
    (sem_Constraint_ReferenceConstraint _ann _name _atts _table _tableAtts _onUpdate _onDelete )
sem_Constraint (UniqueConstraint _ann _name _x )  =
    (sem_Constraint_UniqueConstraint _ann _name _x )
-- semantic domain
type T_Constraint  = Catalog ->
                     LocalBindings ->
                     (Maybe LocalBindings) ->
                     ([Maybe LocalBindings]) ->
                     ( Constraint,Constraint)
data Inh_Constraint  = Inh_Constraint {cat_Inh_Constraint :: Catalog,lib_Inh_Constraint :: LocalBindings,overrideLib_Inh_Constraint :: Maybe LocalBindings,overrideLibs_Inh_Constraint :: [Maybe LocalBindings]}
data Syn_Constraint  = Syn_Constraint {annotatedTree_Syn_Constraint :: Constraint,originalTree_Syn_Constraint :: Constraint}
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_Constraint _lhsOannotatedTree _lhsOoriginalTree ))
sem_Constraint_CheckConstraint :: Annotation ->
                                  String ->
                                  T_Expression  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Constraint
              _lhsOoriginalTree :: Constraint
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 467, column 23)
              _exprOexpectedType =
                  {-# LINE 467 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 1645 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CheckConstraint ann_ name_ _exprIannotatedTree
                  {-# LINE 1650 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CheckConstraint ann_ name_ _exprIoriginalTree
                  {-# LINE 1655 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1660 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1665 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1670 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1675 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 1680 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 1685 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Constraint_PrimaryKeyConstraint :: Annotation ->
                                       String ->
                                       ([String]) ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint ann_ name_ x_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  PrimaryKeyConstraint ann_ name_ x_
                  {-# LINE 1704 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  PrimaryKeyConstraint ann_ name_ x_
                  {-# LINE 1709 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1714 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1719 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
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
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
                  {-# LINE 1740 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
                  {-# LINE 1745 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1750 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1755 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_Constraint_UniqueConstraint :: Annotation ->
                                   String ->
                                   ([String]) ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint ann_ name_ x_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Constraint
              _lhsOoriginalTree :: Constraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  UniqueConstraint ann_ name_ x_
                  {-# LINE 1772 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  UniqueConstraint ann_ name_ x_
                  {-# LINE 1777 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1782 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1787 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ConstraintList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
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
type T_ConstraintList  = Catalog ->
                         LocalBindings ->
                         (Maybe LocalBindings) ->
                         ([Maybe LocalBindings]) ->
                         ( ConstraintList,ConstraintList)
data Inh_ConstraintList  = Inh_ConstraintList {cat_Inh_ConstraintList :: Catalog,lib_Inh_ConstraintList :: LocalBindings,overrideLib_Inh_ConstraintList :: Maybe LocalBindings,overrideLibs_Inh_ConstraintList :: [Maybe LocalBindings]}
data Syn_ConstraintList  = Syn_ConstraintList {annotatedTree_Syn_ConstraintList :: ConstraintList,originalTree_Syn_ConstraintList :: ConstraintList}
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_ConstraintList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: ConstraintList
              _lhsOoriginalTree :: ConstraintList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: Constraint
              _hdIoriginalTree :: Constraint
              _tlIannotatedTree :: ConstraintList
              _tlIoriginalTree :: ConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 1859 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 1864 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1869 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1874 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1879 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1884 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 1889 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 1894 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 1899 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 1904 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 1909 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 1914 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: ConstraintList
              _lhsOoriginalTree :: ConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1932 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 1937 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 1942 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 1947 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         expectedType         : Maybe Type
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         ntAnnotatedTree      : Expression 
         ntType               : [(String,Type)]
         originalTree         : SELF 
         tbAnnotatedTree      : Expression 
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
            local originalTree : _
      alternative Case:
         child ann            : {Annotation}
         child cases          : CaseExpressionListExpressionPairList 
         child els            : MaybeExpression 
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
            local originalTree : _
      alternative CaseSimple:
         child ann            : {Annotation}
         child value          : Expression 
         child cases          : CaseExpressionListExpressionPairList 
         child els            : MaybeExpression 
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
            local originalTree : _
      alternative Cast:
         child ann            : {Annotation}
         child expr           : Expression 
         child tn             : TypeName 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local originalTree : _
      alternative Exists:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local liftedColumnName : _
            local ntType      : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
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
            local originalTree : _
      alternative FunCall:
         child ann            : {Annotation}
         child funName        : {String}
         child args           : ExpressionList 
         visit 0:
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local _tup1       : _
            local tpe         : {Et}
            local prototype   : _
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
            local originalTree : _
      alternative Identifier:
         child ann            : {Annotation}
         child i              : {String}
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local useLib      : _
            local tpe         : {Et}
            local backTree    : _
            local ntType      : {E [(String,Type)]}
            local annotatedTree : _
            local originalTree : _
      alternative InPredicate:
         child ann            : {Annotation}
         child expr           : Expression 
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
            local originalTree : _
      alternative LiftOperator:
         child ann            : {Annotation}
         child oper           : {String}
         child flav           : {LiftFlavour}
         child args           : ExpressionList 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local tpe         : {Et}
            local backTree    : _
            local liftedColumnName : _
            local ntType      : _
            local annotatedTree : _
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
            local originalTree : _
      alternative PIdentifier:
         child ann            : {Annotation}
         child ex             : Expression 
         visit 0:
            local annotatedTree : _
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
            local originalTree : _
      alternative ScalarSubQuery:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local prototype   : {Maybe FunctionPrototype}
            local tbUType     : {E ([(String,Type)], [(String,Type)])}
            local liftedColumnName : _
            local ntType      : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
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
            local originalTree : _
      alternative WindowFn:
         child ann            : {Annotation}
         child fn             : Expression 
         child partitionBy    : ExpressionList 
         child orderBy        : ExpressionList 
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
            local originalTree : _
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
                 | PIdentifier (Annotation) (Expression) 
                 | Placeholder (Annotation) 
                 | PositionalArg (Annotation) (Integer) 
                 | ScalarSubQuery (Annotation) (SelectExpression) 
                 | StringLit (Annotation) (String) 
                 | WindowFn (Annotation) (Expression) (ExpressionList) (ExpressionList) (Direction) (FrameClause) 
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
    (sem_Expression_LiftOperator _ann _oper _flav (sem_ExpressionList _args ) )
sem_Expression (NullLit _ann )  =
    (sem_Expression_NullLit _ann )
sem_Expression (PIdentifier _ann _ex )  =
    (sem_Expression_PIdentifier _ann (sem_Expression _ex ) )
sem_Expression (Placeholder _ann )  =
    (sem_Expression_Placeholder _ann )
sem_Expression (PositionalArg _ann _p )  =
    (sem_Expression_PositionalArg _ann _p )
sem_Expression (ScalarSubQuery _ann _sel )  =
    (sem_Expression_ScalarSubQuery _ann (sem_SelectExpression _sel ) )
sem_Expression (StringLit _ann _value )  =
    (sem_Expression_StringLit _ann _value )
sem_Expression (WindowFn _ann _fn _partitionBy _orderBy _dir _frm )  =
    (sem_Expression_WindowFn _ann (sem_Expression _fn ) (sem_ExpressionList _partitionBy ) (sem_ExpressionList _orderBy ) _dir _frm )
-- semantic domain
type T_Expression  = Catalog ->
                     (Maybe Type) ->
                     LocalBindings ->
                     (Maybe LocalBindings) ->
                     ([Maybe LocalBindings]) ->
                     ( Expression,Expression,([(String,Type)]),Expression,Expression,(Maybe ([(String,Type)],[(String,Type)])),(Maybe Type))
data Inh_Expression  = Inh_Expression {cat_Inh_Expression :: Catalog,expectedType_Inh_Expression :: Maybe Type,lib_Inh_Expression :: LocalBindings,overrideLib_Inh_Expression :: Maybe LocalBindings,overrideLibs_Inh_Expression :: [Maybe LocalBindings]}
data Syn_Expression  = Syn_Expression {annotatedTree_Syn_Expression :: Expression,ntAnnotatedTree_Syn_Expression :: Expression,ntType_Syn_Expression :: [(String,Type)],originalTree_Syn_Expression :: Expression,tbAnnotatedTree_Syn_Expression :: Expression,tbUType_Syn_Expression :: Maybe ([(String,Type)],[(String,Type)]),uType_Syn_Expression :: Maybe Type}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression sem (Inh_Expression _lhsIcat _lhsIexpectedType _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedType _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_Expression _lhsOannotatedTree _lhsOntAnnotatedTree _lhsOntType _lhsOoriginalTree _lhsOtbAnnotatedTree _lhsOtbUType _lhsOuType ))
sem_Expression_BooleanLit :: Annotation ->
                             Bool ->
                             T_Expression 
sem_Expression_BooleanLit ann_ b_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 2296 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 2301 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 2308 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 2313 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 2320 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 2325 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 2330 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 2335 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 152, column 19)
              _tpe =
                  {-# LINE 152 "./TypeChecking/Expressions.ag" #-}
                  Right typeBool
                  {-# LINE 2340 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 164, column 9)
              _backTree =
                  {-# LINE 164 "./TypeChecking/Expressions.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 2345 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 2350 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 2360 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 2365 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  BooleanLit ann_ b_
                  {-# LINE 2370 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2375 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_Case :: Annotation ->
                       T_CaseExpressionListExpressionPairList  ->
                       T_MaybeExpression  ->
                       T_Expression 
sem_Expression_Case ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              _casesOcat :: Catalog
              _casesOlib :: LocalBindings
              _casesOoverrideLib :: (Maybe LocalBindings)
              _casesOoverrideLibs :: ([Maybe LocalBindings])
              _elsOcat :: Catalog
              _elsOlib :: LocalBindings
              _elsOoverrideLib :: (Maybe LocalBindings)
              _elsOoverrideLibs :: ([Maybe LocalBindings])
              _casesIannotatedTree :: CaseExpressionListExpressionPairList
              _casesIoriginalTree :: CaseExpressionListExpressionPairList
              _casesIthenTypes :: ([Maybe Type])
              _casesIwhenTypes :: ([[Maybe Type]])
              _elsIannotatedTree :: MaybeExpression
              _elsIoriginalTree :: MaybeExpression
              _elsIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 2421 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 2426 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 2433 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 2438 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 2445 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 2450 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 2455 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 2460 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 265, column 9)
              _whenTypes =
                  {-# LINE 265 "./TypeChecking/Expressions.ag" #-}
                  _casesIwhenTypes
                  {-# LINE 2465 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 266, column 9)
              _thenTypes =
                  {-# LINE 266 "./TypeChecking/Expressions.ag" #-}
                  _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIuType
                  {-# LINE 2470 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 270, column 9)
              _tpe =
                  {-# LINE 270 "./TypeChecking/Expressions.ag" #-}
                  do
                  wt <- mapM lmt $ concat _whenTypes
                  errorWhen (any (/= typeBool) wt)
                      [WrongTypes typeBool wt]
                  tt <- mapM lmt _thenTypes
                  resolveResultSetType _lhsIcat tt
                  {-# LINE 2480 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 278, column 9)
              _backTree =
                  {-# LINE 278 "./TypeChecking/Expressions.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 2485 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 2490 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 2500 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 2505 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Case ann_ _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 2510 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2515 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2520 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2525 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 2530 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 2535 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2540 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2545 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 2550 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 2555 "AstInternal.hs" #-}
              ( _casesIannotatedTree,_casesIoriginalTree,_casesIthenTypes,_casesIwhenTypes) =
                  (cases_ _casesOcat _casesOlib _casesOoverrideLib _casesOoverrideLibs )
              ( _elsIannotatedTree,_elsIoriginalTree,_elsIuType) =
                  (els_ _elsOcat _elsOlib _elsOoverrideLib _elsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_CaseSimple :: Annotation ->
                             T_Expression  ->
                             T_CaseExpressionListExpressionPairList  ->
                             T_MaybeExpression  ->
                             T_Expression 
sem_Expression_CaseSimple ann_ value_ cases_ els_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              _valueOcat :: Catalog
              _valueOexpectedType :: (Maybe Type)
              _valueOlib :: LocalBindings
              _valueOoverrideLib :: (Maybe LocalBindings)
              _valueOoverrideLibs :: ([Maybe LocalBindings])
              _casesOcat :: Catalog
              _casesOlib :: LocalBindings
              _casesOoverrideLib :: (Maybe LocalBindings)
              _casesOoverrideLibs :: ([Maybe LocalBindings])
              _elsOcat :: Catalog
              _elsOlib :: LocalBindings
              _elsOoverrideLib :: (Maybe LocalBindings)
              _elsOoverrideLibs :: ([Maybe LocalBindings])
              _valueIannotatedTree :: Expression
              _valueIntAnnotatedTree :: Expression
              _valueIntType :: ([(String,Type)])
              _valueIoriginalTree :: Expression
              _valueItbAnnotatedTree :: Expression
              _valueItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _valueIuType :: (Maybe Type)
              _casesIannotatedTree :: CaseExpressionListExpressionPairList
              _casesIoriginalTree :: CaseExpressionListExpressionPairList
              _casesIthenTypes :: ([Maybe Type])
              _casesIwhenTypes :: ([[Maybe Type]])
              _elsIannotatedTree :: MaybeExpression
              _elsIoriginalTree :: MaybeExpression
              _elsIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 2618 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 2623 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 2630 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 2635 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 2642 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 2647 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 2652 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 2657 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 265, column 9)
              _whenTypes =
                  {-# LINE 265 "./TypeChecking/Expressions.ag" #-}
                  _casesIwhenTypes
                  {-# LINE 2662 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 266, column 9)
              _thenTypes =
                  {-# LINE 266 "./TypeChecking/Expressions.ag" #-}
                  _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIuType
                  {-# LINE 2667 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 283, column 9)
              _tpe =
                  {-# LINE 283 "./TypeChecking/Expressions.ag" #-}
                  do
                  wt <- mapM lmt $ concat _whenTypes
                  vt <- lmt _valueIuType
                  _ <- resolveResultSetType _lhsIcat (vt : wt)
                  tt <- mapM lmt _thenTypes
                  resolveResultSetType _lhsIcat tt
                  {-# LINE 2677 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 290, column 9)
              _backTree =
                  {-# LINE 290 "./TypeChecking/Expressions.ag" #-}
                  CaseSimple ann_
                             _valueIannotatedTree
                             _casesIannotatedTree
                             _elsIannotatedTree
                  {-# LINE 2685 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 2690 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 2700 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 2705 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CaseSimple ann_ _valueIoriginalTree _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 2710 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2715 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2720 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOexpectedType =
                  {-# LINE 74 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 2725 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2730 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 2735 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 2740 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2745 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2750 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 2755 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 2760 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2765 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2770 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 2775 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 2780 "AstInternal.hs" #-}
              ( _valueIannotatedTree,_valueIntAnnotatedTree,_valueIntType,_valueIoriginalTree,_valueItbAnnotatedTree,_valueItbUType,_valueIuType) =
                  (value_ _valueOcat _valueOexpectedType _valueOlib _valueOoverrideLib _valueOoverrideLibs )
              ( _casesIannotatedTree,_casesIoriginalTree,_casesIthenTypes,_casesIwhenTypes) =
                  (cases_ _casesOcat _casesOlib _casesOoverrideLib _casesOoverrideLibs )
              ( _elsIannotatedTree,_elsIoriginalTree,_elsIuType) =
                  (els_ _elsOcat _elsOlib _elsOoverrideLib _elsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_Cast :: Annotation ->
                       T_Expression  ->
                       T_TypeName  ->
                       T_Expression 
sem_Expression_Cast ann_ expr_ tn_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              _exprOcat :: Catalog
              _exprOexpectedType :: (Maybe Type)
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _tnOcat :: Catalog
              _tnOlib :: LocalBindings
              _tnOoverrideLib :: (Maybe LocalBindings)
              _tnOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              _tnIannotatedTree :: TypeName
              _tnInamedType :: (Maybe Type)
              _tnIoriginalTree :: TypeName
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 2836 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 2841 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 2848 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 2853 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 2860 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 2865 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 2870 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 2875 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 177, column 12)
              _tpe =
                  {-# LINE 177 "./TypeChecking/Expressions.ag" #-}
                  lmt _tnInamedType
                  {-# LINE 2880 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 178, column 12)
              _backTree =
                  {-# LINE 178 "./TypeChecking/Expressions.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 2885 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 342, column 7)
              _liftedColumnName =
                  {-# LINE 342 "./TypeChecking/Expressions.ag" #-}
                  case _tnIannotatedTree of
                    SimpleTypeName _ tn -> tn
                    _ -> ""
                  {-# LINE 2892 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 2902 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
                  {-# LINE 2907 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Cast ann_ _exprIoriginalTree _tnIoriginalTree
                  {-# LINE 2912 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 2917 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2922 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOexpectedType =
                  {-# LINE 74 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 2927 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2932 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 2937 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 2942 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 2947 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 2952 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 2957 "AstInternal.hs" #-}
              -- copy rule (down)
              _tnOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 2962 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
              ( _tnIannotatedTree,_tnInamedType,_tnIoriginalTree) =
                  (tn_ _tnOcat _tnOlib _tnOoverrideLib _tnOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_Exists :: Annotation ->
                         T_SelectExpression  ->
                         T_Expression 
sem_Expression_Exists ann_ sel_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOoriginalTree :: Expression
              _selOcat :: Catalog
              _selOlib :: LocalBindings
              _selOoverrideLib :: (Maybe LocalBindings)
              _selOoverrideLibs :: ([Maybe LocalBindings])
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: SelectExpression
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 3005 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 3010 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 3017 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 3022 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 3029 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 3034 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 3039 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 3044 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 3049 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 3059 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 380, column 9)
              _tpe =
                  {-# LINE 380 "./TypeChecking/Expressions.ag" #-}
                  Right typeBool
                  {-# LINE 3064 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 381, column 9)
              _backTree =
                  {-# LINE 381 "./TypeChecking/Expressions.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 3069 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 558, column 29)
              _selOexpectedTypes =
                  {-# LINE 558 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 3074 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Exists ann_ _selIannotatedTree
                  {-# LINE 3079 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Exists ann_ _selIoriginalTree
                  {-# LINE 3084 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3089 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3094 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3099 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 3104 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 3109 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOlib _selOoverrideLib _selOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_FloatLit :: Annotation ->
                           Double ->
                           T_Expression 
sem_Expression_FloatLit ann_ d_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 3141 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 3146 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 3153 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 3158 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 3165 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 3170 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 3175 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 3180 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 151, column 17)
              _tpe =
                  {-# LINE 151 "./TypeChecking/Expressions.ag" #-}
                  Right typeNumeric
                  {-# LINE 3185 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 162, column 9)
              _backTree =
                  {-# LINE 162 "./TypeChecking/Expressions.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 3190 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 3195 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 3205 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 3210 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  FloatLit ann_ d_
                  {-# LINE 3215 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3220 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_FunCall :: Annotation ->
                          String ->
                          T_ExpressionList  ->
                          T_Expression 
sem_Expression_FunCall ann_ funName_ args_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _argsOoverrideLibs :: ([Maybe LocalBindings])
              _tpe :: Et
              _argsOexpectedTypes :: ([Maybe Type])
              _lhsOoriginalTree :: Expression
              _argsOcat :: Catalog
              _argsOlib :: LocalBindings
              _argsOoverrideLib :: (Maybe LocalBindings)
              _argsIannotatedTree :: ExpressionList
              _argsIoriginalTree :: ExpressionList
              _argsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _argsIuType :: ([Maybe Type])
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 3259 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 3266 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 3271 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 3278 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 3283 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 64, column 9)
              _tbUType =
                  {-# LINE 64 "./TypeChecking/Expressions.ag" #-}
                  case (funName_,_argsItbUTypes) of
                   (".", [_,Just t]) -> Right t
                   _ -> Left [InternalError "bad context for tbUType"]
                  {-# LINE 3290 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 3295 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 126, column 15)
              _argsOoverrideLibs =
                  {-# LINE 126 "./TypeChecking/Expressions.ag" #-}
                  case funName_ of
                    "." | [Identifier _ i,_] <- _argsIannotatedTree
                            -> [Nothing
                               ,either (const Nothing) Just $
                                        lbUpdateDot _lhsIcat i _lhsIlib]
                    _ -> []
                  {-# LINE 3305 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 185, column 9)
              __tup1 =
                  {-# LINE 185 "./TypeChecking/Expressions.ag" #-}
                  either (\e -> (Left e, Nothing)) id $ do
                  args <- mapM lmt _argsIuType
                  efp <- findCallMatch _lhsIcat
                                       funName_
                                       args
                  let (_,_,r,_) = efp
                  return (Right r, Just efp)
                  {-# LINE 3316 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 185, column 9)
              (_tpe,_) =
                  {-# LINE 185 "./TypeChecking/Expressions.ag" #-}
                  __tup1
                  {-# LINE 3321 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 185, column 9)
              (_,_prototype) =
                  {-# LINE 186 "./TypeChecking/Expressions.ag" #-}
                  __tup1
                  {-# LINE 3326 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 195, column 9)
              _backTree =
                  {-# LINE 195 "./TypeChecking/Expressions.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 3331 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 336, column 7)
              _liftedColumnName =
                  {-# LINE 336 "./TypeChecking/Expressions.ag" #-}
                  case funName_ of
                    "." -> getName _backTree
                    x | isOperatorName x -> "?column?"
                    _ -> funName_
                  {-# LINE 3339 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 3349 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 517, column 9)
              _argsOexpectedTypes =
                  {-# LINE 517 "./TypeChecking/Expressions.ag" #-}
                  maybe [] id $
                  case (funName_,_lhsIexpectedType) of
                    ("!rowctor", Just (AnonymousRecordType ts)) -> return $ map Just ts
                    _ -> do
                         (_,t,_,_) <- _prototype
                         return $ map Just t
                  {-# LINE 3359 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIannotatedTree
                  {-# LINE 3364 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  FunCall ann_ funName_ _argsIoriginalTree
                  {-# LINE 3369 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3374 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3379 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3384 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 3389 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsIoriginalTree,_argsItbUTypes,_argsIuType) =
                  (args_ _argsOcat _argsOexpectedTypes _argsOlib _argsOoverrideLib _argsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_Identifier :: Annotation ->
                             String ->
                             T_Expression 
sem_Expression_Identifier ann_ i_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _ntType :: (E [(String,Type)])
              _lhsOoriginalTree :: Expression
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 3422 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 3427 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 3434 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 3439 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 3446 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 3451 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 68, column 9)
              _tbUType =
                  {-# LINE 68 "./TypeChecking/Expressions.ag" #-}
                  catCompositeAttrsPair _lhsIcat relationComposites i_
                  {-# LINE 3456 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 3461 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 323, column 9)
              _useLib =
                  {-# LINE 323 "./TypeChecking/Expressions.ag" #-}
                  maybe _lhsIlib id _lhsIoverrideLib
                  {-# LINE 3466 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 324, column 9)
              _tpe =
                  {-# LINE 324 "./TypeChecking/Expressions.ag" #-}
                  unwrapLookup <$> lbLookupID _useLib     i_
                  {-# LINE 3471 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 325, column 9)
              _backTree =
                  {-# LINE 325 "./TypeChecking/Expressions.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 3476 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 328, column 9)
              _ntType =
                  {-# LINE 328 "./TypeChecking/Expressions.ag" #-}
                  if i_ == "*"
                  then unwrapStar <$> lbExpandStar _useLib
                  else (\t -> [(i_, t)]) <$> unwrapLookup <$> lbLookupID _useLib     i_
                  {-# LINE 3483 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 3488 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Identifier ann_ i_
                  {-# LINE 3493 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3498 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_InPredicate :: Annotation ->
                              T_Expression  ->
                              Bool ->
                              T_InList  ->
                              T_Expression 
sem_Expression_InPredicate ann_ expr_ i_ list_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              _exprOcat :: Catalog
              _exprOexpectedType :: (Maybe Type)
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _listOcat :: Catalog
              _listOlib :: LocalBindings
              _listOoverrideLib :: (Maybe LocalBindings)
              _listOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              _listIannotatedTree :: InList
              _listIlistType :: (Either [TypeError] Type)
              _listIoriginalTree :: InList
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 3549 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 3554 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 3561 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 3566 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 3573 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 3578 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 3583 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 3588 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 3593 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 3603 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 408, column 9)
              _tpe =
                  {-# LINE 408 "./TypeChecking/Expressions.ag" #-}
                  do
                  lt <- _listIlistType
                  expt <- lmt _exprIuType
                  _ <- resolveResultSetType _lhsIcat [expt, lt]
                  return typeBool
                  {-# LINE 3612 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 413, column 9)
              _backTree =
                  {-# LINE 413 "./TypeChecking/Expressions.ag" #-}
                  InPredicate ann_
                              _exprIannotatedTree
                              i_
                              _listIannotatedTree
                  {-# LINE 3620 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
                  {-# LINE 3625 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  InPredicate ann_ _exprIoriginalTree i_ _listIoriginalTree
                  {-# LINE 3630 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3635 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3640 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOexpectedType =
                  {-# LINE 74 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 3645 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3650 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 3655 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 3660 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3665 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3670 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 3675 "AstInternal.hs" #-}
              -- copy rule (down)
              _listOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 3680 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
              ( _listIannotatedTree,_listIlistType,_listIoriginalTree) =
                  (list_ _listOcat _listOlib _listOoverrideLib _listOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_IntegerLit :: Annotation ->
                             Integer ->
                             T_Expression 
sem_Expression_IntegerLit ann_ i_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 3714 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 3719 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 3726 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 3731 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 3738 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 3743 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 3748 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 3753 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 149, column 19)
              _tpe =
                  {-# LINE 149 "./TypeChecking/Expressions.ag" #-}
                  Right typeInt
                  {-# LINE 3758 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 158, column 9)
              _backTree =
                  {-# LINE 158 "./TypeChecking/Expressions.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 3763 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 3768 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 3778 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 3783 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  IntegerLit ann_ i_
                  {-# LINE 3788 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3793 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_LiftOperator :: Annotation ->
                               String ->
                               LiftFlavour ->
                               T_ExpressionList  ->
                               T_Expression 
sem_Expression_LiftOperator ann_ oper_ flav_ args_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _argsOexpectedTypes :: ([Maybe Type])
              _lhsOoriginalTree :: Expression
              _argsOcat :: Catalog
              _argsOlib :: LocalBindings
              _argsOoverrideLib :: (Maybe LocalBindings)
              _argsOoverrideLibs :: ([Maybe LocalBindings])
              _argsIannotatedTree :: ExpressionList
              _argsIoriginalTree :: ExpressionList
              _argsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _argsIuType :: ([Maybe Type])
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 3834 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 3839 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 3846 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 3851 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 3858 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 3863 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 3868 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 3873 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 218, column 9)
              _tpe =
                  {-# LINE 218 "./TypeChecking/Expressions.ag" #-}
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
                  {-# LINE 3891 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 232, column 9)
              _backTree =
                  {-# LINE 232 "./TypeChecking/Expressions.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIannotatedTree
                  {-# LINE 3896 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 3901 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 3911 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 525, column 9)
              _argsOexpectedTypes =
                  {-# LINE 525 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 3916 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIannotatedTree
                  {-# LINE 3921 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  LiftOperator ann_ oper_ flav_ _argsIoriginalTree
                  {-# LINE 3926 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 3931 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 3936 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 3941 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 3946 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 3951 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsIoriginalTree,_argsItbUTypes,_argsIuType) =
                  (args_ _argsOcat _argsOexpectedTypes _argsOlib _argsOoverrideLib _argsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_NullLit :: Annotation ->
                          T_Expression 
sem_Expression_NullLit ann_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 3982 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 3987 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 3994 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 3999 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 4006 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 4011 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 4016 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 4021 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 154, column 16)
              _tpe =
                  {-# LINE 154 "./TypeChecking/Expressions.ag" #-}
                  Right UnknownType
                  {-# LINE 4026 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 166, column 9)
              _backTree =
                  {-# LINE 166 "./TypeChecking/Expressions.ag" #-}
                  NullLit ann_
                  {-# LINE 4031 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 4036 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 4046 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 4051 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  NullLit ann_
                  {-# LINE 4056 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4061 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_PIdentifier :: Annotation ->
                              T_Expression  ->
                              T_Expression 
sem_Expression_PIdentifier ann_ ex_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _lhsOoriginalTree :: Expression
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _exOcat :: Catalog
              _exOexpectedType :: (Maybe Type)
              _exOlib :: LocalBindings
              _exOoverrideLib :: (Maybe LocalBindings)
              _exOoverrideLibs :: ([Maybe LocalBindings])
              _exIannotatedTree :: Expression
              _exIntAnnotatedTree :: Expression
              _exIntType :: ([(String,Type)])
              _exIoriginalTree :: Expression
              _exItbAnnotatedTree :: Expression
              _exItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exIuType :: (Maybe Type)
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  PIdentifier ann_ _exIannotatedTree
                  {-# LINE 4095 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  PIdentifier ann_ _exIoriginalTree
                  {-# LINE 4100 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4105 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4110 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOntAnnotatedTree =
                  {-# LINE 39 "./TypeChecking/Expressions.ag" #-}
                  _exIntAnnotatedTree
                  {-# LINE 4115 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOntType =
                  {-# LINE 40 "./TypeChecking/Expressions.ag" #-}
                  _exIntType
                  {-# LINE 4120 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOtbAnnotatedTree =
                  {-# LINE 42 "./TypeChecking/Expressions.ag" #-}
                  _exItbAnnotatedTree
                  {-# LINE 4125 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOtbUType =
                  {-# LINE 43 "./TypeChecking/Expressions.ag" #-}
                  _exItbUType
                  {-# LINE 4130 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOuType =
                  {-# LINE 41 "./TypeChecking/Expressions.ag" #-}
                  _exIuType
                  {-# LINE 4135 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4140 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOexpectedType =
                  {-# LINE 74 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 4145 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4150 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 4155 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 4160 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIntAnnotatedTree,_exIntType,_exIoriginalTree,_exItbAnnotatedTree,_exItbUType,_exIuType) =
                  (ex_ _exOcat _exOexpectedType _exOlib _exOoverrideLib _exOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_Placeholder :: Annotation ->
                              T_Expression 
sem_Expression_Placeholder ann_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 4191 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 4196 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 4203 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 4208 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 4215 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 4220 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 4225 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 4230 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 4235 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 4245 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 374, column 9)
              _tpe =
                  {-# LINE 374 "./TypeChecking/Expressions.ag" #-}
                  Right UnknownType
                  {-# LINE 4250 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 375, column 9)
              _backTree =
                  {-# LINE 375 "./TypeChecking/Expressions.ag" #-}
                  Placeholder ann_
                  {-# LINE 4255 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Placeholder ann_
                  {-# LINE 4260 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Placeholder ann_
                  {-# LINE 4265 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4270 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_PositionalArg :: Annotation ->
                                Integer ->
                                T_Expression 
sem_Expression_PositionalArg ann_ p_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 4300 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 4305 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 4312 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 4317 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 4324 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 4329 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 4334 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 4339 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 4344 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 4354 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 368, column 9)
              _tpe =
                  {-# LINE 368 "./TypeChecking/Expressions.ag" #-}
                  unwrapLookup <$> lbLookupID _lhsIlib ('$':show p_)
                  {-# LINE 4359 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 369, column 9)
              _backTree =
                  {-# LINE 369 "./TypeChecking/Expressions.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 4364 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 4369 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  PositionalArg ann_ p_
                  {-# LINE 4374 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4379 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_ScalarSubQuery :: Annotation ->
                                 T_SelectExpression  ->
                                 T_Expression 
sem_Expression_ScalarSubQuery ann_ sel_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOoriginalTree :: Expression
              _selOcat :: Catalog
              _selOlib :: LocalBindings
              _selOoverrideLib :: (Maybe LocalBindings)
              _selOoverrideLibs :: ([Maybe LocalBindings])
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: SelectExpression
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 4418 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 4423 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 4430 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 4435 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 4442 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 4447 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 4452 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 4457 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 4462 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 4472 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 392, column 9)
              _tpe =
                  {-# LINE 392 "./TypeChecking/Expressions.ag" #-}
                  do
                  selType <- lmt (map snd <$> _selIuType)
                  case length selType of
                    0 -> Left [InternalError "no columns in scalar subquery?"]
                    1 -> Right $ head selType
                    _ -> Right $ AnonymousRecordType selType
                  {-# LINE 4482 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 400, column 9)
              _backTree =
                  {-# LINE 400 "./TypeChecking/Expressions.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 4487 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 558, column 29)
              _selOexpectedTypes =
                  {-# LINE 558 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 4492 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIannotatedTree
                  {-# LINE 4497 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ScalarSubQuery ann_ _selIoriginalTree
                  {-# LINE 4502 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4507 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4512 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4517 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 4522 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 4527 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOlib _selOoverrideLib _selOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_StringLit :: Annotation ->
                            String ->
                            T_Expression 
sem_Expression_StringLit ann_ value_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _lhsOoriginalTree :: Expression
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 4559 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 4564 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 4571 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 4576 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 4583 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 4588 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 4593 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 4598 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 150, column 18)
              _tpe =
                  {-# LINE 150 "./TypeChecking/Expressions.ag" #-}
                  Right UnknownType
                  {-# LINE 4603 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 160, column 9)
              _backTree =
                  {-# LINE 160 "./TypeChecking/Expressions.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 4608 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  {-# LINE 353 "./TypeChecking/Expressions.ag" #-}
                  ""
                  {-# LINE 4613 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 4623 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 4628 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  StringLit ann_ value_
                  {-# LINE 4633 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4638 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
sem_Expression_WindowFn :: Annotation ->
                           T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList  ->
                           Direction ->
                           FrameClause ->
                           T_Expression 
sem_Expression_WindowFn ann_ fn_ partitionBy_ orderBy_ dir_ frm_  =
    (\ _lhsIcat
       _lhsIexpectedType
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Expression
              _prototype :: (Maybe FunctionPrototype)
              _lhsOntAnnotatedTree :: Expression
              _lhsOntType :: ([(String,Type)])
              _lhsOtbAnnotatedTree :: Expression
              _lhsOtbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tbUType :: (E ([(String,Type)], [(String,Type)]))
              _lhsOuType :: (Maybe Type)
              _tpe :: Et
              _partitionByOexpectedTypes :: ([Maybe Type])
              _orderByOexpectedTypes :: ([Maybe Type])
              _lhsOoriginalTree :: Expression
              _fnOcat :: Catalog
              _fnOexpectedType :: (Maybe Type)
              _fnOlib :: LocalBindings
              _fnOoverrideLib :: (Maybe LocalBindings)
              _fnOoverrideLibs :: ([Maybe LocalBindings])
              _partitionByOcat :: Catalog
              _partitionByOlib :: LocalBindings
              _partitionByOoverrideLib :: (Maybe LocalBindings)
              _partitionByOoverrideLibs :: ([Maybe LocalBindings])
              _orderByOcat :: Catalog
              _orderByOlib :: LocalBindings
              _orderByOoverrideLib :: (Maybe LocalBindings)
              _orderByOoverrideLibs :: ([Maybe LocalBindings])
              _fnIannotatedTree :: Expression
              _fnIntAnnotatedTree :: Expression
              _fnIntType :: ([(String,Type)])
              _fnIoriginalTree :: Expression
              _fnItbAnnotatedTree :: Expression
              _fnItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _fnIuType :: (Maybe Type)
              _partitionByIannotatedTree :: ExpressionList
              _partitionByIoriginalTree :: ExpressionList
              _partitionByItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _partitionByIuType :: ([Maybe Type])
              _orderByIannotatedTree :: ExpressionList
              _orderByIoriginalTree :: ExpressionList
              _orderByItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _orderByIuType :: ([Maybe Type])
              -- "./TypeChecking/Expressions.ag"(line 14, column 9)
              _lhsOannotatedTree =
                  {-# LINE 14 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
                  {-# LINE 4702 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  {-# LINE 27 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 4707 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  {-# LINE 34 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
                  {-# LINE 4714 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  {-# LINE 37 "./TypeChecking/Expressions.ag" #-}
                  either (const []) id _ntType
                  {-# LINE 4719 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  {-# LINE 53 "./TypeChecking/Expressions.ag" #-}
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
                  {-# LINE 4726 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  {-# LINE 57 "./TypeChecking/Expressions.ag" #-}
                  either (const Nothing) Just _tbUType
                  {-# LINE 4731 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  {-# LINE 62 "./TypeChecking/Expressions.ag" #-}
                  Left [InternalError "bad context for tbUType"]
                  {-# LINE 4736 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  {-# LINE 83 "./TypeChecking/Expressions.ag" #-}
                  etmt _tpe
                  {-# LINE 4741 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 199, column 9)
              _tpe =
                  {-# LINE 199 "./TypeChecking/Expressions.ag" #-}
                  lmt _fnIuType
                  {-# LINE 4746 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 200, column 9)
              _backTree =
                  {-# LINE 200 "./TypeChecking/Expressions.ag" #-}
                  WindowFn ann_
                           _fnIannotatedTree
                           _partitionByIannotatedTree
                           _orderByIannotatedTree
                           dir_
                           frm_
                  {-# LINE 4756 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 346, column 7)
              _liftedColumnName =
                  {-# LINE 346 "./TypeChecking/Expressions.ag" #-}
                  let (FunCall _ fn _) = _fnIannotatedTree
                  in fn
                  {-# LINE 4762 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  {-# LINE 359 "./TypeChecking/Expressions.ag" #-}
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
                  {-# LINE 4772 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 527, column 9)
              _partitionByOexpectedTypes =
                  {-# LINE 527 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 4777 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 528, column 9)
              _orderByOexpectedTypes =
                  {-# LINE 528 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 4782 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree dir_ frm_
                  {-# LINE 4787 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  WindowFn ann_ _fnIoriginalTree _partitionByIoriginalTree _orderByIoriginalTree dir_ frm_
                  {-# LINE 4792 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4797 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4802 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOexpectedType =
                  {-# LINE 74 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedType
                  {-# LINE 4807 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4812 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 4817 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 4822 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4827 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4832 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 4837 "AstInternal.hs" #-}
              -- copy rule (down)
              _partitionByOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 4842 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4847 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4852 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 4857 "AstInternal.hs" #-}
              -- copy rule (down)
              _orderByOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 4862 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIntAnnotatedTree,_fnIntType,_fnIoriginalTree,_fnItbAnnotatedTree,_fnItbUType,_fnIuType) =
                  (fn_ _fnOcat _fnOexpectedType _fnOlib _fnOoverrideLib _fnOoverrideLibs )
              ( _partitionByIannotatedTree,_partitionByIoriginalTree,_partitionByItbUTypes,_partitionByIuType) =
                  (partitionBy_ _partitionByOcat _partitionByOexpectedTypes _partitionByOlib _partitionByOoverrideLib _partitionByOoverrideLibs )
              ( _orderByIannotatedTree,_orderByIoriginalTree,_orderByItbUTypes,_orderByIuType) =
                  (orderBy_ _orderByOcat _orderByOexpectedTypes _orderByOlib _orderByOoverrideLib _orderByOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOntAnnotatedTree,_lhsOntType,_lhsOoriginalTree,_lhsOtbAnnotatedTree,_lhsOtbUType,_lhsOuType)))
-- ExpressionDirectionPair -------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : Expression 
         child x2             : {Direction}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ExpressionDirectionPair  = ( (Expression),(Direction))
-- cata
sem_ExpressionDirectionPair :: ExpressionDirectionPair  ->
                               T_ExpressionDirectionPair 
sem_ExpressionDirectionPair ( x1,x2)  =
    (sem_ExpressionDirectionPair_Tuple (sem_Expression x1 ) x2 )
-- semantic domain
type T_ExpressionDirectionPair  = Catalog ->
                                  LocalBindings ->
                                  (Maybe LocalBindings) ->
                                  ([Maybe LocalBindings]) ->
                                  ( ExpressionDirectionPair,ExpressionDirectionPair)
data Inh_ExpressionDirectionPair  = Inh_ExpressionDirectionPair {cat_Inh_ExpressionDirectionPair :: Catalog,lib_Inh_ExpressionDirectionPair :: LocalBindings,overrideLib_Inh_ExpressionDirectionPair :: Maybe LocalBindings,overrideLibs_Inh_ExpressionDirectionPair :: [Maybe LocalBindings]}
data Syn_ExpressionDirectionPair  = Syn_ExpressionDirectionPair {annotatedTree_Syn_ExpressionDirectionPair :: ExpressionDirectionPair,originalTree_Syn_ExpressionDirectionPair :: ExpressionDirectionPair}
wrap_ExpressionDirectionPair :: T_ExpressionDirectionPair  ->
                                Inh_ExpressionDirectionPair  ->
                                Syn_ExpressionDirectionPair 
wrap_ExpressionDirectionPair sem (Inh_ExpressionDirectionPair _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_ExpressionDirectionPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionDirectionPair_Tuple :: T_Expression  ->
                                     Direction ->
                                     T_ExpressionDirectionPair 
sem_ExpressionDirectionPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _x1OexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: ExpressionDirectionPair
              _lhsOoriginalTree :: ExpressionDirectionPair
              _x1Ocat :: Catalog
              _x1Olib :: LocalBindings
              _x1OoverrideLib :: (Maybe LocalBindings)
              _x1OoverrideLibs :: ([Maybe LocalBindings])
              _x1IannotatedTree :: Expression
              _x1IntAnnotatedTree :: Expression
              _x1IntType :: ([(String,Type)])
              _x1IoriginalTree :: Expression
              _x1ItbAnnotatedTree :: Expression
              _x1ItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _x1IuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 470, column 13)
              _x1OexpectedType =
                  {-# LINE 470 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 4936 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,x2_)
                  {-# LINE 4941 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,x2_)
                  {-# LINE 4946 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 4951 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 4956 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 4961 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 4966 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 4971 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 4976 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1IntAnnotatedTree,_x1IntType,_x1IoriginalTree,_x1ItbAnnotatedTree,_x1ItbUType,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedType _x1Olib _x1OoverrideLib _x1OoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ExpressionDirectionPairList ---------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : ExpressionDirectionPair 
         child tl             : ExpressionDirectionPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ExpressionDirectionPairList  = [(ExpressionDirectionPair)]
-- cata
sem_ExpressionDirectionPairList :: ExpressionDirectionPairList  ->
                                   T_ExpressionDirectionPairList 
sem_ExpressionDirectionPairList list  =
    (Prelude.foldr sem_ExpressionDirectionPairList_Cons sem_ExpressionDirectionPairList_Nil (Prelude.map sem_ExpressionDirectionPair list) )
-- semantic domain
type T_ExpressionDirectionPairList  = Catalog ->
                                      LocalBindings ->
                                      (Maybe LocalBindings) ->
                                      ([Maybe LocalBindings]) ->
                                      ( ExpressionDirectionPairList,ExpressionDirectionPairList)
data Inh_ExpressionDirectionPairList  = Inh_ExpressionDirectionPairList {cat_Inh_ExpressionDirectionPairList :: Catalog,lib_Inh_ExpressionDirectionPairList :: LocalBindings,overrideLib_Inh_ExpressionDirectionPairList :: Maybe LocalBindings,overrideLibs_Inh_ExpressionDirectionPairList :: [Maybe LocalBindings]}
data Syn_ExpressionDirectionPairList  = Syn_ExpressionDirectionPairList {annotatedTree_Syn_ExpressionDirectionPairList :: ExpressionDirectionPairList,originalTree_Syn_ExpressionDirectionPairList :: ExpressionDirectionPairList}
wrap_ExpressionDirectionPairList :: T_ExpressionDirectionPairList  ->
                                    Inh_ExpressionDirectionPairList  ->
                                    Syn_ExpressionDirectionPairList 
wrap_ExpressionDirectionPairList sem (Inh_ExpressionDirectionPairList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_ExpressionDirectionPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionDirectionPairList_Cons :: T_ExpressionDirectionPair  ->
                                        T_ExpressionDirectionPairList  ->
                                        T_ExpressionDirectionPairList 
sem_ExpressionDirectionPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: ExpressionDirectionPairList
              _lhsOoriginalTree :: ExpressionDirectionPairList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: ExpressionDirectionPair
              _hdIoriginalTree :: ExpressionDirectionPair
              _tlIannotatedTree :: ExpressionDirectionPairList
              _tlIoriginalTree :: ExpressionDirectionPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5050 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 5055 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5060 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5065 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5070 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5075 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 5080 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 5085 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5090 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5095 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 5100 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 5105 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ExpressionDirectionPairList_Nil :: T_ExpressionDirectionPairList 
sem_ExpressionDirectionPairList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: ExpressionDirectionPairList
              _lhsOoriginalTree :: ExpressionDirectionPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5123 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5128 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5133 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5138 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ExpressionList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         expectedTypes        : [Maybe Type]
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         tbUTypes             : [Maybe ([(String,Type)],[(String,Type)])]
         uType                : [Maybe Type]
   alternatives:
      alternative Cons:
         child hd             : Expression 
         child tl             : ExpressionList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ExpressionList  = [(Expression)]
-- cata
sem_ExpressionList :: ExpressionList  ->
                      T_ExpressionList 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_ExpressionList  = Catalog ->
                         ([Maybe Type]) ->
                         LocalBindings ->
                         (Maybe LocalBindings) ->
                         ([Maybe LocalBindings]) ->
                         ( ExpressionList,ExpressionList,([Maybe ([(String,Type)],[(String,Type)])]),([Maybe Type]))
data Inh_ExpressionList  = Inh_ExpressionList {cat_Inh_ExpressionList :: Catalog,expectedTypes_Inh_ExpressionList :: [Maybe Type],lib_Inh_ExpressionList :: LocalBindings,overrideLib_Inh_ExpressionList :: Maybe LocalBindings,overrideLibs_Inh_ExpressionList :: [Maybe LocalBindings]}
data Syn_ExpressionList  = Syn_ExpressionList {annotatedTree_Syn_ExpressionList :: ExpressionList,originalTree_Syn_ExpressionList :: ExpressionList,tbUTypes_Syn_ExpressionList :: [Maybe ([(String,Type)],[(String,Type)])],uType_Syn_ExpressionList :: [Maybe Type]}
wrap_ExpressionList :: T_ExpressionList  ->
                       Inh_ExpressionList  ->
                       Syn_ExpressionList 
wrap_ExpressionList sem (Inh_ExpressionList _lhsIcat _lhsIexpectedTypes _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOtbUTypes,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedTypes _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_ExpressionList _lhsOannotatedTree _lhsOoriginalTree _lhsOtbUTypes _lhsOuType ))
sem_ExpressionList_Cons :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList 
sem_ExpressionList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOtbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _lhsOuType :: ([Maybe Type])
              _hdOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdOexpectedType :: (Maybe Type)
              _tlOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: ExpressionList
              _lhsOoriginalTree :: ExpressionList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _hdIannotatedTree :: Expression
              _hdIntAnnotatedTree :: Expression
              _hdIntType :: ([(String,Type)])
              _hdIoriginalTree :: Expression
              _hdItbAnnotatedTree :: Expression
              _hdItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _hdIuType :: (Maybe Type)
              _tlIannotatedTree :: ExpressionList
              _tlIoriginalTree :: ExpressionList
              _tlItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _tlIuType :: ([Maybe Type])
              -- "./TypeChecking/Expressions.ag"(line 71, column 12)
              _lhsOtbUTypes =
                  {-# LINE 71 "./TypeChecking/Expressions.ag" #-}
                  _hdItbUType : _tlItbUTypes
                  {-# LINE 5226 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 86, column 12)
              _lhsOuType =
                  {-# LINE 86 "./TypeChecking/Expressions.ag" #-}
                  _hdIuType : _tlIuType
                  {-# LINE 5231 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 135, column 12)
              _hdOoverrideLib =
                  {-# LINE 135 "./TypeChecking/Expressions.ag" #-}
                  case _lhsIoverrideLibs of
                     h : _ -> h
                     _ -> Nothing
                  {-# LINE 5238 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 138, column 12)
              _tlOoverrideLibs =
                  {-# LINE 138 "./TypeChecking/Expressions.ag" #-}
                  case _lhsIoverrideLibs of
                    _ : t -> t
                    _ -> []
                  {-# LINE 5245 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 473, column 12)
              _hdOexpectedType =
                  {-# LINE 473 "./TypeChecking/Expressions.ag" #-}
                  case _lhsIexpectedTypes of
                    (t:_) -> t
                    _ -> Nothing
                  {-# LINE 5252 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 476, column 12)
              _tlOexpectedTypes =
                  {-# LINE 476 "./TypeChecking/Expressions.ag" #-}
                  case _lhsIexpectedTypes of
                  (_:ts) -> ts
                  _ -> []
                  {-# LINE 5259 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5264 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 5269 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5274 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5279 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5284 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5289 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 5294 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5299 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5304 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 5309 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIntAnnotatedTree,_hdIntType,_hdIoriginalTree,_hdItbAnnotatedTree,_hdItbUType,_hdIuType) =
                  (hd_ _hdOcat _hdOexpectedType _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree,_tlItbUTypes,_tlIuType) =
                  (tl_ _tlOcat _tlOexpectedTypes _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOtbUTypes,_lhsOuType)))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOtbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _lhsOuType :: ([Maybe Type])
              _lhsOannotatedTree :: ExpressionList
              _lhsOoriginalTree :: ExpressionList
              -- "./TypeChecking/Expressions.ag"(line 72, column 11)
              _lhsOtbUTypes =
                  {-# LINE 72 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 5330 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 87, column 11)
              _lhsOuType =
                  {-# LINE 87 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 5335 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5340 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5345 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5350 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5355 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOtbUTypes,_lhsOuType)))
-- ExpressionListList ------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         expectedTypes        : [Maybe Type]
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         uType                : [[Maybe Type]]
   alternatives:
      alternative Cons:
         child hd             : ExpressionList 
         child tl             : ExpressionListList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
type ExpressionListList  = [(ExpressionList)]
-- cata
sem_ExpressionListList :: ExpressionListList  ->
                          T_ExpressionListList 
sem_ExpressionListList list  =
    (Prelude.foldr sem_ExpressionListList_Cons sem_ExpressionListList_Nil (Prelude.map sem_ExpressionList list) )
-- semantic domain
type T_ExpressionListList  = Catalog ->
                             ([Maybe Type]) ->
                             LocalBindings ->
                             (Maybe LocalBindings) ->
                             ([Maybe LocalBindings]) ->
                             ( ExpressionListList,ExpressionListList,([[Maybe Type]]))
data Inh_ExpressionListList  = Inh_ExpressionListList {cat_Inh_ExpressionListList :: Catalog,expectedTypes_Inh_ExpressionListList :: [Maybe Type],lib_Inh_ExpressionListList :: LocalBindings,overrideLib_Inh_ExpressionListList :: Maybe LocalBindings,overrideLibs_Inh_ExpressionListList :: [Maybe LocalBindings]}
data Syn_ExpressionListList  = Syn_ExpressionListList {annotatedTree_Syn_ExpressionListList :: ExpressionListList,originalTree_Syn_ExpressionListList :: ExpressionListList,uType_Syn_ExpressionListList :: [[Maybe Type]]}
wrap_ExpressionListList :: T_ExpressionListList  ->
                           Inh_ExpressionListList  ->
                           Syn_ExpressionListList 
wrap_ExpressionListList sem (Inh_ExpressionListList _lhsIcat _lhsIexpectedTypes _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedTypes _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_ExpressionListList _lhsOannotatedTree _lhsOoriginalTree _lhsOuType ))
sem_ExpressionListList_Cons :: T_ExpressionList  ->
                               T_ExpressionListList  ->
                               T_ExpressionListList 
sem_ExpressionListList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOuType :: ([[Maybe Type]])
              _hdOexpectedTypes :: ([Maybe Type])
              _tlOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: ExpressionListList
              _lhsOoriginalTree :: ExpressionListList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: ExpressionList
              _hdIoriginalTree :: ExpressionList
              _hdItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _hdIuType :: ([Maybe Type])
              _tlIannotatedTree :: ExpressionListList
              _tlIoriginalTree :: ExpressionListList
              _tlIuType :: ([[Maybe Type]])
              -- "./TypeChecking/Expressions.ag"(line 93, column 12)
              _lhsOuType =
                  {-# LINE 93 "./TypeChecking/Expressions.ag" #-}
                  _hdIuType : _tlIuType
                  {-# LINE 5437 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 554, column 12)
              _hdOexpectedTypes =
                  {-# LINE 554 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 5442 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 555, column 12)
              _tlOexpectedTypes =
                  {-# LINE 555 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 5447 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5452 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 5457 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5462 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5467 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5472 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5477 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 5482 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 5487 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5492 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5497 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 5502 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 5507 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIoriginalTree,_hdItbUTypes,_hdIuType) =
                  (hd_ _hdOcat _hdOexpectedTypes _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIuType) =
                  (tl_ _tlOcat _tlOexpectedTypes _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOuType)))
sem_ExpressionListList_Nil :: T_ExpressionListList 
sem_ExpressionListList_Nil  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOuType :: ([[Maybe Type]])
              _lhsOannotatedTree :: ExpressionListList
              _lhsOoriginalTree :: ExpressionListList
              -- "./TypeChecking/Expressions.ag"(line 94, column 11)
              _lhsOuType =
                  {-# LINE 94 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 5527 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5532 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5537 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5542 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5547 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOuType)))
-- ExpressionListStatementListPair -----------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
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
type T_ExpressionListStatementListPair  = Catalog ->
                                          LocalBindings ->
                                          (Maybe LocalBindings) ->
                                          ([Maybe LocalBindings]) ->
                                          ( ExpressionListStatementListPair,ExpressionListStatementListPair)
data Inh_ExpressionListStatementListPair  = Inh_ExpressionListStatementListPair {cat_Inh_ExpressionListStatementListPair :: Catalog,lib_Inh_ExpressionListStatementListPair :: LocalBindings,overrideLib_Inh_ExpressionListStatementListPair :: Maybe LocalBindings,overrideLibs_Inh_ExpressionListStatementListPair :: [Maybe LocalBindings]}
data Syn_ExpressionListStatementListPair  = Syn_ExpressionListStatementListPair {annotatedTree_Syn_ExpressionListStatementListPair :: ExpressionListStatementListPair,originalTree_Syn_ExpressionListStatementListPair :: ExpressionListStatementListPair}
wrap_ExpressionListStatementListPair :: T_ExpressionListStatementListPair  ->
                                        Inh_ExpressionListStatementListPair  ->
                                        Syn_ExpressionListStatementListPair 
wrap_ExpressionListStatementListPair sem (Inh_ExpressionListStatementListPair _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_ExpressionListStatementListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionListStatementListPair_Tuple :: T_ExpressionList  ->
                                             T_StatementList  ->
                                             T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _x1OexpectedTypes :: ([Maybe Type])
              _x2OcatUpdates :: ([CatalogUpdate])
              _x2OlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: ExpressionListStatementListPair
              _lhsOoriginalTree :: ExpressionListStatementListPair
              _x1Ocat :: Catalog
              _x1Olib :: LocalBindings
              _x1OoverrideLib :: (Maybe LocalBindings)
              _x1OoverrideLibs :: ([Maybe LocalBindings])
              _x2Ocat :: Catalog
              _x2Olib :: LocalBindings
              _x2OoverrideLib :: (Maybe LocalBindings)
              _x2OoverrideLibs :: ([Maybe LocalBindings])
              _x1IannotatedTree :: ExpressionList
              _x1IoriginalTree :: ExpressionList
              _x1ItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _x1IuType :: ([Maybe Type])
              _x2IannotatedTree :: StatementList
              _x2IoriginalTree :: StatementList
              _x2IproducedCat :: Catalog
              _x2IproducedLib :: LocalBindings
              -- "./TypeChecking/Expressions.ag"(line 531, column 13)
              _x1OexpectedTypes =
                  {-# LINE 531 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 5622 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 121, column 9)
              _x2OcatUpdates =
                  {-# LINE 121 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 5627 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 122, column 9)
              _x2OlibUpdates =
                  {-# LINE 122 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 5632 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 5637 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 5642 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5647 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5652 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5657 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5662 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 5667 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 5672 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5677 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5682 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 5687 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 5692 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1IoriginalTree,_x1ItbUTypes,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedTypes _x1Olib _x1OoverrideLib _x1OoverrideLibs )
              ( _x2IannotatedTree,_x2IoriginalTree,_x2IproducedCat,_x2IproducedLib) =
                  (x2_ _x2Ocat _x2OcatUpdates _x2Olib _x2OlibUpdates _x2OoverrideLib _x2OoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ExpressionListStatementListPairList -------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
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
type T_ExpressionListStatementListPairList  = Catalog ->
                                              LocalBindings ->
                                              (Maybe LocalBindings) ->
                                              ([Maybe LocalBindings]) ->
                                              ( ExpressionListStatementListPairList,ExpressionListStatementListPairList)
data Inh_ExpressionListStatementListPairList  = Inh_ExpressionListStatementListPairList {cat_Inh_ExpressionListStatementListPairList :: Catalog,lib_Inh_ExpressionListStatementListPairList :: LocalBindings,overrideLib_Inh_ExpressionListStatementListPairList :: Maybe LocalBindings,overrideLibs_Inh_ExpressionListStatementListPairList :: [Maybe LocalBindings]}
data Syn_ExpressionListStatementListPairList  = Syn_ExpressionListStatementListPairList {annotatedTree_Syn_ExpressionListStatementListPairList :: ExpressionListStatementListPairList,originalTree_Syn_ExpressionListStatementListPairList :: ExpressionListStatementListPairList}
wrap_ExpressionListStatementListPairList :: T_ExpressionListStatementListPairList  ->
                                            Inh_ExpressionListStatementListPairList  ->
                                            Syn_ExpressionListStatementListPairList 
wrap_ExpressionListStatementListPairList sem (Inh_ExpressionListStatementListPairList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_ExpressionListStatementListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionListStatementListPairList_Cons :: T_ExpressionListStatementListPair  ->
                                                T_ExpressionListStatementListPairList  ->
                                                T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: ExpressionListStatementListPairList
              _lhsOoriginalTree :: ExpressionListStatementListPairList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: ExpressionListStatementListPair
              _hdIoriginalTree :: ExpressionListStatementListPair
              _tlIannotatedTree :: ExpressionListStatementListPairList
              _tlIoriginalTree :: ExpressionListStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 5768 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 5773 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5778 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5783 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5788 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5793 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 5798 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 5803 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5808 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5813 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 5818 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 5823 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ExpressionListStatementListPairList_Nil :: T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: ExpressionListStatementListPairList
              _lhsOoriginalTree :: ExpressionListStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5841 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 5846 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5851 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5856 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ExpressionRoot ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
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
type T_ExpressionRoot  = Catalog ->
                         LocalBindings ->
                         ( ExpressionRoot,ExpressionRoot)
data Inh_ExpressionRoot  = Inh_ExpressionRoot {cat_Inh_ExpressionRoot :: Catalog,lib_Inh_ExpressionRoot :: LocalBindings}
data Syn_ExpressionRoot  = Syn_ExpressionRoot {annotatedTree_Syn_ExpressionRoot :: ExpressionRoot,originalTree_Syn_ExpressionRoot :: ExpressionRoot}
wrap_ExpressionRoot :: T_ExpressionRoot  ->
                       Inh_ExpressionRoot  ->
                       Syn_ExpressionRoot 
wrap_ExpressionRoot sem (Inh_ExpressionRoot _lhsIcat _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib )
     in  (Syn_ExpressionRoot _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionRoot_ExpressionRoot :: T_Expression  ->
                                     T_ExpressionRoot 
sem_ExpressionRoot_ExpressionRoot expr_  =
    (\ _lhsIcat
       _lhsIlib ->
         (let _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: ExpressionRoot
              _lhsOoriginalTree :: ExpressionRoot
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 118, column 22)
              _exprOoverrideLib =
                  {-# LINE 118 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 5917 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 119, column 22)
              _exprOoverrideLibs =
                  {-# LINE 119 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 5922 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 510, column 22)
              _exprOexpectedType =
                  {-# LINE 510 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 5927 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ExpressionRoot _exprIannotatedTree
                  {-# LINE 5932 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ExpressionRoot _exprIoriginalTree
                  {-# LINE 5937 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 5942 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 5947 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 5952 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 5957 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ExpressionStatementListPair ---------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
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
type T_ExpressionStatementListPair  = Catalog ->
                                      LocalBindings ->
                                      (Maybe LocalBindings) ->
                                      ([Maybe LocalBindings]) ->
                                      ( ExpressionStatementListPair,ExpressionStatementListPair)
data Inh_ExpressionStatementListPair  = Inh_ExpressionStatementListPair {cat_Inh_ExpressionStatementListPair :: Catalog,lib_Inh_ExpressionStatementListPair :: LocalBindings,overrideLib_Inh_ExpressionStatementListPair :: Maybe LocalBindings,overrideLibs_Inh_ExpressionStatementListPair :: [Maybe LocalBindings]}
data Syn_ExpressionStatementListPair  = Syn_ExpressionStatementListPair {annotatedTree_Syn_ExpressionStatementListPair :: ExpressionStatementListPair,originalTree_Syn_ExpressionStatementListPair :: ExpressionStatementListPair}
wrap_ExpressionStatementListPair :: T_ExpressionStatementListPair  ->
                                    Inh_ExpressionStatementListPair  ->
                                    Syn_ExpressionStatementListPair 
wrap_ExpressionStatementListPair sem (Inh_ExpressionStatementListPair _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_ExpressionStatementListPair _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionStatementListPair_Tuple :: T_Expression  ->
                                         T_StatementList  ->
                                         T_ExpressionStatementListPair 
sem_ExpressionStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _x1OexpectedType :: (Maybe Type)
              _x2OcatUpdates :: ([CatalogUpdate])
              _x2OlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: ExpressionStatementListPair
              _lhsOoriginalTree :: ExpressionStatementListPair
              _x1Ocat :: Catalog
              _x1Olib :: LocalBindings
              _x1OoverrideLib :: (Maybe LocalBindings)
              _x1OoverrideLibs :: ([Maybe LocalBindings])
              _x2Ocat :: Catalog
              _x2Olib :: LocalBindings
              _x2OoverrideLib :: (Maybe LocalBindings)
              _x2OoverrideLibs :: ([Maybe LocalBindings])
              _x1IannotatedTree :: Expression
              _x1IntAnnotatedTree :: Expression
              _x1IntType :: ([(String,Type)])
              _x1IoriginalTree :: Expression
              _x1ItbAnnotatedTree :: Expression
              _x1ItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _x1IuType :: (Maybe Type)
              _x2IannotatedTree :: StatementList
              _x2IoriginalTree :: StatementList
              _x2IproducedCat :: Catalog
              _x2IproducedLib :: LocalBindings
              -- "./TypeChecking/Expressions.ag"(line 513, column 13)
              _x1OexpectedType =
                  {-# LINE 513 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 6037 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 125, column 9)
              _x2OcatUpdates =
                  {-# LINE 125 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 6042 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 126, column 9)
              _x2OlibUpdates =
                  {-# LINE 126 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 6047 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IannotatedTree,_x2IannotatedTree)
                  {-# LINE 6052 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (_x1IoriginalTree,_x2IoriginalTree)
                  {-# LINE 6057 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6062 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6067 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6072 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6077 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 6082 "AstInternal.hs" #-}
              -- copy rule (down)
              _x1OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 6087 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6092 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6097 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 6102 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 6107 "AstInternal.hs" #-}
              ( _x1IannotatedTree,_x1IntAnnotatedTree,_x1IntType,_x1IoriginalTree,_x1ItbAnnotatedTree,_x1ItbUType,_x1IuType) =
                  (x1_ _x1Ocat _x1OexpectedType _x1Olib _x1OoverrideLib _x1OoverrideLibs )
              ( _x2IannotatedTree,_x2IoriginalTree,_x2IproducedCat,_x2IproducedLib) =
                  (x2_ _x2Ocat _x2OcatUpdates _x2Olib _x2OlibUpdates _x2OoverrideLib _x2OoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ExpressionStatementListPairList -----------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
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
type T_ExpressionStatementListPairList  = Catalog ->
                                          LocalBindings ->
                                          (Maybe LocalBindings) ->
                                          ([Maybe LocalBindings]) ->
                                          ( ExpressionStatementListPairList,ExpressionStatementListPairList)
data Inh_ExpressionStatementListPairList  = Inh_ExpressionStatementListPairList {cat_Inh_ExpressionStatementListPairList :: Catalog,lib_Inh_ExpressionStatementListPairList :: LocalBindings,overrideLib_Inh_ExpressionStatementListPairList :: Maybe LocalBindings,overrideLibs_Inh_ExpressionStatementListPairList :: [Maybe LocalBindings]}
data Syn_ExpressionStatementListPairList  = Syn_ExpressionStatementListPairList {annotatedTree_Syn_ExpressionStatementListPairList :: ExpressionStatementListPairList,originalTree_Syn_ExpressionStatementListPairList :: ExpressionStatementListPairList}
wrap_ExpressionStatementListPairList :: T_ExpressionStatementListPairList  ->
                                        Inh_ExpressionStatementListPairList  ->
                                        Syn_ExpressionStatementListPairList 
wrap_ExpressionStatementListPairList sem (Inh_ExpressionStatementListPairList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_ExpressionStatementListPairList _lhsOannotatedTree _lhsOoriginalTree ))
sem_ExpressionStatementListPairList_Cons :: T_ExpressionStatementListPair  ->
                                            T_ExpressionStatementListPairList  ->
                                            T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: ExpressionStatementListPairList
              _lhsOoriginalTree :: ExpressionStatementListPairList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: ExpressionStatementListPair
              _hdIoriginalTree :: ExpressionStatementListPair
              _tlIannotatedTree :: ExpressionStatementListPairList
              _tlIoriginalTree :: ExpressionStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 6183 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 6188 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6193 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6198 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6203 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6208 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 6213 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 6218 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6223 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6228 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 6233 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 6238 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_ExpressionStatementListPairList_Nil :: T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: ExpressionStatementListPairList
              _lhsOoriginalTree :: ExpressionStatementListPairList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 6256 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 6261 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6266 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6271 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- FnBody ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
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
                 LocalBindings ->
                 (Maybe LocalBindings) ->
                 ([Maybe LocalBindings]) ->
                 ( FnBody,FnBody)
data Inh_FnBody  = Inh_FnBody {cat_Inh_FnBody :: Catalog,lib_Inh_FnBody :: LocalBindings,overrideLib_Inh_FnBody :: Maybe LocalBindings,overrideLibs_Inh_FnBody :: [Maybe LocalBindings]}
data Syn_FnBody  = Syn_FnBody {annotatedTree_Syn_FnBody :: FnBody,originalTree_Syn_FnBody :: FnBody}
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_FnBody _lhsOannotatedTree _lhsOoriginalTree ))
sem_FnBody_PlpgsqlFnBody :: Annotation ->
                            T_Statement  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody ann_ blk_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _blkOinProducedCat :: Catalog
              _lhsOannotatedTree :: FnBody
              _lhsOoriginalTree :: FnBody
              _blkOcat :: Catalog
              _blkOlib :: LocalBindings
              _blkOoverrideLib :: (Maybe LocalBindings)
              _blkOoverrideLibs :: ([Maybe LocalBindings])
              _blkIannotatedTree :: Statement
              _blkIcatUpdates :: ([CatalogUpdate])
              _blkIlibUpdates :: ([LocalBindingsUpdate])
              _blkIoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 104, column 9)
              _blkOinProducedCat =
                  {-# LINE 104 "./TypeChecking/Statements.ag" #-}
                  emptyCatalog
                  {-# LINE 6346 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _blkIannotatedTree
                  {-# LINE 6351 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  PlpgsqlFnBody ann_ _blkIoriginalTree
                  {-# LINE 6356 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6361 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6366 "AstInternal.hs" #-}
              -- copy rule (down)
              _blkOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6371 "AstInternal.hs" #-}
              -- copy rule (down)
              _blkOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6376 "AstInternal.hs" #-}
              -- copy rule (down)
              _blkOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 6381 "AstInternal.hs" #-}
              -- copy rule (down)
              _blkOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 6386 "AstInternal.hs" #-}
              ( _blkIannotatedTree,_blkIcatUpdates,_blkIlibUpdates,_blkIoriginalTree) =
                  (blk_ _blkOcat _blkOinProducedCat _blkOlib _blkOoverrideLib _blkOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_FnBody_SqlFnBody :: Annotation ->
                        T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody ann_ sts_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: FnBody
              _lhsOoriginalTree :: FnBody
              _stsOcat :: Catalog
              _stsOlib :: LocalBindings
              _stsOoverrideLib :: (Maybe LocalBindings)
              _stsOoverrideLibs :: ([Maybe LocalBindings])
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 129, column 9)
              _stsOcatUpdates =
                  {-# LINE 129 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 6414 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 130, column 9)
              _stsOlibUpdates =
                  {-# LINE 130 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 6419 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIannotatedTree
                  {-# LINE 6424 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SqlFnBody ann_ _stsIoriginalTree
                  {-# LINE 6429 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6434 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6439 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6444 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6449 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 6454 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 6459 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOlib _stsOlibUpdates _stsOoverrideLib _stsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- InList ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         listType             : Either [TypeError] Type
         originalTree         : SELF 
   alternatives:
      alternative InList:
         child ann            : {Annotation}
         child exprs          : ExpressionList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative InSelect:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
type T_InList  = Catalog ->
                 LocalBindings ->
                 (Maybe LocalBindings) ->
                 ([Maybe LocalBindings]) ->
                 ( InList,(Either [TypeError] Type),InList)
data Inh_InList  = Inh_InList {cat_Inh_InList :: Catalog,lib_Inh_InList :: LocalBindings,overrideLib_Inh_InList :: Maybe LocalBindings,overrideLibs_Inh_InList :: [Maybe LocalBindings]}
data Syn_InList  = Syn_InList {annotatedTree_Syn_InList :: InList,listType_Syn_InList :: Either [TypeError] Type,originalTree_Syn_InList :: InList}
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_InList _lhsOannotatedTree _lhsOlistType _lhsOoriginalTree ))
sem_InList_InList :: Annotation ->
                     T_ExpressionList  ->
                     T_InList 
sem_InList_InList ann_ exprs_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOlistType :: (Either [TypeError] Type)
              _exprsOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: InList
              _lhsOoriginalTree :: InList
              _exprsOcat :: Catalog
              _exprsOlib :: LocalBindings
              _exprsOoverrideLib :: (Maybe LocalBindings)
              _exprsOoverrideLibs :: ([Maybe LocalBindings])
              _exprsIannotatedTree :: ExpressionList
              _exprsIoriginalTree :: ExpressionList
              _exprsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _exprsIuType :: ([Maybe Type])
              -- "./TypeChecking/Expressions.ag"(line 423, column 9)
              _lhsOlistType =
                  {-# LINE 423 "./TypeChecking/Expressions.ag" #-}
                  mapM lmt _exprsIuType >>= resolveResultSetType _lhsIcat
                  {-# LINE 6538 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 534, column 14)
              _exprsOexpectedTypes =
                  {-# LINE 534 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 6543 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  InList ann_ _exprsIannotatedTree
                  {-# LINE 6548 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  InList ann_ _exprsIoriginalTree
                  {-# LINE 6553 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6558 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6563 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6568 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6573 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 6578 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 6583 "AstInternal.hs" #-}
              ( _exprsIannotatedTree,_exprsIoriginalTree,_exprsItbUTypes,_exprsIuType) =
                  (exprs_ _exprsOcat _exprsOexpectedTypes _exprsOlib _exprsOoverrideLib _exprsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree)))
sem_InList_InSelect :: Annotation ->
                       T_SelectExpression  ->
                       T_InList 
sem_InList_InSelect ann_ sel_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOlistType :: (Either [TypeError] Type)
              _selOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: InList
              _lhsOoriginalTree :: InList
              _selOcat :: Catalog
              _selOlib :: LocalBindings
              _selOoverrideLib :: (Maybe LocalBindings)
              _selOoverrideLibs :: ([Maybe LocalBindings])
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: SelectExpression
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Expressions.ag"(line 425, column 9)
              _lhsOlistType =
                  {-# LINE 425 "./TypeChecking/Expressions.ag" #-}
                  do
                  st <- lmt (map snd <$> _selIuType)
                  case length st of
                            0 -> Left [InternalError
                                       "got subquery with no columns? in inselect"]
                            1 -> Right $ head st
                            _ -> Right $ AnonymousRecordType st
                  {-# LINE 6617 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 560, column 16)
              _selOexpectedTypes =
                  {-# LINE 560 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 6622 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  InSelect ann_ _selIannotatedTree
                  {-# LINE 6627 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  InSelect ann_ _selIoriginalTree
                  {-# LINE 6632 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6637 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6642 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6647 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6652 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 6657 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 6662 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOlib _selOoverrideLib _selOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree)))
-- JoinExpression ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative JoinOn:
         child ann            : {Annotation}
         child expr           : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative JoinUsing:
         child ann            : {Annotation}
         child x              : {[String]}
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data JoinExpression  = JoinOn (Annotation) (Expression) 
                     | JoinUsing (Annotation) ([String]) 
                     deriving ( Data,Eq,Show,Typeable)
-- cata
sem_JoinExpression :: JoinExpression  ->
                      T_JoinExpression 
sem_JoinExpression (JoinOn _ann _expr )  =
    (sem_JoinExpression_JoinOn _ann (sem_Expression _expr ) )
sem_JoinExpression (JoinUsing _ann _x )  =
    (sem_JoinExpression_JoinUsing _ann _x )
-- semantic domain
type T_JoinExpression  = Catalog ->
                         LocalBindings ->
                         (Maybe LocalBindings) ->
                         ([Maybe LocalBindings]) ->
                         ( JoinExpression,JoinExpression)
data Inh_JoinExpression  = Inh_JoinExpression {cat_Inh_JoinExpression :: Catalog,lib_Inh_JoinExpression :: LocalBindings,overrideLib_Inh_JoinExpression :: Maybe LocalBindings,overrideLibs_Inh_JoinExpression :: [Maybe LocalBindings]}
data Syn_JoinExpression  = Syn_JoinExpression {annotatedTree_Syn_JoinExpression :: JoinExpression,originalTree_Syn_JoinExpression :: JoinExpression}
wrap_JoinExpression :: T_JoinExpression  ->
                       Inh_JoinExpression  ->
                       Syn_JoinExpression 
wrap_JoinExpression sem (Inh_JoinExpression _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_JoinExpression _lhsOannotatedTree _lhsOoriginalTree ))
sem_JoinExpression_JoinOn :: Annotation ->
                             T_Expression  ->
                             T_JoinExpression 
sem_JoinExpression_JoinOn ann_ expr_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: JoinExpression
              _lhsOoriginalTree :: JoinExpression
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 481, column 14)
              _exprOexpectedType =
                  {-# LINE 481 "./TypeChecking/Expressions.ag" #-}
                  Just typeBool
                  {-# LINE 6742 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  JoinOn ann_ _exprIannotatedTree
                  {-# LINE 6747 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  JoinOn ann_ _exprIoriginalTree
                  {-# LINE 6752 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6757 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6762 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6767 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6772 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 6777 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 6782 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_JoinExpression_JoinUsing :: Annotation ->
                                ([String]) ->
                                T_JoinExpression 
sem_JoinExpression_JoinUsing ann_ x_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: JoinExpression
              _lhsOoriginalTree :: JoinExpression
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  JoinUsing ann_ x_
                  {-# LINE 6800 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  JoinUsing ann_ x_
                  {-# LINE 6805 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6810 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6815 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- MaybeBoolExpression -----------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
type T_MaybeBoolExpression  = Catalog ->
                              LocalBindings ->
                              (Maybe LocalBindings) ->
                              ([Maybe LocalBindings]) ->
                              ( MaybeBoolExpression,MaybeBoolExpression)
data Inh_MaybeBoolExpression  = Inh_MaybeBoolExpression {cat_Inh_MaybeBoolExpression :: Catalog,lib_Inh_MaybeBoolExpression :: LocalBindings,overrideLib_Inh_MaybeBoolExpression :: Maybe LocalBindings,overrideLibs_Inh_MaybeBoolExpression :: [Maybe LocalBindings]}
data Syn_MaybeBoolExpression  = Syn_MaybeBoolExpression {annotatedTree_Syn_MaybeBoolExpression :: MaybeBoolExpression,originalTree_Syn_MaybeBoolExpression :: MaybeBoolExpression}
wrap_MaybeBoolExpression :: T_MaybeBoolExpression  ->
                            Inh_MaybeBoolExpression  ->
                            Syn_MaybeBoolExpression 
wrap_MaybeBoolExpression sem (Inh_MaybeBoolExpression _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_MaybeBoolExpression _lhsOannotatedTree _lhsOoriginalTree ))
sem_MaybeBoolExpression_Just :: T_Expression  ->
                                T_MaybeBoolExpression 
sem_MaybeBoolExpression_Just just_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: MaybeBoolExpression
              _justOexpectedType :: (Maybe Type)
              _lhsOoriginalTree :: MaybeBoolExpression
              _justOcat :: Catalog
              _justOlib :: LocalBindings
              _justOoverrideLib :: (Maybe LocalBindings)
              _justOoverrideLibs :: ([Maybe LocalBindings])
              _justIannotatedTree :: Expression
              _justIntAnnotatedTree :: Expression
              _justIntType :: ([(String,Type)])
              _justIoriginalTree :: Expression
              _justItbAnnotatedTree :: Expression
              _justItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _justIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 107, column 9)
              _lhsOannotatedTree =
                  {-# LINE 107 "./TypeChecking/Expressions.ag" #-}
                  let t = _justIuType
                  in if t `elem` [Nothing,Just typeBool]
                     then Just _justIannotatedTree
                     else Just $ addTypeErrors [ExpressionMustBeBool] _justIannotatedTree
                  {-# LINE 6890 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 484, column 12)
              _justOexpectedType =
                  {-# LINE 484 "./TypeChecking/Expressions.ag" #-}
                  Just typeBool
                  {-# LINE 6895 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 6900 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 6905 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6910 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 6915 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 6920 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 6925 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 6930 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIntAnnotatedTree,_justIntType,_justIoriginalTree,_justItbAnnotatedTree,_justItbUType,_justIuType) =
                  (just_ _justOcat _justOexpectedType _justOlib _justOoverrideLib _justOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_MaybeBoolExpression_Nothing :: T_MaybeBoolExpression 
sem_MaybeBoolExpression_Nothing  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: MaybeBoolExpression
              _lhsOoriginalTree :: MaybeBoolExpression
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 6946 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 6951 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 6956 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 6961 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- MaybeExpression ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         uType                : Maybe Type
   alternatives:
      alternative Just:
         child just           : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
type T_MaybeExpression  = Catalog ->
                          LocalBindings ->
                          (Maybe LocalBindings) ->
                          ([Maybe LocalBindings]) ->
                          ( MaybeExpression,MaybeExpression,(Maybe Type))
data Inh_MaybeExpression  = Inh_MaybeExpression {cat_Inh_MaybeExpression :: Catalog,lib_Inh_MaybeExpression :: LocalBindings,overrideLib_Inh_MaybeExpression :: Maybe LocalBindings,overrideLibs_Inh_MaybeExpression :: [Maybe LocalBindings]}
data Syn_MaybeExpression  = Syn_MaybeExpression {annotatedTree_Syn_MaybeExpression :: MaybeExpression,originalTree_Syn_MaybeExpression :: MaybeExpression,uType_Syn_MaybeExpression :: Maybe Type}
wrap_MaybeExpression :: T_MaybeExpression  ->
                        Inh_MaybeExpression  ->
                        Syn_MaybeExpression 
wrap_MaybeExpression sem (Inh_MaybeExpression _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_MaybeExpression _lhsOannotatedTree _lhsOoriginalTree _lhsOuType ))
sem_MaybeExpression_Just :: T_Expression  ->
                            T_MaybeExpression 
sem_MaybeExpression_Just just_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOuType :: (Maybe Type)
              _justOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: MaybeExpression
              _lhsOoriginalTree :: MaybeExpression
              _justOcat :: Catalog
              _justOlib :: LocalBindings
              _justOoverrideLib :: (Maybe LocalBindings)
              _justOoverrideLibs :: ([Maybe LocalBindings])
              _justIannotatedTree :: Expression
              _justIntAnnotatedTree :: Expression
              _justIntType :: ([(String,Type)])
              _justIoriginalTree :: Expression
              _justItbAnnotatedTree :: Expression
              _justItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _justIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 100, column 12)
              _lhsOuType =
                  {-# LINE 100 "./TypeChecking/Expressions.ag" #-}
                  _justIuType
                  {-# LINE 7035 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 487, column 12)
              _justOexpectedType =
                  {-# LINE 487 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 7040 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 7045 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 7050 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7055 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7060 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7065 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7070 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 7075 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 7080 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIntAnnotatedTree,_justIntType,_justIoriginalTree,_justItbAnnotatedTree,_justItbUType,_justIuType) =
                  (just_ _justOcat _justOexpectedType _justOlib _justOoverrideLib _justOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOuType)))
sem_MaybeExpression_Nothing :: T_MaybeExpression 
sem_MaybeExpression_Nothing  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOuType :: (Maybe Type)
              _lhsOannotatedTree :: MaybeExpression
              _lhsOoriginalTree :: MaybeExpression
              -- "./TypeChecking/Expressions.ag"(line 101, column 15)
              _lhsOuType =
                  {-# LINE 101 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 7097 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 7102 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 7107 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7112 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7117 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOuType)))
-- MaybeSelectList ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         listType             : [(String,Type)]
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
type T_MaybeSelectList  = Catalog ->
                          LocalBindings ->
                          (Maybe LocalBindings) ->
                          ([Maybe LocalBindings]) ->
                          ( MaybeSelectList,([(String,Type)]),MaybeSelectList)
data Inh_MaybeSelectList  = Inh_MaybeSelectList {cat_Inh_MaybeSelectList :: Catalog,lib_Inh_MaybeSelectList :: LocalBindings,overrideLib_Inh_MaybeSelectList :: Maybe LocalBindings,overrideLibs_Inh_MaybeSelectList :: [Maybe LocalBindings]}
data Syn_MaybeSelectList  = Syn_MaybeSelectList {annotatedTree_Syn_MaybeSelectList :: MaybeSelectList,listType_Syn_MaybeSelectList :: [(String,Type)],originalTree_Syn_MaybeSelectList :: MaybeSelectList}
wrap_MaybeSelectList :: T_MaybeSelectList  ->
                        Inh_MaybeSelectList  ->
                        Syn_MaybeSelectList 
wrap_MaybeSelectList sem (Inh_MaybeSelectList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_MaybeSelectList _lhsOannotatedTree _lhsOlistType _lhsOoriginalTree ))
sem_MaybeSelectList_Just :: T_SelectList  ->
                            T_MaybeSelectList 
sem_MaybeSelectList_Just just_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOlistType :: ([(String,Type)])
              _lhsOannotatedTree :: MaybeSelectList
              _lhsOoriginalTree :: MaybeSelectList
              _justOcat :: Catalog
              _justOlib :: LocalBindings
              _justOoverrideLib :: (Maybe LocalBindings)
              _justOoverrideLibs :: ([Maybe LocalBindings])
              _justIannotatedTree :: SelectList
              _justIlibUpdates :: ([LocalBindingsUpdate])
              _justIlistType :: ([(String,Type)])
              _justIoriginalTree :: SelectList
              -- "./TypeChecking/SelectLists.ag"(line 39, column 12)
              _lhsOlistType =
                  {-# LINE 39 "./TypeChecking/SelectLists.ag" #-}
                  _justIlistType
                  {-# LINE 7187 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 7192 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 7197 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7202 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7207 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7212 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7217 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 7222 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 7227 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIlibUpdates,_justIlistType,_justIoriginalTree) =
                  (just_ _justOcat _justOlib _justOoverrideLib _justOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree)))
sem_MaybeSelectList_Nothing :: T_MaybeSelectList 
sem_MaybeSelectList_Nothing  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOlistType :: ([(String,Type)])
              _lhsOannotatedTree :: MaybeSelectList
              _lhsOoriginalTree :: MaybeSelectList
              -- "./TypeChecking/SelectLists.ag"(line 40, column 15)
              _lhsOlistType =
                  {-# LINE 40 "./TypeChecking/SelectLists.ag" #-}
                  []
                  {-# LINE 7244 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 7249 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 7254 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7259 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7264 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree)))
-- OnExpr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
   alternatives:
      alternative Just:
         child just           : JoinExpression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nothing:
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
type T_OnExpr  = Catalog ->
                 LocalBindings ->
                 (Maybe LocalBindings) ->
                 ([Maybe LocalBindings]) ->
                 ( OnExpr,OnExpr)
data Inh_OnExpr  = Inh_OnExpr {cat_Inh_OnExpr :: Catalog,lib_Inh_OnExpr :: LocalBindings,overrideLib_Inh_OnExpr :: Maybe LocalBindings,overrideLibs_Inh_OnExpr :: [Maybe LocalBindings]}
data Syn_OnExpr  = Syn_OnExpr {annotatedTree_Syn_OnExpr :: OnExpr,originalTree_Syn_OnExpr :: OnExpr}
wrap_OnExpr :: T_OnExpr  ->
               Inh_OnExpr  ->
               Syn_OnExpr 
wrap_OnExpr sem (Inh_OnExpr _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_OnExpr _lhsOannotatedTree _lhsOoriginalTree ))
sem_OnExpr_Just :: T_JoinExpression  ->
                   T_OnExpr 
sem_OnExpr_Just just_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: OnExpr
              _lhsOoriginalTree :: OnExpr
              _justOcat :: Catalog
              _justOlib :: LocalBindings
              _justOoverrideLib :: (Maybe LocalBindings)
              _justOoverrideLibs :: ([Maybe LocalBindings])
              _justIannotatedTree :: JoinExpression
              _justIoriginalTree :: JoinExpression
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIannotatedTree
                  {-# LINE 7330 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Just _justIoriginalTree
                  {-# LINE 7335 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7340 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7345 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7350 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7355 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 7360 "AstInternal.hs" #-}
              -- copy rule (down)
              _justOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 7365 "AstInternal.hs" #-}
              ( _justIannotatedTree,_justIoriginalTree) =
                  (just_ _justOcat _justOlib _justOoverrideLib _justOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_OnExpr_Nothing :: T_OnExpr 
sem_OnExpr_Nothing  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: OnExpr
              _lhsOoriginalTree :: OnExpr
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 7381 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Nothing
                  {-# LINE 7386 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7391 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7396 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- ParamDef ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
         pos                  : Int
      synthesized attributes:
         annotatedTree        : SELF 
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
type T_ParamDef  = Catalog ->
                   LocalBindings ->
                   (Maybe LocalBindings) ->
                   ([Maybe LocalBindings]) ->
                   Int ->
                   ( ParamDef,(Maybe Type),ParamDef,ParamName)
data Inh_ParamDef  = Inh_ParamDef {cat_Inh_ParamDef :: Catalog,lib_Inh_ParamDef :: LocalBindings,overrideLib_Inh_ParamDef :: Maybe LocalBindings,overrideLibs_Inh_ParamDef :: [Maybe LocalBindings],pos_Inh_ParamDef :: Int}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef,namedType_Syn_ParamDef :: Maybe Type,originalTree_Syn_ParamDef :: ParamDef,paramName_Syn_ParamDef :: ParamName}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs _lhsIpos )  =
    (let ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree,_lhsOparamName) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs _lhsIpos )
     in  (Syn_ParamDef _lhsOannotatedTree _lhsOnamedType _lhsOoriginalTree _lhsOparamName ))
sem_ParamDef_ParamDef :: Annotation ->
                         String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef ann_ name_ typ_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs
       _lhsIpos ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOparamName :: ParamName
              _lhsOannotatedTree :: ParamDef
              _lhsOoriginalTree :: ParamDef
              _typOcat :: Catalog
              _typOlib :: LocalBindings
              _typOoverrideLib :: (Maybe LocalBindings)
              _typOoverrideLibs :: ([Maybe LocalBindings])
              _typIannotatedTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/CreateFunction.ag"(line 45, column 9)
              _lhsOnamedType =
                  {-# LINE 45 "./TypeChecking/CreateFunction.ag" #-}
                  _typInamedType
                  {-# LINE 7478 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 47, column 9)
              _lhsOparamName =
                  {-# LINE 47 "./TypeChecking/CreateFunction.ag" #-}
                  NamedParam _lhsIpos name_
                  {-# LINE 7483 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIannotatedTree
                  {-# LINE 7488 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDef ann_ name_ _typIoriginalTree
                  {-# LINE 7493 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7498 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7503 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7508 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7513 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 7518 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 7523 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOlib _typOoverrideLib _typOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree,_lhsOparamName)))
sem_ParamDef_ParamDefTp :: Annotation ->
                           T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp ann_ typ_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs
       _lhsIpos ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOparamName :: ParamName
              _lhsOannotatedTree :: ParamDef
              _lhsOoriginalTree :: ParamDef
              _typOcat :: Catalog
              _typOlib :: LocalBindings
              _typOoverrideLib :: (Maybe LocalBindings)
              _typOoverrideLibs :: ([Maybe LocalBindings])
              _typIannotatedTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/CreateFunction.ag"(line 45, column 9)
              _lhsOnamedType =
                  {-# LINE 45 "./TypeChecking/CreateFunction.ag" #-}
                  _typInamedType
                  {-# LINE 7551 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 49, column 9)
              _lhsOparamName =
                  {-# LINE 49 "./TypeChecking/CreateFunction.ag" #-}
                  UnnamedParam _lhsIpos
                  {-# LINE 7556 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIannotatedTree
                  {-# LINE 7561 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ParamDefTp ann_ _typIoriginalTree
                  {-# LINE 7566 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7571 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7576 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7581 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7586 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 7591 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 7596 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOlib _typOoverrideLib _typOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree,_lhsOparamName)))
-- ParamDefList ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
         pos                  : Int
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         params               : [(ParamName, Maybe Type)]
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
type T_ParamDefList  = Catalog ->
                       LocalBindings ->
                       (Maybe LocalBindings) ->
                       ([Maybe LocalBindings]) ->
                       Int ->
                       ( ParamDefList,ParamDefList,([(ParamName, Maybe Type)]))
data Inh_ParamDefList  = Inh_ParamDefList {cat_Inh_ParamDefList :: Catalog,lib_Inh_ParamDefList :: LocalBindings,overrideLib_Inh_ParamDefList :: Maybe LocalBindings,overrideLibs_Inh_ParamDefList :: [Maybe LocalBindings],pos_Inh_ParamDefList :: Int}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList,originalTree_Syn_ParamDefList :: ParamDefList,params_Syn_ParamDefList :: [(ParamName, Maybe Type)]}
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs _lhsIpos )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOparams) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs _lhsIpos )
     in  (Syn_ParamDefList _lhsOannotatedTree _lhsOoriginalTree _lhsOparams ))
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs
       _lhsIpos ->
         (let _lhsOparams :: ([(ParamName, Maybe Type)])
              _hdOpos :: Int
              _tlOpos :: Int
              _lhsOannotatedTree :: ParamDefList
              _lhsOoriginalTree :: ParamDefList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: ParamDef
              _hdInamedType :: (Maybe Type)
              _hdIoriginalTree :: ParamDef
              _hdIparamName :: ParamName
              _tlIannotatedTree :: ParamDefList
              _tlIoriginalTree :: ParamDefList
              _tlIparams :: ([(ParamName, Maybe Type)])
              -- "./TypeChecking/CreateFunction.ag"(line 53, column 13)
              _lhsOparams =
                  {-# LINE 53 "./TypeChecking/CreateFunction.ag" #-}
                  ((_hdIparamName, _hdInamedType) : _tlIparams)
                  {-# LINE 7680 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 54, column 13)
              _hdOpos =
                  {-# LINE 54 "./TypeChecking/CreateFunction.ag" #-}
                  _lhsIpos
                  {-# LINE 7685 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 55, column 13)
              _tlOpos =
                  {-# LINE 55 "./TypeChecking/CreateFunction.ag" #-}
                  _lhsIpos + 1
                  {-# LINE 7690 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 7695 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 7700 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7705 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7710 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7715 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7720 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 7725 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 7730 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7735 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7740 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 7745 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 7750 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdInamedType,_hdIoriginalTree,_hdIparamName) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs _hdOpos )
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIparams) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs _tlOpos )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOparams)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs
       _lhsIpos ->
         (let _lhsOparams :: ([(ParamName, Maybe Type)])
              _lhsOannotatedTree :: ParamDefList
              _lhsOoriginalTree :: ParamDefList
              -- "./TypeChecking/CreateFunction.ag"(line 52, column 12)
              _lhsOparams =
                  {-# LINE 52 "./TypeChecking/CreateFunction.ag" #-}
                  []
                  {-# LINE 7770 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 7775 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 7780 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7785 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7790 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOparams)))
-- Root --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         producedCat          : Catalog
         producedLib          : LocalBindings
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
type T_Root  = Catalog ->
               LocalBindings ->
               ( Root,Root,Catalog,LocalBindings)
data Inh_Root  = Inh_Root {cat_Inh_Root :: Catalog,lib_Inh_Root :: LocalBindings}
data Syn_Root  = Syn_Root {annotatedTree_Syn_Root :: Root,originalTree_Syn_Root :: Root,producedCat_Syn_Root :: Catalog,producedLib_Syn_Root :: LocalBindings}
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root _lhsIcat _lhsIlib )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib) =
             (sem _lhsIcat _lhsIlib )
     in  (Syn_Root _lhsOannotatedTree _lhsOoriginalTree _lhsOproducedCat _lhsOproducedLib ))
sem_Root_Root :: T_StatementList  ->
                 T_Root 
sem_Root_Root statements_  =
    (\ _lhsIcat
       _lhsIlib ->
         (let _statementsOoverrideLib :: (Maybe LocalBindings)
              _statementsOoverrideLibs :: ([Maybe LocalBindings])
              _statementsOcatUpdates :: ([CatalogUpdate])
              _statementsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Root
              _lhsOoriginalTree :: Root
              _lhsOproducedCat :: Catalog
              _lhsOproducedLib :: LocalBindings
              _statementsOcat :: Catalog
              _statementsOlib :: LocalBindings
              _statementsIannotatedTree :: StatementList
              _statementsIoriginalTree :: StatementList
              _statementsIproducedCat :: Catalog
              _statementsIproducedLib :: LocalBindings
              -- "./TypeChecking/Expressions.ag"(line 122, column 12)
              _statementsOoverrideLib =
                  {-# LINE 122 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 7853 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 123, column 12)
              _statementsOoverrideLibs =
                  {-# LINE 123 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 7858 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 107, column 12)
              _statementsOcatUpdates =
                  {-# LINE 107 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 7863 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 108, column 12)
              _statementsOlibUpdates =
                  {-# LINE 108 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 7868 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Root _statementsIannotatedTree
                  {-# LINE 7873 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Root _statementsIoriginalTree
                  {-# LINE 7878 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 7883 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 7888 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOproducedCat =
                  {-# LINE 27 "./TypeChecking/Statements.ag" #-}
                  _statementsIproducedCat
                  {-# LINE 7893 "AstInternal.hs" #-}
              -- copy rule (up)
              _lhsOproducedLib =
                  {-# LINE 28 "./TypeChecking/Statements.ag" #-}
                  _statementsIproducedLib
                  {-# LINE 7898 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 7903 "AstInternal.hs" #-}
              -- copy rule (down)
              _statementsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 7908 "AstInternal.hs" #-}
              ( _statementsIannotatedTree,_statementsIoriginalTree,_statementsIproducedCat,_statementsIproducedLib) =
                  (statements_ _statementsOcat _statementsOcatUpdates _statementsOlib _statementsOlibUpdates _statementsOoverrideLib _statementsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib)))
-- RowConstraint -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
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
         child expr           : Expression 
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
         child table          : {String}
         child att            : {Maybe String}
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
                    | RowCheckConstraint (Annotation) (String) (Expression) 
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
    (sem_RowConstraint_RowCheckConstraint _ann _name (sem_Expression _expr ) )
sem_RowConstraint (RowPrimaryKeyConstraint _ann _name )  =
    (sem_RowConstraint_RowPrimaryKeyConstraint _ann _name )
sem_RowConstraint (RowReferenceConstraint _ann _name _table _att _onUpdate _onDelete )  =
    (sem_RowConstraint_RowReferenceConstraint _ann _name _table _att _onUpdate _onDelete )
sem_RowConstraint (RowUniqueConstraint _ann _name )  =
    (sem_RowConstraint_RowUniqueConstraint _ann _name )
-- semantic domain
type T_RowConstraint  = Catalog ->
                        LocalBindings ->
                        (Maybe LocalBindings) ->
                        ([Maybe LocalBindings]) ->
                        ( RowConstraint,RowConstraint)
data Inh_RowConstraint  = Inh_RowConstraint {cat_Inh_RowConstraint :: Catalog,lib_Inh_RowConstraint :: LocalBindings,overrideLib_Inh_RowConstraint :: Maybe LocalBindings,overrideLibs_Inh_RowConstraint :: [Maybe LocalBindings]}
data Syn_RowConstraint  = Syn_RowConstraint {annotatedTree_Syn_RowConstraint :: RowConstraint,originalTree_Syn_RowConstraint :: RowConstraint}
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_RowConstraint _lhsOannotatedTree _lhsOoriginalTree ))
sem_RowConstraint_NotNullConstraint :: Annotation ->
                                       String ->
                                       T_RowConstraint 
sem_RowConstraint_NotNullConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  NotNullConstraint ann_ name_
                  {-# LINE 8017 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  NotNullConstraint ann_ name_
                  {-# LINE 8022 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8027 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8032 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_NullConstraint :: Annotation ->
                                    String ->
                                    T_RowConstraint 
sem_RowConstraint_NullConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  NullConstraint ann_ name_
                  {-# LINE 8048 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  NullConstraint ann_ name_
                  {-# LINE 8053 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8058 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8063 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowCheckConstraint :: Annotation ->
                                        String ->
                                        T_Expression  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 490, column 26)
              _exprOexpectedType =
                  {-# LINE 490 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 8092 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RowCheckConstraint ann_ name_ _exprIannotatedTree
                  {-# LINE 8097 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RowCheckConstraint ann_ name_ _exprIoriginalTree
                  {-# LINE 8102 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8107 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8112 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8117 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8122 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8127 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8132 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowPrimaryKeyConstraint :: Annotation ->
                                             String ->
                                             T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint ann_ name_
                  {-# LINE 8150 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RowPrimaryKeyConstraint ann_ name_
                  {-# LINE 8155 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8160 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8165 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowReferenceConstraint :: Annotation ->
                                            String ->
                                            String ->
                                            (Maybe String) ->
                                            Cascade ->
                                            Cascade ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
                  {-# LINE 8185 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
                  {-# LINE 8190 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8195 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8200 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraint_RowUniqueConstraint :: Annotation ->
                                         String ->
                                         T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint ann_ name_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: RowConstraint
              _lhsOoriginalTree :: RowConstraint
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  RowUniqueConstraint ann_ name_
                  {-# LINE 8216 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  RowUniqueConstraint ann_ name_
                  {-# LINE 8221 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8226 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8231 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- RowConstraintList -------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
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
type T_RowConstraintList  = Catalog ->
                            LocalBindings ->
                            (Maybe LocalBindings) ->
                            ([Maybe LocalBindings]) ->
                            ( RowConstraintList,RowConstraintList)
data Inh_RowConstraintList  = Inh_RowConstraintList {cat_Inh_RowConstraintList :: Catalog,lib_Inh_RowConstraintList :: LocalBindings,overrideLib_Inh_RowConstraintList :: Maybe LocalBindings,overrideLibs_Inh_RowConstraintList :: [Maybe LocalBindings]}
data Syn_RowConstraintList  = Syn_RowConstraintList {annotatedTree_Syn_RowConstraintList :: RowConstraintList,originalTree_Syn_RowConstraintList :: RowConstraintList}
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_RowConstraintList _lhsOannotatedTree _lhsOoriginalTree ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: RowConstraintList
              _lhsOoriginalTree :: RowConstraintList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: RowConstraint
              _hdIoriginalTree :: RowConstraint
              _tlIannotatedTree :: RowConstraintList
              _tlIoriginalTree :: RowConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 8303 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 8308 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8313 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8318 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8323 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8328 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8333 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8338 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8343 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8348 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8353 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8358 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: RowConstraintList
              _lhsOoriginalTree :: RowConstraintList
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8376 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 8381 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 8386 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8391 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree)))
-- SelectExpression --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         expectedTypes        : [Maybe Type]
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         libUpdates           : [LocalBindingsUpdate]
         originalTree         : SELF 
         uType                : Maybe [(String,Type)]
   alternatives:
      alternative CombineSelect:
         child ann            : {Annotation}
         child ctype          : {CombineType}
         child sel1           : SelectExpression 
         child sel2           : SelectExpression 
         visit 0:
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local originalTree : _
      alternative Select:
         child ann            : {Annotation}
         child selDistinct    : {Distinct}
         child selSelectList  : SelectList 
         child selTref        : TableRefList 
         child selWhere       : MaybeBoolExpression 
         child selGroupBy     : ExpressionList 
         child selHaving      : MaybeBoolExpression 
         child selOrderBy     : ExpressionDirectionPairList 
         child selLimit       : MaybeExpression 
         child selOffset      : MaybeExpression 
         visit 0:
            local newLib      : _
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local originalTree : _
      alternative Values:
         child ann            : {Annotation}
         child vll            : ExpressionListList 
         visit 0:
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local originalTree : _
      alternative WithSelect:
         child ann            : {Annotation}
         child withs          : WithQueryList 
         child ex             : SelectExpression 
         visit 0:
            local tpe         : {Et}
            local backTree    : _
            local annotatedTree : _
            local originalTree : _
-}
data SelectExpression  = CombineSelect (Annotation) (CombineType) (SelectExpression) (SelectExpression) 
                       | Select (Annotation) (Distinct) (SelectList) (TableRefList) (MaybeBoolExpression) (ExpressionList) (MaybeBoolExpression) (ExpressionDirectionPairList) (MaybeExpression) (MaybeExpression) 
                       | Values (Annotation) (ExpressionListList) 
                       | WithSelect (Annotation) (WithQueryList) (SelectExpression) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectExpression :: SelectExpression  ->
                        T_SelectExpression 
sem_SelectExpression (CombineSelect _ann _ctype _sel1 _sel2 )  =
    (sem_SelectExpression_CombineSelect _ann _ctype (sem_SelectExpression _sel1 ) (sem_SelectExpression _sel2 ) )
sem_SelectExpression (Select _ann _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selLimit _selOffset )  =
    (sem_SelectExpression_Select _ann _selDistinct (sem_SelectList _selSelectList ) (sem_TableRefList _selTref ) (sem_MaybeBoolExpression _selWhere ) (sem_ExpressionList _selGroupBy ) (sem_MaybeBoolExpression _selHaving ) (sem_ExpressionDirectionPairList _selOrderBy ) (sem_MaybeExpression _selLimit ) (sem_MaybeExpression _selOffset ) )
sem_SelectExpression (Values _ann _vll )  =
    (sem_SelectExpression_Values _ann (sem_ExpressionListList _vll ) )
sem_SelectExpression (WithSelect _ann _withs _ex )  =
    (sem_SelectExpression_WithSelect _ann (sem_WithQueryList _withs ) (sem_SelectExpression _ex ) )
-- semantic domain
type T_SelectExpression  = Catalog ->
                           ([Maybe Type]) ->
                           LocalBindings ->
                           (Maybe LocalBindings) ->
                           ([Maybe LocalBindings]) ->
                           ( SelectExpression,([LocalBindingsUpdate]),SelectExpression,(Maybe [(String,Type)]))
data Inh_SelectExpression  = Inh_SelectExpression {cat_Inh_SelectExpression :: Catalog,expectedTypes_Inh_SelectExpression :: [Maybe Type],lib_Inh_SelectExpression :: LocalBindings,overrideLib_Inh_SelectExpression :: Maybe LocalBindings,overrideLibs_Inh_SelectExpression :: [Maybe LocalBindings]}
data Syn_SelectExpression  = Syn_SelectExpression {annotatedTree_Syn_SelectExpression :: SelectExpression,libUpdates_Syn_SelectExpression :: [LocalBindingsUpdate],originalTree_Syn_SelectExpression :: SelectExpression,uType_Syn_SelectExpression :: Maybe [(String,Type)]}
wrap_SelectExpression :: T_SelectExpression  ->
                         Inh_SelectExpression  ->
                         Syn_SelectExpression 
wrap_SelectExpression sem (Inh_SelectExpression _lhsIcat _lhsIexpectedTypes _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOuType) =
             (sem _lhsIcat _lhsIexpectedTypes _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_SelectExpression _lhsOannotatedTree _lhsOlibUpdates _lhsOoriginalTree _lhsOuType ))
sem_SelectExpression_CombineSelect :: Annotation ->
                                      CombineType ->
                                      T_SelectExpression  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression 
sem_SelectExpression_CombineSelect ann_ ctype_ sel1_ sel2_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: SelectExpression
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _lhsOuType :: (Maybe [(String,Type)])
              _lhsOoriginalTree :: SelectExpression
              _sel1Ocat :: Catalog
              _sel1OexpectedTypes :: ([Maybe Type])
              _sel1Olib :: LocalBindings
              _sel1OoverrideLib :: (Maybe LocalBindings)
              _sel1OoverrideLibs :: ([Maybe LocalBindings])
              _sel2Ocat :: Catalog
              _sel2OexpectedTypes :: ([Maybe Type])
              _sel2Olib :: LocalBindings
              _sel2OoverrideLib :: (Maybe LocalBindings)
              _sel2OoverrideLibs :: ([Maybe LocalBindings])
              _sel1IannotatedTree :: SelectExpression
              _sel1IlibUpdates :: ([LocalBindingsUpdate])
              _sel1IoriginalTree :: SelectExpression
              _sel1IuType :: (Maybe [(String,Type)])
              _sel2IannotatedTree :: SelectExpression
              _sel2IlibUpdates :: ([LocalBindingsUpdate])
              _sel2IoriginalTree :: SelectExpression
              _sel2IuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/SelectStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/SelectStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 8523 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 115, column 9)
              _lhsOlibUpdates =
                  {-# LINE 115 "./TypeChecking/SelectStatement.ag" #-}
                  []
                  {-# LINE 8528 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 142, column 9)
              _tpe =
                  {-# LINE 142 "./TypeChecking/SelectStatement.ag" #-}
                  do
                  sel1t <- lmt ((SetOfType . CompositeType) <$> _sel1IuType)
                  sel2t <- lmt ((SetOfType . CompositeType) <$> _sel2IuType)
                  typeCheckCombineSelect _lhsIcat sel1t sel2t
                  {-# LINE 8536 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 148, column 9)
              _backTree =
                  {-# LINE 148 "./TypeChecking/SelectStatement.ag" #-}
                  CombineSelect ann_ ctype_
                                _sel1IannotatedTree
                                _sel2IannotatedTree
                  {-# LINE 8543 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 159, column 9)
              _lhsOuType =
                  {-# LINE 159 "./TypeChecking/SelectStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 8548 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CombineSelect ann_ ctype_ _sel1IannotatedTree _sel2IannotatedTree
                  {-# LINE 8553 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CombineSelect ann_ ctype_ _sel1IoriginalTree _sel2IoriginalTree
                  {-# LINE 8558 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8563 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8568 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1OexpectedTypes =
                  {-# LINE 546 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 8573 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8578 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8583 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel1OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8588 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8593 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2OexpectedTypes =
                  {-# LINE 546 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 8598 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8603 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8608 "AstInternal.hs" #-}
              -- copy rule (down)
              _sel2OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8613 "AstInternal.hs" #-}
              ( _sel1IannotatedTree,_sel1IlibUpdates,_sel1IoriginalTree,_sel1IuType) =
                  (sel1_ _sel1Ocat _sel1OexpectedTypes _sel1Olib _sel1OoverrideLib _sel1OoverrideLibs )
              ( _sel2IannotatedTree,_sel2IlibUpdates,_sel2IoriginalTree,_sel2IuType) =
                  (sel2_ _sel2Ocat _sel2OexpectedTypes _sel2Olib _sel2OoverrideLib _sel2OoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOuType)))
sem_SelectExpression_Select :: Annotation ->
                               Distinct ->
                               T_SelectList  ->
                               T_TableRefList  ->
                               T_MaybeBoolExpression  ->
                               T_ExpressionList  ->
                               T_MaybeBoolExpression  ->
                               T_ExpressionDirectionPairList  ->
                               T_MaybeExpression  ->
                               T_MaybeExpression  ->
                               T_SelectExpression 
sem_SelectExpression_Select ann_ selDistinct_ selSelectList_ selTref_ selWhere_ selGroupBy_ selHaving_ selOrderBy_ selLimit_ selOffset_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _selGroupByOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: SelectExpression
              _selSelectListOlib :: LocalBindings
              _selWhereOlib :: LocalBindings
              _selGroupByOlib :: LocalBindings
              _selOrderByOlib :: LocalBindings
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _lhsOuType :: (Maybe [(String,Type)])
              _lhsOoriginalTree :: SelectExpression
              _selSelectListOcat :: Catalog
              _selSelectListOoverrideLib :: (Maybe LocalBindings)
              _selSelectListOoverrideLibs :: ([Maybe LocalBindings])
              _selTrefOcat :: Catalog
              _selTrefOlib :: LocalBindings
              _selTrefOoverrideLib :: (Maybe LocalBindings)
              _selTrefOoverrideLibs :: ([Maybe LocalBindings])
              _selWhereOcat :: Catalog
              _selWhereOoverrideLib :: (Maybe LocalBindings)
              _selWhereOoverrideLibs :: ([Maybe LocalBindings])
              _selGroupByOcat :: Catalog
              _selGroupByOoverrideLib :: (Maybe LocalBindings)
              _selGroupByOoverrideLibs :: ([Maybe LocalBindings])
              _selHavingOcat :: Catalog
              _selHavingOlib :: LocalBindings
              _selHavingOoverrideLib :: (Maybe LocalBindings)
              _selHavingOoverrideLibs :: ([Maybe LocalBindings])
              _selOrderByOcat :: Catalog
              _selOrderByOoverrideLib :: (Maybe LocalBindings)
              _selOrderByOoverrideLibs :: ([Maybe LocalBindings])
              _selLimitOcat :: Catalog
              _selLimitOlib :: LocalBindings
              _selLimitOoverrideLib :: (Maybe LocalBindings)
              _selLimitOoverrideLibs :: ([Maybe LocalBindings])
              _selOffsetOcat :: Catalog
              _selOffsetOlib :: LocalBindings
              _selOffsetOoverrideLib :: (Maybe LocalBindings)
              _selOffsetOoverrideLibs :: ([Maybe LocalBindings])
              _selSelectListIannotatedTree :: SelectList
              _selSelectListIlibUpdates :: ([LocalBindingsUpdate])
              _selSelectListIlistType :: ([(String,Type)])
              _selSelectListIoriginalTree :: SelectList
              _selTrefIannotatedTree :: TableRefList
              _selTrefIlibUpdates :: ([LocalBindingsUpdate])
              _selTrefIoriginalTree :: TableRefList
              _selWhereIannotatedTree :: MaybeBoolExpression
              _selWhereIoriginalTree :: MaybeBoolExpression
              _selGroupByIannotatedTree :: ExpressionList
              _selGroupByIoriginalTree :: ExpressionList
              _selGroupByItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _selGroupByIuType :: ([Maybe Type])
              _selHavingIannotatedTree :: MaybeBoolExpression
              _selHavingIoriginalTree :: MaybeBoolExpression
              _selOrderByIannotatedTree :: ExpressionDirectionPairList
              _selOrderByIoriginalTree :: ExpressionDirectionPairList
              _selLimitIannotatedTree :: MaybeExpression
              _selLimitIoriginalTree :: MaybeExpression
              _selLimitIuType :: (Maybe Type)
              _selOffsetIannotatedTree :: MaybeExpression
              _selOffsetIoriginalTree :: MaybeExpression
              _selOffsetIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 537, column 14)
              _selGroupByOexpectedTypes =
                  {-# LINE 537 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 8701 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/SelectStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 8706 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 98, column 10)
              _newLib =
                  {-# LINE 98 "./TypeChecking/SelectStatement.ag" #-}
                  case foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _selTrefIlibUpdates of
                    Left x -> error $ "selectexpression-select-loc.newlib " ++ show x
                    Right e -> e
                  {-# LINE 8713 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 101, column 10)
              _selSelectListOlib =
                  {-# LINE 101 "./TypeChecking/SelectStatement.ag" #-}
                  _newLib
                  {-# LINE 8718 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 102, column 10)
              _selWhereOlib =
                  {-# LINE 102 "./TypeChecking/SelectStatement.ag" #-}
                  _newLib
                  {-# LINE 8723 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 103, column 10)
              _selGroupByOlib =
                  {-# LINE 103 "./TypeChecking/SelectStatement.ag" #-}
                  _newLib
                  {-# LINE 8728 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 104, column 10)
              _selOrderByOlib =
                  {-# LINE 104 "./TypeChecking/SelectStatement.ag" #-}
                  _newLib
                  {-# LINE 8733 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/SelectStatement.ag" #-}
                  _selSelectListIlibUpdates
                  {-# LINE 8738 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 129, column 9)
              _tpe =
                  {-# LINE 129 "./TypeChecking/SelectStatement.ag" #-}
                  Right $ SetOfType $ CompositeType _selSelectListIlistType
                  {-# LINE 8743 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 131, column 9)
              _backTree =
                  {-# LINE 131 "./TypeChecking/SelectStatement.ag" #-}
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
                  {-# LINE 8757 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 159, column 9)
              _lhsOuType =
                  {-# LINE 159 "./TypeChecking/SelectStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 8762 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Select ann_ selDistinct_ _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
                  {-# LINE 8767 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Select ann_ selDistinct_ _selSelectListIoriginalTree _selTrefIoriginalTree _selWhereIoriginalTree _selGroupByIoriginalTree _selHavingIoriginalTree _selOrderByIoriginalTree _selLimitIoriginalTree _selOffsetIoriginalTree
                  {-# LINE 8772 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 8777 "AstInternal.hs" #-}
              -- copy rule (down)
              _selSelectListOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8782 "AstInternal.hs" #-}
              -- copy rule (down)
              _selSelectListOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8787 "AstInternal.hs" #-}
              -- copy rule (down)
              _selSelectListOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8792 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8797 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8802 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8807 "AstInternal.hs" #-}
              -- copy rule (down)
              _selTrefOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8812 "AstInternal.hs" #-}
              -- copy rule (down)
              _selWhereOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8817 "AstInternal.hs" #-}
              -- copy rule (down)
              _selWhereOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8822 "AstInternal.hs" #-}
              -- copy rule (down)
              _selWhereOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8827 "AstInternal.hs" #-}
              -- copy rule (down)
              _selGroupByOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8832 "AstInternal.hs" #-}
              -- copy rule (down)
              _selGroupByOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8837 "AstInternal.hs" #-}
              -- copy rule (down)
              _selGroupByOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8842 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8847 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8852 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8857 "AstInternal.hs" #-}
              -- copy rule (down)
              _selHavingOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8862 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOrderByOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8867 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOrderByOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8872 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOrderByOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8877 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8882 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8887 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8892 "AstInternal.hs" #-}
              -- copy rule (down)
              _selLimitOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8897 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 8902 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 8907 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 8912 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOffsetOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 8917 "AstInternal.hs" #-}
              ( _selSelectListIannotatedTree,_selSelectListIlibUpdates,_selSelectListIlistType,_selSelectListIoriginalTree) =
                  (selSelectList_ _selSelectListOcat _selSelectListOlib _selSelectListOoverrideLib _selSelectListOoverrideLibs )
              ( _selTrefIannotatedTree,_selTrefIlibUpdates,_selTrefIoriginalTree) =
                  (selTref_ _selTrefOcat _selTrefOlib _selTrefOoverrideLib _selTrefOoverrideLibs )
              ( _selWhereIannotatedTree,_selWhereIoriginalTree) =
                  (selWhere_ _selWhereOcat _selWhereOlib _selWhereOoverrideLib _selWhereOoverrideLibs )
              ( _selGroupByIannotatedTree,_selGroupByIoriginalTree,_selGroupByItbUTypes,_selGroupByIuType) =
                  (selGroupBy_ _selGroupByOcat _selGroupByOexpectedTypes _selGroupByOlib _selGroupByOoverrideLib _selGroupByOoverrideLibs )
              ( _selHavingIannotatedTree,_selHavingIoriginalTree) =
                  (selHaving_ _selHavingOcat _selHavingOlib _selHavingOoverrideLib _selHavingOoverrideLibs )
              ( _selOrderByIannotatedTree,_selOrderByIoriginalTree) =
                  (selOrderBy_ _selOrderByOcat _selOrderByOlib _selOrderByOoverrideLib _selOrderByOoverrideLibs )
              ( _selLimitIannotatedTree,_selLimitIoriginalTree,_selLimitIuType) =
                  (selLimit_ _selLimitOcat _selLimitOlib _selLimitOoverrideLib _selLimitOoverrideLibs )
              ( _selOffsetIannotatedTree,_selOffsetIoriginalTree,_selOffsetIuType) =
                  (selOffset_ _selOffsetOcat _selOffsetOlib _selOffsetOoverrideLib _selOffsetOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOuType)))
sem_SelectExpression_Values :: Annotation ->
                               T_ExpressionListList  ->
                               T_SelectExpression 
sem_SelectExpression_Values ann_ vll_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _vllOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: SelectExpression
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _lhsOuType :: (Maybe [(String,Type)])
              _lhsOoriginalTree :: SelectExpression
              _vllOcat :: Catalog
              _vllOlib :: LocalBindings
              _vllOoverrideLib :: (Maybe LocalBindings)
              _vllOoverrideLibs :: ([Maybe LocalBindings])
              _vllIannotatedTree :: ExpressionListList
              _vllIoriginalTree :: ExpressionListList
              _vllIuType :: ([[Maybe Type]])
              -- "./TypeChecking/Expressions.ag"(line 551, column 14)
              _vllOexpectedTypes =
                  {-# LINE 551 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 8961 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/SelectStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 8966 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 115, column 9)
              _lhsOlibUpdates =
                  {-# LINE 115 "./TypeChecking/SelectStatement.ag" #-}
                  []
                  {-# LINE 8971 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 124, column 9)
              _tpe =
                  {-# LINE 124 "./TypeChecking/SelectStatement.ag" #-}
                  typeCheckValuesExpr
                              _lhsIcat
                              _vllIuType
                  {-# LINE 8978 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 127, column 9)
              _backTree =
                  {-# LINE 127 "./TypeChecking/SelectStatement.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 8983 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 159, column 9)
              _lhsOuType =
                  {-# LINE 159 "./TypeChecking/SelectStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 8988 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Values ann_ _vllIannotatedTree
                  {-# LINE 8993 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Values ann_ _vllIoriginalTree
                  {-# LINE 8998 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9003 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9008 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9013 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 9018 "AstInternal.hs" #-}
              -- copy rule (down)
              _vllOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 9023 "AstInternal.hs" #-}
              ( _vllIannotatedTree,_vllIoriginalTree,_vllIuType) =
                  (vll_ _vllOcat _vllOexpectedTypes _vllOlib _vllOoverrideLib _vllOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOuType)))
sem_SelectExpression_WithSelect :: Annotation ->
                                   T_WithQueryList  ->
                                   T_SelectExpression  ->
                                   T_SelectExpression 
sem_SelectExpression_WithSelect ann_ withs_ ex_  =
    (\ _lhsIcat
       _lhsIexpectedTypes
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: SelectExpression
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: Et
              _exOcat :: Catalog
              _withsOcatUpdates :: ([CatalogUpdate])
              _lhsOuType :: (Maybe [(String,Type)])
              _lhsOoriginalTree :: SelectExpression
              _withsOcat :: Catalog
              _withsOlib :: LocalBindings
              _withsOoverrideLib :: (Maybe LocalBindings)
              _withsOoverrideLibs :: ([Maybe LocalBindings])
              _exOexpectedTypes :: ([Maybe Type])
              _exOlib :: LocalBindings
              _exOoverrideLib :: (Maybe LocalBindings)
              _exOoverrideLibs :: ([Maybe LocalBindings])
              _withsIannotatedTree :: WithQueryList
              _withsIoriginalTree :: WithQueryList
              _withsIproducedCat :: Catalog
              _exIannotatedTree :: SelectExpression
              _exIlibUpdates :: ([LocalBindingsUpdate])
              _exIoriginalTree :: SelectExpression
              _exIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/SelectStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  {-# LINE 29 "./TypeChecking/SelectStatement.ag" #-}
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 9063 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 119, column 9)
              _lhsOlibUpdates =
                  {-# LINE 119 "./TypeChecking/SelectStatement.ag" #-}
                  _exIlibUpdates
                  {-# LINE 9068 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 152, column 9)
              _tpe =
                  {-# LINE 152 "./TypeChecking/SelectStatement.ag" #-}
                  lmt ((SetOfType . CompositeType) <$> _exIuType)
                  {-# LINE 9073 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 153, column 9)
              _backTree =
                  {-# LINE 153 "./TypeChecking/SelectStatement.ag" #-}
                  WithSelect ann_ _withsIannotatedTree _exIannotatedTree
                  {-# LINE 9078 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 154, column 9)
              _exOcat =
                  {-# LINE 154 "./TypeChecking/SelectStatement.ag" #-}
                  _withsIproducedCat
                  {-# LINE 9083 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 155, column 9)
              _withsOcatUpdates =
                  {-# LINE 155 "./TypeChecking/SelectStatement.ag" #-}
                  []
                  {-# LINE 9088 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 159, column 9)
              _lhsOuType =
                  {-# LINE 159 "./TypeChecking/SelectStatement.ag" #-}
                  etmt (_tpe     >>= unwrapSetOfComposite)
                  {-# LINE 9093 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  WithSelect ann_ _withsIannotatedTree _exIannotatedTree
                  {-# LINE 9098 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  WithSelect ann_ _withsIoriginalTree _exIoriginalTree
                  {-# LINE 9103 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9108 "AstInternal.hs" #-}
              -- copy rule (down)
              _withsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9113 "AstInternal.hs" #-}
              -- copy rule (down)
              _withsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9118 "AstInternal.hs" #-}
              -- copy rule (down)
              _withsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 9123 "AstInternal.hs" #-}
              -- copy rule (down)
              _withsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 9128 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOexpectedTypes =
                  {-# LINE 546 "./TypeChecking/Expressions.ag" #-}
                  _lhsIexpectedTypes
                  {-# LINE 9133 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9138 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 9143 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 9148 "AstInternal.hs" #-}
              ( _withsIannotatedTree,_withsIoriginalTree,_withsIproducedCat) =
                  (withs_ _withsOcat _withsOcatUpdates _withsOlib _withsOoverrideLib _withsOoverrideLibs )
              ( _exIannotatedTree,_exIlibUpdates,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedTypes _exOlib _exOoverrideLib _exOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree,_lhsOuType)))
-- SelectItem --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         itemType             : [(String,Type)]
         originalTree         : SELF 
   alternatives:
      alternative SelExp:
         child ann            : {Annotation}
         child ex             : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SelectItem:
         child ann            : {Annotation}
         child ex             : Expression 
         child name           : {String}
         visit 0:
            local annotatedTree : _
            local originalTree : _
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
type T_SelectItem  = Catalog ->
                     LocalBindings ->
                     (Maybe LocalBindings) ->
                     ([Maybe LocalBindings]) ->
                     ( SelectItem,([(String,Type)]),SelectItem)
data Inh_SelectItem  = Inh_SelectItem {cat_Inh_SelectItem :: Catalog,lib_Inh_SelectItem :: LocalBindings,overrideLib_Inh_SelectItem :: Maybe LocalBindings,overrideLibs_Inh_SelectItem :: [Maybe LocalBindings]}
data Syn_SelectItem  = Syn_SelectItem {annotatedTree_Syn_SelectItem :: SelectItem,itemType_Syn_SelectItem :: [(String,Type)],originalTree_Syn_SelectItem :: SelectItem}
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOitemType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_SelectItem _lhsOannotatedTree _lhsOitemType _lhsOoriginalTree ))
sem_SelectItem_SelExp :: Annotation ->
                         T_Expression  ->
                         T_SelectItem 
sem_SelectItem_SelExp ann_ ex_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exOexpectedType :: (Maybe Type)
              _lhsOitemType :: ([(String,Type)])
              _lhsOannotatedTree :: SelectItem
              _lhsOoriginalTree :: SelectItem
              _exOcat :: Catalog
              _exOlib :: LocalBindings
              _exOoverrideLib :: (Maybe LocalBindings)
              _exOoverrideLibs :: ([Maybe LocalBindings])
              _exIannotatedTree :: Expression
              _exIntAnnotatedTree :: Expression
              _exIntType :: ([(String,Type)])
              _exIoriginalTree :: Expression
              _exItbAnnotatedTree :: Expression
              _exItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 493, column 25)
              _exOexpectedType =
                  {-# LINE 493 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 9233 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 33, column 9)
              _annotatedTree =
                  {-# LINE 33 "./TypeChecking/SelectLists.ag" #-}
                  SelExp ann_ _exIntAnnotatedTree
                  {-# LINE 9238 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 61, column 9)
              _lhsOitemType =
                  {-# LINE 61 "./TypeChecking/SelectLists.ag" #-}
                  unwrapSetofs _exIntType
                  {-# LINE 9243 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SelExp ann_ _exIoriginalTree
                  {-# LINE 9248 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9253 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9258 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9263 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9268 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 9273 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 9278 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIntAnnotatedTree,_exIntType,_exIoriginalTree,_exItbAnnotatedTree,_exItbUType,_exIuType) =
                  (ex_ _exOcat _exOexpectedType _exOlib _exOoverrideLib _exOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOitemType,_lhsOoriginalTree)))
sem_SelectItem_SelectItem :: Annotation ->
                             T_Expression  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem ann_ ex_ name_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exOexpectedType :: (Maybe Type)
              _lhsOitemType :: ([(String,Type)])
              _lhsOannotatedTree :: SelectItem
              _lhsOoriginalTree :: SelectItem
              _exOcat :: Catalog
              _exOlib :: LocalBindings
              _exOoverrideLib :: (Maybe LocalBindings)
              _exOoverrideLibs :: ([Maybe LocalBindings])
              _exIannotatedTree :: Expression
              _exIntAnnotatedTree :: Expression
              _exIntType :: ([(String,Type)])
              _exIoriginalTree :: Expression
              _exItbAnnotatedTree :: Expression
              _exItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 493, column 25)
              _exOexpectedType =
                  {-# LINE 493 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 9310 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 35, column 9)
              _annotatedTree =
                  {-# LINE 35 "./TypeChecking/SelectLists.ag" #-}
                  SelectItem ann_ _exIannotatedTree name_
                  {-# LINE 9315 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 63, column 9)
              _lhsOitemType =
                  {-# LINE 63 "./TypeChecking/SelectLists.ag" #-}
                  case _exIntType of
                    [(_,t)] -> [(name_, unwrapSetof t)]
                    _ -> []
                  {-# LINE 9322 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SelectItem ann_ _exIoriginalTree name_
                  {-# LINE 9327 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9332 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9337 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9342 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9347 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 9352 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 9357 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIntAnnotatedTree,_exIntType,_exIoriginalTree,_exItbAnnotatedTree,_exItbUType,_exIuType) =
                  (ex_ _exOcat _exOexpectedType _exOlib _exOoverrideLib _exOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOitemType,_lhsOoriginalTree)))
-- SelectItemList ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         listType             : [(String,Type)]
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
type SelectItemList  = [(SelectItem)]
-- cata
sem_SelectItemList :: SelectItemList  ->
                      T_SelectItemList 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList  = Catalog ->
                         LocalBindings ->
                         (Maybe LocalBindings) ->
                         ([Maybe LocalBindings]) ->
                         ( SelectItemList,([(String,Type)]),SelectItemList)
data Inh_SelectItemList  = Inh_SelectItemList {cat_Inh_SelectItemList :: Catalog,lib_Inh_SelectItemList :: LocalBindings,overrideLib_Inh_SelectItemList :: Maybe LocalBindings,overrideLibs_Inh_SelectItemList :: [Maybe LocalBindings]}
data Syn_SelectItemList  = Syn_SelectItemList {annotatedTree_Syn_SelectItemList :: SelectItemList,listType_Syn_SelectItemList :: [(String,Type)],originalTree_Syn_SelectItemList :: SelectItemList}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_SelectItemList _lhsOannotatedTree _lhsOlistType _lhsOoriginalTree ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOlistType :: ([(String,Type)])
              _lhsOannotatedTree :: SelectItemList
              _lhsOoriginalTree :: SelectItemList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: SelectItem
              _hdIitemType :: ([(String,Type)])
              _hdIoriginalTree :: SelectItem
              _tlIannotatedTree :: SelectItemList
              _tlIlistType :: ([(String,Type)])
              _tlIoriginalTree :: SelectItemList
              -- "./TypeChecking/SelectLists.ag"(line 43, column 12)
              _lhsOlistType =
                  {-# LINE 43 "./TypeChecking/SelectLists.ag" #-}
                  _hdIitemType ++ _tlIlistType
                  {-# LINE 9435 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 9440 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 9445 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9450 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9455 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9460 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9465 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 9470 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 9475 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9480 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9485 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 9490 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 9495 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIitemType,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIlistType,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOlistType :: ([(String,Type)])
              _lhsOannotatedTree :: SelectItemList
              _lhsOoriginalTree :: SelectItemList
              -- "./TypeChecking/SelectLists.ag"(line 44, column 11)
              _lhsOlistType =
                  {-# LINE 44 "./TypeChecking/SelectLists.ag" #-}
                  []
                  {-# LINE 9514 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 9519 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 9524 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 9529 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9534 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlistType,_lhsOoriginalTree)))
-- SelectList --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         libUpdates           : [LocalBindingsUpdate]
         listType             : [(String,Type)]
         originalTree         : SELF 
   alternatives:
      alternative SelectList:
         child ann            : {Annotation}
         child items          : SelectItemList 
         child into           : {[Expression]}
         visit 0:
            local intoFroms   : {E ([(String,Type)],[(String,Type)])}
            local tpe         : _
            local annotatedTree : _
            local originalTree : _
-}
data SelectList  = SelectList (Annotation) (SelectItemList) ([Expression]) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _ann _items _into )  =
    (sem_SelectList_SelectList _ann (sem_SelectItemList _items ) _into )
-- semantic domain
type T_SelectList  = Catalog ->
                     LocalBindings ->
                     (Maybe LocalBindings) ->
                     ([Maybe LocalBindings]) ->
                     ( SelectList,([LocalBindingsUpdate]),([(String,Type)]),SelectList)
data Inh_SelectList  = Inh_SelectList {cat_Inh_SelectList :: Catalog,lib_Inh_SelectList :: LocalBindings,overrideLib_Inh_SelectList :: Maybe LocalBindings,overrideLibs_Inh_SelectList :: [Maybe LocalBindings]}
data Syn_SelectList  = Syn_SelectList {annotatedTree_Syn_SelectList :: SelectList,libUpdates_Syn_SelectList :: [LocalBindingsUpdate],listType_Syn_SelectList :: [(String,Type)],originalTree_Syn_SelectList :: SelectList}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOlistType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_SelectList _lhsOannotatedTree _lhsOlibUpdates _lhsOlistType _lhsOoriginalTree ))
sem_SelectList_SelectList :: Annotation ->
                             T_SelectItemList  ->
                             ([Expression]) ->
                             T_SelectList 
sem_SelectList_SelectList ann_ items_ into_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOlistType :: ([(String,Type)])
              _intoFroms :: (E ([(String,Type)],[(String,Type)]))
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: SelectList
              _lhsOoriginalTree :: SelectList
              _itemsOcat :: Catalog
              _itemsOlib :: LocalBindings
              _itemsOoverrideLib :: (Maybe LocalBindings)
              _itemsOoverrideLibs :: ([Maybe LocalBindings])
              _itemsIannotatedTree :: SelectItemList
              _itemsIlistType :: ([(String,Type)])
              _itemsIoriginalTree :: SelectItemList
              -- "./TypeChecking/SelectLists.ag"(line 80, column 9)
              _lhsOlistType =
                  {-# LINE 80 "./TypeChecking/SelectLists.ag" #-}
                  _itemsIlistType
                  {-# LINE 9607 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 82, column 9)
              _intoFroms =
                  {-# LINE 82 "./TypeChecking/SelectLists.ag" #-}
                  returnWhen (into_ == []) ([],[]) $ do
                  intoTypes <- either (Left . concat) Right $ listEither $ map getIntoType into_
                  let ft = _itemsIlistType
                  return (intoTypes,ft)
                  where
                    getIntoType :: Expression -> E (String,Type)
                    getIntoType n = do
                                    let n1 = getName n
                                    t <- unwrapLookup <$> lbLookupID _lhsIlib n1
                                    return (n1,t)
                  {-# LINE 9621 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 96, column 9)
              _tpe =
                  {-# LINE 96 "./TypeChecking/SelectLists.ag" #-}
                  returnWhen (into_ == []) () $ do
                  (it,ft) <- _intoFroms
                  checkAssignmentsValid _lhsIcat (map snd ft) (map snd it)
                  {-# LINE 9628 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 101, column 9)
              _lhsOlibUpdates =
                  {-# LINE 101 "./TypeChecking/SelectLists.ag" #-}
                  maybe [] id $ do
                  _ <- etmt _tpe
                  (it,ft) <- etmt _intoFroms
                  return $ case it of
                    [(n,PgRecord _)] -> [LBIds "set record actual fields from select into"
                                               Nothing
                                               [(n,PgRecord $ Just $ CompositeType ft)]]
                    _ -> []
                  {-# LINE 9640 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectLists.ag"(line 134, column 9)
              _lhsOannotatedTree =
                  {-# LINE 134 "./TypeChecking/SelectLists.ag" #-}
                  addTypeErrors (tes _tpe    ) $
                  SelectList ann_
                             _itemsIannotatedTree
                             into_
                  {-# LINE 9648 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  SelectList ann_ _itemsIannotatedTree into_
                  {-# LINE 9653 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SelectList ann_ _itemsIoriginalTree into_
                  {-# LINE 9658 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 9663 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 9668 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 9673 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 9678 "AstInternal.hs" #-}
              -- copy rule (down)
              _itemsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 9683 "AstInternal.hs" #-}
              ( _itemsIannotatedTree,_itemsIlistType,_itemsIoriginalTree) =
                  (items_ _itemsOcat _itemsOlib _itemsOoverrideLib _itemsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOlistType,_lhsOoriginalTree)))
-- Statement ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         inProducedCat        : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         catUpdates           : [CatalogUpdate]
         libUpdates           : [LocalBindingsUpdate]
         originalTree         : SELF 
   alternatives:
      alternative AlterSequence:
         child ann            : {Annotation}
         child name           : {String}
         child ownedBy        : Expression 
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local originalTree : _
      alternative AlterTable:
         child ann            : {Annotation}
         child name           : {String}
         child actions        : AlterTableActionList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Assignment:
         child ann            : {Annotation}
         child target         : Expression 
         child value          : Expression 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local originalTree : _
      alternative Block:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child vars           : VarDefList 
         child sts            : StatementList 
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local originalTree : _
      alternative CaseStatement:
         child ann            : {Annotation}
         child cases          : ExpressionListStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative CaseStatementSimple:
         child ann            : {Annotation}
         child val            : Expression 
         child cases          : ExpressionListStatementListPairList 
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
         child table          : {String}
         child targetCols     : {[String]}
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
         child name           : {String}
         child typ            : TypeName 
         child checkName      : {String}
         child check          : MaybeBoolExpression 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local statementType : {Maybe StatementType}
            local catUpdates  : {[CatalogUpdate]}
            local annotatedTree : _
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
            local originalTree : _
      alternative CreateTableAs:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : SelectExpression 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local catUpdates  : {[CatalogUpdate]}
            local attrs       : {Either [TypeError] [(String,Type)]}
            local backTree    : _
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local originalTree : _
      alternative CreateTrigger:
         child ann            : {Annotation}
         child name           : {String}
         child wh             : {TriggerWhen}
         child events         : {[TriggerEvent]}
         child tbl            : {String}
         child firing         : {TriggerFire}
         child fnName         : {String}
         child fnArgs         : ExpressionList 
         visit 0:
            local annotatedTree : _
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
            local originalTree : _
      alternative CreateView:
         child ann            : {Annotation}
         child name           : {String}
         child expr           : SelectExpression 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local originalTree : _
      alternative Delete:
         child ann            : {Annotation}
         child table          : Expression 
         child using          : TableRefList 
         child whr            : MaybeBoolExpression 
         child returning      : MaybeSelectList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local statementType : {Maybe StatementType}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local lib         : _
            local annotatedTree : _
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
            local originalTree : _
      alternative DropSomething:
         child ann            : {Annotation}
         child dropType       : {DropType}
         child ifE            : {IfExists}
         child names          : {[String]}
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Execute:
         child ann            : {Annotation}
         child expr           : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ExecuteInto:
         child ann            : {Annotation}
         child expr           : Expression 
         child targets        : {[String]}
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
         child var            : Expression 
         child from           : Expression 
         child to             : Expression 
         child sts            : StatementList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local originalTree : _
      alternative ForSelectStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child var            : Expression 
         child sel            : SelectExpression 
         child sts            : StatementList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local originalTree : _
      alternative If:
         child ann            : {Annotation}
         child cases          : ExpressionStatementListPairList 
         child els            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Insert:
         child ann            : {Annotation}
         child table          : Expression 
         child targetCols     : {[String]}
         child insData        : SelectExpression 
         child returning      : MaybeSelectList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local statementType : {Maybe StatementType}
            local columnTypes : {Either [TypeError] [(String,Type)]}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
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
            local libUpdates  : _
            local annotatedTree : _
            local originalTree : _
      alternative NullStatement:
         child ann            : {Annotation}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Perform:
         child ann            : {Annotation}
         child expr           : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Raise:
         child ann            : {Annotation}
         child level          : {RaiseType}
         child message        : {String}
         child args           : ExpressionList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Return:
         child ann            : {Annotation}
         child value          : MaybeExpression 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local statementType : {Maybe StatementType}
            local annotatedTree : _
            local originalTree : _
      alternative ReturnNext:
         child ann            : {Annotation}
         child expr           : Expression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative ReturnQuery:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative SelectStatement:
         child ann            : {Annotation}
         child ex             : SelectExpression 
         visit 0:
            local tpe         : {Either [TypeError] Type}
            local statementType : {Maybe StatementType}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local libUpdates  : _
            local annotatedTree : _
            local originalTree : _
      alternative Set:
         child ann            : {Annotation}
         child name           : {String}
         child values         : {[SetValue]}
         visit 0:
            local libUpdates  : _
            local annotatedTree : _
            local originalTree : _
      alternative Truncate:
         child ann            : {Annotation}
         child tables         : {[String]}
         child restartIdentity : {RestartIdentity}
         child cascade        : {Cascade}
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Update:
         child ann            : {Annotation}
         child table          : Expression 
         child assigns        : ExpressionList 
         child fromList       : TableRefList 
         child whr            : MaybeBoolExpression 
         child returning      : MaybeSelectList 
         visit 0:
            local libUpdates  : _
            local tpe         : {Either [TypeError] Type}
            local statementType : {Maybe StatementType}
            local backTree    : _
            local catUpdates  : {[CatalogUpdate]}
            local lib         : _
            local annotatedTree : _
            local originalTree : _
      alternative WhileStatement:
         child ann            : {Annotation}
         child lb             : {Maybe String}
         child expr           : Expression 
         child sts            : StatementList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
-}
data Statement  = AlterSequence (Annotation) (String) (Expression) 
                | AlterTable (Annotation) (String) (AlterTableActionList) 
                | Assignment (Annotation) (Expression) (Expression) 
                | Block (Annotation) (Maybe String) (VarDefList) (StatementList) 
                | CaseStatement (Annotation) (ExpressionListStatementListPairList) (StatementList) 
                | CaseStatementSimple (Annotation) (Expression) (ExpressionListStatementListPairList) (StatementList) 
                | ContinueStatement (Annotation) (Maybe String) 
                | Copy (Annotation) (String) ([String]) (CopySource) 
                | CopyData (Annotation) (String) 
                | CreateDomain (Annotation) (String) (TypeName) (String) (MaybeBoolExpression) 
                | CreateFunction (Annotation) (String) (ParamDefList) (TypeName) (Replace) (Language) (FnBody) (Volatility) 
                | CreateLanguage (Annotation) (String) 
                | CreateSequence (Annotation) (String) (Integer) (Integer) (Integer) (Integer) (Integer) 
                | CreateTable (Annotation) (String) (AttributeDefList) (ConstraintList) 
                | CreateTableAs (Annotation) (String) (SelectExpression) 
                | CreateTrigger (Annotation) (String) (TriggerWhen) ([TriggerEvent]) (String) (TriggerFire) (String) (ExpressionList) 
                | CreateType (Annotation) (String) (TypeAttributeDefList) 
                | CreateView (Annotation) (String) (SelectExpression) 
                | Delete (Annotation) (Expression) (TableRefList) (MaybeBoolExpression) (MaybeSelectList) 
                | DropFunction (Annotation) (IfExists) (StringTypeNameListPairList) (Cascade) 
                | DropSomething (Annotation) (DropType) (IfExists) ([String]) (Cascade) 
                | Execute (Annotation) (Expression) 
                | ExecuteInto (Annotation) (Expression) ([String]) 
                | ExitStatement (Annotation) (Maybe String) 
                | ForIntegerStatement (Annotation) (Maybe String) (Expression) (Expression) (Expression) (StatementList) 
                | ForSelectStatement (Annotation) (Maybe String) (Expression) (SelectExpression) (StatementList) 
                | If (Annotation) (ExpressionStatementListPairList) (StatementList) 
                | Insert (Annotation) (Expression) ([String]) (SelectExpression) (MaybeSelectList) 
                | LoopStatement (Annotation) (Maybe String) (StatementList) 
                | Notify (Annotation) (String) 
                | NullStatement (Annotation) 
                | Perform (Annotation) (Expression) 
                | Raise (Annotation) (RaiseType) (String) (ExpressionList) 
                | Return (Annotation) (MaybeExpression) 
                | ReturnNext (Annotation) (Expression) 
                | ReturnQuery (Annotation) (SelectExpression) 
                | SelectStatement (Annotation) (SelectExpression) 
                | Set (Annotation) (String) ([SetValue]) 
                | Truncate (Annotation) ([String]) (RestartIdentity) (Cascade) 
                | Update (Annotation) (Expression) (ExpressionList) (TableRefList) (MaybeBoolExpression) (MaybeSelectList) 
                | WhileStatement (Annotation) (Maybe String) (Expression) (StatementList) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (AlterSequence _ann _name _ownedBy )  =
    (sem_Statement_AlterSequence _ann _name (sem_Expression _ownedBy ) )
sem_Statement (AlterTable _ann _name _actions )  =
    (sem_Statement_AlterTable _ann _name (sem_AlterTableActionList _actions ) )
sem_Statement (Assignment _ann _target _value )  =
    (sem_Statement_Assignment _ann (sem_Expression _target ) (sem_Expression _value ) )
sem_Statement (Block _ann _lb _vars _sts )  =
    (sem_Statement_Block _ann _lb (sem_VarDefList _vars ) (sem_StatementList _sts ) )
sem_Statement (CaseStatement _ann _cases _els )  =
    (sem_Statement_CaseStatement _ann (sem_ExpressionListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (CaseStatementSimple _ann _val _cases _els )  =
    (sem_Statement_CaseStatementSimple _ann (sem_Expression _val ) (sem_ExpressionListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (ContinueStatement _ann _lb )  =
    (sem_Statement_ContinueStatement _ann _lb )
sem_Statement (Copy _ann _table _targetCols _source )  =
    (sem_Statement_Copy _ann _table _targetCols _source )
sem_Statement (CopyData _ann _insData )  =
    (sem_Statement_CopyData _ann _insData )
sem_Statement (CreateDomain _ann _name _typ _checkName _check )  =
    (sem_Statement_CreateDomain _ann _name (sem_TypeName _typ ) _checkName (sem_MaybeBoolExpression _check ) )
sem_Statement (CreateFunction _ann _name _params _rettype _rep _lang _body _vol )  =
    (sem_Statement_CreateFunction _ann _name (sem_ParamDefList _params ) (sem_TypeName _rettype ) _rep _lang (sem_FnBody _body ) _vol )
sem_Statement (CreateLanguage _ann _name )  =
    (sem_Statement_CreateLanguage _ann _name )
sem_Statement (CreateSequence _ann _name _incr _min _max _start _cache )  =
    (sem_Statement_CreateSequence _ann _name _incr _min _max _start _cache )
sem_Statement (CreateTable _ann _name _atts _cons )  =
    (sem_Statement_CreateTable _ann _name (sem_AttributeDefList _atts ) (sem_ConstraintList _cons ) )
sem_Statement (CreateTableAs _ann _name _expr )  =
    (sem_Statement_CreateTableAs _ann _name (sem_SelectExpression _expr ) )
sem_Statement (CreateTrigger _ann _name _wh _events _tbl _firing _fnName _fnArgs )  =
    (sem_Statement_CreateTrigger _ann _name _wh _events _tbl _firing _fnName (sem_ExpressionList _fnArgs ) )
sem_Statement (CreateType _ann _name _atts )  =
    (sem_Statement_CreateType _ann _name (sem_TypeAttributeDefList _atts ) )
sem_Statement (CreateView _ann _name _expr )  =
    (sem_Statement_CreateView _ann _name (sem_SelectExpression _expr ) )
sem_Statement (Delete _ann _table _using _whr _returning )  =
    (sem_Statement_Delete _ann (sem_Expression _table ) (sem_TableRefList _using ) (sem_MaybeBoolExpression _whr ) (sem_MaybeSelectList _returning ) )
sem_Statement (DropFunction _ann _ifE _sigs _cascade )  =
    (sem_Statement_DropFunction _ann _ifE (sem_StringTypeNameListPairList _sigs ) _cascade )
sem_Statement (DropSomething _ann _dropType _ifE _names _cascade )  =
    (sem_Statement_DropSomething _ann _dropType _ifE _names _cascade )
sem_Statement (Execute _ann _expr )  =
    (sem_Statement_Execute _ann (sem_Expression _expr ) )
sem_Statement (ExecuteInto _ann _expr _targets )  =
    (sem_Statement_ExecuteInto _ann (sem_Expression _expr ) _targets )
sem_Statement (ExitStatement _ann _lb )  =
    (sem_Statement_ExitStatement _ann _lb )
sem_Statement (ForIntegerStatement _ann _lb _var _from _to _sts )  =
    (sem_Statement_ForIntegerStatement _ann _lb (sem_Expression _var ) (sem_Expression _from ) (sem_Expression _to ) (sem_StatementList _sts ) )
sem_Statement (ForSelectStatement _ann _lb _var _sel _sts )  =
    (sem_Statement_ForSelectStatement _ann _lb (sem_Expression _var ) (sem_SelectExpression _sel ) (sem_StatementList _sts ) )
sem_Statement (If _ann _cases _els )  =
    (sem_Statement_If _ann (sem_ExpressionStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (Insert _ann _table _targetCols _insData _returning )  =
    (sem_Statement_Insert _ann (sem_Expression _table ) _targetCols (sem_SelectExpression _insData ) (sem_MaybeSelectList _returning ) )
sem_Statement (LoopStatement _ann _lb _sts )  =
    (sem_Statement_LoopStatement _ann _lb (sem_StatementList _sts ) )
sem_Statement (Notify _ann _name )  =
    (sem_Statement_Notify _ann _name )
sem_Statement (NullStatement _ann )  =
    (sem_Statement_NullStatement _ann )
sem_Statement (Perform _ann _expr )  =
    (sem_Statement_Perform _ann (sem_Expression _expr ) )
sem_Statement (Raise _ann _level _message _args )  =
    (sem_Statement_Raise _ann _level _message (sem_ExpressionList _args ) )
sem_Statement (Return _ann _value )  =
    (sem_Statement_Return _ann (sem_MaybeExpression _value ) )
sem_Statement (ReturnNext _ann _expr )  =
    (sem_Statement_ReturnNext _ann (sem_Expression _expr ) )
sem_Statement (ReturnQuery _ann _sel )  =
    (sem_Statement_ReturnQuery _ann (sem_SelectExpression _sel ) )
sem_Statement (SelectStatement _ann _ex )  =
    (sem_Statement_SelectStatement _ann (sem_SelectExpression _ex ) )
sem_Statement (Set _ann _name _values )  =
    (sem_Statement_Set _ann _name _values )
sem_Statement (Truncate _ann _tables _restartIdentity _cascade )  =
    (sem_Statement_Truncate _ann _tables _restartIdentity _cascade )
sem_Statement (Update _ann _table _assigns _fromList _whr _returning )  =
    (sem_Statement_Update _ann (sem_Expression _table ) (sem_ExpressionList _assigns ) (sem_TableRefList _fromList ) (sem_MaybeBoolExpression _whr ) (sem_MaybeSelectList _returning ) )
sem_Statement (WhileStatement _ann _lb _expr _sts )  =
    (sem_Statement_WhileStatement _ann _lb (sem_Expression _expr ) (sem_StatementList _sts ) )
-- semantic domain
type T_Statement  = Catalog ->
                    Catalog ->
                    LocalBindings ->
                    (Maybe LocalBindings) ->
                    ([Maybe LocalBindings]) ->
                    ( Statement,([CatalogUpdate]),([LocalBindingsUpdate]),Statement)
data Inh_Statement  = Inh_Statement {cat_Inh_Statement :: Catalog,inProducedCat_Inh_Statement :: Catalog,lib_Inh_Statement :: LocalBindings,overrideLib_Inh_Statement :: Maybe LocalBindings,overrideLibs_Inh_Statement :: [Maybe LocalBindings]}
data Syn_Statement  = Syn_Statement {annotatedTree_Syn_Statement :: Statement,catUpdates_Syn_Statement :: [CatalogUpdate],libUpdates_Syn_Statement :: [LocalBindingsUpdate],originalTree_Syn_Statement :: Statement}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIcat _lhsIinProducedCat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIinProducedCat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_Statement _lhsOannotatedTree _lhsOcatUpdates _lhsOlibUpdates _lhsOoriginalTree ))
sem_Statement_AlterSequence :: Annotation ->
                               String ->
                               T_Expression  ->
                               T_Statement 
sem_Statement_AlterSequence ann_ name_ ownedBy_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _ownedByOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _ownedByOcat :: Catalog
              _ownedByOlib :: LocalBindings
              _ownedByOoverrideLib :: (Maybe LocalBindings)
              _ownedByOoverrideLibs :: ([Maybe LocalBindings])
              _ownedByIannotatedTree :: Expression
              _ownedByIntAnnotatedTree :: Expression
              _ownedByIntType :: ([(String,Type)])
              _ownedByIoriginalTree :: Expression
              _ownedByItbAnnotatedTree :: Expression
              _ownedByItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _ownedByIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 502, column 21)
              _ownedByOexpectedType =
                  {-# LINE 502 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 10279 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10284 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10289 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10294 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  AlterSequence ann_ name_ _ownedByIannotatedTree
                  {-# LINE 10299 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  AlterSequence ann_ name_ _ownedByIoriginalTree
                  {-# LINE 10304 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10309 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10314 "AstInternal.hs" #-}
              -- copy rule (down)
              _ownedByOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10319 "AstInternal.hs" #-}
              -- copy rule (down)
              _ownedByOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10324 "AstInternal.hs" #-}
              -- copy rule (down)
              _ownedByOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10329 "AstInternal.hs" #-}
              -- copy rule (down)
              _ownedByOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10334 "AstInternal.hs" #-}
              ( _ownedByIannotatedTree,_ownedByIntAnnotatedTree,_ownedByIntType,_ownedByIoriginalTree,_ownedByItbAnnotatedTree,_ownedByItbUType,_ownedByIuType) =
                  (ownedBy_ _ownedByOcat _ownedByOexpectedType _ownedByOlib _ownedByOoverrideLib _ownedByOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_AlterTable :: Annotation ->
                            String ->
                            T_AlterTableActionList  ->
                            T_Statement 
sem_Statement_AlterTable ann_ name_ actions_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _actionsOcat :: Catalog
              _actionsOlib :: LocalBindings
              _actionsOoverrideLib :: (Maybe LocalBindings)
              _actionsOoverrideLibs :: ([Maybe LocalBindings])
              _actionsIannotatedTree :: AlterTableActionList
              _actionsIoriginalTree :: AlterTableActionList
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10362 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10367 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  AlterTable ann_ name_ _actionsIannotatedTree
                  {-# LINE 10372 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  AlterTable ann_ name_ _actionsIoriginalTree
                  {-# LINE 10377 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10382 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10387 "AstInternal.hs" #-}
              -- copy rule (down)
              _actionsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10392 "AstInternal.hs" #-}
              -- copy rule (down)
              _actionsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10397 "AstInternal.hs" #-}
              -- copy rule (down)
              _actionsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10402 "AstInternal.hs" #-}
              -- copy rule (down)
              _actionsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10407 "AstInternal.hs" #-}
              ( _actionsIannotatedTree,_actionsIoriginalTree) =
                  (actions_ _actionsOcat _actionsOlib _actionsOoverrideLib _actionsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Assignment :: Annotation ->
                            T_Expression  ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_Assignment ann_ target_ value_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _valueOexpectedType :: (Maybe Type)
              _targetOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _lhsOoriginalTree :: Statement
              _targetOcat :: Catalog
              _targetOlib :: LocalBindings
              _targetOoverrideLib :: (Maybe LocalBindings)
              _targetOoverrideLibs :: ([Maybe LocalBindings])
              _valueOcat :: Catalog
              _valueOlib :: LocalBindings
              _valueOoverrideLib :: (Maybe LocalBindings)
              _valueOoverrideLibs :: ([Maybe LocalBindings])
              _targetIannotatedTree :: Expression
              _targetIntAnnotatedTree :: Expression
              _targetIntType :: ([(String,Type)])
              _targetIoriginalTree :: Expression
              _targetItbAnnotatedTree :: Expression
              _targetItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _targetIuType :: (Maybe Type)
              _valueIannotatedTree :: Expression
              _valueIntAnnotatedTree :: Expression
              _valueIntType :: ([(String,Type)])
              _valueIoriginalTree :: Expression
              _valueItbAnnotatedTree :: Expression
              _valueItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _valueIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 496, column 18)
              _valueOexpectedType =
                  {-# LINE 496 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 10456 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 503, column 18)
              _targetOexpectedType =
                  {-# LINE 503 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 10461 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 10469 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 10474 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10479 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10484 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 20, column 9)
              _tpe =
                  {-# LINE 20 "./TypeChecking/Plpgsql.ag" #-}
                  do
                  fromType <- lmt _valueIuType
                  toType <- unwrapLookup <$> lbLookupID _lhsIlib (getName _targetIannotatedTree)
                  checkAssignmentValid _lhsIcat fromType toType
                  return $ Pseudo Void
                  {-# LINE 10493 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 26, column 9)
              _backTree =
                  {-# LINE 26 "./TypeChecking/Plpgsql.ag" #-}
                  Assignment ann_ _targetIannotatedTree _valueIannotatedTree
                  {-# LINE 10498 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 27, column 9)
              _catUpdates =
                  {-# LINE 27 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 10503 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 28, column 9)
              _statementType =
                  {-# LINE 28 "./TypeChecking/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 10508 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Assignment ann_ _targetIannotatedTree _valueIannotatedTree
                  {-# LINE 10513 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Assignment ann_ _targetIoriginalTree _valueIoriginalTree
                  {-# LINE 10518 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10523 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10528 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10533 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10538 "AstInternal.hs" #-}
              -- copy rule (down)
              _targetOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10543 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10548 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10553 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10558 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10563 "AstInternal.hs" #-}
              ( _targetIannotatedTree,_targetIntAnnotatedTree,_targetIntType,_targetIoriginalTree,_targetItbAnnotatedTree,_targetItbUType,_targetIuType) =
                  (target_ _targetOcat _targetOexpectedType _targetOlib _targetOoverrideLib _targetOoverrideLibs )
              ( _valueIannotatedTree,_valueIntAnnotatedTree,_valueIntType,_valueIoriginalTree,_valueItbAnnotatedTree,_valueItbUType,_valueIuType) =
                  (value_ _valueOcat _valueOexpectedType _valueOlib _valueOoverrideLib _valueOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Block :: Annotation ->
                       (Maybe String) ->
                       T_VarDefList  ->
                       T_StatementList  ->
                       T_Statement 
sem_Statement_Block ann_ lb_ vars_ sts_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlib :: LocalBindings
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _varsOcat :: Catalog
              _varsOlib :: LocalBindings
              _varsOoverrideLib :: (Maybe LocalBindings)
              _varsOoverrideLibs :: ([Maybe LocalBindings])
              _stsOcat :: Catalog
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOoverrideLib :: (Maybe LocalBindings)
              _stsOoverrideLibs :: ([Maybe LocalBindings])
              _varsIannotatedTree :: VarDefList
              _varsIdefs :: ([(String,Maybe Type)])
              _varsIoriginalTree :: VarDefList
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10605 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 100, column 13)
              _lhsOcatUpdates =
                  {-# LINE 100 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10610 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 101, column 13)
              _stsOcatUpdates =
                  {-# LINE 101 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10615 "AstInternal.hs" #-}
              -- "./TypeChecking/Block.ag"(line 20, column 9)
              _stsOlib =
                  {-# LINE 20 "./TypeChecking/Block.ag" #-}
                  fromRight _lhsIlib $
                  lbUpdate _lhsIcat
                           (LBIds "declarations" lb_ $ mapMaybe lv _varsIdefs)
                           _lhsIlib
                  where
                    lv (_,Nothing) = Nothing
                    lv (s,Just t) = Just (s,t)
                  {-# LINE 10626 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Block ann_ lb_ _varsIannotatedTree _stsIannotatedTree
                  {-# LINE 10631 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Block ann_ lb_ _varsIoriginalTree _stsIoriginalTree
                  {-# LINE 10636 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10641 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10646 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOlibUpdates =
                  {-# LINE 19 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10651 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10656 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10661 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10666 "AstInternal.hs" #-}
              -- copy rule (down)
              _varsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10671 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10676 "AstInternal.hs" #-}
              -- copy rule (from local)
              _stsOlibUpdates =
                  {-# LINE 23 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 10681 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10686 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10691 "AstInternal.hs" #-}
              ( _varsIannotatedTree,_varsIdefs,_varsIoriginalTree) =
                  (vars_ _varsOcat _varsOlib _varsOoverrideLib _varsOoverrideLibs )
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOlib _stsOlibUpdates _stsOoverrideLib _stsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CaseStatement :: Annotation ->
                               T_ExpressionListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _elsOcatUpdates :: ([CatalogUpdate])
              _elsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _casesOcat :: Catalog
              _casesOlib :: LocalBindings
              _casesOoverrideLib :: (Maybe LocalBindings)
              _casesOoverrideLibs :: ([Maybe LocalBindings])
              _elsOcat :: Catalog
              _elsOlib :: LocalBindings
              _elsOoverrideLib :: (Maybe LocalBindings)
              _elsOoverrideLibs :: ([Maybe LocalBindings])
              _casesIannotatedTree :: ExpressionListStatementListPairList
              _casesIoriginalTree :: ExpressionListStatementListPairList
              _elsIannotatedTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedCat :: Catalog
              _elsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10731 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10736 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  {-# LINE 134 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10741 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  {-# LINE 135 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10746 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatement ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 10751 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatement ann_ _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 10756 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10761 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10766 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10771 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10776 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10781 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10786 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10791 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10796 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10801 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10806 "AstInternal.hs" #-}
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  (cases_ _casesOcat _casesOlib _casesOoverrideLib _casesOoverrideLibs )
              ( _elsIannotatedTree,_elsIoriginalTree,_elsIproducedCat,_elsIproducedLib) =
                  (els_ _elsOcat _elsOcatUpdates _elsOlib _elsOlibUpdates _elsOoverrideLib _elsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CaseStatementSimple :: Annotation ->
                                     T_Expression  ->
                                     T_ExpressionListStatementListPairList  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_CaseStatementSimple ann_ val_ cases_ els_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _valOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _elsOcatUpdates :: ([CatalogUpdate])
              _elsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _valOcat :: Catalog
              _valOlib :: LocalBindings
              _valOoverrideLib :: (Maybe LocalBindings)
              _valOoverrideLibs :: ([Maybe LocalBindings])
              _casesOcat :: Catalog
              _casesOlib :: LocalBindings
              _casesOoverrideLib :: (Maybe LocalBindings)
              _casesOoverrideLibs :: ([Maybe LocalBindings])
              _elsOcat :: Catalog
              _elsOlib :: LocalBindings
              _elsOoverrideLib :: (Maybe LocalBindings)
              _elsOoverrideLibs :: ([Maybe LocalBindings])
              _valIannotatedTree :: Expression
              _valIntAnnotatedTree :: Expression
              _valIntType :: ([(String,Type)])
              _valIoriginalTree :: Expression
              _valItbAnnotatedTree :: Expression
              _valItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _valIuType :: (Maybe Type)
              _casesIannotatedTree :: ExpressionListStatementListPairList
              _casesIoriginalTree :: ExpressionListStatementListPairList
              _elsIannotatedTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedCat :: Catalog
              _elsIproducedLib :: LocalBindings
              -- "./TypeChecking/Expressions.ag"(line 497, column 27)
              _valOexpectedType =
                  {-# LINE 497 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 10859 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10864 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10869 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  {-# LINE 134 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10874 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  {-# LINE 135 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10879 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatementSimple ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 10884 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CaseStatementSimple ann_ _valIoriginalTree _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 10889 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 10894 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 10899 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10904 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10909 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10914 "AstInternal.hs" #-}
              -- copy rule (down)
              _valOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10919 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10924 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10929 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10934 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10939 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 10944 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 10949 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 10954 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 10959 "AstInternal.hs" #-}
              ( _valIannotatedTree,_valIntAnnotatedTree,_valIntType,_valIoriginalTree,_valItbAnnotatedTree,_valItbUType,_valIuType) =
                  (val_ _valOcat _valOexpectedType _valOlib _valOoverrideLib _valOoverrideLibs )
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  (cases_ _casesOcat _casesOlib _casesOoverrideLib _casesOoverrideLibs )
              ( _elsIannotatedTree,_elsIoriginalTree,_elsIproducedCat,_elsIproducedLib) =
                  (els_ _elsOcat _elsOcatUpdates _elsOlib _elsOlibUpdates _elsOoverrideLib _elsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ContinueStatement :: Annotation ->
                                   (Maybe String) ->
                                   T_Statement 
sem_Statement_ContinueStatement ann_ lb_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10984 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 10989 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ContinueStatement ann_ lb_
                  {-# LINE 10994 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ContinueStatement ann_ lb_
                  {-# LINE 10999 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11004 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11009 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Copy :: Annotation ->
                      String ->
                      ([String]) ->
                      CopySource ->
                      T_Statement 
sem_Statement_Copy ann_ table_ targetCols_ source_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11030 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11035 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Copy ann_ table_ targetCols_ source_
                  {-# LINE 11040 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Copy ann_ table_ targetCols_ source_
                  {-# LINE 11045 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11050 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11055 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CopyData :: Annotation ->
                          String ->
                          T_Statement 
sem_Statement_CopyData ann_ insData_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11074 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11079 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 11084 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CopyData ann_ insData_
                  {-# LINE 11089 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11094 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11099 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateDomain :: Annotation ->
                              String ->
                              T_TypeName  ->
                              String ->
                              T_MaybeBoolExpression  ->
                              T_Statement 
sem_Statement_CreateDomain ann_ name_ typ_ checkName_ check_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _checkOlib :: LocalBindings
              _lhsOoriginalTree :: Statement
              _typOcat :: Catalog
              _typOlib :: LocalBindings
              _typOoverrideLib :: (Maybe LocalBindings)
              _typOoverrideLibs :: ([Maybe LocalBindings])
              _checkOcat :: Catalog
              _checkOoverrideLib :: (Maybe LocalBindings)
              _checkOoverrideLibs :: ([Maybe LocalBindings])
              _typIannotatedTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              _checkIannotatedTree :: MaybeBoolExpression
              _checkIoriginalTree :: MaybeBoolExpression
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11140 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11145 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11150 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11155 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 65, column 9)
              _tpe =
                  {-# LINE 65 "./TypeChecking/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11160 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 66, column 9)
              _backTree =
                  {-# LINE 66 "./TypeChecking/MiscCreates.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree checkName_ _checkIannotatedTree
                  {-# LINE 11165 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 67, column 9)
              _statementType =
                  {-# LINE 67 "./TypeChecking/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 11170 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 68, column 9)
              _catUpdates =
                  {-# LINE 68 "./TypeChecking/MiscCreates.ag" #-}
                  maybe [] (\t -> [CatCreateDomain (DomainType name_) t]) _typInamedType
                  {-# LINE 11175 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 70, column 9)
              _checkOlib =
                  {-# LINE 70 "./TypeChecking/MiscCreates.ag" #-}
                  either (const _lhsIlib) id $ do
                  nt <- lmt _typInamedType
                  lbUpdate _lhsIcat
                    (LBIds "domain check value" Nothing [("value", nt)])
                    _lhsIlib
                  {-# LINE 11184 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIannotatedTree checkName_ _checkIannotatedTree
                  {-# LINE 11189 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateDomain ann_ name_ _typIoriginalTree checkName_ _checkIoriginalTree
                  {-# LINE 11194 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11199 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11204 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11209 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 11214 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 11219 "AstInternal.hs" #-}
              -- copy rule (down)
              _checkOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11224 "AstInternal.hs" #-}
              -- copy rule (down)
              _checkOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 11229 "AstInternal.hs" #-}
              -- copy rule (down)
              _checkOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 11234 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOlib _typOoverrideLib _typOoverrideLibs )
              ( _checkIannotatedTree,_checkIoriginalTree) =
                  (check_ _checkOcat _checkOlib _checkOoverrideLib _checkOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
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
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _bodyOlib :: LocalBindings
              _paramsOpos :: Int
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _bodyOcat :: Catalog
              _lhsOoriginalTree :: Statement
              _paramsOcat :: Catalog
              _paramsOlib :: LocalBindings
              _paramsOoverrideLib :: (Maybe LocalBindings)
              _paramsOoverrideLibs :: ([Maybe LocalBindings])
              _rettypeOcat :: Catalog
              _rettypeOlib :: LocalBindings
              _rettypeOoverrideLib :: (Maybe LocalBindings)
              _rettypeOoverrideLibs :: ([Maybe LocalBindings])
              _bodyOoverrideLib :: (Maybe LocalBindings)
              _bodyOoverrideLibs :: ([Maybe LocalBindings])
              _paramsIannotatedTree :: ParamDefList
              _paramsIoriginalTree :: ParamDefList
              _paramsIparams :: ([(ParamName, Maybe Type)])
              _rettypeIannotatedTree :: TypeName
              _rettypeInamedType :: (Maybe Type)
              _rettypeIoriginalTree :: TypeName
              _bodyIannotatedTree :: FnBody
              _bodyIoriginalTree :: FnBody
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11290 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11295 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11300 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11305 "AstInternal.hs" #-}
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
                  {-# LINE 11325 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 79, column 9)
              _paramsOpos =
                  {-# LINE 79 "./TypeChecking/CreateFunction.ag" #-}
                  1
                  {-# LINE 11330 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 88, column 9)
              _tpe =
                  {-# LINE 88 "./TypeChecking/CreateFunction.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11335 "AstInternal.hs" #-}
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
                  {-# LINE 11350 "AstInternal.hs" #-}
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
                  {-# LINE 11362 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 109, column 9)
              _statementType =
                  {-# LINE 109 "./TypeChecking/CreateFunction.ag" #-}
                  Nothing
                  {-# LINE 11367 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateFunction.ag"(line 110, column 9)
              _bodyOcat =
                  {-# LINE 110 "./TypeChecking/CreateFunction.ag" #-}
                  _lhsIinProducedCat
                  {-# LINE 11372 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CreateFunction ann_ name_ _paramsIannotatedTree _rettypeIannotatedTree rep_ lang_ _bodyIannotatedTree vol_
                  {-# LINE 11377 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateFunction ann_ name_ _paramsIoriginalTree _rettypeIoriginalTree rep_ lang_ _bodyIoriginalTree vol_
                  {-# LINE 11382 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11387 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11392 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11397 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 11402 "AstInternal.hs" #-}
              -- copy rule (down)
              _paramsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 11407 "AstInternal.hs" #-}
              -- copy rule (down)
              _rettypeOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11412 "AstInternal.hs" #-}
              -- copy rule (down)
              _rettypeOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11417 "AstInternal.hs" #-}
              -- copy rule (down)
              _rettypeOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 11422 "AstInternal.hs" #-}
              -- copy rule (down)
              _rettypeOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 11427 "AstInternal.hs" #-}
              -- copy rule (down)
              _bodyOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 11432 "AstInternal.hs" #-}
              -- copy rule (down)
              _bodyOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 11437 "AstInternal.hs" #-}
              ( _paramsIannotatedTree,_paramsIoriginalTree,_paramsIparams) =
                  (params_ _paramsOcat _paramsOlib _paramsOoverrideLib _paramsOoverrideLibs _paramsOpos )
              ( _rettypeIannotatedTree,_rettypeInamedType,_rettypeIoriginalTree) =
                  (rettype_ _rettypeOcat _rettypeOlib _rettypeOoverrideLib _rettypeOoverrideLibs )
              ( _bodyIannotatedTree,_bodyIoriginalTree) =
                  (body_ _bodyOcat _bodyOlib _bodyOoverrideLib _bodyOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateLanguage :: Annotation ->
                                String ->
                                T_Statement 
sem_Statement_CreateLanguage ann_ name_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11468 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11473 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11478 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11483 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 78, column 9)
              _tpe =
                  {-# LINE 78 "./TypeChecking/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11488 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 79, column 9)
              _backTree =
                  {-# LINE 79 "./TypeChecking/MiscCreates.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11493 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 80, column 9)
              _statementType =
                  {-# LINE 80 "./TypeChecking/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 11498 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 81, column 9)
              _catUpdates =
                  {-# LINE 81 "./TypeChecking/MiscCreates.ag" #-}
                  [CatCreateFunction FunName "plpgsql_call_handler" [] (Pseudo LanguageHandler) False
                  ,CatCreateFunction FunName "plpgsql_validator" [ScalarType "oid"] (Pseudo Void) False]
                  {-# LINE 11504 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11509 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateLanguage ann_ name_
                  {-# LINE 11514 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11519 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
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
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11543 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11548 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11553 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
                  {-# LINE 11558 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
                  {-# LINE 11563 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11568 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11573 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateTable :: Annotation ->
                             String ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable ann_ name_ atts_ cons_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _attrs :: ([(String,Type)])
              _statementType :: (Maybe StatementType)
              _consOlib :: LocalBindings
              _lhsOoriginalTree :: Statement
              _attsOcat :: Catalog
              _attsOlib :: LocalBindings
              _attsOoverrideLib :: (Maybe LocalBindings)
              _attsOoverrideLibs :: ([Maybe LocalBindings])
              _consOcat :: Catalog
              _consOoverrideLib :: (Maybe LocalBindings)
              _consOoverrideLibs :: ([Maybe LocalBindings])
              _attsIannotatedTree :: AttributeDefList
              _attsIattrs :: ([(String, Maybe Type)])
              _attsIoriginalTree :: AttributeDefList
              _consIannotatedTree :: ConstraintList
              _consIoriginalTree :: ConstraintList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11614 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11619 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11624 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11629 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 31, column 9)
              _tpe =
                  {-# LINE 31 "./TypeChecking/CreateTable.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11634 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 32, column 9)
              _catUpdates =
                  {-# LINE 32 "./TypeChecking/CreateTable.ag" #-}
                  [CatCreateTable name_ _attrs     defaultSystemColumns]
                  {-# LINE 11639 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 35, column 9)
              _attrs =
                  {-# LINE 35 "./TypeChecking/CreateTable.ag" #-}
                  mapMaybe okAt _attsIattrs
                  where
                    okAt (s, Just t) = Just (s,t)
                    okAt (_,Nothing) = Nothing
                  {-# LINE 11647 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 40, column 9)
              _statementType =
                  {-# LINE 40 "./TypeChecking/CreateTable.ag" #-}
                  Nothing
                  {-# LINE 11652 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 41, column 9)
              _backTree =
                  {-# LINE 41 "./TypeChecking/CreateTable.ag" #-}
                  CreateTable ann_
                              name_
                              _attsIannotatedTree
                              _consIannotatedTree
                  {-# LINE 11660 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 45, column 9)
              _consOlib =
                  {-# LINE 45 "./TypeChecking/CreateTable.ag" #-}
                  case lbUpdate _lhsIcat
                         (LBIds "attributedefs" Nothing _attrs    )
                         _lhsIlib of
                     Left x -> error $ "statement-createtable-cons.lib " ++ show x
                     Right e -> e
                  {-# LINE 11669 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIannotatedTree _consIannotatedTree
                  {-# LINE 11674 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTable ann_ name_ _attsIoriginalTree _consIoriginalTree
                  {-# LINE 11679 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11684 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11689 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11694 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 11699 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 11704 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11709 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 11714 "AstInternal.hs" #-}
              -- copy rule (down)
              _consOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 11719 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIattrs,_attsIoriginalTree) =
                  (atts_ _attsOcat _attsOlib _attsOoverrideLib _attsOoverrideLibs )
              ( _consIannotatedTree,_consIoriginalTree) =
                  (cons_ _consOcat _consOlib _consOoverrideLib _consOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateTableAs :: Annotation ->
                               String ->
                               T_SelectExpression  ->
                               T_Statement 
sem_Statement_CreateTableAs ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exprOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _attrs :: (Either [TypeError] [(String,Type)])
              _statementType :: (Maybe StatementType)
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: SelectExpression
              _exprIlibUpdates :: ([LocalBindingsUpdate])
              _exprIoriginalTree :: SelectExpression
              _exprIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Expressions.ag"(line 562, column 32)
              _exprOexpectedTypes =
                  {-# LINE 562 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 11756 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11764 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11769 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11774 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11779 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 64, column 9)
              _tpe =
                  {-# LINE 64 "./TypeChecking/CreateTable.ag" #-}
                  CompositeType <$> lmt _exprIuType
                  {-# LINE 11784 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 65, column 9)
              _catUpdates =
                  {-# LINE 65 "./TypeChecking/CreateTable.ag" #-}
                  either (const []) id $ do
                  ats <- _attrs
                  return [CatCreateTable name_ ats defaultSystemColumns]
                  {-# LINE 11791 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 71, column 9)
              _attrs =
                  {-# LINE 71 "./TypeChecking/CreateTable.ag" #-}
                  lmt _exprIuType
                  {-# LINE 11796 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 73, column 9)
              _backTree =
                  {-# LINE 73 "./TypeChecking/CreateTable.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 11801 "AstInternal.hs" #-}
              -- "./TypeChecking/CreateTable.ag"(line 74, column 9)
              _statementType =
                  {-# LINE 74 "./TypeChecking/CreateTable.ag" #-}
                  Nothing
                  {-# LINE 11806 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIannotatedTree
                  {-# LINE 11811 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTableAs ann_ name_ _exprIoriginalTree
                  {-# LINE 11816 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11821 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11826 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11831 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 11836 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 11841 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIlibUpdates,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedTypes _exprOlib _exprOoverrideLib _exprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateTrigger :: Annotation ->
                               String ->
                               TriggerWhen ->
                               ([TriggerEvent]) ->
                               String ->
                               TriggerFire ->
                               String ->
                               T_ExpressionList  ->
                               T_Statement 
sem_Statement_CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ fnArgs_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _fnArgsOexpectedTypes :: ([Maybe Type])
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _fnArgsOcat :: Catalog
              _fnArgsOlib :: LocalBindings
              _fnArgsOoverrideLib :: (Maybe LocalBindings)
              _fnArgsOoverrideLibs :: ([Maybe LocalBindings])
              _fnArgsIannotatedTree :: ExpressionList
              _fnArgsIoriginalTree :: ExpressionList
              _fnArgsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _fnArgsIuType :: ([Maybe Type])
              -- "./TypeChecking/Expressions.ag"(line 540, column 21)
              _fnArgsOexpectedTypes =
                  {-# LINE 540 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 11877 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11882 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11887 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIannotatedTree
                  {-# LINE 11892 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIoriginalTree
                  {-# LINE 11897 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 11902 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 11907 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnArgsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 11912 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnArgsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 11917 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnArgsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 11922 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnArgsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 11927 "AstInternal.hs" #-}
              ( _fnArgsIannotatedTree,_fnArgsIoriginalTree,_fnArgsItbUTypes,_fnArgsIuType) =
                  (fnArgs_ _fnArgsOcat _fnArgsOexpectedTypes _fnArgsOlib _fnArgsOoverrideLib _fnArgsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateType :: Annotation ->
                            String ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType ann_ name_ atts_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _lhsOoriginalTree :: Statement
              _attsOcat :: Catalog
              _attsOlib :: LocalBindings
              _attsOoverrideLib :: (Maybe LocalBindings)
              _attsOoverrideLibs :: ([Maybe LocalBindings])
              _attsIannotatedTree :: TypeAttributeDefList
              _attsIattrs :: ([(String, Maybe Type)])
              _attsIoriginalTree :: TypeAttributeDefList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 11962 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 11967 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 11972 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 11977 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 48, column 9)
              _tpe =
                  {-# LINE 48 "./TypeChecking/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 11982 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 49, column 9)
              _attrs =
                  {-# LINE 49 "./TypeChecking/MiscCreates.ag" #-}
                  mapMaybe okAt _attsIattrs
                  where
                    okAt (s, Just t) = Just (s,t)
                    okAt (_,Nothing) = Nothing
                  {-# LINE 11990 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 53, column 9)
              _backTree =
                  {-# LINE 53 "./TypeChecking/MiscCreates.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 11995 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 54, column 9)
              _statementType =
                  {-# LINE 54 "./TypeChecking/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 12000 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 55, column 9)
              _catUpdates =
                  {-# LINE 55 "./TypeChecking/MiscCreates.ag" #-}
                  [CatCreateComposite name_ _attrs    ]
                  {-# LINE 12005 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIannotatedTree
                  {-# LINE 12010 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateType ann_ name_ _attsIoriginalTree
                  {-# LINE 12015 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12020 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12025 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12030 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12035 "AstInternal.hs" #-}
              -- copy rule (down)
              _attsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12040 "AstInternal.hs" #-}
              ( _attsIannotatedTree,_attsIattrs,_attsIoriginalTree) =
                  (atts_ _attsOcat _attsOlib _attsOoverrideLib _attsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_CreateView :: Annotation ->
                            String ->
                            T_SelectExpression  ->
                            T_Statement 
sem_Statement_CreateView ann_ name_ expr_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exprOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: SelectExpression
              _exprIlibUpdates :: ([LocalBindingsUpdate])
              _exprIoriginalTree :: SelectExpression
              _exprIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Expressions.ag"(line 562, column 32)
              _exprOexpectedTypes =
                  {-# LINE 562 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 12074 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 12082 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12087 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12092 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12097 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 15, column 9)
              _tpe =
                  {-# LINE 15 "./TypeChecking/MiscCreates.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12102 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 16, column 9)
              _backTree =
                  {-# LINE 16 "./TypeChecking/MiscCreates.ag" #-}
                  CreateView ann_ name_ _exprIannotatedTree
                  {-# LINE 12107 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 17, column 9)
              _catUpdates =
                  {-# LINE 17 "./TypeChecking/MiscCreates.ag" #-}
                  maybe [] (\a -> [CatCreateView name_ a]) _exprIuType
                  {-# LINE 12112 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 19, column 9)
              _statementType =
                  {-# LINE 19 "./TypeChecking/MiscCreates.ag" #-}
                  Nothing
                  {-# LINE 12117 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  CreateView ann_ name_ _exprIannotatedTree
                  {-# LINE 12122 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  CreateView ann_ name_ _exprIoriginalTree
                  {-# LINE 12127 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12132 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12137 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12142 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12147 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12152 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIlibUpdates,_exprIoriginalTree,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedTypes _exprOlib _exprOoverrideLib _exprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Delete :: Annotation ->
                        T_Expression  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpression  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Delete ann_ table_ using_ whr_ returning_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _tableOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _whrOlib :: LocalBindings
              _returningOlib :: LocalBindings
              _lhsOoriginalTree :: Statement
              _tableOcat :: Catalog
              _tableOlib :: LocalBindings
              _tableOoverrideLib :: (Maybe LocalBindings)
              _tableOoverrideLibs :: ([Maybe LocalBindings])
              _usingOcat :: Catalog
              _usingOlib :: LocalBindings
              _usingOoverrideLib :: (Maybe LocalBindings)
              _usingOoverrideLibs :: ([Maybe LocalBindings])
              _whrOcat :: Catalog
              _whrOoverrideLib :: (Maybe LocalBindings)
              _whrOoverrideLibs :: ([Maybe LocalBindings])
              _returningOcat :: Catalog
              _returningOoverrideLib :: (Maybe LocalBindings)
              _returningOoverrideLibs :: ([Maybe LocalBindings])
              _tableIannotatedTree :: Expression
              _tableIntAnnotatedTree :: Expression
              _tableIntType :: ([(String,Type)])
              _tableIoriginalTree :: Expression
              _tableItbAnnotatedTree :: Expression
              _tableItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tableIuType :: (Maybe Type)
              _usingIannotatedTree :: TableRefList
              _usingIlibUpdates :: ([LocalBindingsUpdate])
              _usingIoriginalTree :: TableRefList
              _whrIannotatedTree :: MaybeBoolExpression
              _whrIoriginalTree :: MaybeBoolExpression
              _returningIannotatedTree :: MaybeSelectList
              _returningIlistType :: ([(String,Type)])
              _returningIoriginalTree :: MaybeSelectList
              -- "./TypeChecking/Expressions.ag"(line 573, column 28)
              _tableOexpectedType =
                  {-# LINE 573 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 12211 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 12219 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12224 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12229 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12234 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 13, column 9)
              _tpe =
                  {-# LINE 13 "./TypeChecking/Delete.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12239 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 14, column 9)
              _statementType =
                  {-# LINE 14 "./TypeChecking/Delete.ag" #-}
                  do
                  pt <- sequence $ getPlaceholderTypes _whrIannotatedTree
                  return (pt,_returningIlistType)
                  {-# LINE 12246 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 18, column 9)
              _backTree =
                  {-# LINE 18 "./TypeChecking/Delete.ag" #-}
                  Delete ann_ _tableItbAnnotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
                  {-# LINE 12251 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 19, column 9)
              _catUpdates =
                  {-# LINE 19 "./TypeChecking/Delete.ag" #-}
                  []
                  {-# LINE 12256 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 21, column 9)
              _lib =
                  {-# LINE 21 "./TypeChecking/Delete.ag" #-}
                  either (const _lhsIlib) id $ do
                  a <- lmt (allAtts <$> _tableItbUType)
                  lbUpdate _lhsIcat (LBIds "delete table attrs" (Just $ getName _tableIannotatedTree) a) _lhsIlib
                  {-# LINE 12263 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 25, column 9)
              _whrOlib =
                  {-# LINE 25 "./TypeChecking/Delete.ag" #-}
                  _lib
                  {-# LINE 12268 "AstInternal.hs" #-}
              -- "./TypeChecking/Delete.ag"(line 26, column 9)
              _returningOlib =
                  {-# LINE 26 "./TypeChecking/Delete.ag" #-}
                  _lib
                  {-# LINE 12273 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Delete ann_ _tableIannotatedTree _usingIannotatedTree _whrIannotatedTree _returningIannotatedTree
                  {-# LINE 12278 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Delete ann_ _tableIoriginalTree _usingIoriginalTree _whrIoriginalTree _returningIoriginalTree
                  {-# LINE 12283 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12288 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12293 "AstInternal.hs" #-}
              -- copy rule (from local)
              _tableOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 12298 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12303 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12308 "AstInternal.hs" #-}
              -- copy rule (down)
              _usingOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12313 "AstInternal.hs" #-}
              -- copy rule (from local)
              _usingOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 12318 "AstInternal.hs" #-}
              -- copy rule (down)
              _usingOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12323 "AstInternal.hs" #-}
              -- copy rule (down)
              _usingOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12328 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12333 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12338 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12343 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12348 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12353 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12358 "AstInternal.hs" #-}
              ( _tableIannotatedTree,_tableIntAnnotatedTree,_tableIntType,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType,_tableIuType) =
                  (table_ _tableOcat _tableOexpectedType _tableOlib _tableOoverrideLib _tableOoverrideLibs )
              ( _usingIannotatedTree,_usingIlibUpdates,_usingIoriginalTree) =
                  (using_ _usingOcat _usingOlib _usingOoverrideLib _usingOoverrideLibs )
              ( _whrIannotatedTree,_whrIoriginalTree) =
                  (whr_ _whrOcat _whrOlib _whrOoverrideLib _whrOoverrideLibs )
              ( _returningIannotatedTree,_returningIlistType,_returningIoriginalTree) =
                  (returning_ _returningOcat _returningOlib _returningOoverrideLib _returningOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_DropFunction :: Annotation ->
                              IfExists ->
                              T_StringTypeNameListPairList  ->
                              Cascade ->
                              T_Statement 
sem_Statement_DropFunction ann_ ifE_ sigs_ cascade_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _lhsOoriginalTree :: Statement
              _sigsOcat :: Catalog
              _sigsOlib :: LocalBindings
              _sigsOoverrideLib :: (Maybe LocalBindings)
              _sigsOoverrideLibs :: ([Maybe LocalBindings])
              _sigsIannotatedTree :: StringTypeNameListPairList
              _sigsIfnSigs :: ([(String,[Maybe Type])])
              _sigsIoriginalTree :: StringTypeNameListPairList
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 12400 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12405 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12410 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12415 "AstInternal.hs" #-}
              -- "./TypeChecking/Drops.ag"(line 10, column 9)
              _tpe =
                  {-# LINE 10 "./TypeChecking/Drops.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 12420 "AstInternal.hs" #-}
              -- "./TypeChecking/Drops.ag"(line 11, column 9)
              _backTree =
                  {-# LINE 11 "./TypeChecking/Drops.ag" #-}
                  DropFunction ann_ ifE_ _sigsIannotatedTree cascade_
                  {-# LINE 12425 "AstInternal.hs" #-}
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
                  {-# LINE 12439 "AstInternal.hs" #-}
              -- "./TypeChecking/Drops.ag"(line 23, column 9)
              _statementType =
                  {-# LINE 23 "./TypeChecking/Drops.ag" #-}
                  Nothing
                  {-# LINE 12444 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  DropFunction ann_ ifE_ _sigsIannotatedTree cascade_
                  {-# LINE 12449 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  DropFunction ann_ ifE_ _sigsIoriginalTree cascade_
                  {-# LINE 12454 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12459 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12464 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12469 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12474 "AstInternal.hs" #-}
              -- copy rule (down)
              _sigsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12479 "AstInternal.hs" #-}
              ( _sigsIannotatedTree,_sigsIfnSigs,_sigsIoriginalTree) =
                  (sigs_ _sigsOcat _sigsOlib _sigsOoverrideLib _sigsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_DropSomething :: Annotation ->
                               DropType ->
                               IfExists ->
                               ([String]) ->
                               Cascade ->
                               T_Statement 
sem_Statement_DropSomething ann_ dropType_ ifE_ names_ cascade_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12503 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12508 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
                  {-# LINE 12513 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
                  {-# LINE 12518 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12523 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12528 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Execute :: Annotation ->
                         T_Expression  ->
                         T_Statement 
sem_Statement_Execute ann_ expr_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 499, column 9)
              _exprOexpectedType =
                  {-# LINE 499 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 12559 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12564 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12569 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Execute ann_ _exprIannotatedTree
                  {-# LINE 12574 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Execute ann_ _exprIoriginalTree
                  {-# LINE 12579 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12584 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12589 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12594 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12599 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12604 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12609 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ExecuteInto :: Annotation ->
                             T_Expression  ->
                             ([String]) ->
                             T_Statement 
sem_Statement_ExecuteInto ann_ expr_ targets_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 499, column 9)
              _exprOexpectedType =
                  {-# LINE 499 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 12643 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12648 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12653 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIannotatedTree targets_
                  {-# LINE 12658 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ExecuteInto ann_ _exprIoriginalTree targets_
                  {-# LINE 12663 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12668 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12673 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12678 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12683 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12688 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12693 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ExitStatement :: Annotation ->
                               (Maybe String) ->
                               T_Statement 
sem_Statement_ExitStatement ann_ lb_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12714 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12719 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ExitStatement ann_ lb_
                  {-# LINE 12724 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ExitStatement ann_ lb_
                  {-# LINE 12729 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 12734 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12739 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ForIntegerStatement :: Annotation ->
                                     (Maybe String) ->
                                     T_Expression  ->
                                     T_Expression  ->
                                     T_Expression  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement ann_ lb_ var_ from_ to_ sts_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _fromOexpectedType :: (Maybe Type)
              _toOexpectedType :: (Maybe Type)
              _varOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _lhsOoriginalTree :: Statement
              _varOcat :: Catalog
              _varOlib :: LocalBindings
              _varOoverrideLib :: (Maybe LocalBindings)
              _varOoverrideLibs :: ([Maybe LocalBindings])
              _fromOcat :: Catalog
              _fromOlib :: LocalBindings
              _fromOoverrideLib :: (Maybe LocalBindings)
              _fromOoverrideLibs :: ([Maybe LocalBindings])
              _toOcat :: Catalog
              _toOlib :: LocalBindings
              _toOoverrideLib :: (Maybe LocalBindings)
              _toOoverrideLibs :: ([Maybe LocalBindings])
              _stsOcat :: Catalog
              _stsOlib :: LocalBindings
              _stsOoverrideLib :: (Maybe LocalBindings)
              _stsOoverrideLibs :: ([Maybe LocalBindings])
              _varIannotatedTree :: Expression
              _varIntAnnotatedTree :: Expression
              _varIntType :: ([(String,Type)])
              _varIoriginalTree :: Expression
              _varItbAnnotatedTree :: Expression
              _varItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _varIuType :: (Maybe Type)
              _fromIannotatedTree :: Expression
              _fromIntAnnotatedTree :: Expression
              _fromIntType :: ([(String,Type)])
              _fromIoriginalTree :: Expression
              _fromItbAnnotatedTree :: Expression
              _fromItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _fromIuType :: (Maybe Type)
              _toIannotatedTree :: Expression
              _toIntAnnotatedTree :: Expression
              _toIntType :: ([(String,Type)])
              _toIoriginalTree :: Expression
              _toItbAnnotatedTree :: Expression
              _toItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _toIuType :: (Maybe Type)
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Expressions.ag"(line 500, column 27)
              _fromOexpectedType =
                  {-# LINE 500 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 12811 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 501, column 27)
              _toOexpectedType =
                  {-# LINE 501 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 12816 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 504, column 46)
              _varOexpectedType =
                  {-# LINE 504 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 12821 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 12829 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 12834 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 12839 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12844 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12849 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 12854 "AstInternal.hs" #-}
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
                  {-# LINE 12866 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 51, column 9)
              _backTree =
                  {-# LINE 51 "./TypeChecking/Plpgsql.ag" #-}
                  ForIntegerStatement ann_ lb_ _varIannotatedTree _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 12871 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 52, column 9)
              _catUpdates =
                  {-# LINE 52 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 12876 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 53, column 9)
              _statementType =
                  {-# LINE 53 "./TypeChecking/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 12881 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ForIntegerStatement ann_ lb_ _varIannotatedTree _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
                  {-# LINE 12886 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ForIntegerStatement ann_ lb_ _varIoriginalTree _fromIoriginalTree _toIoriginalTree _stsIoriginalTree
                  {-# LINE 12891 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 12896 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12901 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12906 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12911 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12916 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12921 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12926 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12931 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12936 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12941 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12946 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12951 "AstInternal.hs" #-}
              -- copy rule (down)
              _toOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12956 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 12961 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 12966 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 12971 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 12976 "AstInternal.hs" #-}
              ( _varIannotatedTree,_varIntAnnotatedTree,_varIntType,_varIoriginalTree,_varItbAnnotatedTree,_varItbUType,_varIuType) =
                  (var_ _varOcat _varOexpectedType _varOlib _varOoverrideLib _varOoverrideLibs )
              ( _fromIannotatedTree,_fromIntAnnotatedTree,_fromIntType,_fromIoriginalTree,_fromItbAnnotatedTree,_fromItbUType,_fromIuType) =
                  (from_ _fromOcat _fromOexpectedType _fromOlib _fromOoverrideLib _fromOoverrideLibs )
              ( _toIannotatedTree,_toIntAnnotatedTree,_toIntType,_toIoriginalTree,_toItbAnnotatedTree,_toItbUType,_toIuType) =
                  (to_ _toOcat _toOexpectedType _toOlib _toOoverrideLib _toOoverrideLibs )
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOlib _stsOlibUpdates _stsOoverrideLib _stsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ForSelectStatement :: Annotation ->
                                    (Maybe String) ->
                                    T_Expression  ->
                                    T_SelectExpression  ->
                                    T_StatementList  ->
                                    T_Statement 
sem_Statement_ForSelectStatement ann_ lb_ var_ sel_ sts_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
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
              _lhsOoriginalTree :: Statement
              _varOcat :: Catalog
              _varOlib :: LocalBindings
              _varOoverrideLib :: (Maybe LocalBindings)
              _varOoverrideLibs :: ([Maybe LocalBindings])
              _selOcat :: Catalog
              _selOlib :: LocalBindings
              _selOoverrideLib :: (Maybe LocalBindings)
              _selOoverrideLibs :: ([Maybe LocalBindings])
              _stsOcat :: Catalog
              _stsOoverrideLib :: (Maybe LocalBindings)
              _stsOoverrideLibs :: ([Maybe LocalBindings])
              _varIannotatedTree :: Expression
              _varIntAnnotatedTree :: Expression
              _varIntType :: ([(String,Type)])
              _varIoriginalTree :: Expression
              _varItbAnnotatedTree :: Expression
              _varItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _varIuType :: (Maybe Type)
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: SelectExpression
              _selIuType :: (Maybe [(String,Type)])
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Expressions.ag"(line 504, column 46)
              _varOexpectedType =
                  {-# LINE 504 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 13040 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 564, column 9)
              _selOexpectedTypes =
                  {-# LINE 564 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 13045 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 13053 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 13058 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 13063 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13068 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13073 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13078 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 58, column 9)
              _tpe =
                  {-# LINE 58 "./TypeChecking/Plpgsql.ag" #-}
                  do
                  st <- lmt (CompositeType <$> _selIuType)
                  toType <- lmt _varIuType
                  checkAssignmentValid _lhsIcat st toType
                  return $ Pseudo Void
                  {-# LINE 13087 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 68, column 9)
              _stsOlib =
                  {-# LINE 68 "./TypeChecking/Plpgsql.ag" #-}
                  either (const _lhsIlib) id $ do
                  _ <- _tpe
                  st <- lmt (CompositeType <$> _selIuType)
                  lbUpdate _lhsIcat (LBIds "for loop record type" Nothing [(getName _varIannotatedTree,st)]) _lhsIlib
                  {-# LINE 13095 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 74, column 9)
              _backTree =
                  {-# LINE 74 "./TypeChecking/Plpgsql.ag" #-}
                  ForSelectStatement ann_ lb_ _varIannotatedTree _selIannotatedTree _stsIannotatedTree
                  {-# LINE 13100 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 75, column 9)
              _catUpdates =
                  {-# LINE 75 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 13105 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 76, column 9)
              _statementType =
                  {-# LINE 76 "./TypeChecking/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 13110 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ForSelectStatement ann_ lb_ _varIannotatedTree _selIannotatedTree _stsIannotatedTree
                  {-# LINE 13115 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ForSelectStatement ann_ lb_ _varIoriginalTree _selIoriginalTree _stsIoriginalTree
                  {-# LINE 13120 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13125 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13130 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13135 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13140 "AstInternal.hs" #-}
              -- copy rule (down)
              _varOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13145 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13150 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13155 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13160 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13165 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13170 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13175 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13180 "AstInternal.hs" #-}
              ( _varIannotatedTree,_varIntAnnotatedTree,_varIntType,_varIoriginalTree,_varItbAnnotatedTree,_varItbUType,_varIuType) =
                  (var_ _varOcat _varOexpectedType _varOlib _varOoverrideLib _varOoverrideLibs )
              ( _selIannotatedTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOlib _selOoverrideLib _selOoverrideLibs )
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOlib _stsOlibUpdates _stsOoverrideLib _stsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_If :: Annotation ->
                    T_ExpressionStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If ann_ cases_ els_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _elsOcatUpdates :: ([CatalogUpdate])
              _elsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _casesOcat :: Catalog
              _casesOlib :: LocalBindings
              _casesOoverrideLib :: (Maybe LocalBindings)
              _casesOoverrideLibs :: ([Maybe LocalBindings])
              _elsOcat :: Catalog
              _elsOlib :: LocalBindings
              _elsOoverrideLib :: (Maybe LocalBindings)
              _elsOoverrideLibs :: ([Maybe LocalBindings])
              _casesIannotatedTree :: ExpressionStatementListPairList
              _casesIoriginalTree :: ExpressionStatementListPairList
              _elsIannotatedTree :: StatementList
              _elsIoriginalTree :: StatementList
              _elsIproducedCat :: Catalog
              _elsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13222 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13227 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _elsOcatUpdates =
                  {-# LINE 134 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13232 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _elsOlibUpdates =
                  {-# LINE 135 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13237 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  If ann_ _casesIannotatedTree _elsIannotatedTree
                  {-# LINE 13242 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  If ann_ _casesIoriginalTree _elsIoriginalTree
                  {-# LINE 13247 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13252 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13257 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13262 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13267 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13272 "AstInternal.hs" #-}
              -- copy rule (down)
              _casesOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13277 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13282 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13287 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13292 "AstInternal.hs" #-}
              -- copy rule (down)
              _elsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13297 "AstInternal.hs" #-}
              ( _casesIannotatedTree,_casesIoriginalTree) =
                  (cases_ _casesOcat _casesOlib _casesOoverrideLib _casesOoverrideLibs )
              ( _elsIannotatedTree,_elsIoriginalTree,_elsIproducedCat,_elsIproducedLib) =
                  (els_ _elsOcat _elsOcatUpdates _elsOlib _elsOlibUpdates _elsOoverrideLib _elsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Insert :: Annotation ->
                        T_Expression  ->
                        ([String]) ->
                        T_SelectExpression  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Insert ann_ table_ targetCols_ insData_ returning_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
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
              _lhsOoriginalTree :: Statement
              _tableOcat :: Catalog
              _tableOlib :: LocalBindings
              _tableOoverrideLib :: (Maybe LocalBindings)
              _tableOoverrideLibs :: ([Maybe LocalBindings])
              _insDataOcat :: Catalog
              _insDataOlib :: LocalBindings
              _insDataOoverrideLib :: (Maybe LocalBindings)
              _insDataOoverrideLibs :: ([Maybe LocalBindings])
              _returningOcat :: Catalog
              _returningOoverrideLib :: (Maybe LocalBindings)
              _returningOoverrideLibs :: ([Maybe LocalBindings])
              _tableIannotatedTree :: Expression
              _tableIntAnnotatedTree :: Expression
              _tableIntType :: ([(String,Type)])
              _tableIoriginalTree :: Expression
              _tableItbAnnotatedTree :: Expression
              _tableItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tableIuType :: (Maybe Type)
              _insDataIannotatedTree :: SelectExpression
              _insDataIlibUpdates :: ([LocalBindingsUpdate])
              _insDataIoriginalTree :: SelectExpression
              _insDataIuType :: (Maybe [(String,Type)])
              _returningIannotatedTree :: MaybeSelectList
              _returningIlistType :: ([(String,Type)])
              _returningIoriginalTree :: MaybeSelectList
              -- "./TypeChecking/Expressions.ag"(line 573, column 28)
              _tableOexpectedType =
                  {-# LINE 573 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 13355 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 13363 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 13368 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 13373 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13378 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 14, column 9)
              _tpe =
                  {-# LINE 14 "./TypeChecking/Insert.ag" #-}
                  either Left (const $ Right $ Pseudo Void) _columnTypes
                  {-# LINE 13383 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 15, column 9)
              _statementType =
                  {-# LINE 15 "./TypeChecking/Insert.ag" #-}
                  Just (catMaybes $ getPlaceholderTypes _insDataIannotatedTree
                       ,_returningIlistType)
                  {-# LINE 13389 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 20, column 9)
              _columnTypes =
                  {-# LINE 20 "./TypeChecking/Insert.ag" #-}
                  do
                  atts <- lmt (allAtts <$> _tableItbUType)
                  expAtts <- lmt _insDataIuType
                  tAtts <- case targetCols_ of
                                [] -> return atts
                                _ -> mapM (lkpA atts) targetCols_
                  checkAssignmentsValid _lhsIcat (map snd expAtts) (map snd tAtts)
                  return tAtts
                  where
                    lkpA :: [(String,Type)] -> String -> E (String,Type)
                    lkpA m n = maybe (Left [UnrecognisedIdentifier n])
                                     (\t -> Right (n,t))
                                     $ lookup n m
                  {-# LINE 13406 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 35, column 9)
              _backTree =
                  {-# LINE 35 "./TypeChecking/Insert.ag" #-}
                  Insert ann_ _tableItbAnnotatedTree
                         targetCols_
                         _insDataIannotatedTree
                         _returningIannotatedTree
                  {-# LINE 13414 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 39, column 9)
              _catUpdates =
                  {-# LINE 39 "./TypeChecking/Insert.ag" #-}
                  []
                  {-# LINE 13419 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 40, column 9)
              _insDataOexpectedTypes =
                  {-# LINE 40 "./TypeChecking/Insert.ag" #-}
                  maybe [] id $ do
                  ts <- etmt $ _columnTypes
                  return $ map (Just . snd) ts
                  {-# LINE 13426 "AstInternal.hs" #-}
              -- "./TypeChecking/Insert.ag"(line 44, column 9)
              _returningOlib =
                  {-# LINE 44 "./TypeChecking/Insert.ag" #-}
                  either (const _lhsIlib) id $ do
                    atts <- lmt (allAtts <$> _tableItbUType)
                    lbUpdate _lhsIcat (LBIds "insert target table" (Just $ getName _tableIannotatedTree) atts) _lhsIlib
                  {-# LINE 13433 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Insert ann_ _tableIannotatedTree targetCols_ _insDataIannotatedTree _returningIannotatedTree
                  {-# LINE 13438 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Insert ann_ _tableIoriginalTree targetCols_ _insDataIoriginalTree _returningIoriginalTree
                  {-# LINE 13443 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13448 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13453 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13458 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13463 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13468 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13473 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13478 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13483 "AstInternal.hs" #-}
              -- copy rule (down)
              _insDataOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13488 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13493 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13498 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13503 "AstInternal.hs" #-}
              ( _tableIannotatedTree,_tableIntAnnotatedTree,_tableIntType,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType,_tableIuType) =
                  (table_ _tableOcat _tableOexpectedType _tableOlib _tableOoverrideLib _tableOoverrideLibs )
              ( _insDataIannotatedTree,_insDataIlibUpdates,_insDataIoriginalTree,_insDataIuType) =
                  (insData_ _insDataOcat _insDataOexpectedTypes _insDataOlib _insDataOoverrideLib _insDataOoverrideLibs )
              ( _returningIannotatedTree,_returningIlistType,_returningIoriginalTree) =
                  (returning_ _returningOcat _returningOlib _returningOoverrideLib _returningOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_LoopStatement :: Annotation ->
                               (Maybe String) ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_LoopStatement ann_ lb_ sts_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _stsOcat :: Catalog
              _stsOlib :: LocalBindings
              _stsOoverrideLib :: (Maybe LocalBindings)
              _stsOoverrideLibs :: ([Maybe LocalBindings])
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13539 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13544 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13549 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13554 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  LoopStatement ann_ lb_ _stsIannotatedTree
                  {-# LINE 13559 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  LoopStatement ann_ lb_ _stsIoriginalTree
                  {-# LINE 13564 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13569 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13574 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13579 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13584 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13589 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13594 "AstInternal.hs" #-}
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOlib _stsOlibUpdates _stsOoverrideLib _stsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Notify :: Annotation ->
                        String ->
                        T_Statement 
sem_Statement_Notify ann_ name_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13615 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13620 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13625 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Notify ann_ name_
                  {-# LINE 13630 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Notify ann_ name_
                  {-# LINE 13635 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13640 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13645 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_NullStatement :: Annotation ->
                               T_Statement 
sem_Statement_NullStatement ann_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13663 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13668 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 13673 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  NullStatement ann_
                  {-# LINE 13678 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13683 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13688 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Perform :: Annotation ->
                         T_Expression  ->
                         T_Statement 
sem_Statement_Perform ann_ expr_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 499, column 9)
              _exprOexpectedType =
                  {-# LINE 499 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 13719 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13724 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13729 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Perform ann_ _exprIannotatedTree
                  {-# LINE 13734 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Perform ann_ _exprIoriginalTree
                  {-# LINE 13739 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13744 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13749 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13754 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13759 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13764 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13769 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Raise :: Annotation ->
                       RaiseType ->
                       String ->
                       T_ExpressionList  ->
                       T_Statement 
sem_Statement_Raise ann_ level_ message_ args_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _argsOexpectedTypes :: ([Maybe Type])
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _argsOcat :: Catalog
              _argsOlib :: LocalBindings
              _argsOoverrideLib :: (Maybe LocalBindings)
              _argsOoverrideLibs :: ([Maybe LocalBindings])
              _argsIannotatedTree :: ExpressionList
              _argsIoriginalTree :: ExpressionList
              _argsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _argsIuType :: ([Maybe Type])
              -- "./TypeChecking/Expressions.ag"(line 541, column 13)
              _argsOexpectedTypes =
                  {-# LINE 541 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 13801 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13806 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13811 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Raise ann_ level_ message_ _argsIannotatedTree
                  {-# LINE 13816 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Raise ann_ level_ message_ _argsIoriginalTree
                  {-# LINE 13821 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 13826 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13831 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13836 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13841 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13846 "AstInternal.hs" #-}
              -- copy rule (down)
              _argsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13851 "AstInternal.hs" #-}
              ( _argsIannotatedTree,_argsIoriginalTree,_argsItbUTypes,_argsIuType) =
                  (args_ _argsOcat _argsOexpectedTypes _argsOlib _argsOoverrideLib _argsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Return :: Annotation ->
                        T_MaybeExpression  ->
                        T_Statement 
sem_Statement_Return ann_ value_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _catUpdates :: ([CatalogUpdate])
              _statementType :: (Maybe StatementType)
              _lhsOoriginalTree :: Statement
              _valueOcat :: Catalog
              _valueOlib :: LocalBindings
              _valueOoverrideLib :: (Maybe LocalBindings)
              _valueOoverrideLibs :: ([Maybe LocalBindings])
              _valueIannotatedTree :: MaybeExpression
              _valueIoriginalTree :: MaybeExpression
              _valueIuType :: (Maybe Type)
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 13885 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 13890 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 13895 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13900 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 12, column 9)
              _tpe =
                  {-# LINE 12 "./TypeChecking/Plpgsql.ag" #-}
                  maybe (Right $ Pseudo Void) Right _valueIuType
                  {-# LINE 13905 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 13, column 9)
              _backTree =
                  {-# LINE 13 "./TypeChecking/Plpgsql.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 13910 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 14, column 9)
              _catUpdates =
                  {-# LINE 14 "./TypeChecking/Plpgsql.ag" #-}
                  []
                  {-# LINE 13915 "AstInternal.hs" #-}
              -- "./TypeChecking/Plpgsql.ag"(line 15, column 9)
              _statementType =
                  {-# LINE 15 "./TypeChecking/Plpgsql.ag" #-}
                  Nothing
                  {-# LINE 13920 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Return ann_ _valueIannotatedTree
                  {-# LINE 13925 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Return ann_ _valueIoriginalTree
                  {-# LINE 13930 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 13935 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 13940 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 13945 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 13950 "AstInternal.hs" #-}
              -- copy rule (down)
              _valueOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 13955 "AstInternal.hs" #-}
              ( _valueIannotatedTree,_valueIoriginalTree,_valueIuType) =
                  (value_ _valueOcat _valueOlib _valueOoverrideLib _valueOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ReturnNext :: Annotation ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_ReturnNext ann_ expr_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 499, column 9)
              _exprOexpectedType =
                  {-# LINE 499 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 13988 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13993 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 13998 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIannotatedTree
                  {-# LINE 14003 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnNext ann_ _exprIoriginalTree
                  {-# LINE 14008 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14013 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14018 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14023 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14028 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14033 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14038 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_ReturnQuery :: Annotation ->
                             T_SelectExpression  ->
                             T_Statement 
sem_Statement_ReturnQuery ann_ sel_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _selOexpectedTypes :: ([Maybe Type])
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _selOcat :: Catalog
              _selOlib :: LocalBindings
              _selOoverrideLib :: (Maybe LocalBindings)
              _selOoverrideLibs :: ([Maybe LocalBindings])
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: SelectExpression
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Expressions.ag"(line 564, column 9)
              _selOexpectedTypes =
                  {-# LINE 564 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 14068 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14073 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14078 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIannotatedTree
                  {-# LINE 14083 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ReturnQuery ann_ _selIoriginalTree
                  {-# LINE 14088 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14093 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14098 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14103 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14108 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14113 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14118 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOlib _selOoverrideLib _selOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_SelectStatement :: Annotation ->
                                 T_SelectExpression  ->
                                 T_Statement 
sem_Statement_SelectStatement ann_ ex_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: Statement
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _tpe :: (Either [TypeError] Type)
              _statementType :: (Maybe StatementType)
              _catUpdates :: ([CatalogUpdate])
              _lhsOoriginalTree :: Statement
              _exOcat :: Catalog
              _exOlib :: LocalBindings
              _exOoverrideLib :: (Maybe LocalBindings)
              _exOoverrideLibs :: ([Maybe LocalBindings])
              _exIannotatedTree :: SelectExpression
              _exIlibUpdates :: ([LocalBindingsUpdate])
              _exIoriginalTree :: SelectExpression
              _exIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Expressions.ag"(line 565, column 23)
              _exOexpectedTypes =
                  {-# LINE 565 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 14151 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 14159 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 14164 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 14169 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 14, column 9)
              _tpe =
                  {-# LINE 14 "./TypeChecking/SelectStatement.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 14174 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 15, column 9)
              _statementType =
                  {-# LINE 15 "./TypeChecking/SelectStatement.ag" #-}
                  do
                  pt <- sequence $ getPlaceholderTypes _exIannotatedTree
                  st <- _exIuType
                  return (pt
                         ,case st of
                            [(_,(Pseudo Void))] -> []
                            t -> t)
                  {-# LINE 14185 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 23, column 9)
              _backTree =
                  {-# LINE 23 "./TypeChecking/SelectStatement.ag" #-}
                  SelectStatement ann_ _exIannotatedTree
                  {-# LINE 14190 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 24, column 9)
              _catUpdates =
                  {-# LINE 24 "./TypeChecking/SelectStatement.ag" #-}
                  []
                  {-# LINE 14195 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 111, column 9)
              _libUpdates =
                  {-# LINE 111 "./TypeChecking/SelectStatement.ag" #-}
                  _exIlibUpdates
                  {-# LINE 14200 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  SelectStatement ann_ _exIannotatedTree
                  {-# LINE 14205 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SelectStatement ann_ _exIoriginalTree
                  {-# LINE 14210 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14215 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14220 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14225 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14230 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14235 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIlibUpdates,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedTypes _exOlib _exOoverrideLib _exOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Set :: Annotation ->
                     String ->
                     ([SetValue]) ->
                     T_Statement 
sem_Statement_Set ann_ name_ values_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14257 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14262 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14267 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Set ann_ name_ values_
                  {-# LINE 14272 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Set ann_ name_ values_
                  {-# LINE 14277 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14282 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14287 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Truncate :: Annotation ->
                          ([String]) ->
                          RestartIdentity ->
                          Cascade ->
                          T_Statement 
sem_Statement_Truncate ann_ tables_ restartIdentity_ cascade_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14308 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14313 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Truncate ann_ tables_ restartIdentity_ cascade_
                  {-# LINE 14318 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Truncate ann_ tables_ restartIdentity_ cascade_
                  {-# LINE 14323 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14328 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14333 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_Update :: Annotation ->
                        T_Expression  ->
                        T_ExpressionList  ->
                        T_TableRefList  ->
                        T_MaybeBoolExpression  ->
                        T_MaybeSelectList  ->
                        T_Statement 
sem_Statement_Update ann_ table_ assigns_ fromList_ whr_ returning_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
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
              _lhsOoriginalTree :: Statement
              _tableOcat :: Catalog
              _tableOlib :: LocalBindings
              _tableOoverrideLib :: (Maybe LocalBindings)
              _tableOoverrideLibs :: ([Maybe LocalBindings])
              _assignsOcat :: Catalog
              _assignsOoverrideLib :: (Maybe LocalBindings)
              _assignsOoverrideLibs :: ([Maybe LocalBindings])
              _fromListOcat :: Catalog
              _fromListOlib :: LocalBindings
              _fromListOoverrideLib :: (Maybe LocalBindings)
              _fromListOoverrideLibs :: ([Maybe LocalBindings])
              _whrOcat :: Catalog
              _whrOoverrideLib :: (Maybe LocalBindings)
              _whrOoverrideLibs :: ([Maybe LocalBindings])
              _returningOcat :: Catalog
              _returningOoverrideLib :: (Maybe LocalBindings)
              _returningOoverrideLibs :: ([Maybe LocalBindings])
              _tableIannotatedTree :: Expression
              _tableIntAnnotatedTree :: Expression
              _tableIntType :: ([(String,Type)])
              _tableIoriginalTree :: Expression
              _tableItbAnnotatedTree :: Expression
              _tableItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tableIuType :: (Maybe Type)
              _assignsIannotatedTree :: ExpressionList
              _assignsIoriginalTree :: ExpressionList
              _assignsItbUTypes :: ([Maybe ([(String,Type)],[(String,Type)])])
              _assignsIuType :: ([Maybe Type])
              _fromListIannotatedTree :: TableRefList
              _fromListIlibUpdates :: ([LocalBindingsUpdate])
              _fromListIoriginalTree :: TableRefList
              _whrIannotatedTree :: MaybeBoolExpression
              _whrIoriginalTree :: MaybeBoolExpression
              _returningIannotatedTree :: MaybeSelectList
              _returningIlistType :: ([(String,Type)])
              _returningIoriginalTree :: MaybeSelectList
              -- "./TypeChecking/Expressions.ag"(line 542, column 14)
              _assignsOexpectedTypes =
                  {-# LINE 542 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 14400 "AstInternal.hs" #-}
              -- "./TypeChecking/Expressions.ag"(line 573, column 28)
              _tableOexpectedType =
                  {-# LINE 573 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 14405 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 82, column 9)
              _lhsOannotatedTree =
                  {-# LINE 82 "./TypeChecking/Statements.ag" #-}
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
                  {-# LINE 14413 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 88, column 9)
              _lhsOcatUpdates =
                  {-# LINE 88 "./TypeChecking/Statements.ag" #-}
                  _catUpdates
                  {-# LINE 14418 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 89, column 9)
              _lhsOlibUpdates =
                  {-# LINE 89 "./TypeChecking/Statements.ag" #-}
                  _libUpdates
                  {-# LINE 14423 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 94, column 9)
              _libUpdates =
                  {-# LINE 94 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14428 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 13, column 9)
              _tpe =
                  {-# LINE 13 "./TypeChecking/Update.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 14433 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 18, column 9)
              _statementType =
                  {-# LINE 18 "./TypeChecking/Update.ag" #-}
                  do
                  pt <- sequence $ getPlaceholderTypes _assignsIannotatedTree
                                   ++ getPlaceholderTypes _whrIannotatedTree
                  return (pt,_returningIlistType)
                  {-# LINE 14441 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 24, column 9)
              _backTree =
                  {-# LINE 24 "./TypeChecking/Update.ag" #-}
                  Update ann_
                         _tableItbAnnotatedTree
                         _assignsIannotatedTree
                         _fromListIannotatedTree
                         _whrIannotatedTree
                         _returningIannotatedTree
                  {-# LINE 14451 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 30, column 9)
              _catUpdates =
                  {-# LINE 30 "./TypeChecking/Update.ag" #-}
                  []
                  {-# LINE 14456 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 35, column 9)
              _lib =
                  {-# LINE 35 "./TypeChecking/Update.ag" #-}
                  either (const _lhsIlib) id $ do
                  a <- lmt (allAtts <$> _tableItbUType)
                  lbUpdate _lhsIcat (LBIds "updated table attrs" (Just $ getName _tableIannotatedTree) a) _lhsIlib
                  {-# LINE 14463 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 39, column 9)
              _whrOlib =
                  {-# LINE 39 "./TypeChecking/Update.ag" #-}
                  _lib
                  {-# LINE 14468 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 40, column 9)
              _assignsOlib =
                  {-# LINE 40 "./TypeChecking/Update.ag" #-}
                  _lib
                  {-# LINE 14473 "AstInternal.hs" #-}
              -- "./TypeChecking/Update.ag"(line 41, column 9)
              _returningOlib =
                  {-# LINE 41 "./TypeChecking/Update.ag" #-}
                  _lib
                  {-# LINE 14478 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Update ann_ _tableIannotatedTree _assignsIannotatedTree _fromListIannotatedTree _whrIannotatedTree _returningIannotatedTree
                  {-# LINE 14483 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Update ann_ _tableIoriginalTree _assignsIoriginalTree _fromListIoriginalTree _whrIoriginalTree _returningIoriginalTree
                  {-# LINE 14488 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14493 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14498 "AstInternal.hs" #-}
              -- copy rule (from local)
              _tableOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 14503 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14508 "AstInternal.hs" #-}
              -- copy rule (down)
              _tableOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14513 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14518 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14523 "AstInternal.hs" #-}
              -- copy rule (down)
              _assignsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14528 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromListOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14533 "AstInternal.hs" #-}
              -- copy rule (from local)
              _fromListOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lib
                  {-# LINE 14538 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromListOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14543 "AstInternal.hs" #-}
              -- copy rule (down)
              _fromListOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14548 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14553 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14558 "AstInternal.hs" #-}
              -- copy rule (down)
              _whrOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14563 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14568 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14573 "AstInternal.hs" #-}
              -- copy rule (down)
              _returningOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14578 "AstInternal.hs" #-}
              ( _tableIannotatedTree,_tableIntAnnotatedTree,_tableIntType,_tableIoriginalTree,_tableItbAnnotatedTree,_tableItbUType,_tableIuType) =
                  (table_ _tableOcat _tableOexpectedType _tableOlib _tableOoverrideLib _tableOoverrideLibs )
              ( _assignsIannotatedTree,_assignsIoriginalTree,_assignsItbUTypes,_assignsIuType) =
                  (assigns_ _assignsOcat _assignsOexpectedTypes _assignsOlib _assignsOoverrideLib _assignsOoverrideLibs )
              ( _fromListIannotatedTree,_fromListIlibUpdates,_fromListIoriginalTree) =
                  (fromList_ _fromListOcat _fromListOlib _fromListOoverrideLib _fromListOoverrideLibs )
              ( _whrIannotatedTree,_whrIoriginalTree) =
                  (whr_ _whrOcat _whrOlib _whrOoverrideLib _whrOoverrideLibs )
              ( _returningIannotatedTree,_returningIlistType,_returningIoriginalTree) =
                  (returning_ _returningOcat _returningOlib _returningOoverrideLib _returningOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_Statement_WhileStatement :: Annotation ->
                                (Maybe String) ->
                                T_Expression  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement ann_ lb_ expr_ sts_  =
    (\ _lhsIcat
       _lhsIinProducedCat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exprOexpectedType :: (Maybe Type)
              _lhsOcatUpdates :: ([CatalogUpdate])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _stsOcatUpdates :: ([CatalogUpdate])
              _stsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: Statement
              _lhsOoriginalTree :: Statement
              _exprOcat :: Catalog
              _exprOlib :: LocalBindings
              _exprOoverrideLib :: (Maybe LocalBindings)
              _exprOoverrideLibs :: ([Maybe LocalBindings])
              _stsOcat :: Catalog
              _stsOlib :: LocalBindings
              _stsOoverrideLib :: (Maybe LocalBindings)
              _stsOoverrideLibs :: ([Maybe LocalBindings])
              _exprIannotatedTree :: Expression
              _exprIntAnnotatedTree :: Expression
              _exprIntType :: ([(String,Type)])
              _exprIoriginalTree :: Expression
              _exprItbAnnotatedTree :: Expression
              _exprItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _exprIuType :: (Maybe Type)
              _stsIannotatedTree :: StatementList
              _stsIoriginalTree :: StatementList
              _stsIproducedCat :: Catalog
              _stsIproducedLib :: LocalBindings
              -- "./TypeChecking/Expressions.ag"(line 499, column 9)
              _exprOexpectedType =
                  {-# LINE 499 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 14631 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 116, column 9)
              _lhsOcatUpdates =
                  {-# LINE 116 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14636 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  {-# LINE 117 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14641 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 138, column 9)
              _stsOcatUpdates =
                  {-# LINE 138 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14646 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 139, column 9)
              _stsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/Statements.ag" #-}
                  []
                  {-# LINE 14651 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  WhileStatement ann_ lb_ _exprIannotatedTree _stsIannotatedTree
                  {-# LINE 14656 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  WhileStatement ann_ lb_ _exprIoriginalTree _stsIoriginalTree
                  {-# LINE 14661 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14666 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14671 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14676 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14681 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14686 "AstInternal.hs" #-}
              -- copy rule (down)
              _exprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14691 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 14696 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 14701 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14706 "AstInternal.hs" #-}
              -- copy rule (down)
              _stsOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14711 "AstInternal.hs" #-}
              ( _exprIannotatedTree,_exprIntAnnotatedTree,_exprIntType,_exprIoriginalTree,_exprItbAnnotatedTree,_exprItbUType,_exprIuType) =
                  (expr_ _exprOcat _exprOexpectedType _exprOlib _exprOoverrideLib _exprOoverrideLibs )
              ( _stsIannotatedTree,_stsIoriginalTree,_stsIproducedCat,_stsIproducedLib) =
                  (sts_ _stsOcat _stsOcatUpdates _stsOlib _stsOlibUpdates _stsOoverrideLib _stsOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOlibUpdates,_lhsOoriginalTree)))
-- StatementList -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         catUpdates           : [CatalogUpdate]
         lib                  : LocalBindings
         libUpdates           : [LocalBindingsUpdate]
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
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
            local originalTree : _
      alternative Nil:
         visit 0:
            local newCat      : _
            local newLib      : _
            local annotatedTree : _
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
                        LocalBindings ->
                        ([LocalBindingsUpdate]) ->
                        (Maybe LocalBindings) ->
                        ([Maybe LocalBindings]) ->
                        ( StatementList,StatementList,Catalog,LocalBindings)
data Inh_StatementList  = Inh_StatementList {cat_Inh_StatementList :: Catalog,catUpdates_Inh_StatementList :: [CatalogUpdate],lib_Inh_StatementList :: LocalBindings,libUpdates_Inh_StatementList :: [LocalBindingsUpdate],overrideLib_Inh_StatementList :: Maybe LocalBindings,overrideLibs_Inh_StatementList :: [Maybe LocalBindings]}
data Syn_StatementList  = Syn_StatementList {annotatedTree_Syn_StatementList :: StatementList,originalTree_Syn_StatementList :: StatementList,producedCat_Syn_StatementList :: Catalog,producedLib_Syn_StatementList :: LocalBindings}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIcat _lhsIcatUpdates _lhsIlib _lhsIlibUpdates _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib) =
             (sem _lhsIcat _lhsIcatUpdates _lhsIlib _lhsIlibUpdates _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_StatementList _lhsOannotatedTree _lhsOoriginalTree _lhsOproducedCat _lhsOproducedLib ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIcatUpdates
       _lhsIlib
       _lhsIlibUpdates
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
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
              _lhsOoriginalTree :: StatementList
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: Statement
              _hdIcatUpdates :: ([CatalogUpdate])
              _hdIlibUpdates :: ([LocalBindingsUpdate])
              _hdIoriginalTree :: Statement
              _tlIannotatedTree :: StatementList
              _tlIoriginalTree :: StatementList
              _tlIproducedCat :: Catalog
              _tlIproducedLib :: LocalBindings
              -- "./TypeChecking/Statements.ag"(line 56, column 9)
              _newCat =
                  {-# LINE 56 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
                  {-# LINE 14808 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 57, column 9)
              _newLib =
                  {-# LINE 57 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIlib $ foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _lhsIlibUpdates
                  {-# LINE 14813 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 59, column 9)
              _hdOcat =
                  {-# LINE 59 "./TypeChecking/Statements.ag" #-}
                  _newCat
                  {-# LINE 14818 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 60, column 9)
              _tlOcat =
                  {-# LINE 60 "./TypeChecking/Statements.ag" #-}
                  _newCat
                  {-# LINE 14823 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 61, column 9)
              _hdOlib =
                  {-# LINE 61 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 14828 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 62, column 9)
              _tlOlib =
                  {-# LINE 62 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 14833 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _lhsOproducedCat =
                  {-# LINE 66 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedCat
                  {-# LINE 14838 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 67, column 9)
              _lhsOproducedLib =
                  {-# LINE 67 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedLib
                  {-# LINE 14843 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _tlOcatUpdates =
                  {-# LINE 70 "./TypeChecking/Statements.ag" #-}
                  _hdIcatUpdates
                  {-# LINE 14848 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 71, column 9)
              _tlOlibUpdates =
                  {-# LINE 71 "./TypeChecking/Statements.ag" #-}
                  _hdIlibUpdates
                  {-# LINE 14853 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 97, column 12)
              _hdOinProducedCat =
                  {-# LINE 97 "./TypeChecking/Statements.ag" #-}
                  _tlIproducedCat
                  {-# LINE 14858 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 14863 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 14868 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14873 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14878 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14883 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14888 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 14893 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 14898 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIcatUpdates,_hdIlibUpdates,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOinProducedCat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIproducedCat,_tlIproducedLib) =
                  (tl_ _tlOcat _tlOcatUpdates _tlOlib _tlOlibUpdates _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIcat
       _lhsIcatUpdates
       _lhsIlib
       _lhsIlibUpdates
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOproducedCat :: Catalog
              _lhsOproducedLib :: LocalBindings
              _lhsOannotatedTree :: StatementList
              _lhsOoriginalTree :: StatementList
              -- "./TypeChecking/Statements.ag"(line 56, column 9)
              _newCat =
                  {-# LINE 56 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
                  {-# LINE 14920 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 57, column 9)
              _newLib =
                  {-# LINE 57 "./TypeChecking/Statements.ag" #-}
                  fromRight _lhsIlib $ foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _lhsIlibUpdates
                  {-# LINE 14925 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 73, column 9)
              _lhsOproducedCat =
                  {-# LINE 73 "./TypeChecking/Statements.ag" #-}
                  _newCat
                  {-# LINE 14930 "AstInternal.hs" #-}
              -- "./TypeChecking/Statements.ag"(line 74, column 9)
              _lhsOproducedLib =
                  {-# LINE 74 "./TypeChecking/Statements.ag" #-}
                  _newLib
                  {-# LINE 14935 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 14940 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 14945 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 14950 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 14955 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedCat,_lhsOproducedLib)))
-- StringTypeNameListPair --------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         fnSig                : (String,[Maybe Type])
         originalTree         : SELF 
   alternatives:
      alternative Tuple:
         child x1             : {String}
         child x2             : TypeNameList 
         visit 0:
            local annotatedTree : _
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
                                 LocalBindings ->
                                 (Maybe LocalBindings) ->
                                 ([Maybe LocalBindings]) ->
                                 ( StringTypeNameListPair,((String,[Maybe Type])),StringTypeNameListPair)
data Inh_StringTypeNameListPair  = Inh_StringTypeNameListPair {cat_Inh_StringTypeNameListPair :: Catalog,lib_Inh_StringTypeNameListPair :: LocalBindings,overrideLib_Inh_StringTypeNameListPair :: Maybe LocalBindings,overrideLibs_Inh_StringTypeNameListPair :: [Maybe LocalBindings]}
data Syn_StringTypeNameListPair  = Syn_StringTypeNameListPair {annotatedTree_Syn_StringTypeNameListPair :: StringTypeNameListPair,fnSig_Syn_StringTypeNameListPair :: (String,[Maybe Type]),originalTree_Syn_StringTypeNameListPair :: StringTypeNameListPair}
wrap_StringTypeNameListPair :: T_StringTypeNameListPair  ->
                               Inh_StringTypeNameListPair  ->
                               Syn_StringTypeNameListPair 
wrap_StringTypeNameListPair sem (Inh_StringTypeNameListPair _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOfnSig,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_StringTypeNameListPair _lhsOannotatedTree _lhsOfnSig _lhsOoriginalTree ))
sem_StringTypeNameListPair_Tuple :: String ->
                                    T_TypeNameList  ->
                                    T_StringTypeNameListPair 
sem_StringTypeNameListPair_Tuple x1_ x2_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOfnSig :: ((String,[Maybe Type]))
              _lhsOannotatedTree :: StringTypeNameListPair
              _lhsOoriginalTree :: StringTypeNameListPair
              _x2Ocat :: Catalog
              _x2Olib :: LocalBindings
              _x2OoverrideLib :: (Maybe LocalBindings)
              _x2OoverrideLibs :: ([Maybe LocalBindings])
              _x2IannotatedTree :: TypeNameList
              _x2InamedTypes :: ([Maybe Type])
              _x2IoriginalTree :: TypeNameList
              -- "./TypeChecking/Drops.ag"(line 32, column 13)
              _lhsOfnSig =
                  {-# LINE 32 "./TypeChecking/Drops.ag" #-}
                  (x1_, _x2InamedTypes)
                  {-# LINE 15020 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (x1_,_x2IannotatedTree)
                  {-# LINE 15025 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (x1_,_x2IoriginalTree)
                  {-# LINE 15030 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15035 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15040 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15045 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15050 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15055 "AstInternal.hs" #-}
              -- copy rule (down)
              _x2OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15060 "AstInternal.hs" #-}
              ( _x2IannotatedTree,_x2InamedTypes,_x2IoriginalTree) =
                  (x2_ _x2Ocat _x2Olib _x2OoverrideLib _x2OoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOfnSig,_lhsOoriginalTree)))
-- StringTypeNameListPairList ----------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         fnSigs               : [(String,[Maybe Type])]
         originalTree         : SELF 
   alternatives:
      alternative Cons:
         child hd             : StringTypeNameListPair 
         child tl             : StringTypeNameListPairList 
         visit 0:
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local annotatedTree : _
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
                                     LocalBindings ->
                                     (Maybe LocalBindings) ->
                                     ([Maybe LocalBindings]) ->
                                     ( StringTypeNameListPairList,([(String,[Maybe Type])]),StringTypeNameListPairList)
data Inh_StringTypeNameListPairList  = Inh_StringTypeNameListPairList {cat_Inh_StringTypeNameListPairList :: Catalog,lib_Inh_StringTypeNameListPairList :: LocalBindings,overrideLib_Inh_StringTypeNameListPairList :: Maybe LocalBindings,overrideLibs_Inh_StringTypeNameListPairList :: [Maybe LocalBindings]}
data Syn_StringTypeNameListPairList  = Syn_StringTypeNameListPairList {annotatedTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList,fnSigs_Syn_StringTypeNameListPairList :: [(String,[Maybe Type])],originalTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList}
wrap_StringTypeNameListPairList :: T_StringTypeNameListPairList  ->
                                   Inh_StringTypeNameListPairList  ->
                                   Syn_StringTypeNameListPairList 
wrap_StringTypeNameListPairList sem (Inh_StringTypeNameListPairList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOfnSigs,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_StringTypeNameListPairList _lhsOannotatedTree _lhsOfnSigs _lhsOoriginalTree ))
sem_StringTypeNameListPairList_Cons :: T_StringTypeNameListPair  ->
                                       T_StringTypeNameListPairList  ->
                                       T_StringTypeNameListPairList 
sem_StringTypeNameListPairList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOfnSigs :: ([(String,[Maybe Type])])
              _lhsOannotatedTree :: StringTypeNameListPairList
              _lhsOoriginalTree :: StringTypeNameListPairList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: StringTypeNameListPair
              _hdIfnSig :: ((String,[Maybe Type]))
              _hdIoriginalTree :: StringTypeNameListPair
              _tlIannotatedTree :: StringTypeNameListPairList
              _tlIfnSigs :: ([(String,[Maybe Type])])
              _tlIoriginalTree :: StringTypeNameListPairList
              -- "./TypeChecking/Drops.ag"(line 27, column 12)
              _lhsOfnSigs =
                  {-# LINE 27 "./TypeChecking/Drops.ag" #-}
                  _hdIfnSig : _tlIfnSigs
                  {-# LINE 15138 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 15143 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 15148 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15153 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15158 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15163 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15168 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15173 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15178 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15183 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15188 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15193 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15198 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIfnSig,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIfnSigs,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOfnSigs,_lhsOoriginalTree)))
sem_StringTypeNameListPairList_Nil :: T_StringTypeNameListPairList 
sem_StringTypeNameListPairList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOfnSigs :: ([(String,[Maybe Type])])
              _lhsOannotatedTree :: StringTypeNameListPairList
              _lhsOoriginalTree :: StringTypeNameListPairList
              -- "./TypeChecking/Drops.ag"(line 28, column 11)
              _lhsOfnSigs =
                  {-# LINE 28 "./TypeChecking/Drops.ag" #-}
                  []
                  {-# LINE 15217 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15222 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15227 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15232 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15237 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOfnSigs,_lhsOoriginalTree)))
-- TableRef ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         libUpdates           : [LocalBindingsUpdate]
         originalTree         : SELF 
   alternatives:
      alternative JoinedTref:
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
            local originalTree : _
      alternative SubTref:
         child ann            : {Annotation}
         child sel            : SelectExpression 
         child alias          : {TableAlias}
         visit 0:
            local errs        : _
            local selectAttrs : {Either [TypeError] [(String,Type)]}
            local backTree    : _
            local annotatedTree : _
            local originalTree : _
      alternative Tref:
         child ann            : {Annotation}
         child tbl            : Expression 
         child alias          : {TableAlias}
         visit 0:
            local errs        : _
            local backTree    : _
            local annotatedTree : _
            local originalTree : _
      alternative TrefFun:
         child ann            : {Annotation}
         child fn             : Expression 
         child alias          : {TableAlias}
         visit 0:
            local errs        : _
            local eqfunIdens  : {Either [TypeError] (String,[(String,Type)])}
            local qfunIdens   : _
            local backTree    : _
            local annotatedTree : _
            local originalTree : _
-}
data TableRef  = JoinedTref (Annotation) (TableRef) (Natural) (JoinType) (TableRef) (OnExpr) (TableAlias) 
               | SubTref (Annotation) (SelectExpression) (TableAlias) 
               | Tref (Annotation) (Expression) (TableAlias) 
               | TrefFun (Annotation) (Expression) (TableAlias) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TableRef :: TableRef  ->
                T_TableRef 
sem_TableRef (JoinedTref _ann _tbl _nat _joinType _tbl1 _onExpr _alias )  =
    (sem_TableRef_JoinedTref _ann (sem_TableRef _tbl ) _nat _joinType (sem_TableRef _tbl1 ) (sem_OnExpr _onExpr ) _alias )
sem_TableRef (SubTref _ann _sel _alias )  =
    (sem_TableRef_SubTref _ann (sem_SelectExpression _sel ) _alias )
sem_TableRef (Tref _ann _tbl _alias )  =
    (sem_TableRef_Tref _ann (sem_Expression _tbl ) _alias )
sem_TableRef (TrefFun _ann _fn _alias )  =
    (sem_TableRef_TrefFun _ann (sem_Expression _fn ) _alias )
-- semantic domain
type T_TableRef  = Catalog ->
                   LocalBindings ->
                   (Maybe LocalBindings) ->
                   ([Maybe LocalBindings]) ->
                   ( TableRef,([LocalBindingsUpdate]),TableRef)
data Inh_TableRef  = Inh_TableRef {cat_Inh_TableRef :: Catalog,lib_Inh_TableRef :: LocalBindings,overrideLib_Inh_TableRef :: Maybe LocalBindings,overrideLibs_Inh_TableRef :: [Maybe LocalBindings]}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef,libUpdates_Syn_TableRef :: [LocalBindingsUpdate],originalTree_Syn_TableRef :: TableRef}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_TableRef _lhsOannotatedTree _lhsOlibUpdates _lhsOoriginalTree ))
sem_TableRef_JoinedTref :: Annotation ->
                           T_TableRef  ->
                           Natural ->
                           JoinType ->
                           T_TableRef  ->
                           T_OnExpr  ->
                           TableAlias ->
                           T_TableRef 
sem_TableRef_JoinedTref ann_ tbl_ nat_ joinType_ tbl1_ onExpr_ alias_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _newLib :: (Either [TypeError] LocalBindings)
              _onExprOlib :: LocalBindings
              _lhsOoriginalTree :: TableRef
              _tblOcat :: Catalog
              _tblOlib :: LocalBindings
              _tblOoverrideLib :: (Maybe LocalBindings)
              _tblOoverrideLibs :: ([Maybe LocalBindings])
              _tbl1Ocat :: Catalog
              _tbl1Olib :: LocalBindings
              _tbl1OoverrideLib :: (Maybe LocalBindings)
              _tbl1OoverrideLibs :: ([Maybe LocalBindings])
              _onExprOcat :: Catalog
              _onExprOoverrideLib :: (Maybe LocalBindings)
              _onExprOoverrideLibs :: ([Maybe LocalBindings])
              _tblIannotatedTree :: TableRef
              _tblIlibUpdates :: ([LocalBindingsUpdate])
              _tblIoriginalTree :: TableRef
              _tbl1IannotatedTree :: TableRef
              _tbl1IlibUpdates :: ([LocalBindingsUpdate])
              _tbl1IoriginalTree :: TableRef
              _onExprIannotatedTree :: OnExpr
              _onExprIoriginalTree :: OnExpr
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  {-# LINE 91 "./TypeChecking/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 15371 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 180, column 9)
              _errs =
                  {-# LINE 180 "./TypeChecking/TableRefs.ag" #-}
                  fromLeft [] _newLib
                  ++ _joinErrors
                  {-# LINE 15377 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 182, column 9)
              _lhsOlibUpdates =
                  {-# LINE 182 "./TypeChecking/TableRefs.ag" #-}
                  if _joinErrors     == []
                  then _libUpdates
                  else []
                  {-# LINE 15384 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 187, column 9)
              _joinErrors =
                  {-# LINE 187 "./TypeChecking/TableRefs.ag" #-}
                  fromLeft [] (foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _libUpdates    )
                  {-# LINE 15389 "AstInternal.hs" #-}
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
                  {-# LINE 15405 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 202, column 9)
              _newLib =
                  {-# LINE 202 "./TypeChecking/TableRefs.ag" #-}
                  case (_tblIlibUpdates, _tbl1IlibUpdates) of
                    ([u1],[u2]) -> lbUpdate _lhsIcat
                                     (LBJoinTref "join" u1 u2 (Right []) Nothing) _lhsIlib
                    _ -> Right _lhsIlib
                  {-# LINE 15413 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 206, column 9)
              _onExprOlib =
                  {-# LINE 206 "./TypeChecking/TableRefs.ag" #-}
                  fromRight _lhsIlib _newLib
                  {-# LINE 15418 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 265, column 9)
              _backTree =
                  {-# LINE 265 "./TypeChecking/TableRefs.ag" #-}
                  JoinedTref ann_
                             _tblIannotatedTree
                             nat_
                             joinType_
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
                             alias_
                  {-# LINE 15429 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  JoinedTref ann_ _tblIannotatedTree nat_ joinType_ _tbl1IannotatedTree _onExprIannotatedTree alias_
                  {-# LINE 15434 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  JoinedTref ann_ _tblIoriginalTree nat_ joinType_ _tbl1IoriginalTree _onExprIoriginalTree alias_
                  {-# LINE 15439 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15444 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15449 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15454 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15459 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15464 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1Ocat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15469 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1Olib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15474 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1OoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15479 "AstInternal.hs" #-}
              -- copy rule (down)
              _tbl1OoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15484 "AstInternal.hs" #-}
              -- copy rule (down)
              _onExprOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15489 "AstInternal.hs" #-}
              -- copy rule (down)
              _onExprOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15494 "AstInternal.hs" #-}
              -- copy rule (down)
              _onExprOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15499 "AstInternal.hs" #-}
              ( _tblIannotatedTree,_tblIlibUpdates,_tblIoriginalTree) =
                  (tbl_ _tblOcat _tblOlib _tblOoverrideLib _tblOoverrideLibs )
              ( _tbl1IannotatedTree,_tbl1IlibUpdates,_tbl1IoriginalTree) =
                  (tbl1_ _tbl1Ocat _tbl1Olib _tbl1OoverrideLib _tbl1OoverrideLibs )
              ( _onExprIannotatedTree,_onExprIoriginalTree) =
                  (onExpr_ _onExprOcat _onExprOlib _onExprOoverrideLib _onExprOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_TableRef_SubTref :: Annotation ->
                        T_SelectExpression  ->
                        TableAlias ->
                        T_TableRef 
sem_TableRef_SubTref ann_ sel_ alias_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _selOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: TableRef
              _selectAttrs :: (Either [TypeError] [(String,Type)])
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOoriginalTree :: TableRef
              _selOcat :: Catalog
              _selOlib :: LocalBindings
              _selOoverrideLib :: (Maybe LocalBindings)
              _selOoverrideLibs :: ([Maybe LocalBindings])
              _selIannotatedTree :: SelectExpression
              _selIlibUpdates :: ([LocalBindingsUpdate])
              _selIoriginalTree :: SelectExpression
              _selIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Expressions.ag"(line 567, column 15)
              _selOexpectedTypes =
                  {-# LINE 567 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 15533 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  {-# LINE 91 "./TypeChecking/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 15538 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 134, column 9)
              _errs =
                  {-# LINE 134 "./TypeChecking/TableRefs.ag" #-}
                  case _selectAttrs     of
                          Left e -> e
                          Right _ -> []
                  {-# LINE 15545 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 138, column 9)
              _selectAttrs =
                  {-# LINE 138 "./TypeChecking/TableRefs.ag" #-}
                  lmt _selIuType
                  {-# LINE 15550 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 139, column 9)
              _lhsOlibUpdates =
                  {-# LINE 139 "./TypeChecking/TableRefs.ag" #-}
                  [LBTref "sub query" (getAlias "" alias_)
                                  (fromRight [] _selectAttrs    ) []]
                  {-# LINE 15556 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 259, column 9)
              _backTree =
                  {-# LINE 259 "./TypeChecking/TableRefs.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 15561 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  SubTref ann_ _selIannotatedTree alias_
                  {-# LINE 15566 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SubTref ann_ _selIoriginalTree alias_
                  {-# LINE 15571 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15576 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15581 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15586 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15591 "AstInternal.hs" #-}
              -- copy rule (down)
              _selOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15596 "AstInternal.hs" #-}
              ( _selIannotatedTree,_selIlibUpdates,_selIoriginalTree,_selIuType) =
                  (sel_ _selOcat _selOexpectedTypes _selOlib _selOoverrideLib _selOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_TableRef_Tref :: Annotation ->
                     T_Expression  ->
                     TableAlias ->
                     T_TableRef 
sem_TableRef_Tref ann_ tbl_ alias_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _tblOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: TableRef
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOoriginalTree :: TableRef
              _tblOcat :: Catalog
              _tblOlib :: LocalBindings
              _tblOoverrideLib :: (Maybe LocalBindings)
              _tblOoverrideLibs :: ([Maybe LocalBindings])
              _tblIannotatedTree :: Expression
              _tblIntAnnotatedTree :: Expression
              _tblIntType :: ([(String,Type)])
              _tblIoriginalTree :: Expression
              _tblItbAnnotatedTree :: Expression
              _tblItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _tblIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 507, column 12)
              _tblOexpectedType =
                  {-# LINE 507 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 15628 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  {-# LINE 91 "./TypeChecking/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 15633 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 147, column 9)
              _errs =
                  {-# LINE 147 "./TypeChecking/TableRefs.ag" #-}
                  []
                  {-# LINE 15638 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 148, column 9)
              _lhsOlibUpdates =
                  {-# LINE 148 "./TypeChecking/TableRefs.ag" #-}
                  maybe [] id $ do
                  let n = getName _tblIannotatedTree
                  (pu,pr) <- _tblItbUType
                  return [LBTref ("tref: " ++ n)
                            (getAlias n alias_)
                            pu
                            pr]
                  {-# LINE 15649 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 261, column 9)
              _backTree =
                  {-# LINE 261 "./TypeChecking/TableRefs.ag" #-}
                  Tref ann_ _tblItbAnnotatedTree alias_
                  {-# LINE 15654 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  Tref ann_ _tblIannotatedTree alias_
                  {-# LINE 15659 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  Tref ann_ _tblIoriginalTree alias_
                  {-# LINE 15664 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15669 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15674 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15679 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15684 "AstInternal.hs" #-}
              -- copy rule (down)
              _tblOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15689 "AstInternal.hs" #-}
              ( _tblIannotatedTree,_tblIntAnnotatedTree,_tblIntType,_tblIoriginalTree,_tblItbAnnotatedTree,_tblItbUType,_tblIuType) =
                  (tbl_ _tblOcat _tblOexpectedType _tblOlib _tblOoverrideLib _tblOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_TableRef_TrefFun :: Annotation ->
                        T_Expression  ->
                        TableAlias ->
                        T_TableRef 
sem_TableRef_TrefFun ann_ fn_ alias_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _fnOexpectedType :: (Maybe Type)
              _lhsOannotatedTree :: TableRef
              _eqfunIdens :: (Either [TypeError] (String,[(String,Type)]))
              _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOoriginalTree :: TableRef
              _fnOcat :: Catalog
              _fnOlib :: LocalBindings
              _fnOoverrideLib :: (Maybe LocalBindings)
              _fnOoverrideLibs :: ([Maybe LocalBindings])
              _fnIannotatedTree :: Expression
              _fnIntAnnotatedTree :: Expression
              _fnIntType :: ([(String,Type)])
              _fnIoriginalTree :: Expression
              _fnItbAnnotatedTree :: Expression
              _fnItbUType :: (Maybe ([(String,Type)],[(String,Type)]))
              _fnIuType :: (Maybe Type)
              -- "./TypeChecking/Expressions.ag"(line 506, column 15)
              _fnOexpectedType =
                  {-# LINE 506 "./TypeChecking/Expressions.ag" #-}
                  Nothing
                  {-# LINE 15722 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 91, column 9)
              _lhsOannotatedTree =
                  {-# LINE 91 "./TypeChecking/TableRefs.ag" #-}
                  addTypeErrors _errs     _backTree
                  {-# LINE 15727 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 164, column 9)
              _errs =
                  {-# LINE 164 "./TypeChecking/TableRefs.ag" #-}
                  case _eqfunIdens of
                    Left e -> e
                    Right _ -> []
                  {-# LINE 15734 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 170, column 9)
              _eqfunIdens =
                  {-# LINE 170 "./TypeChecking/TableRefs.ag" #-}
                  funIdens _lhsIcat (getAlias "" alias_) _fnIannotatedTree _fnIuType
                  {-# LINE 15739 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 171, column 9)
              _lhsOlibUpdates =
                  {-# LINE 171 "./TypeChecking/TableRefs.ag" #-}
                  [LBTref "fn"
                                  (fst _qfunIdens    )
                                  (snd _qfunIdens    )
                                  []]
                  {-# LINE 15747 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 175, column 9)
              _qfunIdens =
                  {-# LINE 175 "./TypeChecking/TableRefs.ag" #-}
                  fromRight ("",[]) _eqfunIdens
                  {-# LINE 15752 "AstInternal.hs" #-}
              -- "./TypeChecking/TableRefs.ag"(line 263, column 9)
              _backTree =
                  {-# LINE 263 "./TypeChecking/TableRefs.ag" #-}
                  TrefFun ann_ _fnIannotatedTree alias_
                  {-# LINE 15757 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  TrefFun ann_ _fnIannotatedTree alias_
                  {-# LINE 15762 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  TrefFun ann_ _fnIoriginalTree alias_
                  {-# LINE 15767 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15772 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15777 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15782 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15787 "AstInternal.hs" #-}
              -- copy rule (down)
              _fnOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15792 "AstInternal.hs" #-}
              ( _fnIannotatedTree,_fnIntAnnotatedTree,_fnIntType,_fnIoriginalTree,_fnItbAnnotatedTree,_fnItbUType,_fnIuType) =
                  (fn_ _fnOcat _fnOexpectedType _fnOlib _fnOoverrideLib _fnOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree)))
-- TableRefList ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         libUpdates           : [LocalBindingsUpdate]
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
type TableRefList  = [(TableRef)]
-- cata
sem_TableRefList :: TableRefList  ->
                    T_TableRefList 
sem_TableRefList list  =
    (Prelude.foldr sem_TableRefList_Cons sem_TableRefList_Nil (Prelude.map sem_TableRef list) )
-- semantic domain
type T_TableRefList  = Catalog ->
                       LocalBindings ->
                       (Maybe LocalBindings) ->
                       ([Maybe LocalBindings]) ->
                       ( TableRefList,([LocalBindingsUpdate]),TableRefList)
data Inh_TableRefList  = Inh_TableRefList {cat_Inh_TableRefList :: Catalog,lib_Inh_TableRefList :: LocalBindings,overrideLib_Inh_TableRefList :: Maybe LocalBindings,overrideLibs_Inh_TableRefList :: [Maybe LocalBindings]}
data Syn_TableRefList  = Syn_TableRefList {annotatedTree_Syn_TableRefList :: TableRefList,libUpdates_Syn_TableRefList :: [LocalBindingsUpdate],originalTree_Syn_TableRefList :: TableRefList}
wrap_TableRefList :: T_TableRefList  ->
                     Inh_TableRefList  ->
                     Syn_TableRefList 
wrap_TableRefList sem (Inh_TableRefList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_TableRefList _lhsOannotatedTree _lhsOlibUpdates _lhsOoriginalTree ))
sem_TableRefList_Cons :: T_TableRef  ->
                         T_TableRefList  ->
                         T_TableRefList 
sem_TableRefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: TableRefList
              _lhsOoriginalTree :: TableRefList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: TableRef
              _hdIlibUpdates :: ([LocalBindingsUpdate])
              _hdIoriginalTree :: TableRef
              _tlIannotatedTree :: TableRefList
              _tlIlibUpdates :: ([LocalBindingsUpdate])
              _tlIoriginalTree :: TableRefList
              -- "./TypeChecking/TableRefs.ag"(line 97, column 9)
              _lhsOlibUpdates =
                  {-# LINE 97 "./TypeChecking/TableRefs.ag" #-}
                  _hdIlibUpdates
                  {-# LINE 15870 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 15875 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 15880 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15885 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15890 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15895 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15900 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15905 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15910 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 15915 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 15920 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 15925 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 15930 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIlibUpdates,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIlibUpdates,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree)))
sem_TableRefList_Nil :: T_TableRefList 
sem_TableRefList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOlibUpdates :: ([LocalBindingsUpdate])
              _lhsOannotatedTree :: TableRefList
              _lhsOoriginalTree :: TableRefList
              -- "./TypeChecking/TableRefs.ag"(line 95, column 9)
              _lhsOlibUpdates =
                  {-# LINE 95 "./TypeChecking/TableRefs.ag" #-}
                  []
                  {-# LINE 15949 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15954 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 15959 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 15964 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 15969 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOlibUpdates,_lhsOoriginalTree)))
-- TypeAttributeDef --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         attrName             : String
         namedType            : Maybe Type
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
type T_TypeAttributeDef  = Catalog ->
                           LocalBindings ->
                           (Maybe LocalBindings) ->
                           ([Maybe LocalBindings]) ->
                           ( TypeAttributeDef,String,(Maybe Type),TypeAttributeDef)
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {cat_Inh_TypeAttributeDef :: Catalog,lib_Inh_TypeAttributeDef :: LocalBindings,overrideLib_Inh_TypeAttributeDef :: Maybe LocalBindings,overrideLibs_Inh_TypeAttributeDef :: [Maybe LocalBindings]}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef,attrName_Syn_TypeAttributeDef :: String,namedType_Syn_TypeAttributeDef :: Maybe Type,originalTree_Syn_TypeAttributeDef :: TypeAttributeDef}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_TypeAttributeDef _lhsOannotatedTree _lhsOattrName _lhsOnamedType _lhsOoriginalTree ))
sem_TypeAttributeDef_TypeAttDef :: Annotation ->
                                   String ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef ann_ name_ typ_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOattrName :: String
              _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeAttributeDef
              _lhsOoriginalTree :: TypeAttributeDef
              _typOcat :: Catalog
              _typOlib :: LocalBindings
              _typOoverrideLib :: (Maybe LocalBindings)
              _typOoverrideLibs :: ([Maybe LocalBindings])
              _typIannotatedTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/MiscCreates.ag"(line 37, column 9)
              _lhsOattrName =
                  {-# LINE 37 "./TypeChecking/MiscCreates.ag" #-}
                  name_
                  {-# LINE 16039 "AstInternal.hs" #-}
              -- "./TypeChecking/MiscCreates.ag"(line 38, column 9)
              _lhsOnamedType =
                  {-# LINE 38 "./TypeChecking/MiscCreates.ag" #-}
                  _typInamedType
                  {-# LINE 16044 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIannotatedTree
                  {-# LINE 16049 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  TypeAttDef ann_ name_ _typIoriginalTree
                  {-# LINE 16054 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16059 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16064 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16069 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16074 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 16079 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 16084 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOlib _typOoverrideLib _typOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOattrName,_lhsOnamedType,_lhsOoriginalTree)))
-- TypeAttributeDefList ----------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         attrs                : [(String, Maybe Type)]
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
type T_TypeAttributeDefList  = Catalog ->
                               LocalBindings ->
                               (Maybe LocalBindings) ->
                               ([Maybe LocalBindings]) ->
                               ( TypeAttributeDefList,([(String, Maybe Type)]),TypeAttributeDefList)
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {cat_Inh_TypeAttributeDefList :: Catalog,lib_Inh_TypeAttributeDefList :: LocalBindings,overrideLib_Inh_TypeAttributeDefList :: Maybe LocalBindings,overrideLibs_Inh_TypeAttributeDefList :: [Maybe LocalBindings]}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList,attrs_Syn_TypeAttributeDefList :: [(String, Maybe Type)],originalTree_Syn_TypeAttributeDefList :: TypeAttributeDefList}
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_TypeAttributeDefList _lhsOannotatedTree _lhsOattrs _lhsOoriginalTree ))
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOattrs :: ([(String, Maybe Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOoriginalTree :: TypeAttributeDefList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: TypeAttributeDef
              _hdIattrName :: String
              _hdInamedType :: (Maybe Type)
              _hdIoriginalTree :: TypeAttributeDef
              _tlIannotatedTree :: TypeAttributeDefList
              _tlIattrs :: ([(String, Maybe Type)])
              _tlIoriginalTree :: TypeAttributeDefList
              -- "./TypeChecking/MiscCreates.ag"(line 43, column 12)
              _lhsOattrs =
                  {-# LINE 43 "./TypeChecking/MiscCreates.ag" #-}
                  (_hdIattrName, _hdInamedType) : _tlIattrs
                  {-# LINE 16163 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 16168 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 16173 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16178 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16183 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16188 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16193 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 16198 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 16203 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16208 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16213 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 16218 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 16223 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIattrName,_hdInamedType,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIattrs,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOattrs :: ([(String, Maybe Type)])
              _lhsOannotatedTree :: TypeAttributeDefList
              _lhsOoriginalTree :: TypeAttributeDefList
              -- "./TypeChecking/MiscCreates.ag"(line 44, column 11)
              _lhsOattrs =
                  {-# LINE 44 "./TypeChecking/MiscCreates.ag" #-}
                  []
                  {-# LINE 16242 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 16247 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 16252 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16257 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16262 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOattrs,_lhsOoriginalTree)))
-- TypeName ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
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
            local originalTree : _
      alternative PrecTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         child prec           : {Integer}
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
            local originalTree : _
      alternative SetOfTypeName:
         child ann            : {Annotation}
         child typ            : TypeName 
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
            local originalTree : _
      alternative SimpleTypeName:
         child ann            : {Annotation}
         child tn             : {String}
         visit 0:
            local tpe         : _
            local backTree    : _
            local annotatedTree : _
            local originalTree : _
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
type T_TypeName  = Catalog ->
                   LocalBindings ->
                   (Maybe LocalBindings) ->
                   ([Maybe LocalBindings]) ->
                   ( TypeName,(Maybe Type),TypeName)
data Inh_TypeName  = Inh_TypeName {cat_Inh_TypeName :: Catalog,lib_Inh_TypeName :: LocalBindings,overrideLib_Inh_TypeName :: Maybe LocalBindings,overrideLibs_Inh_TypeName :: [Maybe LocalBindings]}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName,namedType_Syn_TypeName :: Maybe Type,originalTree_Syn_TypeName :: TypeName}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_TypeName _lhsOannotatedTree _lhsOnamedType _lhsOoriginalTree ))
sem_TypeName_ArrayTypeName :: Annotation ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName ann_ typ_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeName
              _lhsOoriginalTree :: TypeName
              _typOcat :: Catalog
              _typOlib :: LocalBindings
              _typOoverrideLib :: (Maybe LocalBindings)
              _typOoverrideLibs :: ([Maybe LocalBindings])
              _typIannotatedTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  etmt _tpe
                  {-# LINE 16364 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 16369 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 27, column 9)
              _tpe =
                  {-# LINE 27 "./TypeChecking/Misc.ag" #-}
                  lmt _typInamedType >>=  Right . ArrayType
                  {-# LINE 16374 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 28, column 9)
              _backTree =
                  {-# LINE 28 "./TypeChecking/Misc.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 16379 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIannotatedTree
                  {-# LINE 16384 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  ArrayTypeName ann_ _typIoriginalTree
                  {-# LINE 16389 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16394 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16399 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16404 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 16409 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 16414 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOlib _typOoverrideLib _typOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_PrecTypeName :: Annotation ->
                             String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName ann_ tn_ prec_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeName
              _lhsOoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  etmt _tpe
                  {-# LINE 16434 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 16439 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 33, column 9)
              _tpe =
                  {-# LINE 33 "./TypeChecking/Misc.ag" #-}
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
                  {-# LINE 16444 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 34, column 9)
              _backTree =
                  {-# LINE 34 "./TypeChecking/Misc.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 16449 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 16454 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  PrecTypeName ann_ tn_ prec_
                  {-# LINE 16459 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16464 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_SetOfTypeName :: Annotation ->
                              T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName ann_ typ_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeName
              _lhsOoriginalTree :: TypeName
              _typOcat :: Catalog
              _typOlib :: LocalBindings
              _typOoverrideLib :: (Maybe LocalBindings)
              _typOoverrideLibs :: ([Maybe LocalBindings])
              _typIannotatedTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  etmt _tpe
                  {-# LINE 16488 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 16493 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 30, column 9)
              _tpe =
                  {-# LINE 30 "./TypeChecking/Misc.ag" #-}
                  lmt _typInamedType >>=  Right . SetOfType
                  {-# LINE 16498 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 31, column 9)
              _backTree =
                  {-# LINE 31 "./TypeChecking/Misc.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 16503 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIannotatedTree
                  {-# LINE 16508 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SetOfTypeName ann_ _typIoriginalTree
                  {-# LINE 16513 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16518 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16523 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16528 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 16533 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 16538 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOlib _typOoverrideLib _typOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree)))
sem_TypeName_SimpleTypeName :: Annotation ->
                               String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName ann_ tn_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOnamedType :: (Maybe Type)
              _lhsOannotatedTree :: TypeName
              _lhsOoriginalTree :: TypeName
              -- "./TypeChecking/Misc.ag"(line 19, column 10)
              _lhsOnamedType =
                  {-# LINE 19 "./TypeChecking/Misc.ag" #-}
                  etmt _tpe
                  {-# LINE 16557 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 20, column 10)
              _lhsOannotatedTree =
                  {-# LINE 20 "./TypeChecking/Misc.ag" #-}
                  addTypeErrors (tes _tpe    ) _backTree
                  {-# LINE 16562 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 24, column 9)
              _tpe =
                  {-# LINE 24 "./TypeChecking/Misc.ag" #-}
                  catLookupType _lhsIcat $ canonicalizeTypeName tn_
                  {-# LINE 16567 "AstInternal.hs" #-}
              -- "./TypeChecking/Misc.ag"(line 25, column 9)
              _backTree =
                  {-# LINE 25 "./TypeChecking/Misc.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 16572 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 16577 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  SimpleTypeName ann_ tn_
                  {-# LINE 16582 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16587 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedType,_lhsOoriginalTree)))
-- TypeNameList ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         namedTypes           : [Maybe Type]
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
type TypeNameList  = [(TypeName)]
-- cata
sem_TypeNameList :: TypeNameList  ->
                    T_TypeNameList 
sem_TypeNameList list  =
    (Prelude.foldr sem_TypeNameList_Cons sem_TypeNameList_Nil (Prelude.map sem_TypeName list) )
-- semantic domain
type T_TypeNameList  = Catalog ->
                       LocalBindings ->
                       (Maybe LocalBindings) ->
                       ([Maybe LocalBindings]) ->
                       ( TypeNameList,([Maybe Type]),TypeNameList)
data Inh_TypeNameList  = Inh_TypeNameList {cat_Inh_TypeNameList :: Catalog,lib_Inh_TypeNameList :: LocalBindings,overrideLib_Inh_TypeNameList :: Maybe LocalBindings,overrideLibs_Inh_TypeNameList :: [Maybe LocalBindings]}
data Syn_TypeNameList  = Syn_TypeNameList {annotatedTree_Syn_TypeNameList :: TypeNameList,namedTypes_Syn_TypeNameList :: [Maybe Type],originalTree_Syn_TypeNameList :: TypeNameList}
wrap_TypeNameList :: T_TypeNameList  ->
                     Inh_TypeNameList  ->
                     Syn_TypeNameList 
wrap_TypeNameList sem (Inh_TypeNameList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOnamedTypes,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_TypeNameList _lhsOannotatedTree _lhsOnamedTypes _lhsOoriginalTree ))
sem_TypeNameList_Cons :: T_TypeName  ->
                         T_TypeNameList  ->
                         T_TypeNameList 
sem_TypeNameList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOnamedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: TypeNameList
              _lhsOoriginalTree :: TypeNameList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: TypeName
              _hdInamedType :: (Maybe Type)
              _hdIoriginalTree :: TypeName
              _tlIannotatedTree :: TypeNameList
              _tlInamedTypes :: ([Maybe Type])
              _tlIoriginalTree :: TypeNameList
              -- "./TypeChecking/Drops.ag"(line 37, column 12)
              _lhsOnamedTypes =
                  {-# LINE 37 "./TypeChecking/Drops.ag" #-}
                  _hdInamedType : _tlInamedTypes
                  {-# LINE 16663 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 16668 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 16673 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16678 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16683 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16688 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16693 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 16698 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 16703 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16708 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16713 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 16718 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 16723 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdInamedType,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlInamedTypes,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOnamedTypes,_lhsOoriginalTree)))
sem_TypeNameList_Nil :: T_TypeNameList 
sem_TypeNameList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOnamedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: TypeNameList
              _lhsOoriginalTree :: TypeNameList
              -- "./TypeChecking/Drops.ag"(line 38, column 11)
              _lhsOnamedTypes =
                  {-# LINE 38 "./TypeChecking/Drops.ag" #-}
                  []
                  {-# LINE 16742 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 16747 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 16752 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16757 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16762 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOnamedTypes,_lhsOoriginalTree)))
-- VarDef ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         def                  : (String,Maybe Type)
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
type T_VarDef  = Catalog ->
                 LocalBindings ->
                 (Maybe LocalBindings) ->
                 ([Maybe LocalBindings]) ->
                 ( VarDef,((String,Maybe Type)),VarDef)
data Inh_VarDef  = Inh_VarDef {cat_Inh_VarDef :: Catalog,lib_Inh_VarDef :: LocalBindings,overrideLib_Inh_VarDef :: Maybe LocalBindings,overrideLibs_Inh_VarDef :: [Maybe LocalBindings]}
data Syn_VarDef  = Syn_VarDef {annotatedTree_Syn_VarDef :: VarDef,def_Syn_VarDef :: (String,Maybe Type),originalTree_Syn_VarDef :: VarDef}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOdef,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_VarDef _lhsOannotatedTree _lhsOdef _lhsOoriginalTree ))
sem_VarDef_VarDef :: Annotation ->
                     String ->
                     T_TypeName  ->
                     (Maybe Expression) ->
                     T_VarDef 
sem_VarDef_VarDef ann_ name_ typ_ value_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOdef :: ((String,Maybe Type))
              _lhsOannotatedTree :: VarDef
              _lhsOoriginalTree :: VarDef
              _typOcat :: Catalog
              _typOlib :: LocalBindings
              _typOoverrideLib :: (Maybe LocalBindings)
              _typOoverrideLibs :: ([Maybe LocalBindings])
              _typIannotatedTree :: TypeName
              _typInamedType :: (Maybe Type)
              _typIoriginalTree :: TypeName
              -- "./TypeChecking/Block.ag"(line 10, column 14)
              _lhsOdef =
                  {-# LINE 10 "./TypeChecking/Block.ag" #-}
                  (name_, if _typInamedType == Just (Pseudo Record)
                          then Just (PgRecord Nothing)
                          else _typInamedType)
                  {-# LINE 16834 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIannotatedTree value_
                  {-# LINE 16839 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  VarDef ann_ name_ _typIoriginalTree value_
                  {-# LINE 16844 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16849 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16854 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16859 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16864 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 16869 "AstInternal.hs" #-}
              -- copy rule (down)
              _typOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 16874 "AstInternal.hs" #-}
              ( _typIannotatedTree,_typInamedType,_typIoriginalTree) =
                  (typ_ _typOcat _typOlib _typOoverrideLib _typOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOdef,_lhsOoriginalTree)))
-- VarDefList --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         defs                 : [(String,Maybe Type)]
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
type T_VarDefList  = Catalog ->
                     LocalBindings ->
                     (Maybe LocalBindings) ->
                     ([Maybe LocalBindings]) ->
                     ( VarDefList,([(String,Maybe Type)]),VarDefList)
data Inh_VarDefList  = Inh_VarDefList {cat_Inh_VarDefList :: Catalog,lib_Inh_VarDefList :: LocalBindings,overrideLib_Inh_VarDefList :: Maybe LocalBindings,overrideLibs_Inh_VarDefList :: [Maybe LocalBindings]}
data Syn_VarDefList  = Syn_VarDefList {annotatedTree_Syn_VarDefList :: VarDefList,defs_Syn_VarDefList :: [(String,Maybe Type)],originalTree_Syn_VarDefList :: VarDefList}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOdefs,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_VarDefList _lhsOannotatedTree _lhsOdefs _lhsOoriginalTree ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOdefs :: ([(String,Maybe Type)])
              _lhsOannotatedTree :: VarDefList
              _lhsOoriginalTree :: VarDefList
              _hdOcat :: Catalog
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOcat :: Catalog
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: VarDef
              _hdIdef :: ((String,Maybe Type))
              _hdIoriginalTree :: VarDef
              _tlIannotatedTree :: VarDefList
              _tlIdefs :: ([(String,Maybe Type)])
              _tlIoriginalTree :: VarDefList
              -- "./TypeChecking/Block.ag"(line 15, column 12)
              _lhsOdefs =
                  {-# LINE 15 "./TypeChecking/Block.ag" #-}
                  _hdIdef : _tlIdefs
                  {-# LINE 16952 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 16957 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 16962 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 16967 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 16972 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16977 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 16982 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 16987 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 16992 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 16997 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17002 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 17007 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 17012 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIdef,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIdefs,_tlIoriginalTree) =
                  (tl_ _tlOcat _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOoriginalTree)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOdefs :: ([(String,Maybe Type)])
              _lhsOannotatedTree :: VarDefList
              _lhsOoriginalTree :: VarDefList
              -- "./TypeChecking/Block.ag"(line 16, column 11)
              _lhsOdefs =
                  {-# LINE 16 "./TypeChecking/Block.ag" #-}
                  []
                  {-# LINE 17031 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17036 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17041 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17046 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17051 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOdefs,_lhsOoriginalTree)))
-- WithQuery ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         catUpdates           : [CatalogUpdate]
         originalTree         : SELF 
   alternatives:
      alternative WithQuery:
         child ann            : {Annotation}
         child name           : {String}
         child ex             : SelectExpression 
         visit 0:
            local tpe         : _
            local backTree    : _
            local attrs       : _
            local catUpdates  : _
            local statementType : _
            local annotatedTree : _
            local originalTree : _
-}
data WithQuery  = WithQuery (Annotation) (String) (SelectExpression) 
                deriving ( Data,Eq,Show,Typeable)
-- cata
sem_WithQuery :: WithQuery  ->
                 T_WithQuery 
sem_WithQuery (WithQuery _ann _name _ex )  =
    (sem_WithQuery_WithQuery _ann _name (sem_SelectExpression _ex ) )
-- semantic domain
type T_WithQuery  = Catalog ->
                    LocalBindings ->
                    (Maybe LocalBindings) ->
                    ([Maybe LocalBindings]) ->
                    ( WithQuery,([CatalogUpdate]),WithQuery)
data Inh_WithQuery  = Inh_WithQuery {cat_Inh_WithQuery :: Catalog,lib_Inh_WithQuery :: LocalBindings,overrideLib_Inh_WithQuery :: Maybe LocalBindings,overrideLibs_Inh_WithQuery :: [Maybe LocalBindings]}
data Syn_WithQuery  = Syn_WithQuery {annotatedTree_Syn_WithQuery :: WithQuery,catUpdates_Syn_WithQuery :: [CatalogUpdate],originalTree_Syn_WithQuery :: WithQuery}
wrap_WithQuery :: T_WithQuery  ->
                  Inh_WithQuery  ->
                  Syn_WithQuery 
wrap_WithQuery sem (Inh_WithQuery _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOoriginalTree) =
             (sem _lhsIcat _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_WithQuery _lhsOannotatedTree _lhsOcatUpdates _lhsOoriginalTree ))
sem_WithQuery_WithQuery :: Annotation ->
                           String ->
                           T_SelectExpression  ->
                           T_WithQuery 
sem_WithQuery_WithQuery ann_ name_ ex_  =
    (\ _lhsIcat
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _exOexpectedTypes :: ([Maybe Type])
              _lhsOannotatedTree :: WithQuery
              _lhsOoriginalTree :: WithQuery
              _lhsOcatUpdates :: ([CatalogUpdate])
              _exOcat :: Catalog
              _exOlib :: LocalBindings
              _exOoverrideLib :: (Maybe LocalBindings)
              _exOoverrideLibs :: ([Maybe LocalBindings])
              _exIannotatedTree :: SelectExpression
              _exIlibUpdates :: ([LocalBindingsUpdate])
              _exIoriginalTree :: SelectExpression
              _exIuType :: (Maybe [(String,Type)])
              -- "./TypeChecking/Expressions.ag"(line 569, column 17)
              _exOexpectedTypes =
                  {-# LINE 569 "./TypeChecking/Expressions.ag" #-}
                  []
                  {-# LINE 17126 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 246, column 9)
              _tpe =
                  {-# LINE 246 "./TypeChecking/SelectStatement.ag" #-}
                  Right $ Pseudo Void
                  {-# LINE 17131 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 247, column 9)
              _backTree =
                  {-# LINE 247 "./TypeChecking/SelectStatement.ag" #-}
                  WithQuery ann_ name_ _exIannotatedTree
                  {-# LINE 17136 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 248, column 9)
              _attrs =
                  {-# LINE 248 "./TypeChecking/SelectStatement.ag" #-}
                  maybe [] id $ _exIuType
                  {-# LINE 17141 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 249, column 9)
              _catUpdates =
                  {-# LINE 249 "./TypeChecking/SelectStatement.ag" #-}
                  [CatCreateView name_ _attrs    ]
                  {-# LINE 17146 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 250, column 9)
              _statementType =
                  {-# LINE 250 "./TypeChecking/SelectStatement.ag" #-}
                  Nothing
                  {-# LINE 17151 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  WithQuery ann_ name_ _exIannotatedTree
                  {-# LINE 17156 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  WithQuery ann_ name_ _exIoriginalTree
                  {-# LINE 17161 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17166 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17171 "AstInternal.hs" #-}
              -- copy rule (from local)
              _lhsOcatUpdates =
                  {-# LINE 220 "./TypeChecking/SelectStatement.ag" #-}
                  _catUpdates
                  {-# LINE 17176 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOcat =
                  {-# LINE 56 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIcat
                  {-# LINE 17181 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17186 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 17191 "AstInternal.hs" #-}
              -- copy rule (down)
              _exOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 17196 "AstInternal.hs" #-}
              ( _exIannotatedTree,_exIlibUpdates,_exIoriginalTree,_exIuType) =
                  (ex_ _exOcat _exOexpectedTypes _exOlib _exOoverrideLib _exOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOcatUpdates,_lhsOoriginalTree)))
-- WithQueryList -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         catUpdates           : [CatalogUpdate]
         lib                  : LocalBindings
         overrideLib          : Maybe LocalBindings
         overrideLibs         : [Maybe LocalBindings]
      synthesized attributes:
         annotatedTree        : SELF 
         originalTree         : SELF 
         producedCat          : Catalog
   alternatives:
      alternative Cons:
         child hd             : WithQuery 
         child tl             : WithQueryList 
         visit 0:
            local newCat      : _
            local annotatedTree : _
            local originalTree : _
      alternative Nil:
         visit 0:
            local newCat      : _
            local annotatedTree : _
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
                        LocalBindings ->
                        (Maybe LocalBindings) ->
                        ([Maybe LocalBindings]) ->
                        ( WithQueryList,WithQueryList,Catalog)
data Inh_WithQueryList  = Inh_WithQueryList {cat_Inh_WithQueryList :: Catalog,catUpdates_Inh_WithQueryList :: [CatalogUpdate],lib_Inh_WithQueryList :: LocalBindings,overrideLib_Inh_WithQueryList :: Maybe LocalBindings,overrideLibs_Inh_WithQueryList :: [Maybe LocalBindings]}
data Syn_WithQueryList  = Syn_WithQueryList {annotatedTree_Syn_WithQueryList :: WithQueryList,originalTree_Syn_WithQueryList :: WithQueryList,producedCat_Syn_WithQueryList :: Catalog}
wrap_WithQueryList :: T_WithQueryList  ->
                      Inh_WithQueryList  ->
                      Syn_WithQueryList 
wrap_WithQueryList sem (Inh_WithQueryList _lhsIcat _lhsIcatUpdates _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )  =
    (let ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedCat) =
             (sem _lhsIcat _lhsIcatUpdates _lhsIlib _lhsIoverrideLib _lhsIoverrideLibs )
     in  (Syn_WithQueryList _lhsOannotatedTree _lhsOoriginalTree _lhsOproducedCat ))
sem_WithQueryList_Cons :: T_WithQuery  ->
                          T_WithQueryList  ->
                          T_WithQueryList 
sem_WithQueryList_Cons hd_ tl_  =
    (\ _lhsIcat
       _lhsIcatUpdates
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _hdOcat :: Catalog
              _tlOcat :: Catalog
              _lhsOproducedCat :: Catalog
              _tlOcatUpdates :: ([CatalogUpdate])
              _lhsOannotatedTree :: WithQueryList
              _lhsOoriginalTree :: WithQueryList
              _hdOlib :: LocalBindings
              _hdOoverrideLib :: (Maybe LocalBindings)
              _hdOoverrideLibs :: ([Maybe LocalBindings])
              _tlOlib :: LocalBindings
              _tlOoverrideLib :: (Maybe LocalBindings)
              _tlOoverrideLibs :: ([Maybe LocalBindings])
              _hdIannotatedTree :: WithQuery
              _hdIcatUpdates :: ([CatalogUpdate])
              _hdIoriginalTree :: WithQuery
              _tlIannotatedTree :: WithQueryList
              _tlIoriginalTree :: WithQueryList
              _tlIproducedCat :: Catalog
              -- "./TypeChecking/SelectStatement.ag"(line 230, column 9)
              _newCat =
                  {-# LINE 230 "./TypeChecking/SelectStatement.ag" #-}
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
                  {-# LINE 17280 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 232, column 9)
              _hdOcat =
                  {-# LINE 232 "./TypeChecking/SelectStatement.ag" #-}
                  _newCat
                  {-# LINE 17285 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 233, column 9)
              _tlOcat =
                  {-# LINE 233 "./TypeChecking/SelectStatement.ag" #-}
                  _newCat
                  {-# LINE 17290 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 237, column 9)
              _lhsOproducedCat =
                  {-# LINE 237 "./TypeChecking/SelectStatement.ag" #-}
                  _tlIproducedCat
                  {-# LINE 17295 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 240, column 9)
              _tlOcatUpdates =
                  {-# LINE 240 "./TypeChecking/SelectStatement.ag" #-}
                  _hdIcatUpdates
                  {-# LINE 17300 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIannotatedTree _tlIannotatedTree
                  {-# LINE 17305 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  (:) _hdIoriginalTree _tlIoriginalTree
                  {-# LINE 17310 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17315 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17320 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17325 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 17330 "AstInternal.hs" #-}
              -- copy rule (down)
              _hdOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 17335 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOlib =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _lhsIlib
                  {-# LINE 17340 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLib =
                  {-# LINE 114 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLib
                  {-# LINE 17345 "AstInternal.hs" #-}
              -- copy rule (down)
              _tlOoverrideLibs =
                  {-# LINE 115 "./TypeChecking/Expressions.ag" #-}
                  _lhsIoverrideLibs
                  {-# LINE 17350 "AstInternal.hs" #-}
              ( _hdIannotatedTree,_hdIcatUpdates,_hdIoriginalTree) =
                  (hd_ _hdOcat _hdOlib _hdOoverrideLib _hdOoverrideLibs )
              ( _tlIannotatedTree,_tlIoriginalTree,_tlIproducedCat) =
                  (tl_ _tlOcat _tlOcatUpdates _tlOlib _tlOoverrideLib _tlOoverrideLibs )
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedCat)))
sem_WithQueryList_Nil :: T_WithQueryList 
sem_WithQueryList_Nil  =
    (\ _lhsIcat
       _lhsIcatUpdates
       _lhsIlib
       _lhsIoverrideLib
       _lhsIoverrideLibs ->
         (let _lhsOproducedCat :: Catalog
              _lhsOannotatedTree :: WithQueryList
              _lhsOoriginalTree :: WithQueryList
              -- "./TypeChecking/SelectStatement.ag"(line 230, column 9)
              _newCat =
                  {-# LINE 230 "./TypeChecking/SelectStatement.ag" #-}
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
                  {-# LINE 17370 "AstInternal.hs" #-}
              -- "./TypeChecking/SelectStatement.ag"(line 242, column 9)
              _lhsOproducedCat =
                  {-# LINE 242 "./TypeChecking/SelectStatement.ag" #-}
                  _newCat
                  {-# LINE 17375 "AstInternal.hs" #-}
              -- self rule
              _annotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17380 "AstInternal.hs" #-}
              -- self rule
              _originalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  []
                  {-# LINE 17385 "AstInternal.hs" #-}
              -- self rule
              _lhsOannotatedTree =
                  {-# LINE 57 "./TypeChecking/TypeChecking.ag" #-}
                  _annotatedTree
                  {-# LINE 17390 "AstInternal.hs" #-}
              -- self rule
              _lhsOoriginalTree =
                  {-# LINE 63 "./TypeChecking/TypeChecking.ag" #-}
                  _originalTree
                  {-# LINE 17395 "AstInternal.hs" #-}
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedCat)))