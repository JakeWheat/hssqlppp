

-- UUAGC 0.9.23 (AstInternal.ag)
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
import Debug.Trace
import Data.Either
import Control.Applicative
import Control.Arrow
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



{-
================================================================================

= some small utils

I think this should be alright, an identifier referenced in an
expression can only have zero or one dot in it.
-}

splitIdentifier :: String -> (String,String)
splitIdentifier s = let (a,b) = span (/= '.') s
                    in if b == ""
                         then ("", a)
                         else (a,tail b)

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

keepValid :: [(String,Maybe Type)] -> [(String,Type)]
keepValid = mapMaybe f
            where
              f (a,Just b) = Just (a,b)
              f (_,Nothing) = Nothing

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


expandComposite :: Catalog -> Type -> Maybe [(String,Type)]
expandComposite cat (SetOfType t) = expandComposite cat t
expandComposite cat (PgRecord (Just t)) = expandComposite cat t
expandComposite _ (CompositeType fs) = Just fs
expandComposite cat (NamedCompositeType n) = etmt $ catCompositeAttrs cat [] n

unwrapLookup :: (String,[String],Type) -> Type
unwrapLookup (_,_,t) = t

unwrapStar :: [(String,[String],Type)] -> [(String,Type)]
unwrapStar = map uw
             where
               uw (_,n,t) = (last n, t)

allAtts :: ([(String,Type)],[(String,Type)]) -> [(String,Type)]
allAtts (a,b) = a ++ b

pubAtts :: ([(String,Type)],[(String,Type)]) -> [(String,Type)]
pubAtts = fst

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



{-data SiType = SiType (String,Maybe Type)
            | SiStarType [(String,Maybe Type)]-}


unwrapSetofs :: [(String,Type)] -> [(String,Type)]
unwrapSetofs = map (\(n,t) -> (n, unwrapSetof t))

unwrapSetof :: Type -> Type
unwrapSetof (SetOfType u) = u
unwrapSetof v = v



isPgRecord :: Type -> Bool
isPgRecord (PgRecord _) = True
isPgRecord _ = False


defaultSystemColumns :: [(String,Type)]
defaultSystemColumns = [("tableoid", ScalarType "oid")
                       ,("cmax", ScalarType "cid")
                       ,("xmax", ScalarType "xid")
                       ,("cmin", ScalarType "cid")
                       ,("xmin", ScalarType "xid")
                       ,("ctid", ScalarType "tid")]


data ParamName = NamedParam Int String
               | UnnamedParam Int
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
data AlterTableAction  = AddConstraint (Annotation) (Constraint ) 
                       | AlterColumnDefault (Annotation) (String) (Expression ) 
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
                           ( AlterTableAction ,AlterTableAction )
data Inh_AlterTableAction  = Inh_AlterTableAction {cat_Inh_AlterTableAction :: Catalog,lib_Inh_AlterTableAction :: LocalBindings,overrideLib_Inh_AlterTableAction :: (Maybe LocalBindings),overrideLibs_Inh_AlterTableAction :: ([Maybe LocalBindings])}
data Syn_AlterTableAction  = Syn_AlterTableAction {annotatedTree_Syn_AlterTableAction :: AlterTableAction ,originalTree_Syn_AlterTableAction :: AlterTableAction }
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
                  AddConstraint ann_ _conIannotatedTree
              -- self rule
              _originalTree =
                  AddConstraint ann_ _conIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _conOcat =
                  _lhsIcat
              -- copy rule (down)
              _conOlib =
                  _lhsIlib
              -- copy rule (down)
              _conOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _conOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- self rule
              _annotatedTree =
                  AlterColumnDefault ann_ nm_ _defIannotatedTree
              -- self rule
              _originalTree =
                  AlterColumnDefault ann_ nm_ _defIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _defOcat =
                  _lhsIcat
              -- copy rule (down)
              _defOlib =
                  _lhsIlib
              -- copy rule (down)
              _defOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _defOoverrideLibs =
                  _lhsIoverrideLibs
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
type AlterTableActionList  = [AlterTableAction ]
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
                               ( AlterTableActionList ,AlterTableActionList )
data Inh_AlterTableActionList  = Inh_AlterTableActionList {cat_Inh_AlterTableActionList :: Catalog,lib_Inh_AlterTableActionList :: LocalBindings,overrideLib_Inh_AlterTableActionList :: (Maybe LocalBindings),overrideLibs_Inh_AlterTableActionList :: ([Maybe LocalBindings])}
data Syn_AlterTableActionList  = Syn_AlterTableActionList {annotatedTree_Syn_AlterTableActionList :: AlterTableActionList ,originalTree_Syn_AlterTableActionList :: AlterTableActionList }
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
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data AttributeDef  = AttributeDef (Annotation) (String) (TypeName ) (MaybeExpression ) (RowConstraintList ) 
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
                       ( AttributeDef ,String,(Maybe Type),AttributeDef )
data Inh_AttributeDef  = Inh_AttributeDef {cat_Inh_AttributeDef :: Catalog,lib_Inh_AttributeDef :: LocalBindings,overrideLib_Inh_AttributeDef :: (Maybe LocalBindings),overrideLibs_Inh_AttributeDef :: ([Maybe LocalBindings])}
data Syn_AttributeDef  = Syn_AttributeDef {annotatedTree_Syn_AttributeDef :: AttributeDef ,attrName_Syn_AttributeDef :: String,namedType_Syn_AttributeDef :: (Maybe Type),originalTree_Syn_AttributeDef :: AttributeDef }
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
              _originalTree =
                  AttributeDef ann_ name_ _typIoriginalTree _defIoriginalTree _consIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              -- copy rule (down)
              _typOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _typOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _defOcat =
                  _lhsIcat
              -- copy rule (down)
              _defOlib =
                  _lhsIlib
              -- copy rule (down)
              _defOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _defOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _consOcat =
                  _lhsIcat
              -- copy rule (down)
              _consOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _consOoverrideLibs =
                  _lhsIoverrideLibs
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
type AttributeDefList  = [AttributeDef ]
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
                           ( AttributeDefList ,([(String, Maybe Type)]),AttributeDefList )
data Inh_AttributeDefList  = Inh_AttributeDefList {cat_Inh_AttributeDefList :: Catalog,lib_Inh_AttributeDefList :: LocalBindings,overrideLib_Inh_AttributeDefList :: (Maybe LocalBindings),overrideLibs_Inh_AttributeDefList :: ([Maybe LocalBindings])}
data Syn_AttributeDefList  = Syn_AttributeDefList {annotatedTree_Syn_AttributeDefList :: AttributeDefList ,attrs_Syn_AttributeDefList :: ([(String, Maybe Type)]),originalTree_Syn_AttributeDefList :: AttributeDefList }
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
                  (_hdIattrName, _hdInamedType) : _tlIattrs
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type CaseExpressionListExpressionPair  = ( ExpressionList ,Expression )
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
                                           ( CaseExpressionListExpressionPair ,CaseExpressionListExpressionPair ,(Maybe Type),([Maybe Type]))
data Inh_CaseExpressionListExpressionPair  = Inh_CaseExpressionListExpressionPair {cat_Inh_CaseExpressionListExpressionPair :: Catalog,lib_Inh_CaseExpressionListExpressionPair :: LocalBindings,overrideLib_Inh_CaseExpressionListExpressionPair :: (Maybe LocalBindings),overrideLibs_Inh_CaseExpressionListExpressionPair :: ([Maybe LocalBindings])}
data Syn_CaseExpressionListExpressionPair  = Syn_CaseExpressionListExpressionPair {annotatedTree_Syn_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair ,originalTree_Syn_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair ,thenType_Syn_CaseExpressionListExpressionPair :: (Maybe Type),whenTypes_Syn_CaseExpressionListExpressionPair :: ([Maybe Type])}
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
                  _x1IuType
              -- "./TypeChecking/Expressions.ag"(line 260, column 13)
              _lhsOthenType =
                  _x2IuType
              -- "./TypeChecking/Expressions.ag"(line 463, column 13)
              _x1OexpectedTypes =
                  []
              -- "./TypeChecking/Expressions.ag"(line 464, column 13)
              _x2OexpectedType =
                  Nothing
              -- self rule
              _annotatedTree =
                  (_x1IannotatedTree,_x2IannotatedTree)
              -- self rule
              _originalTree =
                  (_x1IoriginalTree,_x2IoriginalTree)
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _x1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x1Olib =
                  _lhsIlib
              -- copy rule (down)
              _x1OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _x1OoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _x2Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x2Olib =
                  _lhsIlib
              -- copy rule (down)
              _x2OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _x2OoverrideLibs =
                  _lhsIoverrideLibs
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
type CaseExpressionListExpressionPairList  = [CaseExpressionListExpressionPair ]
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
                                               ( CaseExpressionListExpressionPairList ,CaseExpressionListExpressionPairList ,([Maybe Type]),([[Maybe Type]]))
data Inh_CaseExpressionListExpressionPairList  = Inh_CaseExpressionListExpressionPairList {cat_Inh_CaseExpressionListExpressionPairList :: Catalog,lib_Inh_CaseExpressionListExpressionPairList :: LocalBindings,overrideLib_Inh_CaseExpressionListExpressionPairList :: (Maybe LocalBindings),overrideLibs_Inh_CaseExpressionListExpressionPairList :: ([Maybe LocalBindings])}
data Syn_CaseExpressionListExpressionPairList  = Syn_CaseExpressionListExpressionPairList {annotatedTree_Syn_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList ,originalTree_Syn_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList ,thenTypes_Syn_CaseExpressionListExpressionPairList :: ([Maybe Type]),whenTypes_Syn_CaseExpressionListExpressionPairList :: ([[Maybe Type]])}
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
                  _hdIwhenTypes : _tlIwhenTypes
              -- "./TypeChecking/Expressions.ag"(line 251, column 10)
              _lhsOthenTypes =
                  _hdIthenType : _tlIthenTypes
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- "./TypeChecking/Expressions.ag"(line 253, column 9)
              _lhsOthenTypes =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data Constraint  = CheckConstraint (Annotation) (String) (Expression ) 
                 | PrimaryKeyConstraint (Annotation) (String) (([String])) 
                 | ReferenceConstraint (Annotation) (String) (([String])) (String) (([String])) (Cascade) (Cascade) 
                 | UniqueConstraint (Annotation) (String) (([String])) 
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
                     ( Constraint ,Constraint )
data Inh_Constraint  = Inh_Constraint {cat_Inh_Constraint :: Catalog,lib_Inh_Constraint :: LocalBindings,overrideLib_Inh_Constraint :: (Maybe LocalBindings),overrideLibs_Inh_Constraint :: ([Maybe LocalBindings])}
data Syn_Constraint  = Syn_Constraint {annotatedTree_Syn_Constraint :: Constraint ,originalTree_Syn_Constraint :: Constraint }
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
                  Nothing
              -- self rule
              _annotatedTree =
                  CheckConstraint ann_ name_ _exprIannotatedTree
              -- self rule
              _originalTree =
                  CheckConstraint ann_ name_ _exprIoriginalTree
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
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
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
                  PrimaryKeyConstraint ann_ name_ x_
              -- self rule
              _originalTree =
                  PrimaryKeyConstraint ann_ name_ x_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
              -- self rule
              _originalTree =
                  ReferenceConstraint ann_ name_ atts_ table_ tableAtts_ onUpdate_ onDelete_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  UniqueConstraint ann_ name_ x_
              -- self rule
              _originalTree =
                  UniqueConstraint ann_ name_ x_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type ConstraintList  = [Constraint ]
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
                         ( ConstraintList ,ConstraintList )
data Inh_ConstraintList  = Inh_ConstraintList {cat_Inh_ConstraintList :: Catalog,lib_Inh_ConstraintList :: LocalBindings,overrideLib_Inh_ConstraintList :: (Maybe LocalBindings),overrideLibs_Inh_ConstraintList :: ([Maybe LocalBindings])}
data Syn_ConstraintList  = Syn_ConstraintList {annotatedTree_Syn_ConstraintList :: ConstraintList ,originalTree_Syn_ConstraintList :: ConstraintList }
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
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                 | Case (Annotation) (CaseExpressionListExpressionPairList ) (MaybeExpression ) 
                 | CaseSimple (Annotation) (Expression ) (CaseExpressionListExpressionPairList ) (MaybeExpression ) 
                 | Cast (Annotation) (Expression ) (TypeName ) 
                 | Exists (Annotation) (SelectExpression ) 
                 | FloatLit (Annotation) (Double) 
                 | FunCall (Annotation) (String) (ExpressionList ) 
                 | Identifier (Annotation) (String) 
                 | InPredicate (Annotation) (Expression ) (Bool) (InList ) 
                 | IntegerLit (Annotation) (Integer) 
                 | LiftOperator (Annotation) (String) (LiftFlavour) (ExpressionList ) 
                 | NullLit (Annotation) 
                 | PIdentifier (Annotation) (Expression ) 
                 | Placeholder (Annotation) 
                 | PositionalArg (Annotation) (Integer) 
                 | ScalarSubQuery (Annotation) (SelectExpression ) 
                 | StringLit (Annotation) (String) 
                 | WindowFn (Annotation) (Expression ) (ExpressionList ) (ExpressionList ) (Direction) (FrameClause) 
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
                     ( Expression ,Expression ,([(String,Type)]),Expression ,Expression ,(Maybe ([(String,Type)],[(String,Type)])),(Maybe Type))
data Inh_Expression  = Inh_Expression {cat_Inh_Expression :: Catalog,expectedType_Inh_Expression :: (Maybe Type),lib_Inh_Expression :: LocalBindings,overrideLib_Inh_Expression :: (Maybe LocalBindings),overrideLibs_Inh_Expression :: ([Maybe LocalBindings])}
data Syn_Expression  = Syn_Expression {annotatedTree_Syn_Expression :: Expression ,ntAnnotatedTree_Syn_Expression :: Expression ,ntType_Syn_Expression :: ([(String,Type)]),originalTree_Syn_Expression :: Expression ,tbAnnotatedTree_Syn_Expression :: Expression ,tbUType_Syn_Expression :: (Maybe ([(String,Type)],[(String,Type)])),uType_Syn_Expression :: (Maybe Type)}
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 152, column 19)
              _tpe =
                  Right typeBool
              -- "./TypeChecking/Expressions.ag"(line 164, column 9)
              _backTree =
                  BooleanLit ann_ b_
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
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
              _originalTree =
                  BooleanLit ann_ b_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 265, column 9)
              _whenTypes =
                  _casesIwhenTypes
              -- "./TypeChecking/Expressions.ag"(line 266, column 9)
              _thenTypes =
                  _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIuType
              -- "./TypeChecking/Expressions.ag"(line 270, column 9)
              _tpe =
                  do
                  wt <- mapM lmt $ concat _whenTypes
                  errorWhen (any (/= typeBool) wt)
                      [WrongTypes typeBool wt]
                  tt <- mapM lmt _thenTypes
                  resolveResultSetType _lhsIcat tt
              -- "./TypeChecking/Expressions.ag"(line 278, column 9)
              _backTree =
                  Case ann_ _casesIannotatedTree _elsIannotatedTree
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
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
              _originalTree =
                  Case ann_ _casesIoriginalTree _elsIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _casesOcat =
                  _lhsIcat
              -- copy rule (down)
              _casesOlib =
                  _lhsIlib
              -- copy rule (down)
              _casesOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _casesOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _elsOcat =
                  _lhsIcat
              -- copy rule (down)
              _elsOlib =
                  _lhsIlib
              -- copy rule (down)
              _elsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _elsOoverrideLibs =
                  _lhsIoverrideLibs
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 265, column 9)
              _whenTypes =
                  _casesIwhenTypes
              -- "./TypeChecking/Expressions.ag"(line 266, column 9)
              _thenTypes =
                  _casesIthenTypes ++ maybe [] ((:[]) . Just) _elsIuType
              -- "./TypeChecking/Expressions.ag"(line 283, column 9)
              _tpe =
                  do
                  wt <- mapM lmt $ concat _whenTypes
                  vt <- lmt _valueIuType
                  _ <- resolveResultSetType _lhsIcat (vt : wt)
                  tt <- mapM lmt _thenTypes
                  resolveResultSetType _lhsIcat tt
              -- "./TypeChecking/Expressions.ag"(line 290, column 9)
              _backTree =
                  CaseSimple ann_
                             _valueIannotatedTree
                             _casesIannotatedTree
                             _elsIannotatedTree
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
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
              _originalTree =
                  CaseSimple ann_ _valueIoriginalTree _casesIoriginalTree _elsIoriginalTree
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
              _valueOlib =
                  _lhsIlib
              -- copy rule (down)
              _valueOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _valueOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _casesOcat =
                  _lhsIcat
              -- copy rule (down)
              _casesOlib =
                  _lhsIlib
              -- copy rule (down)
              _casesOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _casesOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _elsOcat =
                  _lhsIcat
              -- copy rule (down)
              _elsOlib =
                  _lhsIlib
              -- copy rule (down)
              _elsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _elsOoverrideLibs =
                  _lhsIoverrideLibs
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 177, column 12)
              _tpe =
                  lmt _tnInamedType
              -- "./TypeChecking/Expressions.ag"(line 178, column 12)
              _backTree =
                  Cast ann_ _exprIannotatedTree _tnIannotatedTree
              -- "./TypeChecking/Expressions.ag"(line 342, column 7)
              _liftedColumnName =
                  case _tnIannotatedTree of
                    SimpleTypeName _ tn -> tn
                    _ -> ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
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
              _originalTree =
                  Cast ann_ _exprIoriginalTree _tnIoriginalTree
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
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tnOcat =
                  _lhsIcat
              -- copy rule (down)
              _tnOlib =
                  _lhsIlib
              -- copy rule (down)
              _tnOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tnOoverrideLibs =
                  _lhsIoverrideLibs
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/Expressions.ag"(line 380, column 9)
              _tpe =
                  Right typeBool
              -- "./TypeChecking/Expressions.ag"(line 381, column 9)
              _backTree =
                  Exists ann_ _selIannotatedTree
              -- "./TypeChecking/Expressions.ag"(line 558, column 29)
              _selOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  Exists ann_ _selIannotatedTree
              -- self rule
              _originalTree =
                  Exists ann_ _selIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              -- copy rule (down)
              _selOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selOoverrideLibs =
                  _lhsIoverrideLibs
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 151, column 17)
              _tpe =
                  Right typeNumeric
              -- "./TypeChecking/Expressions.ag"(line 162, column 9)
              _backTree =
                  FloatLit ann_ d_
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
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
              _originalTree =
                  FloatLit ann_ d_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 64, column 9)
              _tbUType =
                  case (funName_,_argsItbUTypes) of
                   (".", [_,Just t]) -> Right t
                   _ -> Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 126, column 15)
              _argsOoverrideLibs =
                  case funName_ of
                    "." | [Identifier _ i,_] <- _argsIannotatedTree
                            -> [Nothing
                               ,either (const Nothing) Just $
                                        lbUpdateDot _lhsIcat i _lhsIlib]
                    _ -> []
              -- "./TypeChecking/Expressions.ag"(line 185, column 9)
              __tup1 =
                  either (\e -> (Left e, Nothing)) id $ do
                  args <- mapM lmt _argsIuType
                  efp <- findCallMatch _lhsIcat
                                       funName_
                                       args
                  let (_,_,r,_) = efp
                  return (Right r, Just efp)
              -- "./TypeChecking/Expressions.ag"(line 185, column 9)
              (_tpe,_) =
                  __tup1
              -- "./TypeChecking/Expressions.ag"(line 185, column 9)
              (_,_prototype) =
                  __tup1
              -- "./TypeChecking/Expressions.ag"(line 195, column 9)
              _backTree =
                  FunCall ann_ funName_ _argsIannotatedTree
              -- "./TypeChecking/Expressions.ag"(line 336, column 7)
              _liftedColumnName =
                  case funName_ of
                    "." -> getName _backTree
                    x | isOperatorName x -> "?column?"
                    _ -> funName_
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/Expressions.ag"(line 517, column 9)
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
              _originalTree =
                  FunCall ann_ funName_ _argsIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _argsOcat =
                  _lhsIcat
              -- copy rule (down)
              _argsOlib =
                  _lhsIlib
              -- copy rule (down)
              _argsOoverrideLib =
                  _lhsIoverrideLib
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 68, column 9)
              _tbUType =
                  catCompositeAttrsPair _lhsIcat relationComposites i_
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 323, column 9)
              _useLib =
                  maybe _lhsIlib id _lhsIoverrideLib
              -- "./TypeChecking/Expressions.ag"(line 324, column 9)
              _tpe =
                  unwrapLookup <$> lbLookupID _useLib     i_
              -- "./TypeChecking/Expressions.ag"(line 325, column 9)
              _backTree =
                  Identifier ann_ i_
              -- "./TypeChecking/Expressions.ag"(line 328, column 9)
              _ntType =
                  if i_ == "*"
                  then unwrapStar <$> lbExpandStar _useLib
                  else (\t -> [(i_, t)]) <$> unwrapLookup <$> lbLookupID _useLib     i_
              -- self rule
              _annotatedTree =
                  Identifier ann_ i_
              -- self rule
              _originalTree =
                  Identifier ann_ i_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/Expressions.ag"(line 408, column 9)
              _tpe =
                  do
                  lt <- _listIlistType
                  expt <- lmt _exprIuType
                  _ <- resolveResultSetType _lhsIcat [expt, lt]
                  return typeBool
              -- "./TypeChecking/Expressions.ag"(line 413, column 9)
              _backTree =
                  InPredicate ann_
                              _exprIannotatedTree
                              i_
                              _listIannotatedTree
              -- self rule
              _annotatedTree =
                  InPredicate ann_ _exprIannotatedTree i_ _listIannotatedTree
              -- self rule
              _originalTree =
                  InPredicate ann_ _exprIoriginalTree i_ _listIoriginalTree
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
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _listOcat =
                  _lhsIcat
              -- copy rule (down)
              _listOlib =
                  _lhsIlib
              -- copy rule (down)
              _listOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _listOoverrideLibs =
                  _lhsIoverrideLibs
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 149, column 19)
              _tpe =
                  Right typeInt
              -- "./TypeChecking/Expressions.ag"(line 158, column 9)
              _backTree =
                  IntegerLit ann_ i_
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
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
              _originalTree =
                  IntegerLit ann_ i_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 218, column 9)
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
              -- "./TypeChecking/Expressions.ag"(line 232, column 9)
              _backTree =
                  LiftOperator ann_ oper_ flav_ _argsIannotatedTree
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/Expressions.ag"(line 525, column 9)
              _argsOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  LiftOperator ann_ oper_ flav_ _argsIannotatedTree
              -- self rule
              _originalTree =
                  LiftOperator ann_ oper_ flav_ _argsIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _argsOcat =
                  _lhsIcat
              -- copy rule (down)
              _argsOlib =
                  _lhsIlib
              -- copy rule (down)
              _argsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _argsOoverrideLibs =
                  _lhsIoverrideLibs
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 154, column 16)
              _tpe =
                  Right UnknownType
              -- "./TypeChecking/Expressions.ag"(line 166, column 9)
              _backTree =
                  NullLit ann_
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
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
              _originalTree =
                  NullLit ann_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  PIdentifier ann_ _exIannotatedTree
              -- self rule
              _originalTree =
                  PIdentifier ann_ _exIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (up)
              _lhsOntAnnotatedTree =
                  _exIntAnnotatedTree
              -- copy rule (up)
              _lhsOntType =
                  _exIntType
              -- copy rule (up)
              _lhsOtbAnnotatedTree =
                  _exItbAnnotatedTree
              -- copy rule (up)
              _lhsOtbUType =
                  _exItbUType
              -- copy rule (up)
              _lhsOuType =
                  _exIuType
              -- copy rule (down)
              _exOcat =
                  _lhsIcat
              -- copy rule (down)
              _exOexpectedType =
                  _lhsIexpectedType
              -- copy rule (down)
              _exOlib =
                  _lhsIlib
              -- copy rule (down)
              _exOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exOoverrideLibs =
                  _lhsIoverrideLibs
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/Expressions.ag"(line 374, column 9)
              _tpe =
                  Right UnknownType
              -- "./TypeChecking/Expressions.ag"(line 375, column 9)
              _backTree =
                  Placeholder ann_
              -- self rule
              _annotatedTree =
                  Placeholder ann_
              -- self rule
              _originalTree =
                  Placeholder ann_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/Expressions.ag"(line 368, column 9)
              _tpe =
                  unwrapLookup <$> lbLookupID _lhsIlib ('$':show p_)
              -- "./TypeChecking/Expressions.ag"(line 369, column 9)
              _backTree =
                  PositionalArg ann_ p_
              -- self rule
              _annotatedTree =
                  PositionalArg ann_ p_
              -- self rule
              _originalTree =
                  PositionalArg ann_ p_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/Expressions.ag"(line 392, column 9)
              _tpe =
                  do
                  selType <- lmt (map snd <$> _selIuType)
                  case length selType of
                    0 -> Left [InternalError "no columns in scalar subquery?"]
                    1 -> Right $ head selType
                    _ -> Right $ AnonymousRecordType selType
              -- "./TypeChecking/Expressions.ag"(line 400, column 9)
              _backTree =
                  ScalarSubQuery ann_ _selIannotatedTree
              -- "./TypeChecking/Expressions.ag"(line 558, column 29)
              _selOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  ScalarSubQuery ann_ _selIannotatedTree
              -- self rule
              _originalTree =
                  ScalarSubQuery ann_ _selIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              -- copy rule (down)
              _selOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selOoverrideLibs =
                  _lhsIoverrideLibs
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 150, column 18)
              _tpe =
                  Right UnknownType
              -- "./TypeChecking/Expressions.ag"(line 160, column 9)
              _backTree =
                  StringLit ann_ value_
              -- "./TypeChecking/Expressions.ag"(line 353, column 7)
              _liftedColumnName =
                  ""
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
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
              _originalTree =
                  StringLit ann_ value_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  updateAnnotation
                    (setTypeAddErrorsA _tpe
                     . \a -> a {fnProt = _prototype
                               ,infType = msum [_lhsIexpectedType
                                               ,etmt _tpe
                                               ,Nothing]}) _backTree
              -- "./TypeChecking/Expressions.ag"(line 27, column 9)
              _prototype =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 34, column 9)
              _lhsOntAnnotatedTree =
                  updateAnnotation
                     (setTypeAddErrorsA (either Left (Right . CompositeType) _ntType    )
                     . \a -> a {fnProt = _prototype    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 37, column 9)
              _lhsOntType =
                  either (const []) id _ntType
              -- "./TypeChecking/Expressions.ag"(line 53, column 9)
              _lhsOtbAnnotatedTree =
                  updateAnnotation
                                       (\a -> a {fnProt = _prototype
                                                ,errs = errs a ++ tes _tbUType    }) _backTree
              -- "./TypeChecking/Expressions.ag"(line 57, column 9)
              _lhsOtbUType =
                  either (const Nothing) Just _tbUType
              -- "./TypeChecking/Expressions.ag"(line 62, column 9)
              _tbUType =
                  Left [InternalError "bad context for tbUType"]
              -- "./TypeChecking/Expressions.ag"(line 83, column 9)
              _lhsOuType =
                  etmt _tpe
              -- "./TypeChecking/Expressions.ag"(line 199, column 9)
              _tpe =
                  lmt _fnIuType
              -- "./TypeChecking/Expressions.ag"(line 200, column 9)
              _backTree =
                  WindowFn ann_
                           _fnIannotatedTree
                           _partitionByIannotatedTree
                           _orderByIannotatedTree
                           dir_
                           frm_
              -- "./TypeChecking/Expressions.ag"(line 346, column 7)
              _liftedColumnName =
                  let (FunCall _ fn _) = _fnIannotatedTree
                  in fn
              -- "./TypeChecking/Expressions.ag"(line 359, column 7)
              _ntType =
                  do
                  t <- _tpe
                  return [(case _liftedColumnName     of
                            "" -> "?column?"
                            n -> n
                         ,t)]
              -- "./TypeChecking/Expressions.ag"(line 527, column 9)
              _partitionByOexpectedTypes =
                  []
              -- "./TypeChecking/Expressions.ag"(line 528, column 9)
              _orderByOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  WindowFn ann_ _fnIannotatedTree _partitionByIannotatedTree _orderByIannotatedTree dir_ frm_
              -- self rule
              _originalTree =
                  WindowFn ann_ _fnIoriginalTree _partitionByIoriginalTree _orderByIoriginalTree dir_ frm_
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
              _fnOlib =
                  _lhsIlib
              -- copy rule (down)
              _fnOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _fnOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _partitionByOcat =
                  _lhsIcat
              -- copy rule (down)
              _partitionByOlib =
                  _lhsIlib
              -- copy rule (down)
              _partitionByOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _partitionByOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _orderByOcat =
                  _lhsIcat
              -- copy rule (down)
              _orderByOlib =
                  _lhsIlib
              -- copy rule (down)
              _orderByOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _orderByOoverrideLibs =
                  _lhsIoverrideLibs
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
type ExpressionDirectionPair  = ( Expression ,(Direction))
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
                                  ( ExpressionDirectionPair ,ExpressionDirectionPair )
data Inh_ExpressionDirectionPair  = Inh_ExpressionDirectionPair {cat_Inh_ExpressionDirectionPair :: Catalog,lib_Inh_ExpressionDirectionPair :: LocalBindings,overrideLib_Inh_ExpressionDirectionPair :: (Maybe LocalBindings),overrideLibs_Inh_ExpressionDirectionPair :: ([Maybe LocalBindings])}
data Syn_ExpressionDirectionPair  = Syn_ExpressionDirectionPair {annotatedTree_Syn_ExpressionDirectionPair :: ExpressionDirectionPair ,originalTree_Syn_ExpressionDirectionPair :: ExpressionDirectionPair }
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
                  Nothing
              -- self rule
              _annotatedTree =
                  (_x1IannotatedTree,x2_)
              -- self rule
              _originalTree =
                  (_x1IoriginalTree,x2_)
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _x1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x1Olib =
                  _lhsIlib
              -- copy rule (down)
              _x1OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _x1OoverrideLibs =
                  _lhsIoverrideLibs
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
type ExpressionDirectionPairList  = [ExpressionDirectionPair ]
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
                                      ( ExpressionDirectionPairList ,ExpressionDirectionPairList )
data Inh_ExpressionDirectionPairList  = Inh_ExpressionDirectionPairList {cat_Inh_ExpressionDirectionPairList :: Catalog,lib_Inh_ExpressionDirectionPairList :: LocalBindings,overrideLib_Inh_ExpressionDirectionPairList :: (Maybe LocalBindings),overrideLibs_Inh_ExpressionDirectionPairList :: ([Maybe LocalBindings])}
data Syn_ExpressionDirectionPairList  = Syn_ExpressionDirectionPairList {annotatedTree_Syn_ExpressionDirectionPairList :: ExpressionDirectionPairList ,originalTree_Syn_ExpressionDirectionPairList :: ExpressionDirectionPairList }
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
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type ExpressionList  = [Expression ]
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
                         ( ExpressionList ,ExpressionList ,([Maybe ([(String,Type)],[(String,Type)])]),([Maybe Type]))
data Inh_ExpressionList  = Inh_ExpressionList {cat_Inh_ExpressionList :: Catalog,expectedTypes_Inh_ExpressionList :: ([Maybe Type]),lib_Inh_ExpressionList :: LocalBindings,overrideLib_Inh_ExpressionList :: (Maybe LocalBindings),overrideLibs_Inh_ExpressionList :: ([Maybe LocalBindings])}
data Syn_ExpressionList  = Syn_ExpressionList {annotatedTree_Syn_ExpressionList :: ExpressionList ,originalTree_Syn_ExpressionList :: ExpressionList ,tbUTypes_Syn_ExpressionList :: ([Maybe ([(String,Type)],[(String,Type)])]),uType_Syn_ExpressionList :: ([Maybe Type])}
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
                  _hdItbUType : _tlItbUTypes
              -- "./TypeChecking/Expressions.ag"(line 86, column 12)
              _lhsOuType =
                  _hdIuType : _tlIuType
              -- "./TypeChecking/Expressions.ag"(line 135, column 12)
              _hdOoverrideLib =
                  case _lhsIoverrideLibs of
                     h : t -> h
                     _ -> Nothing
              -- "./TypeChecking/Expressions.ag"(line 138, column 12)
              _tlOoverrideLibs =
                  case _lhsIoverrideLibs of
                    _ : t -> t
                    _ -> []
              -- "./TypeChecking/Expressions.ag"(line 473, column 12)
              _hdOexpectedType =
                  case _lhsIexpectedTypes of
                    (t:_) -> t
                    _ -> Nothing
              -- "./TypeChecking/Expressions.ag"(line 476, column 12)
              _tlOexpectedTypes =
                  case _lhsIexpectedTypes of
                  (_:ts) -> ts
                  _ -> []
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
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
                  []
              -- "./TypeChecking/Expressions.ag"(line 87, column 11)
              _lhsOuType =
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type ExpressionListList  = [ExpressionList ]
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
                             ( ExpressionListList ,ExpressionListList ,([[Maybe Type]]))
data Inh_ExpressionListList  = Inh_ExpressionListList {cat_Inh_ExpressionListList :: Catalog,expectedTypes_Inh_ExpressionListList :: ([Maybe Type]),lib_Inh_ExpressionListList :: LocalBindings,overrideLib_Inh_ExpressionListList :: (Maybe LocalBindings),overrideLibs_Inh_ExpressionListList :: ([Maybe LocalBindings])}
data Syn_ExpressionListList  = Syn_ExpressionListList {annotatedTree_Syn_ExpressionListList :: ExpressionListList ,originalTree_Syn_ExpressionListList :: ExpressionListList ,uType_Syn_ExpressionListList :: ([[Maybe Type]])}
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
                  _hdIuType : _tlIuType
              -- "./TypeChecking/Expressions.ag"(line 554, column 12)
              _hdOexpectedTypes =
                  _lhsIexpectedTypes
              -- "./TypeChecking/Expressions.ag"(line 555, column 12)
              _tlOexpectedTypes =
                  _lhsIexpectedTypes
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type ExpressionListStatementListPair  = ( ExpressionList ,StatementList )
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
                                          ( ExpressionListStatementListPair ,ExpressionListStatementListPair )
data Inh_ExpressionListStatementListPair  = Inh_ExpressionListStatementListPair {cat_Inh_ExpressionListStatementListPair :: Catalog,lib_Inh_ExpressionListStatementListPair :: LocalBindings,overrideLib_Inh_ExpressionListStatementListPair :: (Maybe LocalBindings),overrideLibs_Inh_ExpressionListStatementListPair :: ([Maybe LocalBindings])}
data Syn_ExpressionListStatementListPair  = Syn_ExpressionListStatementListPair {annotatedTree_Syn_ExpressionListStatementListPair :: ExpressionListStatementListPair ,originalTree_Syn_ExpressionListStatementListPair :: ExpressionListStatementListPair }
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
                  []
              -- "./TypeChecking/Statements.ag"(line 117, column 9)
              _x2OcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 118, column 9)
              _x2OlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  (_x1IannotatedTree,_x2IannotatedTree)
              -- self rule
              _originalTree =
                  (_x1IoriginalTree,_x2IoriginalTree)
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _x1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x1Olib =
                  _lhsIlib
              -- copy rule (down)
              _x1OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _x1OoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _x2Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x2Olib =
                  _lhsIlib
              -- copy rule (down)
              _x2OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _x2OoverrideLibs =
                  _lhsIoverrideLibs
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
type ExpressionListStatementListPairList  = [ExpressionListStatementListPair ]
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
                                              ( ExpressionListStatementListPairList ,ExpressionListStatementListPairList )
data Inh_ExpressionListStatementListPairList  = Inh_ExpressionListStatementListPairList {cat_Inh_ExpressionListStatementListPairList :: Catalog,lib_Inh_ExpressionListStatementListPairList :: LocalBindings,overrideLib_Inh_ExpressionListStatementListPairList :: (Maybe LocalBindings),overrideLibs_Inh_ExpressionListStatementListPairList :: ([Maybe LocalBindings])}
data Syn_ExpressionListStatementListPairList  = Syn_ExpressionListStatementListPairList {annotatedTree_Syn_ExpressionListStatementListPairList :: ExpressionListStatementListPairList ,originalTree_Syn_ExpressionListStatementListPairList :: ExpressionListStatementListPairList }
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
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data ExpressionRoot  = ExpressionRoot (Expression ) 
                     deriving ( Show)
-- cata
sem_ExpressionRoot :: ExpressionRoot  ->
                      T_ExpressionRoot 
sem_ExpressionRoot (ExpressionRoot _expr )  =
    (sem_ExpressionRoot_ExpressionRoot (sem_Expression _expr ) )
-- semantic domain
type T_ExpressionRoot  = Catalog ->
                         LocalBindings ->
                         ( ExpressionRoot ,ExpressionRoot )
data Inh_ExpressionRoot  = Inh_ExpressionRoot {cat_Inh_ExpressionRoot :: Catalog,lib_Inh_ExpressionRoot :: LocalBindings}
data Syn_ExpressionRoot  = Syn_ExpressionRoot {annotatedTree_Syn_ExpressionRoot :: ExpressionRoot ,originalTree_Syn_ExpressionRoot :: ExpressionRoot }
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
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 119, column 22)
              _exprOoverrideLibs =
                  []
              -- "./TypeChecking/Expressions.ag"(line 510, column 22)
              _exprOexpectedType =
                  Nothing
              -- self rule
              _annotatedTree =
                  ExpressionRoot _exprIannotatedTree
              -- self rule
              _originalTree =
                  ExpressionRoot _exprIoriginalTree
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
              _exprOlib =
                  _lhsIlib
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
type ExpressionStatementListPair  = ( Expression ,StatementList )
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
                                      ( ExpressionStatementListPair ,ExpressionStatementListPair )
data Inh_ExpressionStatementListPair  = Inh_ExpressionStatementListPair {cat_Inh_ExpressionStatementListPair :: Catalog,lib_Inh_ExpressionStatementListPair :: LocalBindings,overrideLib_Inh_ExpressionStatementListPair :: (Maybe LocalBindings),overrideLibs_Inh_ExpressionStatementListPair :: ([Maybe LocalBindings])}
data Syn_ExpressionStatementListPair  = Syn_ExpressionStatementListPair {annotatedTree_Syn_ExpressionStatementListPair :: ExpressionStatementListPair ,originalTree_Syn_ExpressionStatementListPair :: ExpressionStatementListPair }
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
                  Nothing
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
              _originalTree =
                  (_x1IoriginalTree,_x2IoriginalTree)
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _x1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x1Olib =
                  _lhsIlib
              -- copy rule (down)
              _x1OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _x1OoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _x2Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x2Olib =
                  _lhsIlib
              -- copy rule (down)
              _x2OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _x2OoverrideLibs =
                  _lhsIoverrideLibs
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
type ExpressionStatementListPairList  = [ExpressionStatementListPair ]
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
                                          ( ExpressionStatementListPairList ,ExpressionStatementListPairList )
data Inh_ExpressionStatementListPairList  = Inh_ExpressionStatementListPairList {cat_Inh_ExpressionStatementListPairList :: Catalog,lib_Inh_ExpressionStatementListPairList :: LocalBindings,overrideLib_Inh_ExpressionStatementListPairList :: (Maybe LocalBindings),overrideLibs_Inh_ExpressionStatementListPairList :: ([Maybe LocalBindings])}
data Syn_ExpressionStatementListPairList  = Syn_ExpressionStatementListPairList {annotatedTree_Syn_ExpressionStatementListPairList :: ExpressionStatementListPairList ,originalTree_Syn_ExpressionStatementListPairList :: ExpressionStatementListPairList }
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
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                 LocalBindings ->
                 (Maybe LocalBindings) ->
                 ([Maybe LocalBindings]) ->
                 ( FnBody ,FnBody )
data Inh_FnBody  = Inh_FnBody {cat_Inh_FnBody :: Catalog,lib_Inh_FnBody :: LocalBindings,overrideLib_Inh_FnBody :: (Maybe LocalBindings),overrideLibs_Inh_FnBody :: ([Maybe LocalBindings])}
data Syn_FnBody  = Syn_FnBody {annotatedTree_Syn_FnBody :: FnBody ,originalTree_Syn_FnBody :: FnBody }
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
              -- "./TypeChecking/Statements.ag"(line 100, column 9)
              _blkOinProducedCat =
                  emptyCatalog
              -- self rule
              _annotatedTree =
                  PlpgsqlFnBody ann_ _blkIannotatedTree
              -- self rule
              _originalTree =
                  PlpgsqlFnBody ann_ _blkIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _blkOcat =
                  _lhsIcat
              -- copy rule (down)
              _blkOlib =
                  _lhsIlib
              -- copy rule (down)
              _blkOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _blkOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 125, column 9)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 126, column 9)
              _stsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  SqlFnBody ann_ _stsIannotatedTree
              -- self rule
              _originalTree =
                  SqlFnBody ann_ _stsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOlib =
                  _lhsIlib
              -- copy rule (down)
              _stsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _stsOoverrideLibs =
                  _lhsIoverrideLibs
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
data InList  = InList (Annotation) (ExpressionList ) 
             | InSelect (Annotation) (SelectExpression ) 
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
                 ( InList ,(Either [TypeError] Type),InList )
data Inh_InList  = Inh_InList {cat_Inh_InList :: Catalog,lib_Inh_InList :: LocalBindings,overrideLib_Inh_InList :: (Maybe LocalBindings),overrideLibs_Inh_InList :: ([Maybe LocalBindings])}
data Syn_InList  = Syn_InList {annotatedTree_Syn_InList :: InList ,listType_Syn_InList :: (Either [TypeError] Type),originalTree_Syn_InList :: InList }
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
                  mapM lmt _exprsIuType >>= resolveResultSetType _lhsIcat
              -- "./TypeChecking/Expressions.ag"(line 534, column 14)
              _exprsOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  InList ann_ _exprsIannotatedTree
              -- self rule
              _originalTree =
                  InList ann_ _exprsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprsOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprsOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprsOoverrideLibs =
                  _lhsIoverrideLibs
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
                  do
                  st <- lmt (map snd <$> _selIuType)
                  case length st of
                            0 -> Left [InternalError
                                       "got subquery with no columns? in inselect"]
                            1 -> Right $ head st
                            _ -> Right $ AnonymousRecordType st
              -- "./TypeChecking/Expressions.ag"(line 560, column 16)
              _selOexpectedTypes =
                  []
              -- self rule
              _annotatedTree =
                  InSelect ann_ _selIannotatedTree
              -- self rule
              _originalTree =
                  InSelect ann_ _selIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              -- copy rule (down)
              _selOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selOoverrideLibs =
                  _lhsIoverrideLibs
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
data JoinExpression  = JoinOn (Annotation) (Expression ) 
                     | JoinUsing (Annotation) (([String])) 
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
                         ( JoinExpression ,JoinExpression )
data Inh_JoinExpression  = Inh_JoinExpression {cat_Inh_JoinExpression :: Catalog,lib_Inh_JoinExpression :: LocalBindings,overrideLib_Inh_JoinExpression :: (Maybe LocalBindings),overrideLibs_Inh_JoinExpression :: ([Maybe LocalBindings])}
data Syn_JoinExpression  = Syn_JoinExpression {annotatedTree_Syn_JoinExpression :: JoinExpression ,originalTree_Syn_JoinExpression :: JoinExpression }
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
                  Just typeBool
              -- self rule
              _annotatedTree =
                  JoinOn ann_ _exprIannotatedTree
              -- self rule
              _originalTree =
                  JoinOn ann_ _exprIoriginalTree
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
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
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
                  JoinUsing ann_ x_
              -- self rule
              _originalTree =
                  JoinUsing ann_ x_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type MaybeBoolExpression  = Maybe Expression 
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
                              ( MaybeBoolExpression ,MaybeBoolExpression )
data Inh_MaybeBoolExpression  = Inh_MaybeBoolExpression {cat_Inh_MaybeBoolExpression :: Catalog,lib_Inh_MaybeBoolExpression :: LocalBindings,overrideLib_Inh_MaybeBoolExpression :: (Maybe LocalBindings),overrideLibs_Inh_MaybeBoolExpression :: ([Maybe LocalBindings])}
data Syn_MaybeBoolExpression  = Syn_MaybeBoolExpression {annotatedTree_Syn_MaybeBoolExpression :: MaybeBoolExpression ,originalTree_Syn_MaybeBoolExpression :: MaybeBoolExpression }
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
                  let t = _justIuType
                  in if t `elem` [Nothing,Just typeBool]
                     then Just _justIannotatedTree
                     else Just $ addTypeErrors [ExpressionMustBeBool] _justIannotatedTree
              -- "./TypeChecking/Expressions.ag"(line 484, column 12)
              _justOexpectedType =
                  Just typeBool
              -- self rule
              _annotatedTree =
                  Just _justIannotatedTree
              -- self rule
              _originalTree =
                  Just _justIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _justOcat =
                  _lhsIcat
              -- copy rule (down)
              _justOlib =
                  _lhsIlib
              -- copy rule (down)
              _justOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _justOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- self rule
              _originalTree =
                  Nothing
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type MaybeExpression  = Maybe Expression 
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
                          ( MaybeExpression ,MaybeExpression ,(Maybe Type))
data Inh_MaybeExpression  = Inh_MaybeExpression {cat_Inh_MaybeExpression :: Catalog,lib_Inh_MaybeExpression :: LocalBindings,overrideLib_Inh_MaybeExpression :: (Maybe LocalBindings),overrideLibs_Inh_MaybeExpression :: ([Maybe LocalBindings])}
data Syn_MaybeExpression  = Syn_MaybeExpression {annotatedTree_Syn_MaybeExpression :: MaybeExpression ,originalTree_Syn_MaybeExpression :: MaybeExpression ,uType_Syn_MaybeExpression :: (Maybe Type)}
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
                  _justIuType
              -- "./TypeChecking/Expressions.ag"(line 487, column 12)
              _justOexpectedType =
                  Nothing
              -- self rule
              _annotatedTree =
                  Just _justIannotatedTree
              -- self rule
              _originalTree =
                  Just _justIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _justOcat =
                  _lhsIcat
              -- copy rule (down)
              _justOlib =
                  _lhsIlib
              -- copy rule (down)
              _justOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _justOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- self rule
              _annotatedTree =
                  Nothing
              -- self rule
              _originalTree =
                  Nothing
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                          LocalBindings ->
                          (Maybe LocalBindings) ->
                          ([Maybe LocalBindings]) ->
                          ( MaybeSelectList ,([(String,Type)]),MaybeSelectList )
data Inh_MaybeSelectList  = Inh_MaybeSelectList {cat_Inh_MaybeSelectList :: Catalog,lib_Inh_MaybeSelectList :: LocalBindings,overrideLib_Inh_MaybeSelectList :: (Maybe LocalBindings),overrideLibs_Inh_MaybeSelectList :: ([Maybe LocalBindings])}
data Syn_MaybeSelectList  = Syn_MaybeSelectList {annotatedTree_Syn_MaybeSelectList :: MaybeSelectList ,listType_Syn_MaybeSelectList :: ([(String,Type)]),originalTree_Syn_MaybeSelectList :: MaybeSelectList }
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
                  _justIlistType
              -- self rule
              _annotatedTree =
                  Just _justIannotatedTree
              -- self rule
              _originalTree =
                  Just _justIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _justOcat =
                  _lhsIcat
              -- copy rule (down)
              _justOlib =
                  _lhsIlib
              -- copy rule (down)
              _justOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _justOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _annotatedTree =
                  Nothing
              -- self rule
              _originalTree =
                  Nothing
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type OnExpr  = Maybe JoinExpression 
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
                 ( OnExpr ,OnExpr )
data Inh_OnExpr  = Inh_OnExpr {cat_Inh_OnExpr :: Catalog,lib_Inh_OnExpr :: LocalBindings,overrideLib_Inh_OnExpr :: (Maybe LocalBindings),overrideLibs_Inh_OnExpr :: ([Maybe LocalBindings])}
data Syn_OnExpr  = Syn_OnExpr {annotatedTree_Syn_OnExpr :: OnExpr ,originalTree_Syn_OnExpr :: OnExpr }
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
                  Just _justIannotatedTree
              -- self rule
              _originalTree =
                  Just _justIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _justOcat =
                  _lhsIcat
              -- copy rule (down)
              _justOlib =
                  _lhsIlib
              -- copy rule (down)
              _justOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _justOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- self rule
              _originalTree =
                  Nothing
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data ParamDef  = ParamDef (Annotation) (String) (TypeName ) 
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
                   LocalBindings ->
                   (Maybe LocalBindings) ->
                   ([Maybe LocalBindings]) ->
                   Int ->
                   ( ParamDef ,(Maybe Type),ParamDef ,ParamName)
data Inh_ParamDef  = Inh_ParamDef {cat_Inh_ParamDef :: Catalog,lib_Inh_ParamDef :: LocalBindings,overrideLib_Inh_ParamDef :: (Maybe LocalBindings),overrideLibs_Inh_ParamDef :: ([Maybe LocalBindings]),pos_Inh_ParamDef :: Int}
data Syn_ParamDef  = Syn_ParamDef {annotatedTree_Syn_ParamDef :: ParamDef ,namedType_Syn_ParamDef :: (Maybe Type),originalTree_Syn_ParamDef :: ParamDef ,paramName_Syn_ParamDef :: ParamName}
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
                  _typInamedType
              -- "./TypeChecking/CreateFunction.ag"(line 47, column 9)
              _lhsOparamName =
                  NamedParam _lhsIpos name_
              -- self rule
              _annotatedTree =
                  ParamDef ann_ name_ _typIannotatedTree
              -- self rule
              _originalTree =
                  ParamDef ann_ name_ _typIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              -- copy rule (down)
              _typOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _typOoverrideLibs =
                  _lhsIoverrideLibs
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
                  _typInamedType
              -- "./TypeChecking/CreateFunction.ag"(line 49, column 9)
              _lhsOparamName =
                  UnnamedParam _lhsIpos
              -- self rule
              _annotatedTree =
                  ParamDefTp ann_ _typIannotatedTree
              -- self rule
              _originalTree =
                  ParamDefTp ann_ _typIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              -- copy rule (down)
              _typOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _typOoverrideLibs =
                  _lhsIoverrideLibs
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
type ParamDefList  = [ParamDef ]
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
                       ( ParamDefList ,ParamDefList ,([(ParamName, Maybe Type)]))
data Inh_ParamDefList  = Inh_ParamDefList {cat_Inh_ParamDefList :: Catalog,lib_Inh_ParamDefList :: LocalBindings,overrideLib_Inh_ParamDefList :: (Maybe LocalBindings),overrideLibs_Inh_ParamDefList :: ([Maybe LocalBindings]),pos_Inh_ParamDefList :: Int}
data Syn_ParamDefList  = Syn_ParamDefList {annotatedTree_Syn_ParamDefList :: ParamDefList ,originalTree_Syn_ParamDefList :: ParamDefList ,params_Syn_ParamDefList :: ([(ParamName, Maybe Type)])}
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
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data Root  = Root (StatementList ) 
           deriving ( Show)
-- cata
sem_Root :: Root  ->
            T_Root 
sem_Root (Root _statements )  =
    (sem_Root_Root (sem_StatementList _statements ) )
-- semantic domain
type T_Root  = Catalog ->
               LocalBindings ->
               ( Root ,Root ,Catalog,LocalBindings)
data Inh_Root  = Inh_Root {cat_Inh_Root :: Catalog,lib_Inh_Root :: LocalBindings}
data Syn_Root  = Syn_Root {annotatedTree_Syn_Root :: Root ,originalTree_Syn_Root :: Root ,producedCat_Syn_Root :: Catalog,producedLib_Syn_Root :: LocalBindings}
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
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 123, column 12)
              _statementsOoverrideLibs =
                  []
              -- "./TypeChecking/Statements.ag"(line 103, column 12)
              _statementsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 104, column 12)
              _statementsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Root _statementsIannotatedTree
              -- self rule
              _originalTree =
                  Root _statementsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
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
              _statementsOlib =
                  _lhsIlib
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
                    | RowCheckConstraint (Annotation) (String) (Expression ) 
                    | RowPrimaryKeyConstraint (Annotation) (String) 
                    | RowReferenceConstraint (Annotation) (String) (String) ((Maybe String)) (Cascade) (Cascade) 
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
                        ( RowConstraint ,RowConstraint )
data Inh_RowConstraint  = Inh_RowConstraint {cat_Inh_RowConstraint :: Catalog,lib_Inh_RowConstraint :: LocalBindings,overrideLib_Inh_RowConstraint :: (Maybe LocalBindings),overrideLibs_Inh_RowConstraint :: ([Maybe LocalBindings])}
data Syn_RowConstraint  = Syn_RowConstraint {annotatedTree_Syn_RowConstraint :: RowConstraint ,originalTree_Syn_RowConstraint :: RowConstraint }
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
                  NotNullConstraint ann_ name_
              -- self rule
              _originalTree =
                  NotNullConstraint ann_ name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  NullConstraint ann_ name_
              -- self rule
              _originalTree =
                  NullConstraint ann_ name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  Nothing
              -- self rule
              _annotatedTree =
                  RowCheckConstraint ann_ name_ _exprIannotatedTree
              -- self rule
              _originalTree =
                  RowCheckConstraint ann_ name_ _exprIoriginalTree
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
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
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
                  RowPrimaryKeyConstraint ann_ name_
              -- self rule
              _originalTree =
                  RowPrimaryKeyConstraint ann_ name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
              -- self rule
              _originalTree =
                  RowReferenceConstraint ann_ name_ table_ att_ onUpdate_ onDelete_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  RowUniqueConstraint ann_ name_
              -- self rule
              _originalTree =
                  RowUniqueConstraint ann_ name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type RowConstraintList  = [RowConstraint ]
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
                            ( RowConstraintList ,RowConstraintList )
data Inh_RowConstraintList  = Inh_RowConstraintList {cat_Inh_RowConstraintList :: Catalog,lib_Inh_RowConstraintList :: LocalBindings,overrideLib_Inh_RowConstraintList :: (Maybe LocalBindings),overrideLibs_Inh_RowConstraintList :: ([Maybe LocalBindings])}
data Syn_RowConstraintList  = Syn_RowConstraintList {annotatedTree_Syn_RowConstraintList :: RowConstraintList ,originalTree_Syn_RowConstraintList :: RowConstraintList }
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
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data SelectExpression  = CombineSelect (Annotation) (CombineType) (SelectExpression ) (SelectExpression ) 
                       | Select (Annotation) (Distinct) (SelectList ) (TableRefList ) (MaybeBoolExpression ) (ExpressionList ) (MaybeBoolExpression ) (ExpressionDirectionPairList ) (MaybeExpression ) (MaybeExpression ) 
                       | Values (Annotation) (ExpressionListList ) 
                       | WithSelect (Annotation) (WithQueryList ) (SelectExpression ) 
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
                           ( SelectExpression ,([LocalBindingsUpdate]),SelectExpression ,(Maybe [(String,Type)]))
data Inh_SelectExpression  = Inh_SelectExpression {cat_Inh_SelectExpression :: Catalog,expectedTypes_Inh_SelectExpression :: ([Maybe Type]),lib_Inh_SelectExpression :: LocalBindings,overrideLib_Inh_SelectExpression :: (Maybe LocalBindings),overrideLibs_Inh_SelectExpression :: ([Maybe LocalBindings])}
data Syn_SelectExpression  = Syn_SelectExpression {annotatedTree_Syn_SelectExpression :: SelectExpression ,libUpdates_Syn_SelectExpression :: ([LocalBindingsUpdate]),originalTree_Syn_SelectExpression :: SelectExpression ,uType_Syn_SelectExpression :: (Maybe [(String,Type)])}
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
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/SelectStatement.ag"(line 115, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/SelectStatement.ag"(line 142, column 9)
              _tpe =
                  do
                  sel1t <- lmt ((SetOfType . CompositeType) <$> _sel1IuType)
                  sel2t <- lmt ((SetOfType . CompositeType) <$> _sel2IuType)
                  typeCheckCombineSelect _lhsIcat sel1t sel2t
              -- "./TypeChecking/SelectStatement.ag"(line 148, column 9)
              _backTree =
                  CombineSelect ann_ ctype_
                                _sel1IannotatedTree
                                _sel2IannotatedTree
              -- "./TypeChecking/SelectStatement.ag"(line 159, column 9)
              _lhsOuType =
                  etmt (_tpe     >>= unwrapSetOfComposite)
              -- self rule
              _annotatedTree =
                  CombineSelect ann_ ctype_ _sel1IannotatedTree _sel2IannotatedTree
              -- self rule
              _originalTree =
                  CombineSelect ann_ ctype_ _sel1IoriginalTree _sel2IoriginalTree
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
              _sel1Olib =
                  _lhsIlib
              -- copy rule (down)
              _sel1OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _sel1OoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _sel2Ocat =
                  _lhsIcat
              -- copy rule (down)
              _sel2OexpectedTypes =
                  _lhsIexpectedTypes
              -- copy rule (down)
              _sel2Olib =
                  _lhsIlib
              -- copy rule (down)
              _sel2OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _sel2OoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- "./TypeChecking/SelectStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/SelectStatement.ag"(line 98, column 10)
              _newLib =
                  case foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _selTrefIlibUpdates of
                    Left x -> error $ "selectexpression-select-loc.newlib " ++ show x
                    Right e -> e
              -- "./TypeChecking/SelectStatement.ag"(line 101, column 10)
              _selSelectListOlib =
                  _newLib
              -- "./TypeChecking/SelectStatement.ag"(line 102, column 10)
              _selWhereOlib =
                  _newLib
              -- "./TypeChecking/SelectStatement.ag"(line 103, column 10)
              _selGroupByOlib =
                  _newLib
              -- "./TypeChecking/SelectStatement.ag"(line 104, column 10)
              _selOrderByOlib =
                  _newLib
              -- "./TypeChecking/SelectStatement.ag"(line 117, column 9)
              _lhsOlibUpdates =
                  _selSelectListIlibUpdates
              -- "./TypeChecking/SelectStatement.ag"(line 129, column 9)
              _tpe =
                  Right $ SetOfType $ CompositeType _selSelectListIlistType
              -- "./TypeChecking/SelectStatement.ag"(line 131, column 9)
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
              -- "./TypeChecking/SelectStatement.ag"(line 159, column 9)
              _lhsOuType =
                  etmt (_tpe     >>= unwrapSetOfComposite)
              -- self rule
              _annotatedTree =
                  Select ann_ selDistinct_ _selSelectListIannotatedTree _selTrefIannotatedTree _selWhereIannotatedTree _selGroupByIannotatedTree _selHavingIannotatedTree _selOrderByIannotatedTree _selLimitIannotatedTree _selOffsetIannotatedTree
              -- self rule
              _originalTree =
                  Select ann_ selDistinct_ _selSelectListIoriginalTree _selTrefIoriginalTree _selWhereIoriginalTree _selGroupByIoriginalTree _selHavingIoriginalTree _selOrderByIoriginalTree _selLimitIoriginalTree _selOffsetIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selSelectListOcat =
                  _lhsIcat
              -- copy rule (down)
              _selSelectListOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selSelectListOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _selTrefOcat =
                  _lhsIcat
              -- copy rule (down)
              _selTrefOlib =
                  _lhsIlib
              -- copy rule (down)
              _selTrefOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selTrefOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _selWhereOcat =
                  _lhsIcat
              -- copy rule (down)
              _selWhereOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selWhereOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _selGroupByOcat =
                  _lhsIcat
              -- copy rule (down)
              _selGroupByOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selGroupByOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _selHavingOcat =
                  _lhsIcat
              -- copy rule (down)
              _selHavingOlib =
                  _lhsIlib
              -- copy rule (down)
              _selHavingOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selHavingOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _selOrderByOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOrderByOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selOrderByOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _selLimitOcat =
                  _lhsIcat
              -- copy rule (down)
              _selLimitOlib =
                  _lhsIlib
              -- copy rule (down)
              _selLimitOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selLimitOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _selOffsetOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOffsetOlib =
                  _lhsIlib
              -- copy rule (down)
              _selOffsetOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selOffsetOoverrideLibs =
                  _lhsIoverrideLibs
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
                  _lhsIexpectedTypes
              -- "./TypeChecking/SelectStatement.ag"(line 29, column 9)
              _lhsOannotatedTree =
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/SelectStatement.ag"(line 115, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/SelectStatement.ag"(line 124, column 9)
              _tpe =
                  typeCheckValuesExpr
                              _lhsIcat
                              _vllIuType
              -- "./TypeChecking/SelectStatement.ag"(line 127, column 9)
              _backTree =
                  Values ann_ _vllIannotatedTree
              -- "./TypeChecking/SelectStatement.ag"(line 159, column 9)
              _lhsOuType =
                  etmt (_tpe     >>= unwrapSetOfComposite)
              -- self rule
              _annotatedTree =
                  Values ann_ _vllIannotatedTree
              -- self rule
              _originalTree =
                  Values ann_ _vllIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _vllOcat =
                  _lhsIcat
              -- copy rule (down)
              _vllOlib =
                  _lhsIlib
              -- copy rule (down)
              _vllOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _vllOoverrideLibs =
                  _lhsIoverrideLibs
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
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/SelectStatement.ag"(line 119, column 9)
              _lhsOlibUpdates =
                  _exIlibUpdates
              -- "./TypeChecking/SelectStatement.ag"(line 152, column 9)
              _tpe =
                  lmt ((SetOfType . CompositeType) <$> _exIuType)
              -- "./TypeChecking/SelectStatement.ag"(line 153, column 9)
              _backTree =
                  WithSelect ann_ _withsIannotatedTree _exIannotatedTree
              -- "./TypeChecking/SelectStatement.ag"(line 154, column 9)
              _exOcat =
                  _withsIproducedCat
              -- "./TypeChecking/SelectStatement.ag"(line 155, column 9)
              _withsOcatUpdates =
                  []
              -- "./TypeChecking/SelectStatement.ag"(line 159, column 9)
              _lhsOuType =
                  etmt (_tpe     >>= unwrapSetOfComposite)
              -- self rule
              _annotatedTree =
                  WithSelect ann_ _withsIannotatedTree _exIannotatedTree
              -- self rule
              _originalTree =
                  WithSelect ann_ _withsIoriginalTree _exIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _withsOcat =
                  _lhsIcat
              -- copy rule (down)
              _withsOlib =
                  _lhsIlib
              -- copy rule (down)
              _withsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _withsOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _exOexpectedTypes =
                  _lhsIexpectedTypes
              -- copy rule (down)
              _exOlib =
                  _lhsIlib
              -- copy rule (down)
              _exOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exOoverrideLibs =
                  _lhsIoverrideLibs
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
data SelectItem  = SelExp (Annotation) (Expression ) 
                 | SelectItem (Annotation) (Expression ) (String) 
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
                     ( SelectItem ,([(String,Type)]),SelectItem )
data Inh_SelectItem  = Inh_SelectItem {cat_Inh_SelectItem :: Catalog,lib_Inh_SelectItem :: LocalBindings,overrideLib_Inh_SelectItem :: (Maybe LocalBindings),overrideLibs_Inh_SelectItem :: ([Maybe LocalBindings])}
data Syn_SelectItem  = Syn_SelectItem {annotatedTree_Syn_SelectItem :: SelectItem ,itemType_Syn_SelectItem :: ([(String,Type)]),originalTree_Syn_SelectItem :: SelectItem }
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
                  Nothing
              -- "./TypeChecking/SelectLists.ag"(line 33, column 9)
              _annotatedTree =
                  SelExp ann_ _exIntAnnotatedTree
              -- "./TypeChecking/SelectLists.ag"(line 61, column 9)
              _lhsOitemType =
                  unwrapSetofs _exIntType
              -- self rule
              _originalTree =
                  SelExp ann_ _exIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exOcat =
                  _lhsIcat
              -- copy rule (down)
              _exOlib =
                  _lhsIlib
              -- copy rule (down)
              _exOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- "./TypeChecking/SelectLists.ag"(line 35, column 9)
              _annotatedTree =
                  SelectItem ann_ _exIannotatedTree name_
              -- "./TypeChecking/SelectLists.ag"(line 63, column 9)
              _lhsOitemType =
                  case _exIntType of
                    [(_,t)] -> [(name_, unwrapSetof t)]
                    _ -> []
              -- self rule
              _originalTree =
                  SelectItem ann_ _exIoriginalTree name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exOcat =
                  _lhsIcat
              -- copy rule (down)
              _exOlib =
                  _lhsIlib
              -- copy rule (down)
              _exOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exOoverrideLibs =
                  _lhsIoverrideLibs
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
type SelectItemList  = [SelectItem ]
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
                         ( SelectItemList ,([(String,Type)]),SelectItemList )
data Inh_SelectItemList  = Inh_SelectItemList {cat_Inh_SelectItemList :: Catalog,lib_Inh_SelectItemList :: LocalBindings,overrideLib_Inh_SelectItemList :: (Maybe LocalBindings),overrideLibs_Inh_SelectItemList :: ([Maybe LocalBindings])}
data Syn_SelectItemList  = Syn_SelectItemList {annotatedTree_Syn_SelectItemList :: SelectItemList ,listType_Syn_SelectItemList :: ([(String,Type)]),originalTree_Syn_SelectItemList :: SelectItemList }
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
                  _hdIitemType ++ _tlIlistType
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data SelectList  = SelectList (Annotation) (SelectItemList ) (([Expression])) 
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
                     ( SelectList ,([LocalBindingsUpdate]),([(String,Type)]),SelectList )
data Inh_SelectList  = Inh_SelectList {cat_Inh_SelectList :: Catalog,lib_Inh_SelectList :: LocalBindings,overrideLib_Inh_SelectList :: (Maybe LocalBindings),overrideLibs_Inh_SelectList :: ([Maybe LocalBindings])}
data Syn_SelectList  = Syn_SelectList {annotatedTree_Syn_SelectList :: SelectList ,libUpdates_Syn_SelectList :: ([LocalBindingsUpdate]),listType_Syn_SelectList :: ([(String,Type)]),originalTree_Syn_SelectList :: SelectList }
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
                  _itemsIlistType
              -- "./TypeChecking/SelectLists.ag"(line 82, column 9)
              _intoFroms =
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
              -- "./TypeChecking/SelectLists.ag"(line 96, column 9)
              _tpe =
                  returnWhen (into_ == []) () $ do
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
                             into_
              -- self rule
              _annotatedTree =
                  SelectList ann_ _itemsIannotatedTree into_
              -- self rule
              _originalTree =
                  SelectList ann_ _itemsIoriginalTree into_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _itemsOcat =
                  _lhsIcat
              -- copy rule (down)
              _itemsOlib =
                  _lhsIlib
              -- copy rule (down)
              _itemsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _itemsOoverrideLibs =
                  _lhsIoverrideLibs
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
data Statement  = AlterSequence (Annotation) (String) (Expression ) 
                | AlterTable (Annotation) (String) (AlterTableActionList ) 
                | Assignment (Annotation) (Expression ) (Expression ) 
                | Block (Annotation) ((Maybe String)) (VarDefList ) (StatementList ) 
                | CaseStatement (Annotation) (ExpressionListStatementListPairList ) (StatementList ) 
                | CaseStatementSimple (Annotation) (Expression ) (ExpressionListStatementListPairList ) (StatementList ) 
                | ContinueStatement (Annotation) ((Maybe String)) 
                | Copy (Annotation) (String) (([String])) (CopySource) 
                | CopyData (Annotation) (String) 
                | CreateDomain (Annotation) (String) (TypeName ) (String) (MaybeBoolExpression ) 
                | CreateFunction (Annotation) (String) (ParamDefList ) (TypeName ) (Replace) (Language) (FnBody ) (Volatility) 
                | CreateLanguage (Annotation) (String) 
                | CreateSequence (Annotation) (String) (Integer) (Integer) (Integer) (Integer) (Integer) 
                | CreateTable (Annotation) (String) (AttributeDefList ) (ConstraintList ) 
                | CreateTableAs (Annotation) (String) (SelectExpression ) 
                | CreateTrigger (Annotation) (String) (TriggerWhen) (([TriggerEvent])) (String) (TriggerFire) (String) (ExpressionList ) 
                | CreateType (Annotation) (String) (TypeAttributeDefList ) 
                | CreateView (Annotation) (String) (SelectExpression ) 
                | Delete (Annotation) (Expression ) (TableRefList ) (MaybeBoolExpression ) (MaybeSelectList ) 
                | DropFunction (Annotation) (IfExists) (StringTypeNameListPairList ) (Cascade) 
                | DropSomething (Annotation) (DropType) (IfExists) (([String])) (Cascade) 
                | Execute (Annotation) (Expression ) 
                | ExecuteInto (Annotation) (Expression ) (([String])) 
                | ExitStatement (Annotation) ((Maybe String)) 
                | ForIntegerStatement (Annotation) ((Maybe String)) (Expression ) (Expression ) (Expression ) (StatementList ) 
                | ForSelectStatement (Annotation) ((Maybe String)) (Expression ) (SelectExpression ) (StatementList ) 
                | If (Annotation) (ExpressionStatementListPairList ) (StatementList ) 
                | Insert (Annotation) (Expression ) (([String])) (SelectExpression ) (MaybeSelectList ) 
                | LoopStatement (Annotation) ((Maybe String)) (StatementList ) 
                | Notify (Annotation) (String) 
                | NullStatement (Annotation) 
                | Perform (Annotation) (Expression ) 
                | Raise (Annotation) (RaiseType) (String) (ExpressionList ) 
                | Return (Annotation) (MaybeExpression ) 
                | ReturnNext (Annotation) (Expression ) 
                | ReturnQuery (Annotation) (SelectExpression ) 
                | SelectStatement (Annotation) (SelectExpression ) 
                | Set (Annotation) (String) (([SetValue])) 
                | Truncate (Annotation) (([String])) (RestartIdentity) (Cascade) 
                | Update (Annotation) (Expression ) (ExpressionList ) (TableRefList ) (MaybeBoolExpression ) (MaybeSelectList ) 
                | WhileStatement (Annotation) ((Maybe String)) (Expression ) (StatementList ) 
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
                    ( Statement ,([CatalogUpdate]),([LocalBindingsUpdate]),Statement )
data Inh_Statement  = Inh_Statement {cat_Inh_Statement :: Catalog,inProducedCat_Inh_Statement :: Catalog,lib_Inh_Statement :: LocalBindings,overrideLib_Inh_Statement :: (Maybe LocalBindings),overrideLibs_Inh_Statement :: ([Maybe LocalBindings])}
data Syn_Statement  = Syn_Statement {annotatedTree_Syn_Statement :: Statement ,catUpdates_Syn_Statement :: ([CatalogUpdate]),libUpdates_Syn_Statement :: ([LocalBindingsUpdate]),originalTree_Syn_Statement :: Statement }
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
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  AlterSequence ann_ name_ _ownedByIannotatedTree
              -- self rule
              _originalTree =
                  AlterSequence ann_ name_ _ownedByIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _ownedByOcat =
                  _lhsIcat
              -- copy rule (down)
              _ownedByOlib =
                  _lhsIlib
              -- copy rule (down)
              _ownedByOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _ownedByOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  AlterTable ann_ name_ _actionsIannotatedTree
              -- self rule
              _originalTree =
                  AlterTable ann_ name_ _actionsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _actionsOcat =
                  _lhsIcat
              -- copy rule (down)
              _actionsOlib =
                  _lhsIlib
              -- copy rule (down)
              _actionsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _actionsOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 503, column 18)
              _targetOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 20, column 9)
              _tpe =
                  do
                  fromType <- lmt _valueIuType
                  toType <- unwrapLookup <$> lbLookupID _lhsIlib (getName _targetIannotatedTree)
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
              _originalTree =
                  Assignment ann_ _targetIoriginalTree _valueIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _targetOcat =
                  _lhsIcat
              -- copy rule (down)
              _targetOlib =
                  _lhsIlib
              -- copy rule (down)
              _targetOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _targetOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _valueOcat =
                  _lhsIcat
              -- copy rule (down)
              _valueOlib =
                  _lhsIlib
              -- copy rule (down)
              _valueOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _valueOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 96, column 13)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 97, column 13)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Block.ag"(line 20, column 9)
              _stsOlib =
                  fromRight _lhsIlib $
                  lbUpdate _lhsIcat
                           (LBIds "declarations" lb_ $ mapMaybe lv _varsIdefs)
                           _lhsIlib
                  where
                    lv (s,Nothing) = Nothing
                    lv (s,Just t) = Just (s,t)
              -- self rule
              _annotatedTree =
                  Block ann_ lb_ _varsIannotatedTree _stsIannotatedTree
              -- self rule
              _originalTree =
                  Block ann_ lb_ _varsIoriginalTree _stsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
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
              _varsOlib =
                  _lhsIlib
              -- copy rule (down)
              _varsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _varsOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (from local)
              _stsOlibUpdates =
                  _libUpdates
              -- copy rule (down)
              _stsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _stsOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 130, column 9)
              _elsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 131, column 9)
              _elsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  CaseStatement ann_ _casesIannotatedTree _elsIannotatedTree
              -- self rule
              _originalTree =
                  CaseStatement ann_ _casesIoriginalTree _elsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _casesOcat =
                  _lhsIcat
              -- copy rule (down)
              _casesOlib =
                  _lhsIlib
              -- copy rule (down)
              _casesOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _casesOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _elsOcat =
                  _lhsIcat
              -- copy rule (down)
              _elsOlib =
                  _lhsIlib
              -- copy rule (down)
              _elsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _elsOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 130, column 9)
              _elsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 131, column 9)
              _elsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  CaseStatementSimple ann_ _valIannotatedTree _casesIannotatedTree _elsIannotatedTree
              -- self rule
              _originalTree =
                  CaseStatementSimple ann_ _valIoriginalTree _casesIoriginalTree _elsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _valOcat =
                  _lhsIcat
              -- copy rule (down)
              _valOlib =
                  _lhsIlib
              -- copy rule (down)
              _valOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _valOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _casesOcat =
                  _lhsIcat
              -- copy rule (down)
              _casesOlib =
                  _lhsIlib
              -- copy rule (down)
              _casesOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _casesOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _elsOcat =
                  _lhsIcat
              -- copy rule (down)
              _elsOlib =
                  _lhsIlib
              -- copy rule (down)
              _elsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _elsOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  ContinueStatement ann_ lb_
              -- self rule
              _originalTree =
                  ContinueStatement ann_ lb_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Copy ann_ table_ targetCols_ source_
              -- self rule
              _originalTree =
                  Copy ann_ table_ targetCols_ source_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  CopyData ann_ insData_
              -- self rule
              _originalTree =
                  CopyData ann_ insData_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
              _originalTree =
                  CreateDomain ann_ name_ _typIoriginalTree checkName_ _checkIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              -- copy rule (down)
              _typOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _typOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _checkOcat =
                  _lhsIcat
              -- copy rule (down)
              _checkOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _checkOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/CreateFunction.ag"(line 63, column 9)
              _bodyOlib =
                  either (const _lhsIlib) id $ do
                  rt <- lmt _rettypeInamedType
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
              _originalTree =
                  CreateFunction ann_ name_ _paramsIoriginalTree _rettypeIoriginalTree rep_ lang_ _bodyIoriginalTree vol_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _paramsOcat =
                  _lhsIcat
              -- copy rule (down)
              _paramsOlib =
                  _lhsIlib
              -- copy rule (down)
              _paramsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _paramsOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _rettypeOcat =
                  _lhsIcat
              -- copy rule (down)
              _rettypeOlib =
                  _lhsIlib
              -- copy rule (down)
              _rettypeOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _rettypeOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _bodyOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _bodyOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
              _originalTree =
                  CreateLanguage ann_ name_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
              -- self rule
              _originalTree =
                  CreateSequence ann_ name_ incr_ min_ max_ start_ cache_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
              _originalTree =
                  CreateTable ann_ name_ _attsIoriginalTree _consIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _attsOcat =
                  _lhsIcat
              -- copy rule (down)
              _attsOlib =
                  _lhsIlib
              -- copy rule (down)
              _attsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _attsOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _consOcat =
                  _lhsIcat
              -- copy rule (down)
              _consOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _consOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
              _originalTree =
                  CreateTableAs ann_ name_ _exprIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIannotatedTree
              -- self rule
              _originalTree =
                  CreateTrigger ann_ name_ wh_ events_ tbl_ firing_ fnName_ _fnArgsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _fnArgsOcat =
                  _lhsIcat
              -- copy rule (down)
              _fnArgsOlib =
                  _lhsIlib
              -- copy rule (down)
              _fnArgsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _fnArgsOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
              _originalTree =
                  CreateType ann_ name_ _attsIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _attsOcat =
                  _lhsIcat
              -- copy rule (down)
              _attsOlib =
                  _lhsIlib
              -- copy rule (down)
              _attsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _attsOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
              _originalTree =
                  CreateView ann_ name_ _exprIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exprOcat =
                  _lhsIcat
              -- copy rule (down)
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
              _originalTree =
                  Delete ann_ _tableIoriginalTree _usingIoriginalTree _whrIoriginalTree _returningIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tableOcat =
                  _lhsIcat
              -- copy rule (from local)
              _tableOlib =
                  _lib
              -- copy rule (down)
              _tableOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tableOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _usingOcat =
                  _lhsIcat
              -- copy rule (from local)
              _usingOlib =
                  _lib
              -- copy rule (down)
              _usingOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _usingOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _whrOcat =
                  _lhsIcat
              -- copy rule (down)
              _whrOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _whrOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _returningOcat =
                  _lhsIcat
              -- copy rule (down)
              _returningOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _returningOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
              _originalTree =
                  DropFunction ann_ ifE_ _sigsIoriginalTree cascade_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _sigsOcat =
                  _lhsIcat
              -- copy rule (down)
              _sigsOlib =
                  _lhsIlib
              -- copy rule (down)
              _sigsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _sigsOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
              -- self rule
              _originalTree =
                  DropSomething ann_ dropType_ ifE_ names_ cascade_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Execute ann_ _exprIannotatedTree
              -- self rule
              _originalTree =
                  Execute ann_ _exprIoriginalTree
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
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  ExecuteInto ann_ _exprIannotatedTree targets_
              -- self rule
              _originalTree =
                  ExecuteInto ann_ _exprIoriginalTree targets_
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
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  ExitStatement ann_ lb_
              -- self rule
              _originalTree =
                  ExitStatement ann_ lb_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 501, column 27)
              _toOexpectedType =
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 504, column 46)
              _varOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
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
              -- "./TypeChecking/Plpgsql.ag"(line 51, column 9)
              _backTree =
                  ForIntegerStatement ann_ lb_ _varIannotatedTree _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
              -- "./TypeChecking/Plpgsql.ag"(line 52, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 53, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  ForIntegerStatement ann_ lb_ _varIannotatedTree _fromIannotatedTree _toIannotatedTree _stsIannotatedTree
              -- self rule
              _originalTree =
                  ForIntegerStatement ann_ lb_ _varIoriginalTree _fromIoriginalTree _toIoriginalTree _stsIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _varOcat =
                  _lhsIcat
              -- copy rule (down)
              _varOlib =
                  _lhsIlib
              -- copy rule (down)
              _varOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _varOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _fromOcat =
                  _lhsIcat
              -- copy rule (down)
              _fromOlib =
                  _lhsIlib
              -- copy rule (down)
              _fromOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _fromOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _toOcat =
                  _lhsIcat
              -- copy rule (down)
              _toOlib =
                  _lhsIlib
              -- copy rule (down)
              _toOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _toOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOlib =
                  _lhsIlib
              -- copy rule (down)
              _stsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _stsOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- "./TypeChecking/Expressions.ag"(line 564, column 9)
              _selOexpectedTypes =
                  []
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _stsOlibUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 58, column 9)
              _tpe =
                  do
                  st <- lmt (CompositeType <$> _selIuType)
                  toType <- lmt _varIuType
                  checkAssignmentValid _lhsIcat st toType
                  return $ Pseudo Void
              -- "./TypeChecking/Plpgsql.ag"(line 68, column 9)
              _stsOlib =
                  either (const _lhsIlib) id $ do
                  _ <- _tpe
                  st <- lmt (CompositeType <$> _selIuType)
                  lbUpdate _lhsIcat (LBIds "for loop record type" Nothing [(getName _varIannotatedTree,st)]) _lhsIlib
              -- "./TypeChecking/Plpgsql.ag"(line 74, column 9)
              _backTree =
                  ForSelectStatement ann_ lb_ _varIannotatedTree _selIannotatedTree _stsIannotatedTree
              -- "./TypeChecking/Plpgsql.ag"(line 75, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/Plpgsql.ag"(line 76, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  ForSelectStatement ann_ lb_ _varIannotatedTree _selIannotatedTree _stsIannotatedTree
              -- self rule
              _originalTree =
                  ForSelectStatement ann_ lb_ _varIoriginalTree _selIoriginalTree _stsIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _varOcat =
                  _lhsIcat
              -- copy rule (down)
              _varOlib =
                  _lhsIlib
              -- copy rule (down)
              _varOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _varOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              -- copy rule (down)
              _selOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _stsOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 130, column 9)
              _elsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 131, column 9)
              _elsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  If ann_ _casesIannotatedTree _elsIannotatedTree
              -- self rule
              _originalTree =
                  If ann_ _casesIoriginalTree _elsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _casesOcat =
                  _lhsIcat
              -- copy rule (down)
              _casesOlib =
                  _lhsIlib
              -- copy rule (down)
              _casesOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _casesOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _elsOcat =
                  _lhsIcat
              -- copy rule (down)
              _elsOlib =
                  _lhsIlib
              -- copy rule (down)
              _elsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _elsOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
                  expAtts <- lmt _insDataIuType
                  let eNames = map fst expAtts
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
              _originalTree =
                  Insert ann_ _tableIoriginalTree targetCols_ _insDataIoriginalTree _returningIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tableOcat =
                  _lhsIcat
              -- copy rule (down)
              _tableOlib =
                  _lhsIlib
              -- copy rule (down)
              _tableOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tableOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _insDataOcat =
                  _lhsIcat
              -- copy rule (down)
              _insDataOlib =
                  _lhsIlib
              -- copy rule (down)
              _insDataOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _insDataOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _returningOcat =
                  _lhsIcat
              -- copy rule (down)
              _returningOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _returningOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _stsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  LoopStatement ann_ lb_ _stsIannotatedTree
              -- self rule
              _originalTree =
                  LoopStatement ann_ lb_ _stsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOlib =
                  _lhsIlib
              -- copy rule (down)
              _stsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _stsOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Notify ann_ name_
              -- self rule
              _originalTree =
                  Notify ann_ name_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  NullStatement ann_
              -- self rule
              _originalTree =
                  NullStatement ann_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Perform ann_ _exprIannotatedTree
              -- self rule
              _originalTree =
                  Perform ann_ _exprIoriginalTree
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
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Raise ann_ level_ message_ _argsIannotatedTree
              -- self rule
              _originalTree =
                  Raise ann_ level_ message_ _argsIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _argsOcat =
                  _lhsIcat
              -- copy rule (down)
              _argsOlib =
                  _lhsIlib
              -- copy rule (down)
              _argsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _argsOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
              _originalTree =
                  Return ann_ _valueIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _valueOcat =
                  _lhsIcat
              -- copy rule (down)
              _valueOlib =
                  _lhsIlib
              -- copy rule (down)
              _valueOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _valueOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  ReturnNext ann_ _exprIannotatedTree
              -- self rule
              _originalTree =
                  ReturnNext ann_ _exprIoriginalTree
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
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  ReturnQuery ann_ _selIannotatedTree
              -- self rule
              _originalTree =
                  ReturnQuery ann_ _selIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              -- copy rule (down)
              _selOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/SelectStatement.ag"(line 14, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/SelectStatement.ag"(line 15, column 9)
              _statementType =
                  do
                  pt <- sequence $ getPlaceholderTypes _exIannotatedTree
                  st <- _exIuType
                  return (pt
                         ,case st of
                            [(_,(Pseudo Void))] -> []
                            t -> t)
              -- "./TypeChecking/SelectStatement.ag"(line 23, column 9)
              _backTree =
                  SelectStatement ann_ _exIannotatedTree
              -- "./TypeChecking/SelectStatement.ag"(line 24, column 9)
              _catUpdates =
                  []
              -- "./TypeChecking/SelectStatement.ag"(line 111, column 9)
              _libUpdates =
                  _exIlibUpdates
              -- self rule
              _annotatedTree =
                  SelectStatement ann_ _exIannotatedTree
              -- self rule
              _originalTree =
                  SelectStatement ann_ _exIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _exOcat =
                  _lhsIcat
              -- copy rule (down)
              _exOlib =
                  _lhsIlib
              -- copy rule (down)
              _exOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
              _libUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Set ann_ name_ values_
              -- self rule
              _originalTree =
                  Set ann_ name_ values_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  Truncate ann_ tables_ restartIdentity_ cascade_
              -- self rule
              _originalTree =
                  Truncate ann_ tables_ restartIdentity_ cascade_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
                  []
              -- "./TypeChecking/Expressions.ag"(line 573, column 28)
              _tableOexpectedType =
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 78, column 9)
              _lhsOannotatedTree =
                  updateAnnotation
                      (\a -> a {stType = _statementType
                               ,catUpd = _catUpdates    }) $
                  setTypeAddErrors _tpe     _backTree
              -- "./TypeChecking/Statements.ag"(line 84, column 9)
              _lhsOcatUpdates =
                  _catUpdates
              -- "./TypeChecking/Statements.ag"(line 85, column 9)
              _lhsOlibUpdates =
                  _libUpdates
              -- "./TypeChecking/Statements.ag"(line 90, column 9)
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
              _originalTree =
                  Update ann_ _tableIoriginalTree _assignsIoriginalTree _fromListIoriginalTree _whrIoriginalTree _returningIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tableOcat =
                  _lhsIcat
              -- copy rule (from local)
              _tableOlib =
                  _lib
              -- copy rule (down)
              _tableOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tableOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _assignsOcat =
                  _lhsIcat
              -- copy rule (down)
              _assignsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _assignsOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _fromListOcat =
                  _lhsIcat
              -- copy rule (from local)
              _fromListOlib =
                  _lib
              -- copy rule (down)
              _fromListOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _fromListOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _whrOcat =
                  _lhsIcat
              -- copy rule (down)
              _whrOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _whrOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _returningOcat =
                  _lhsIcat
              -- copy rule (down)
              _returningOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _returningOoverrideLibs =
                  _lhsIoverrideLibs
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
                  Nothing
              -- "./TypeChecking/Statements.ag"(line 112, column 9)
              _lhsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 113, column 9)
              _lhsOlibUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 134, column 9)
              _stsOcatUpdates =
                  []
              -- "./TypeChecking/Statements.ag"(line 135, column 9)
              _stsOlibUpdates =
                  []
              -- self rule
              _annotatedTree =
                  WhileStatement ann_ lb_ _exprIannotatedTree _stsIannotatedTree
              -- self rule
              _originalTree =
                  WhileStatement ann_ lb_ _exprIoriginalTree _stsIoriginalTree
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
              _exprOlib =
                  _lhsIlib
              -- copy rule (down)
              _exprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exprOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _stsOcat =
                  _lhsIcat
              -- copy rule (down)
              _stsOlib =
                  _lhsIlib
              -- copy rule (down)
              _stsOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _stsOoverrideLibs =
                  _lhsIoverrideLibs
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
type StatementList  = [Statement ]
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
                        ( StatementList ,StatementList ,Catalog,LocalBindings)
data Inh_StatementList  = Inh_StatementList {cat_Inh_StatementList :: Catalog,catUpdates_Inh_StatementList :: ([CatalogUpdate]),lib_Inh_StatementList :: LocalBindings,libUpdates_Inh_StatementList :: ([LocalBindingsUpdate]),overrideLib_Inh_StatementList :: (Maybe LocalBindings),overrideLibs_Inh_StatementList :: ([Maybe LocalBindings])}
data Syn_StatementList  = Syn_StatementList {annotatedTree_Syn_StatementList :: StatementList ,originalTree_Syn_StatementList :: StatementList ,producedCat_Syn_StatementList :: Catalog,producedLib_Syn_StatementList :: LocalBindings}
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
              -- "./TypeChecking/Statements.ag"(line 52, column 9)
              _newCat =
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
              -- "./TypeChecking/Statements.ag"(line 53, column 9)
              _newLib =
                  fromRight _lhsIlib $ foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _lhsIlibUpdates
              -- "./TypeChecking/Statements.ag"(line 55, column 9)
              _hdOcat =
                  _newCat
              -- "./TypeChecking/Statements.ag"(line 56, column 9)
              _tlOcat =
                  _newCat
              -- "./TypeChecking/Statements.ag"(line 57, column 9)
              _hdOlib =
                  _newLib
              -- "./TypeChecking/Statements.ag"(line 58, column 9)
              _tlOlib =
                  _newLib
              -- "./TypeChecking/Statements.ag"(line 62, column 9)
              _lhsOproducedCat =
                  _tlIproducedCat
              -- "./TypeChecking/Statements.ag"(line 63, column 9)
              _lhsOproducedLib =
                  _tlIproducedLib
              -- "./TypeChecking/Statements.ag"(line 66, column 9)
              _tlOcatUpdates =
                  _hdIcatUpdates
              -- "./TypeChecking/Statements.ag"(line 67, column 9)
              _tlOlibUpdates =
                  _hdIlibUpdates
              -- "./TypeChecking/Statements.ag"(line 93, column 12)
              _hdOinProducedCat =
                  _tlIproducedCat
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- "./TypeChecking/Statements.ag"(line 52, column 9)
              _newCat =
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
              -- "./TypeChecking/Statements.ag"(line 53, column 9)
              _newLib =
                  fromRight _lhsIlib $ foldM (flip $ lbUpdate _lhsIcat) _lhsIlib _lhsIlibUpdates
              -- "./TypeChecking/Statements.ag"(line 69, column 9)
              _lhsOproducedCat =
                  _newCat
              -- "./TypeChecking/Statements.ag"(line 70, column 9)
              _lhsOproducedLib =
                  _newLib
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type StringTypeNameListPair  = ( (String),TypeNameList )
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
                                 ( StringTypeNameListPair ,((String,[Maybe Type])),StringTypeNameListPair )
data Inh_StringTypeNameListPair  = Inh_StringTypeNameListPair {cat_Inh_StringTypeNameListPair :: Catalog,lib_Inh_StringTypeNameListPair :: LocalBindings,overrideLib_Inh_StringTypeNameListPair :: (Maybe LocalBindings),overrideLibs_Inh_StringTypeNameListPair :: ([Maybe LocalBindings])}
data Syn_StringTypeNameListPair  = Syn_StringTypeNameListPair {annotatedTree_Syn_StringTypeNameListPair :: StringTypeNameListPair ,fnSig_Syn_StringTypeNameListPair :: ((String,[Maybe Type])),originalTree_Syn_StringTypeNameListPair :: StringTypeNameListPair }
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
                  (x1_, _x2InamedTypes)
              -- self rule
              _annotatedTree =
                  (x1_,_x2IannotatedTree)
              -- self rule
              _originalTree =
                  (x1_,_x2IoriginalTree)
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _x2Ocat =
                  _lhsIcat
              -- copy rule (down)
              _x2Olib =
                  _lhsIlib
              -- copy rule (down)
              _x2OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _x2OoverrideLibs =
                  _lhsIoverrideLibs
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
type StringTypeNameListPairList  = [StringTypeNameListPair ]
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
                                     ( StringTypeNameListPairList ,([(String,[Maybe Type])]),StringTypeNameListPairList )
data Inh_StringTypeNameListPairList  = Inh_StringTypeNameListPairList {cat_Inh_StringTypeNameListPairList :: Catalog,lib_Inh_StringTypeNameListPairList :: LocalBindings,overrideLib_Inh_StringTypeNameListPairList :: (Maybe LocalBindings),overrideLibs_Inh_StringTypeNameListPairList :: ([Maybe LocalBindings])}
data Syn_StringTypeNameListPairList  = Syn_StringTypeNameListPairList {annotatedTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList ,fnSigs_Syn_StringTypeNameListPairList :: ([(String,[Maybe Type])]),originalTree_Syn_StringTypeNameListPairList :: StringTypeNameListPairList }
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
                  _hdIfnSig : _tlIfnSigs
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data TableRef  = JoinedTref (Annotation) (TableRef ) (Natural) (JoinType) (TableRef ) (OnExpr ) (TableAlias) 
               | SubTref (Annotation) (SelectExpression ) (TableAlias) 
               | Tref (Annotation) (Expression ) (TableAlias) 
               | TrefFun (Annotation) (Expression ) (TableAlias) 
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
                   ( TableRef ,([LocalBindingsUpdate]),TableRef )
data Inh_TableRef  = Inh_TableRef {cat_Inh_TableRef :: Catalog,lib_Inh_TableRef :: LocalBindings,overrideLib_Inh_TableRef :: (Maybe LocalBindings),overrideLibs_Inh_TableRef :: ([Maybe LocalBindings])}
data Syn_TableRef  = Syn_TableRef {annotatedTree_Syn_TableRef :: TableRef ,libUpdates_Syn_TableRef :: ([LocalBindingsUpdate]),originalTree_Syn_TableRef :: TableRef }
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
                  JoinedTref ann_
                             _tblIannotatedTree
                             nat_
                             joinType_
                             _tbl1IannotatedTree
                             _onExprIannotatedTree
                             alias_
              -- self rule
              _annotatedTree =
                  JoinedTref ann_ _tblIannotatedTree nat_ joinType_ _tbl1IannotatedTree _onExprIannotatedTree alias_
              -- self rule
              _originalTree =
                  JoinedTref ann_ _tblIoriginalTree nat_ joinType_ _tbl1IoriginalTree _onExprIoriginalTree alias_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tblOcat =
                  _lhsIcat
              -- copy rule (down)
              _tblOlib =
                  _lhsIlib
              -- copy rule (down)
              _tblOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tblOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tbl1Ocat =
                  _lhsIcat
              -- copy rule (down)
              _tbl1Olib =
                  _lhsIlib
              -- copy rule (down)
              _tbl1OoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tbl1OoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _onExprOcat =
                  _lhsIcat
              -- copy rule (down)
              _onExprOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _onExprOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- self rule
              _annotatedTree =
                  SubTref ann_ _selIannotatedTree alias_
              -- self rule
              _originalTree =
                  SubTref ann_ _selIoriginalTree alias_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _selOcat =
                  _lhsIcat
              -- copy rule (down)
              _selOlib =
                  _lhsIlib
              -- copy rule (down)
              _selOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _selOoverrideLibs =
                  _lhsIoverrideLibs
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
              -- self rule
              _annotatedTree =
                  Tref ann_ _tblIannotatedTree alias_
              -- self rule
              _originalTree =
                  Tref ann_ _tblIoriginalTree alias_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _tblOcat =
                  _lhsIcat
              -- copy rule (down)
              _tblOlib =
                  _lhsIlib
              -- copy rule (down)
              _tblOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tblOoverrideLibs =
                  _lhsIoverrideLibs
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
                  TrefFun ann_ _fnIannotatedTree alias_
              -- self rule
              _annotatedTree =
                  TrefFun ann_ _fnIannotatedTree alias_
              -- self rule
              _originalTree =
                  TrefFun ann_ _fnIoriginalTree alias_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _fnOcat =
                  _lhsIcat
              -- copy rule (down)
              _fnOlib =
                  _lhsIlib
              -- copy rule (down)
              _fnOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _fnOoverrideLibs =
                  _lhsIoverrideLibs
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
type TableRefList  = [TableRef ]
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
                       ( TableRefList ,([LocalBindingsUpdate]),TableRefList )
data Inh_TableRefList  = Inh_TableRefList {cat_Inh_TableRefList :: Catalog,lib_Inh_TableRefList :: LocalBindings,overrideLib_Inh_TableRefList :: (Maybe LocalBindings),overrideLibs_Inh_TableRefList :: ([Maybe LocalBindings])}
data Syn_TableRefList  = Syn_TableRefList {annotatedTree_Syn_TableRefList :: TableRefList ,libUpdates_Syn_TableRefList :: ([LocalBindingsUpdate]),originalTree_Syn_TableRefList :: TableRefList }
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
                  _hdIlibUpdates
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data TypeAttributeDef  = TypeAttDef (Annotation) (String) (TypeName ) 
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
                           ( TypeAttributeDef ,String,(Maybe Type),TypeAttributeDef )
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {cat_Inh_TypeAttributeDef :: Catalog,lib_Inh_TypeAttributeDef :: LocalBindings,overrideLib_Inh_TypeAttributeDef :: (Maybe LocalBindings),overrideLibs_Inh_TypeAttributeDef :: ([Maybe LocalBindings])}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {annotatedTree_Syn_TypeAttributeDef :: TypeAttributeDef ,attrName_Syn_TypeAttributeDef :: String,namedType_Syn_TypeAttributeDef :: (Maybe Type),originalTree_Syn_TypeAttributeDef :: TypeAttributeDef }
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
                  name_
              -- "./TypeChecking/MiscCreates.ag"(line 38, column 9)
              _lhsOnamedType =
                  _typInamedType
              -- self rule
              _annotatedTree =
                  TypeAttDef ann_ name_ _typIannotatedTree
              -- self rule
              _originalTree =
                  TypeAttDef ann_ name_ _typIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              -- copy rule (down)
              _typOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _typOoverrideLibs =
                  _lhsIoverrideLibs
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
type TypeAttributeDefList  = [TypeAttributeDef ]
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
                               ( TypeAttributeDefList ,([(String, Maybe Type)]),TypeAttributeDefList )
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {cat_Inh_TypeAttributeDefList :: Catalog,lib_Inh_TypeAttributeDefList :: LocalBindings,overrideLib_Inh_TypeAttributeDefList :: (Maybe LocalBindings),overrideLibs_Inh_TypeAttributeDefList :: ([Maybe LocalBindings])}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {annotatedTree_Syn_TypeAttributeDefList :: TypeAttributeDefList ,attrs_Syn_TypeAttributeDefList :: ([(String, Maybe Type)]),originalTree_Syn_TypeAttributeDefList :: TypeAttributeDefList }
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
                  (_hdIattrName, _hdInamedType) : _tlIattrs
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data TypeName  = ArrayTypeName (Annotation) (TypeName ) 
               | PrecTypeName (Annotation) (String) (Integer) 
               | SetOfTypeName (Annotation) (TypeName ) 
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
                   ( TypeName ,(Maybe Type),TypeName )
data Inh_TypeName  = Inh_TypeName {cat_Inh_TypeName :: Catalog,lib_Inh_TypeName :: LocalBindings,overrideLib_Inh_TypeName :: (Maybe LocalBindings),overrideLibs_Inh_TypeName :: ([Maybe LocalBindings])}
data Syn_TypeName  = Syn_TypeName {annotatedTree_Syn_TypeName :: TypeName ,namedType_Syn_TypeName :: (Maybe Type),originalTree_Syn_TypeName :: TypeName }
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
              _originalTree =
                  ArrayTypeName ann_ _typIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              -- copy rule (down)
              _typOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _typOoverrideLibs =
                  _lhsIoverrideLibs
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
              _originalTree =
                  PrecTypeName ann_ tn_ prec_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
              _originalTree =
                  SetOfTypeName ann_ _typIoriginalTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              -- copy rule (down)
              _typOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _typOoverrideLibs =
                  _lhsIoverrideLibs
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
              _originalTree =
                  SimpleTypeName ann_ tn_
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
type TypeNameList  = [TypeName ]
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
                       ( TypeNameList ,([Maybe Type]),TypeNameList )
data Inh_TypeNameList  = Inh_TypeNameList {cat_Inh_TypeNameList :: Catalog,lib_Inh_TypeNameList :: LocalBindings,overrideLib_Inh_TypeNameList :: (Maybe LocalBindings),overrideLibs_Inh_TypeNameList :: ([Maybe LocalBindings])}
data Syn_TypeNameList  = Syn_TypeNameList {annotatedTree_Syn_TypeNameList :: TypeNameList ,namedTypes_Syn_TypeNameList :: ([Maybe Type]),originalTree_Syn_TypeNameList :: TypeNameList }
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
                  _hdInamedType : _tlInamedTypes
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data VarDef  = VarDef (Annotation) (String) (TypeName ) ((Maybe Expression)) 
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
                 ( VarDef ,((String,Maybe Type)),VarDef )
data Inh_VarDef  = Inh_VarDef {cat_Inh_VarDef :: Catalog,lib_Inh_VarDef :: LocalBindings,overrideLib_Inh_VarDef :: (Maybe LocalBindings),overrideLibs_Inh_VarDef :: ([Maybe LocalBindings])}
data Syn_VarDef  = Syn_VarDef {annotatedTree_Syn_VarDef :: VarDef ,def_Syn_VarDef :: ((String,Maybe Type)),originalTree_Syn_VarDef :: VarDef }
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
                  (name_, if _typInamedType == Just (Pseudo Record)
                          then Just (PgRecord Nothing)
                          else _typInamedType)
              -- self rule
              _annotatedTree =
                  VarDef ann_ name_ _typIannotatedTree value_
              -- self rule
              _originalTree =
                  VarDef ann_ name_ _typIoriginalTree value_
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _typOcat =
                  _lhsIcat
              -- copy rule (down)
              _typOlib =
                  _lhsIlib
              -- copy rule (down)
              _typOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _typOoverrideLibs =
                  _lhsIoverrideLibs
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
type VarDefList  = [VarDef ]
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
                     ( VarDefList ,([(String,Maybe Type)]),VarDefList )
data Inh_VarDefList  = Inh_VarDefList {cat_Inh_VarDefList :: Catalog,lib_Inh_VarDefList :: LocalBindings,overrideLib_Inh_VarDefList :: (Maybe LocalBindings),overrideLibs_Inh_VarDefList :: ([Maybe LocalBindings])}
data Syn_VarDefList  = Syn_VarDefList {annotatedTree_Syn_VarDefList :: VarDefList ,defs_Syn_VarDefList :: ([(String,Maybe Type)]),originalTree_Syn_VarDefList :: VarDefList }
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
                  _hdIdef : _tlIdefs
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOcat =
                  _lhsIcat
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOcat =
                  _lhsIcat
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  []
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
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
data WithQuery  = WithQuery (Annotation) (String) (SelectExpression ) 
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
                    ( WithQuery ,([CatalogUpdate]),WithQuery )
data Inh_WithQuery  = Inh_WithQuery {cat_Inh_WithQuery :: Catalog,lib_Inh_WithQuery :: LocalBindings,overrideLib_Inh_WithQuery :: (Maybe LocalBindings),overrideLibs_Inh_WithQuery :: ([Maybe LocalBindings])}
data Syn_WithQuery  = Syn_WithQuery {annotatedTree_Syn_WithQuery :: WithQuery ,catUpdates_Syn_WithQuery :: ([CatalogUpdate]),originalTree_Syn_WithQuery :: WithQuery }
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
                  []
              -- "./TypeChecking/SelectStatement.ag"(line 246, column 9)
              _tpe =
                  Right $ Pseudo Void
              -- "./TypeChecking/SelectStatement.ag"(line 247, column 9)
              _backTree =
                  WithQuery ann_ name_ _exIannotatedTree
              -- "./TypeChecking/SelectStatement.ag"(line 248, column 9)
              _attrs =
                  maybe [] id $ _exIuType
              -- "./TypeChecking/SelectStatement.ag"(line 249, column 9)
              _catUpdates =
                  [CatCreateView name_ _attrs    ]
              -- "./TypeChecking/SelectStatement.ag"(line 250, column 9)
              _statementType =
                  Nothing
              -- self rule
              _annotatedTree =
                  WithQuery ann_ name_ _exIannotatedTree
              -- self rule
              _originalTree =
                  WithQuery ann_ name_ _exIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
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
              _exOlib =
                  _lhsIlib
              -- copy rule (down)
              _exOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _exOoverrideLibs =
                  _lhsIoverrideLibs
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
type WithQueryList  = [WithQuery ]
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
                        ( WithQueryList ,WithQueryList ,Catalog)
data Inh_WithQueryList  = Inh_WithQueryList {cat_Inh_WithQueryList :: Catalog,catUpdates_Inh_WithQueryList :: ([CatalogUpdate]),lib_Inh_WithQueryList :: LocalBindings,overrideLib_Inh_WithQueryList :: (Maybe LocalBindings),overrideLibs_Inh_WithQueryList :: ([Maybe LocalBindings])}
data Syn_WithQueryList  = Syn_WithQueryList {annotatedTree_Syn_WithQueryList :: WithQueryList ,originalTree_Syn_WithQueryList :: WithQueryList ,producedCat_Syn_WithQueryList :: Catalog}
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
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
              -- "./TypeChecking/SelectStatement.ag"(line 232, column 9)
              _hdOcat =
                  _newCat
              -- "./TypeChecking/SelectStatement.ag"(line 233, column 9)
              _tlOcat =
                  _newCat
              -- "./TypeChecking/SelectStatement.ag"(line 237, column 9)
              _lhsOproducedCat =
                  _tlIproducedCat
              -- "./TypeChecking/SelectStatement.ag"(line 240, column 9)
              _tlOcatUpdates =
                  _hdIcatUpdates
              -- self rule
              _annotatedTree =
                  (:) _hdIannotatedTree _tlIannotatedTree
              -- self rule
              _originalTree =
                  (:) _hdIoriginalTree _tlIoriginalTree
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
              -- copy rule (down)
              _hdOlib =
                  _lhsIlib
              -- copy rule (down)
              _hdOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _hdOoverrideLibs =
                  _lhsIoverrideLibs
              -- copy rule (down)
              _tlOlib =
                  _lhsIlib
              -- copy rule (down)
              _tlOoverrideLib =
                  _lhsIoverrideLib
              -- copy rule (down)
              _tlOoverrideLibs =
                  _lhsIoverrideLibs
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
                  fromRight _lhsIcat $ updateCatalog _lhsIcat _lhsIcatUpdates
              -- "./TypeChecking/SelectStatement.ag"(line 242, column 9)
              _lhsOproducedCat =
                  _newCat
              -- self rule
              _annotatedTree =
                  []
              -- self rule
              _originalTree =
                  []
              -- self rule
              _lhsOannotatedTree =
                  _annotatedTree
              -- self rule
              _lhsOoriginalTree =
                  _originalTree
          in  ( _lhsOannotatedTree,_lhsOoriginalTree,_lhsOproducedCat)))