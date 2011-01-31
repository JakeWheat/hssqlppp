
This module contains the implementation of the Catalog data types
and functions, and provides the api for the other type checking
modules.

> {-# LANGUAGE DeriveDataTypeable #-}
>
> module Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
>     (
>      Catalog
>     ,CastContext(..)
>     ,CompositeFlavour(..)
>     ,relationComposites
>     ,CompositeDef
>     ,FunctionPrototype
>     ,DomainDefinition
>     ,FunFlav(..)
>     ,emptyCatalog
>     ,defaultCatalog
>     ,CatalogUpdate(..)
>     ,ppCatUpdate
>     ,updateCatalog
>     ,deconstructCatalog
>     -- type checker stuff
>     ,catCompositeDef
>     ,catCompositeAttrsPair
>     ,catCompositeAttrs
>     ,catCompositePublicAttrs
>     ,catTypeCategory
>     ,catPreferredType
>     ,catCast
>     ,catDomainBaseType
>     ,catLookupFns
>     ,catTypeExists
>     ,catLookupType
>     ,OperatorType(..)
>     ,getOperatorType
>     ,isOperatorName
>     -- comparing catalogs
>     ,CatalogDiff(..)
>     ,compareCatalogs
>     ,ppCatDiff
>     ) where
>
> import Control.Monad
> import Data.List
> import Data.Data
> -- import Debug.Trace
> import Data.Char
>
> import Database.HsSqlPpp.AstInternals.TypeType
> import Database.HsSqlPpp.Utils.Utils
>
> -- | The main datatype, this holds the catalog and context
> -- information to type check against.
> data Catalog = Catalog
>                    {catTypeNames :: [(String, Type)]
>                    ,catDomainDefs :: [DomainDefinition]
>                    ,catCasts :: [(Type,Type,CastContext)]
>                    ,catTypeCategories :: [(Type,String,Bool)]
>                    ,catPrefixOperators :: [FunctionPrototype]
>                    ,catPostfixOperators :: [FunctionPrototype]
>                    ,catBinaryOperators :: [FunctionPrototype]
>                    ,catFunctions :: [FunctionPrototype]
>                    ,catAggregates :: [FunctionPrototype]
>                    ,catWindowFunctions :: [FunctionPrototype]
>                    ,catAttrDefs :: [CompositeDef]
>                    ,catUpdates :: [CatalogUpdate]}
>                    deriving Show
>
> -- | Represents an empty catalog. This doesn't contain things
> -- like the \'and\' operator, 'defaultCatalog' contains these.
> emptyCatalog :: Catalog
> emptyCatalog = Catalog [] [] [] [] [] [] [] [] [] [] [] []
>
> -- | Represents what you probably want to use as a starting point if
> -- you are building an catalog from scratch. It contains
> -- information on built in function like things that aren't in the
> -- PostgreSQL catalog, such as greatest, coalesce, keyword operators
> -- like \'and\', etc..
> defaultCatalog :: Catalog
> defaultCatalog =
>   emptyCatalog {catTypeNames = pseudoTypes
>                ,catBinaryOperators = pe : keywordOperatorTypes
>                ,catFunctions = specialFunctionTypes}
>   where
>     pe = ("=", [Pseudo AnyElement, Pseudo AnyElement], typeBool, False)
>
> -- | Use to note what the flavour of a cast is, i.e. if/when it can
> -- be used implicitly.
> data CastContext = ImplicitCastContext
>                  | AssignmentCastContext
>                  | ExplicitCastContext
>                    deriving (Eq,Show,Ord,Typeable,Data)
>
> -- | Used to distinguish between standalone composite types, and
> -- automatically generated ones, generated from a table or view
> -- respectively.
> data CompositeFlavour = Composite | TableComposite | ViewComposite
>                         deriving (Eq,Ord,Show)
>
> relationComposites :: [CompositeFlavour]
> relationComposites = [TableComposite,ViewComposite]
>
> -- | Provides the definition of a composite type. The components are
> -- composite (or table or view) name, the flavour of the composite,
> -- the types of the composite attributes, and the types of the
> -- system columns iff the composite represents a table type (the
> -- third and fourth components are always 'CompositeType's).
> type CompositeDef = (String, CompositeFlavour, Type, Type)
>
> -- | The components are: function (or operator) name, argument
> -- types, return type and is variadic.
> type FunctionPrototype = (String, [Type], Type, Bool)
>
> -- | The components are domain type, base type (todo: add check
> -- constraint).
> type DomainDefinition = (Type,Type)
>
> data CatalogUpdate =
>     -- | add a new scalar type with the name given, also creates
>     -- an array type automatically
>     CatCreateScalar Type String Bool
>   | CatCreateDomain Type Type
>   | CatCreateComposite String [(String,Type)]
>   | CatCreateCast Type Type CastContext
>   | CatCreateTable String [(String,Type)] [(String,Type)]
>   | CatCreateView String [(String,Type)]
>   | CatCreateFunction FunFlav String [Type] Type Bool
>   | CatDropFunction Bool String [Type]
>     deriving (Eq,Ord,Typeable,Data,Show)
>
> ppCatUpdate :: CatalogUpdate -> String
> ppCatUpdate (CatCreateScalar t c p) =
>   "CatCreateScalar " ++ show t ++ "(" ++ c ++ "," ++ show p ++ ")"
> ppCatUpdate (CatCreateDomain t b) =
>   "CatCreateDomain " ++ show t ++ " as " ++ show b
> ppCatUpdate (CatCreateComposite nm flds) =
>   "CatCreateComposite " ++ nm ++ showFlds flds
> ppCatUpdate (CatCreateCast s t ctx) =
>   "CatCreateCast " ++ show s ++ "->" ++ show t ++ " " ++ show ctx
> ppCatUpdate (CatCreateTable nm flds1 flds2) =
>   "CatCreateTable " ++ nm ++ showFlds flds1 ++ showFlds flds2
> ppCatUpdate (CatCreateView nm flds) =
>   "CatCreateView " ++ nm ++ showFlds flds
> ppCatUpdate (CatCreateFunction flav nm args ret vdc) =
>   "CatCreateFunction " ++ show flav ++ " " ++ nm
>   ++ " returns " ++ show ret
>   ++ "(" ++ intercalate "," (map show args) ++ ")"
>   ++ if vdc then " variadic" else ""
> ppCatUpdate (CatDropFunction _ nm args) =
>   "CatDropFunction " ++ nm ++ "(" ++ show args ++ ")"
>
> showFlds :: [(String,Type)] -> String
> showFlds flds = "(\n" ++ sfs flds ++ ")"
>                 where
>                   sfs ((nm,t):fs) = "    " ++ show nm
>                                     ++ " " ++ show t ++ "\n" ++ sfs fs
>                   sfs [] = ""
>
> data FunFlav = FunPrefix | FunPostfix | FunBinary
>              | FunName | FunAgg | FunWindow
>                deriving (Eq,Show,Ord,Typeable,Data)
>
> -- | Applies a list of 'CatalogUpdate's to an 'Catalog' value
> -- to produce a new Catalog value.
> updateCatalog :: Catalog
>                   -> [CatalogUpdate]
>                   -> Either [TypeError] Catalog
> updateCatalog cat' eus =
>   foldM updateCat' (cat' {catUpdates = catUpdates cat' ++ eus}) eus
>   where
>     updateCat' cat (CatCreateScalar ty catl pref) = do
>       errorWhen (not allowed)
>         [BadCatalogUpdate $ "can only add scalar types\
>                             \this way, got " ++ show ty]
>       let ScalarType nm = ty
>       return $ addTypeWithArray cat nm ty catl pref
>       where
>         allowed = case ty of
>                           ScalarType _ -> True
>                           _ -> False
>
>     updateCat' cat (CatCreateDomain ty baseTy) = do
>       errorWhen (not allowed)
>         [BadCatalogUpdate $ "can only add domain types\
>                             \this way, got " ++ show ty]
>       errorWhen (not baseAllowed)
>         [BadCatalogUpdate $ "can only add domain types\
>                                 \based on scalars, got "
>                                 ++ show baseTy]
>       let DomainType nm = ty
>       catl <- catTypeCategory cat baseTy
>       return (addTypeWithArray cat nm ty catl False) {
>                              catDomainDefs =
>                                (ty,baseTy):catDomainDefs cat
>                              ,catCasts =
>                                (ty,baseTy,ImplicitCastContext):catCasts cat}
>       where
>         allowed = case ty of
>                           DomainType _ -> True
>                           _ -> False
>         baseAllowed = case baseTy of
>                                   ScalarType _ -> True
>                                   _ -> False
>     updateCat' cat (CatCreateComposite nm flds) =
>       return $ (addTypeWithArray cat nm (NamedCompositeType nm) "C" False) {
>                   catAttrDefs =
>                     (nm,Composite,CompositeType flds, CompositeType [])
>                     : catAttrDefs cat}
>
>     updateCat' cat (CatCreateCast src tgt ctx) =
>       return $ cat {catCasts = (src,tgt,ctx):catCasts cat}
>
>     updateCat' cat (CatCreateTable nm attrs sysAttrs) = do
>       checkTypeDoesntExist cat nm (NamedCompositeType nm)
>       return $ (addTypeWithArray cat nm
>                   (NamedCompositeType nm) "C" False) {
>                   catAttrDefs =
>                     (nm,TableComposite
>                     ,CompositeType attrs
>                     , CompositeType sysAttrs)
>                     : catAttrDefs cat}
>
>     updateCat' cat (CatCreateView nm attrs) = do
>       checkTypeDoesntExist cat nm (NamedCompositeType nm)
>       return $ (addTypeWithArray cat nm
>                   (NamedCompositeType nm) "C" False) {
>                   catAttrDefs =
>                     (nm,ViewComposite,CompositeType attrs, CompositeType [])
>                     : catAttrDefs cat}
>
>     updateCat' cat (CatCreateFunction f nm args ret vdc) =
>         return $ case f of
>           FunPrefix -> cat {catPrefixOperators =
>                               fp : catPrefixOperators cat}
>           FunPostfix -> cat {catPostfixOperators =
>                                fp : catPostfixOperators cat}
>           FunBinary -> cat {catBinaryOperators =
>                               fp : catBinaryOperators cat}
>           FunAgg -> cat {catAggregates =
>                            fp : catAggregates cat}
>           FunWindow -> cat {catWindowFunctions =
>                               fp : catWindowFunctions cat}
>           FunName -> cat {catFunctions =
>                             fp : catFunctions cat}
>         where fp = (nm,args,ret,vdc)
>
>     updateCat' cat (CatDropFunction _ifexists nm args) = do
>         let matches =  filter matchingFn (catFunctions cat)
>         errorWhen (null matches)
>                   [BadCatalogUpdate
>                    $ "couldn't find function to drop "
>                      ++ show nm ++ "(" ++ show args++")"]
>         errorWhen (length matches > 1)
>                   [BadCatalogUpdate
>                    $ "multiple matching functions to drop "
>                      ++ show nm ++ "(" ++ show args++")"]
>         return cat {catFunctions = filter (not . matchingFn)
>                                           (catFunctions cat)
>                    ,catUpdates = filter (not.matchingUpdate)
>                                         (catUpdates cat)}
>         where
>           matchingFn (nm1,a1,_,_) =
>             map toLower nm == map toLower nm1 && args == a1
>           matchingUpdate (CatDropFunction _ nm2 a2)
>                          | map toLower nm2 == map toLower nm
>                            && a2 == args = True
>           matchingUpdate (CatCreateFunction _ nm2 a2 _ _)
>                          | map toLower nm2 == map toLower nm
>                            && a2 == args = True
>           matchingUpdate _ = False

todo:
look for matching function in list, if not found then error
remove from list, and remove from update list

>     addTypeWithArray cat nm ty catl pref =
>       cat {catTypeNames =
>                ('_':nm,ArrayType ty)
>                : (nm,ty)
>                : catTypeNames cat
>           ,catTypeCategories =
>                (ArrayType ty,"A",False)
>                : (ty,catl,pref)
>                : catTypeCategories cat}
>     checkTypeDoesntExist cat nm ty = do
>         errorWhen (any (==nm) $ map fst $ catTypeNames cat)
>             [TypeAlreadyExists ty]
>         errorWhen (any (==ty) $ map snd $ catTypeNames cat)
>             [TypeAlreadyExists ty]
>         return ()
> {-
>  | Takes part an 'Catalog' value to produce a list of 'CatalogUpdate's.
>  You can use this to look inside the Catalog data type e.g. if you want to
>  examine a catalog. It should be the case that:
>  @
>   updateCatalog emptyCatalog (deconstructCatalog cat) = cat
>  @ -}
> deconstructCatalog :: Catalog -> [CatalogUpdate]
> deconstructCatalog = catUpdates

--------------------------------------------------------------------------------

= type checking stuff

> catCompositeDef :: Catalog -> [CompositeFlavour] -> String
>                 -> Either [TypeError] CompositeDef
> catCompositeDef cat flvs nm = do
>   let c = filter m $ catAttrDefs cat
>   errorWhen (null c)
>             [UnrecognisedRelation nm]
>   case c of
>     (_,fl1,r,s):[] -> return (nm,fl1,r,s)
>     _ -> Left [InternalError $ "problem getting attributes for: "
>                                ++ show nm ++ ", " ++ show c]
>   where
>     m (n,t,_,_) = n == nm && (null flvs || t `elem` flvs)
>
> catCompositeAttrsPair :: Catalog -> [CompositeFlavour] -> String
>                       -> Either [TypeError] ([(String,Type)],[(String,Type)])
> catCompositeAttrsPair cat flvs ty = do
>    (_,_,CompositeType a,CompositeType b) <- catCompositeDef cat flvs ty
>    return (a,b)
>
> catCompositeAttrs :: Catalog -> [CompositeFlavour] -> String
>                   -> Either [TypeError] [(String,Type)]
> catCompositeAttrs cat flvs ty = do
>   (a,b) <- catCompositeAttrsPair cat flvs ty
>   return $ a ++ b
>
> catCompositePublicAttrs :: Catalog -> [CompositeFlavour] -> String
>                   -> Either [TypeError] [(String,Type)]
> catCompositePublicAttrs cat flvs ty = do
>   (a,_) <- catCompositeAttrsPair cat flvs ty
>   return a
>
> catTypeCategory :: Catalog -> Type -> Either [TypeError] String
> catTypeCategory cat ty =
>   fmap fst $ catGetCategoryInfo cat ty
>
> catPreferredType :: Catalog -> Type -> Either [TypeError] Bool
> catPreferredType cat ty =
>   fmap snd $ catGetCategoryInfo cat ty
>
> catCast :: Catalog -> CastContext -> Type -> Type -> Either [TypeError] Bool
> catCast cat ctx from to = {-trace ("check cast " ++ show from ++ show to) $-}
>     case from of
>       t@(DomainType _) -> do
>                 baseType <- catDomainBaseType cat t
>                 cc <- catCast cat ctx baseType to
>                 return $ (baseType == to) ||
>                                (cc ||
>                                   any (== (from, to, ctx)) (catCasts cat))
>       _ -> Right $ any (==(from,to,ctx)) (catCasts cat)
>
> catDomainBaseType :: Catalog -> Type -> Either [TypeError] Type
> catDomainBaseType cat ty =
>   --check type is domain, check it exists in main list
>   case lookup ty (catDomainDefs cat) of
>       Nothing -> Left [DomainDefNotFound ty]
>       Just t -> Right t
>
> catLookupFns :: Catalog -> String -> [FunctionPrototype]
> catLookupFns cat name =
>     filter (\(nm,_,_,_) -> map toLower nm == map toLower name) catGetAllFns
>     where
>     catGetAllFns =
>         concat [catPrefixOperators cat
>                ,catPostfixOperators cat
>                ,catBinaryOperators cat
>                ,catFunctions cat
>                ,catAggregates cat
>                ,catWindowFunctions cat]

== internal support for type checker fns above

> catGetCategoryInfo :: Catalog -> Type -> Either [TypeError] (String, Bool)
> catGetCategoryInfo cat ty =
>   case ty of
>     SetOfType _ -> Right ("", False)
>     AnonymousRecordType _ -> Right ("", False)
>     ArrayType (Pseudo _) -> Right ("A",False)
>     Pseudo _ -> Right ("P",False)
>     _ -> let l = filter (\(t,_,_) -> ty == t) $ catTypeCategories cat
>          in if null l
>               then Left [InternalError $ "no type category for " ++ show ty]
>               else let (_,c,p):_ =l
>                    in Right (c,p)
>
> catTypeExists :: Catalog -> Type -> Either [TypeError] Type
> catTypeExists cat t =
>     errorWhen (t `notElem` map snd (catTypeNames cat))
>               [UnknownTypeError t] >>
>     Right t
>
> catLookupType :: Catalog -> String -> Either [TypeError] Type
> catLookupType cat name =
>     liftME [UnknownTypeName name] $
>       lookup name (catTypeNames cat)
>

================================================================================

= built in stuff

keyword operators, all of these are built in and don't appear in any
postgresql catalog

This is wrong, these need to be separated into prefix, postfix, binary

> keywordOperatorTypes :: [FunctionPrototype]
> keywordOperatorTypes = [
>   ("!and", [typeBool, typeBool], typeBool, False)
>  ,("!or", [typeBool, typeBool], typeBool, False)
>  ,("!like", [ScalarType "text", ScalarType "text"], typeBool, False)
>  ,("!not", [typeBool], typeBool, False)
>  ,("!isnull", [Pseudo AnyElement], typeBool, False)
>  ,("!isnotnull", [Pseudo AnyElement], typeBool, False)
>  ,("!arrayctor", [ArrayType $ Pseudo AnyElement], Pseudo AnyArray, True)
>  ,("!between", [Pseudo AnyElement
>                ,Pseudo AnyElement
>                ,Pseudo AnyElement], Pseudo AnyElement, False)
>  ,("!substring"
>   ,[ScalarType "text",typeInt,typeInt]
>   ,ScalarType "text"
>   ,False)
>  ,("!arraysub", [Pseudo AnyArray,typeInt], Pseudo AnyElement, False)
>  ]
>
> -- these look like functions, but don't appear in the postgresql catalog.
>
> specialFunctionTypes :: [FunctionPrototype]
> specialFunctionTypes = [
>   ("coalesce", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement, True)
>  ,("nullif", [Pseudo AnyElement, Pseudo AnyElement], Pseudo AnyElement,False)
>  ,("greatest", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement,True)
>  ,("least", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement,True)
>  ]
>
> pseudoTypes :: [(String, Type)]
> pseudoTypes =
>     [("any",Pseudo Any)
>     ,("anyarray",Pseudo AnyArray)
>     ,("anyelement",Pseudo AnyElement)
>     ,("anyenum",Pseudo AnyEnum)
>     ,("anynonarray",Pseudo AnyNonArray)
>     ,("cstring",Pseudo Cstring)
>     ,("record",Pseudo Record)
>     ,("trigger",Pseudo Trigger)
>     ,("void",Pseudo Void)
>     ,("_cstring",ArrayType $ Pseudo Cstring)
>     ,("_record",ArrayType $ Pseudo Record)
>     --,Pseudo Internal
>     --,Pseudo LanguageHandler
>     --,Pseudo Opaque
>     ]

================================================================================

= getOperatorType

used by the pretty printer, not sure this is a very good design

for now, assume that all the overloaded operators that have the
same name are all either binary, prefix or postfix, otherwise the
getoperatortype would need the types of the arguments to determine
the operator type, and the parser would have to be a lot cleverer
although, parsec handles - being unary and binary without breaking
a sweat, so maybe this isn't too difficult?

this is why binary @ operator isn't currently supported

> data OperatorType = BinaryOp | PrefixOp | PostfixOp
>                   deriving (Eq,Show)
>
> getOperatorType :: Catalog -> String -> Either [TypeError] OperatorType
> getOperatorType cat s =
>   case () of
>           _ | s `elem` ["!and", "!or","!like","."] -> Right BinaryOp
>             | s `elem` ["!not"] -> Right PrefixOp
>             | s `elem` ["!isnull", "!isnotnull"] -> Right PostfixOp
>             | any (\(x,_,_,_) -> x == s) (catBinaryOperators cat) ->
>                       Right BinaryOp
>             | any (\(x,_,_,_) -> x == s || (x=="-" && s=="u-"))
>                   (catPrefixOperators cat) ->
>                       Right PrefixOp
>             | any (\(x,_,_,_) -> x == s) (catPostfixOperators cat) ->
>                       Right PostfixOp
>             | otherwise ->
>                 Left [InternalError $ "don't know flavour of operator " ++ s]
>
> isOperatorName :: String -> Bool
> isOperatorName = any (`elem` "+-*/<>=~!@#%^&|`?.")

================================================================================

> -- | items in first catalog and not second, items in second and not first.
> data CatalogDiff = CatalogDiff [CatalogUpdate] [CatalogUpdate]
>                deriving Show
>
> -- | find differences between two catalogs
> compareCatalogs :: Catalog -> Catalog -> Catalog -> CatalogDiff
> compareCatalogs base start end =
>         let baseCatBits = deconstructCatalog base
>             startCatBits = deconstructCatalog start \\ baseCatBits
>             endCatBits = deconstructCatalog end \\ baseCatBits
>             missing = sort $ endCatBits \\ startCatBits
>             extras = sort $ startCatBits \\ endCatBits
>         in CatalogDiff missing extras
>
> -- | print a catdiff in a more human readable way than show.
> ppCatDiff :: CatalogDiff -> String
> ppCatDiff (CatalogDiff missing extr) =
>     "\nmissing:\n"
>     ++ intercalate "\n" (map ppCatUpdate missing)
>     ++ "\nextra:\n"
>     ++ intercalate "\n" (map ppCatUpdate extr)
