Copyright 2009 Jake Wheat

This module contains the implementation of the Environment data types
and functions, and provides the api for the other type checking
modules.

> {-# LANGUAGE DeriveDataTypeable #-}
> {-# OPTIONS_HADDOCK hide  #-}

> module Database.HsSqlPpp.AstInternals.Environment.EnvironmentInternal
>     (
>      Environment
>     ,CastContext(..)
>     ,CompositeFlavour(..)
>     ,relationComposites
>     ,CompositeDef
>     ,FunctionPrototype
>     ,DomainDefinition
>     ,FunFlav(..)
>     ,emptyEnvironment
>     ,defaultEnvironment
>     ,EnvironmentUpdate(..)
>     ,updateEnvironment
>     --,destructEnvironment
>     -- type checker stuff
>     ,envCompositeDef
>     ,envCompositeAttrsPair
>     ,envCompositeAttrs
>     ,envCompositePublicAttrs
>     ,envTypeCategory
>     ,envPreferredType
>     ,envCast
>     ,envDomainBaseType
>     ,envLookupFns
>     ,envTypeExists
>     ,envLookupType
>     ,OperatorType(..)
>     ,getOperatorType
>     ,isOperatorName
>     ) where

> import Control.Monad
> import Data.List
> import Data.Generics
> --import Debug.Trace

> import Database.HsSqlPpp.AstInternals.TypeType
> import Database.HsSqlPpp.Utils

> -- | The main datatype, this holds the catalog and context
> -- information to type check against.
> data Environment = Environment
>                    {envTypeNames :: [(String, Type)]
>                    ,envDomainDefs :: [DomainDefinition]
>                    ,envCasts :: [(Type,Type,CastContext)]
>                    ,envTypeCategories :: [(Type,String,Bool)]
>                    ,envPrefixOperators :: [FunctionPrototype]
>                    ,envPostfixOperators :: [FunctionPrototype]
>                    ,envBinaryOperators :: [FunctionPrototype]
>                    ,envFunctions :: [FunctionPrototype]
>                    ,envAggregates :: [FunctionPrototype]
>                    ,envWindowFunctions :: [FunctionPrototype]
>                    ,envAttrDefs :: [CompositeDef]}
>                    deriving Show

> -- | Represents an empty environment. This doesn't contain things
> -- like the \'and\' operator, and so if you try to use it it will
> -- almost certainly not work.
> emptyEnvironment :: Environment
> emptyEnvironment = Environment [] [] [] [] [] [] [] [] [] [] []

> -- | Represents what you probably want to use as a starting point if
> -- you are building an environment from scratch. It contains
> -- information on built in function like things that aren't in the
> -- PostGreSQL catalog, such as greatest, coalesce, keyword operators
> -- like \'and\', etc..
> defaultEnvironment :: Environment
> defaultEnvironment = emptyEnvironment {
>                       envTypeNames = pseudoTypes
>                      ,envBinaryOperators = ("=",[Pseudo AnyElement
>                                                 ,Pseudo AnyElement],
>                                             typeBool, False):keywordOperatorTypes
>                      ,envFunctions = specialFunctionTypes}


> -- | Use to note what the flavour of a cast is, i.e. if/when it can
> -- be used implicitly.
> data CastContext = ImplicitCastContext
>                  | AssignmentCastContext
>                  | ExplicitCastContext
>                    deriving (Eq,Show,Typeable,Data)

> -- | Used to distinguish between standalone composite types, and
> -- automatically generated ones, generated from a table or view
> -- respectively.
> data CompositeFlavour = Composite | TableComposite | ViewComposite
>                         deriving (Eq,Show)

> relationComposites :: [CompositeFlavour]
> relationComposites = [TableComposite,ViewComposite]


> -- | Provides the definition of a composite type. The components are
> -- composite (or table or view) name, the flavour of the composite,
> -- the types of the composite attributes, and the types of the
> -- system columns iff the composite represents a table type (the
> -- third and fourth components are always 'CompositeType's).
> type CompositeDef = (String, CompositeFlavour, Type, Type)

> -- | The components are: function (or operator) name, argument
> -- types, return type and is variadic.
> type FunctionPrototype = (String, [Type], Type, Bool)

> -- | The components are domain type, base type (todo: add check
> -- constraint).
> type DomainDefinition = (Type,Type)

> data EnvironmentUpdate =
>     -- | add a new scalar type with the name given, also creates
>     -- an array type automatically
>     EnvCreateScalar Type String Bool
>   | EnvCreateDomain Type Type
>   | EnvCreateComposite String [(String,Type)]
>   | EnvCreateCast Type Type CastContext
>   | EnvCreateTable String [(String,Type)] [(String,Type)]
>   | EnvCreateView String [(String,Type)]
>   | EnvCreateFunction FunFlav String [Type] Type Bool
>     deriving (Eq,Show,Typeable,Data)

> data FunFlav = FunPrefix | FunPostfix | FunBinary
>              | FunName | FunAgg | FunWindow
>                deriving (Eq,Show,Typeable,Data)

> -- | Applies a list of 'EnvironmentUpdate's to an 'Environment' value
> -- to produce a new Environment value.
> updateEnvironment :: Environment
>                   -> [EnvironmentUpdate]
>                   -> Either [TypeError] Environment
> updateEnvironment env' eus =
>   foldM updateEnv' env' eus
>   where
>     updateEnv' env eu =
>       case eu of
>         EnvCreateScalar ty cat pref -> do
>                 errorWhen (not allowed)
>                   [BadEnvironmentUpdate $ "can only add scalar types\
>                                           \this way, got " ++ show ty]
>                 let ScalarType nm = ty
>                 return $ addTypeWithArray env nm ty cat pref
>                 where
>                   allowed = case ty of
>                                     ScalarType _ -> True
>                                     _ -> False
>         EnvCreateDomain ty baseTy -> do
>                 errorWhen (not allowed)
>                   [BadEnvironmentUpdate $ "can only add domain types\
>                                           \this way, got " ++ show ty]
>                 errorWhen (not baseAllowed)
>                   [BadEnvironmentUpdate $ "can only add domain types\
>                                           \based on scalars, got "
>                                           ++ show baseTy]
>                 let DomainType nm = ty
>                 let cat = envTypeCategory env baseTy
>                 return (addTypeWithArray env nm ty cat False) {
>                                        envDomainDefs =
>                                          (ty,baseTy):envDomainDefs env
>                                        ,envCasts =
>                                          (ty,baseTy,ImplicitCastContext):envCasts env}
>                 where
>                   allowed = case ty of
>                                             DomainType _ -> True
>                                             _ -> False
>                   baseAllowed = case baseTy of
>                                                     ScalarType _ -> True
>                                                     _ -> False

>         EnvCreateComposite nm flds ->
>                 return $ (addTypeWithArray env nm (NamedCompositeType nm) "C" False) {
>                             envAttrDefs =
>                               (nm,Composite,CompositeType flds, CompositeType [])
>                               : envAttrDefs env}
>         EnvCreateCast src tgt ctx -> return $ env {envCasts = (src,tgt,ctx):envCasts env}
>         EnvCreateTable nm attrs sysAttrs -> do
>                 checkTypeDoesntExist env nm (NamedCompositeType nm)
>                 return $ (addTypeWithArray env nm
>                             (NamedCompositeType nm) "C" False) {
>                             envAttrDefs =
>                               (nm,TableComposite,CompositeType attrs, CompositeType sysAttrs)
>                               : envAttrDefs env}
>         EnvCreateView nm attrs -> {-trace ("create view:" ++ show nm) $-} do
>                 checkTypeDoesntExist env nm (NamedCompositeType nm)
>                 return $ (addTypeWithArray env nm
>                             (NamedCompositeType nm) "C" False) {
>                             envAttrDefs =
>                               (nm,ViewComposite,CompositeType attrs, CompositeType [])
>                               : envAttrDefs env}
>         EnvCreateFunction f nm args ret vdc ->
>             return $ case f of
>               FunPrefix -> env {envPrefixOperators=(nm,args,ret,vdc):envPrefixOperators env}
>               FunPostfix -> env {envPostfixOperators=(nm,args,ret,vdc):envPostfixOperators env}
>               FunBinary -> env {envBinaryOperators=(nm,args,ret,vdc):envBinaryOperators env}
>               FunAgg -> env {envAggregates=(nm,args,ret,vdc):envAggregates env}
>               FunWindow -> env {envWindowFunctions=(nm,args,ret,vdc):envWindowFunctions env}
>               FunName -> env {envFunctions=(nm,args,ret,vdc):envFunctions env}
>     addTypeWithArray env nm ty cat pref =
>       env {envTypeNames =
>                ('_':nm,ArrayType ty)
>                : (nm,ty)
>                : envTypeNames env
>           ,envTypeCategories =
>                (ArrayType ty,"A",False)
>                : (ty,cat,pref)
>                : envTypeCategories env}
>     checkTypeDoesntExist env nm ty = do
>         errorWhen (any (==nm) $ map fst $ envTypeNames env)
>             [TypeAlreadyExists ty]
>         errorWhen (any (==ty) $ map snd $ envTypeNames env)
>             [TypeAlreadyExists ty]
>         return ()


> {-
> -- | Takes part an 'Environment' value to produce a list of 'EnvironmentUpdate's.
> -- You can use this to look inside the Environment data type e.g. if you want to
> -- examine a catalog. It should be the case that:
> --
> -- @
> -- updateEnvironment emptyEnvironment (destructEnvironment env) = env
> -- @
> destructEnvironment :: Environment -> [EnvironmentUpdate]
> destructEnvironment = undefined
> -}

TODO -this shouldn't be too difficult, just bluff it and use quick
check to see if it works


================================================================================

= type checking stuff

> envCompositeDef :: Environment -> [CompositeFlavour] -> String -> Either [TypeError] (CompositeDef)
> envCompositeDef env flvs nm = do
>   let c = filter (\(n,t,_,_) -> n == nm && (null flvs || t `elem` flvs)) $ envAttrDefs env
>   errorWhen (null c)
>             [UnrecognisedRelation nm]
>   case c of
>     (_,fl1,r,s):[] -> return (nm,fl1,r,s)
>     _ -> error $ "problem getting attributes for: " ++ show nm ++ ", " ++ show c

> envCompositeAttrsPair :: Environment -> [CompositeFlavour] -> String
>                       -> Either [TypeError] ([(String,Type)],[(String,Type)])
> envCompositeAttrsPair env flvs ty = do
>    (_,_,CompositeType a,CompositeType b) <- envCompositeDef env flvs ty
>    return (a,b)

> envCompositeAttrs :: Environment -> [CompositeFlavour] -> String
>                   -> Either [TypeError] [(String,Type)]
> envCompositeAttrs env flvs ty = do
>   (a,b) <- envCompositeAttrsPair env flvs ty
>   return $ a ++ b

> envCompositePublicAttrs :: Environment -> [CompositeFlavour] -> String
>                   -> Either [TypeError] [(String,Type)]
> envCompositePublicAttrs env flvs ty = do
>   (a,_) <- envCompositeAttrsPair env flvs ty
>   return a


> envTypeCategory :: Environment -> Type -> String
> envTypeCategory env ty =
>   let (c,_) = envGetCategoryInfo env ty
>   in c

> envPreferredType :: Environment -> Type -> Bool
> envPreferredType env ty =
>   let (_,p) = envGetCategoryInfo env ty
>   in p

> envCast :: Environment -> CastContext -> Type -> Type -> Bool
> envCast env ctx from to = {-trace ("check cast " ++ show from ++ show to) $-}
>     case from of
>       t@(DomainType _) -> let baseType = envDomainBaseType env t
>                           in if baseType == to
>                                then True
>                                else envCast env ctx baseType to ||
>                                     any (==(from,to,ctx)) (envCasts env)
>       _ -> any (==(from,to,ctx)) (envCasts env)


> envDomainBaseType :: Environment -> Type -> Type
> envDomainBaseType env ty =
>   --check type is domain, check it exists in main list
>   case lookup ty (envDomainDefs env) of
>       Nothing -> error "domain not found" -- Left [DomainDefNotFound ty]
>       Just t -> t


> envLookupFns :: Environment -> String -> [FunctionPrototype]
> envLookupFns env name =
>     filter (\(nm,_,_,_) -> nm == name) envGetAllFns
>     where
>     envGetAllFns =
>         concat [envPrefixOperators env
>                ,envPostfixOperators env
>                ,envBinaryOperators env
>                ,envFunctions env
>                ,envAggregates env
>                ,envWindowFunctions env]

== internal support for type checker fns above

> envGetCategoryInfo :: Environment -> Type -> (String, Bool)
> envGetCategoryInfo env ty =
>   case ty of
>     SetOfType _ -> ("", False)
>     AnonymousRecordType _ -> ("", False)
>     ArrayType (Pseudo _) -> ("A",False)
>     Pseudo _ -> ("P",False)
>     _ -> let l = filter (\(t,_,_) -> ty == t) $ envTypeCategories env
>          in if null l
>               then error $ "no type category for " ++ show ty
>               else let (_,c,p):_ =l
>                    in (c,p)

> envTypeExists :: Environment -> Type -> Either [TypeError] Type
> envTypeExists env t =
>     errorWhen (t `notElem` map snd (envTypeNames env))
>               [UnknownTypeError t] >>
>     Right t

> envLookupType :: Environment -> String -> Either [TypeError] Type
> envLookupType env name =
>     liftME [UnknownTypeName name] $
>       lookup name (envTypeNames env)


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
>  ,("!isNull", [Pseudo AnyElement], typeBool, False)
>  ,("!isNotNull", [Pseudo AnyElement], typeBool, False)
>  ,("!arrayCtor", [ArrayType $ Pseudo AnyElement], Pseudo AnyArray, True)
>  ,("!between", [Pseudo AnyElement
>                ,Pseudo AnyElement
>                ,Pseudo AnyElement], Pseudo AnyElement, False)
>  ,("!substring", [ScalarType "text",typeInt,typeInt], ScalarType "text", False)
>  ,("!arraySub", [Pseudo AnyArray,typeInt], Pseudo AnyElement, False)
>  ]

these look like functions, but don't appear in the postgresql catalog.

> specialFunctionTypes :: [FunctionPrototype]
> specialFunctionTypes = [
>   ("coalesce", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement, True)
>  ,("nullif", [Pseudo AnyElement, Pseudo AnyElement], Pseudo AnyElement,False)
>  ,("greatest", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement,True)
>  ,("least", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement,True)
>  ]

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

> getOperatorType :: Environment -> String -> OperatorType
> getOperatorType env s = case () of
>                       _ | s `elem` ["!and", "!or","!like"] -> BinaryOp
>                         | s `elem` ["!not"] -> PrefixOp
>                         | s `elem` ["!isNull", "!isNotNull"] -> PostfixOp
>                         | any (\(x,_,_,_) -> x == s) (envBinaryOperators env) ->
>                             BinaryOp
>                         | any (\(x,_,_,_) -> x == s ||
>                                            (x=="-" && s=="u-"))
>                               (envPrefixOperators env) ->
>                             PrefixOp
>                         | any (\(x,_,_,_) -> x == s) (envPostfixOperators env) ->
>                             PostfixOp
>                         | otherwise ->
>                             error $ "don't know flavour of operator " ++ s

> isOperatorName :: String -> Bool
> isOperatorName = any (`elem` "+-*/<>=~!@#%^&|`?")
