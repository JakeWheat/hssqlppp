Copyright 2009 Jake Wheat

This module contains the implementation of the Environment data types
and functions, and provides the api for the other type checking
modules.

> {-# OPTIONS_HADDOCK hide  #-}

> module Database.HsSqlPpp.TypeChecking.EnvironmentInternal
>     (
>      Environment
>     ,QualifiedIDs
>     ,CastContext(..)
>     ,CompositeFlavour(..)
>     ,CompositeDef
>     ,FunctionPrototype
>     ,DomainDefinition
>     ,FunFlav
>     ,emptyEnvironment
>     ,defaultEnvironment
>     ,EnvironmentUpdate(..)
>     ,updateEnvironment
>     ,destructEnvironment
>     -- type checker stuff
>     ,envExpandStar
>     ,envLookupID
>     ,envCompositeAttrs
>     ,envTypeCategory
>     ,envCast
>     ,envDomainBaseType
>     ,envLookupFns
>      --temporary exports, will become internal
>     ,keywordOperatorTypes
>     ,specialFunctionTypes
>     ) where

> import Control.Monad
> import Data.List

> import Database.HsSqlPpp.TypeChecking.TypeType
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
>                    ,envAttrDefs :: [CompositeDef]
>                    ,envAttrSystemColumns :: [CompositeDef]
>                    ,envIdentifierTypes :: [QualifiedIDs]
>                    ,envJoinIdentifiers :: [String]}


> -- | Represents an empty environment. This doesn't contain things
> -- like the \'and\' operator, and so if you try to use it it will
> -- almost certainly not work.
> emptyEnvironment :: Environment
> emptyEnvironment = Environment [] [] [] [] [] [] [] [] [] [] [] [] []

> -- | Represents what you probably want to use as a starting point if
> -- you are building an environment from scratch. It contains
> -- information on built in function like things that aren't in the
> -- PostGreSQL catalog, such as greatest, coalesce, keyword operators
> -- like \'and\', etc..
> defaultEnvironment :: Environment
> defaultEnvironment = emptyEnvironment {
>                       envTypeNames = map (\t@(Pseudo p) -> (show p,t))
>                                          pseudoTypes
>                      ,envBinaryOperators = keywordOperatorTypes
>                      ,envFunctions = specialFunctionTypes}

> -- | Represents the types of the ids available, currently used for
> -- resolving identifiers inside select expressions. Will probably
> -- change as this is fixed to support more general contexts.  The
> -- components represent the qualifying name (empty string for no
> -- qualifying name), the list of identifier names with their types,
> -- and the list of system column identifier names with their types.
> type QualifiedIDs = (String, ([(String,Type)], [(String,Type)]))

> -- | Use to note what the flavour of a cast is, i.e. if/when it can
> -- be used implicitly.
> data CastContext = ImplicitCastContext
>                  | AssignmentCastContext
>                  | ExplicitCastContext
>                    deriving (Eq,Show)

> -- | Used to distinguish between standalone composite types, and
> -- automatically generated ones, generated from a table or view
> -- respectively.
> data CompositeFlavour = Composite | TableComposite | ViewComposite
>                         deriving (Eq,Show)

> -- | Provides the definition of a composite type. The components are
> -- composite (or table or view) name, the flavour of the composite,
> -- the types of the composite attributes, and the types of the
> -- system columns iff the composite represents a table type (the
> -- third and fourth components are always 'UnnamedCompositeType's).
> type CompositeDef = (String, CompositeFlavour, Type {-, Type-})

> -- | The components are: function (or operator) name, argument
> -- types, and return type.
> type FunctionPrototype = (String, [Type], Type)

> -- | The components are domain type, base type (todo: add check
> -- constraint).
> type DomainDefinition = (Type,Type)

> data EnvironmentUpdate =
>     -- | add a new scalar type with the name given, also creates
>     -- an array type automatically
>     EnvCreateType Type String Bool
>   | EnvCreateDomain Type Type
>   | EnvCreateComposite Type [(String,Type)]
>   | EnvCreateCast Type Type CastContext
>   | EnvCreateTable String [(String,Type)] [(String,Type)]
>   | EnvCreateView String [(String,Type)]
>   | EnvCreateFunction FunFlav String [Type] Type
>     -- | Update the available ids, used internally during type checking
>     -- The second part is a list of join ids, used to expand * correctly
>     -- (if an id is part of a natural join or using list, it only appears
>     -- once when * is expanded, otherwise it could appear once for each
>     -- relation in the join that it appears in.
>   | EnvUpdateIDs [QualifiedIDs] [String]

> data FunFlav = FunPrefix | FunPostfix | FunBinary
>              | FunName | FunAgg

types is (string, type)
domaintypes is type,type


scalar, array, pseudo -> just types
domain type -> type, base type (adds to types, cast also)
composite -> type, unnamed composite type (adds to attr, type)
cast: source type, target type, context
table -> name, unnamed composite type, unnamed comp- adds to attr, type

what about categories?

function flavour: pref, post, bin, fun, agg
 - need name, args, ret

attrs - name, flavour, cols, syscols



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
>         EnvCreateType ty cat pref -> do
>                 errorWhen (not allowed) $
>                   [BadEnvironmentUpdate $ "can only add scalar types\
>                                           \this way, got " ++ show ty]
>                 let ScalarType nm = ty
>                 return $ addTypeWithArray env nm ty cat pref
>                 where
>                   allowed = case ty of
>                                     ScalarType _ -> True
>                                     _ -> False
>         EnvCreateDomain ty baseTy -> do
>                 errorWhen (not allowed) $
>                   [BadEnvironmentUpdate $ "can only add domain types\
>                                           \this way, got " ++ show ty]
>                 errorWhen (not baseAllowed) $
>                   [BadEnvironmentUpdate $ "can only add domain types\
>                                           \based on scalars, got "
>                                           ++ show baseTy]
>                 let DomainType nm = ty
>                 cat <- envTypeCategory env baseTy
>                 return $ (addTypeWithArray env nm ty cat False) {
>                                        envDomainDefs =
>                                          (ty,baseTy):envDomainDefs env}
>                 where
>                   allowed = case ty of
>                                     DomainType _ -> True
>                                     _ -> False
>                   baseAllowed = case baseTy of
>                                   ScalarType _ -> True
>                                   _ -> False
>         EnvCreateComposite ty flds -> do
>                 errorWhen (not allowed)
>                   [BadEnvironmentUpdate $ "can only add composite types\
>                                           \this way, got " ++ show ty]
>                 let CompositeType nm = ty
>                 return $ (addTypeWithArray env nm ty "C" False) {
>                             envAttrDefs =
>                               (nm,Composite,UnnamedCompositeType flds)
>                               : envAttrDefs env}
>                 where
>                   allowed = case ty of
>                                     CompositeType _ -> True
>                                     _ -> False
>         EnvCreateCast src tgt ctx -> return $ env {envCasts = (src,tgt,ctx):envCasts env}
>         EnvCreateTable nm attrs sysAttrs -> do
>                 return $ (addTypeWithArray env nm
>                             (CompositeType nm) "C" False) {
>                             envAttrDefs =
>                               (nm,TableComposite,UnnamedCompositeType attrs)
>                               : envAttrDefs env}
>         EnvCreateView nm attrs ->  do
>                 return $ (addTypeWithArray env nm
>                             (CompositeType nm) "C" False) {
>                             envAttrDefs =
>                               (nm,ViewComposite,UnnamedCompositeType attrs)
>                               : envAttrDefs env}
>         EnvCreateFunction f nm args ret ->
>             return $ case f of
>               FunPrefix -> env {envPrefixOperators=(nm,args,ret):envPrefixOperators env}
>               FunPostfix -> env {envPostfixOperators=(nm,args,ret):envPostfixOperators env}
>               FunBinary -> env {envBinaryOperators=(nm,args,ret):envBinaryOperators env}
>               FunAgg -> env {envAggregates=(nm,args,ret):envAggregates env}
>               FunName -> env {envFunctions=(nm,args,ret):envFunctions env}
>         EnvUpdateIDs qids jids ->
>            return $ env {envIdentifierTypes = qids
>                         ,envJoinIdentifiers = jids}
>     addTypeWithArray env nm ty cat pref =
>       env {envTypeNames =
>                ('_':nm,ArrayType ty)
>                : (nm,ty)
>                : envTypeNames env
>           ,envTypeCategories =
>                (ArrayType ty,"A",False)
>                : (ty,cat,pref)
>                : envTypeCategories env}


> -- | Takes part an 'Environment' value to produce a list of 'EnvironmentUpdate's.
> -- You can use this to look inside the Environment data type e.g. if you want to
> -- examine a catalog. It should be the case that:
> --
> -- @
> -- updateEnvironment emptyEnvironment (destructEnvironment env) = env
> -- @
> destructEnvironment :: Environment -> [EnvironmentUpdate]
> destructEnvironment = undefined

================================================================================

= type checking stuff

> envCompositeAttrs :: Environment -> [CompositeFlavour] -> String -> Either [TypeError] (CompositeDef,CompositeDef)
> envCompositeAttrs = undefined

> envTypeCategory :: Environment -> Type -> Either [TypeError] String
> envTypeCategory = undefined

> envCast :: Environment -> CastContext -> Type -> Type -> Either [TypeError] Bool
> envCast = undefined

> envDomainBaseType :: Environment -> Type -> Either [TypeError] Type
> envDomainBaseType env ty = do
>   --check type is domain, check it exists in main list
>   case lookup ty (envDomainDefs env) of
>       Nothing -> Left [DomainDefNotFound ty]
>       Just t -> Right t

> envGetAllFns :: Environment -> [FunctionPrototype]
> envGetAllFns env =
>   concat [envPrefixOperators env
>          ,envPostfixOperators env
>          ,envBinaryOperators env
>          ,envFunctions env
>          ,envAggregates env]

> envLookupFns :: Environment -> String -> [FunctionPrototype]
> envLookupFns env name =
>     filter (\(nm,_,_) -> nm == name) $ envGetAllFns env


= Attribute identifier scoping

The way this scoping works is we have a list of prefixes/namespaces,
which is generally the table/view name, or the alias given to it, and
then a list of identifiers (with no dots) and their types. When we
look up the type of an identifier, if it has an correlation name we
try to match that against a table name or alias in that list, if it is
not present or not unique then throw an error. Similarly with no
correlation name, we look at all the lists, if the id is not present
or not unique then throw an error.

scopeIdentifierTypes is for expanding *. If we want to access the
common attributes from one of the tables in a using or natural join,
this attribute can be qualified with either of the table names/
aliases. But when we expand the *, we only output these common fields
once, so keep a separate list of these fields used just for expanding
the star. The other twist is that these common fields appear first in
the resultant field list.

System columns: pg also has these - they have names and types like
other attributes, but are not included when expanding stars, so you
only get them when you explicitly ask for them. The main use is using
the oid system column which is heavily used as a target for foreign
key references in the pg catalog.

> envExpandStar :: Environment -> String -> Either [TypeError] [(String,Type)]
> envExpandStar env correlationName =
>     if correlationName == ""
>       then let allFields = concatMap (fst.snd) $ envIdentifierTypes env
>                (commonFields,uncommonFields) =
>                   partition (\(a,_) -> a `elem` envJoinIdentifiers env) allFields
>            in Right $ nub commonFields ++ uncommonFields
>       else
>           case lookup correlationName $ envIdentifierTypes env of
>             Nothing -> Left [UnrecognisedCorrelationName correlationName]
>             Just s -> Right $ fst s


> envLookupID :: Environment -> String -> String -> Either [TypeError] Type
> envLookupID env correlationName iden =
>   if correlationName == ""
>     then let types = concatMap (filter (\ (s, _) -> s == iden))
>                        (map (catPair.snd) $ envIdentifierTypes env)
>          in do
>            errorWhen (null types) [UnrecognisedIdentifier iden]
>            if length types == 1
>               then Right $ (snd . head) types
>               else do
>                 --see if this identifier is in the join list
>                 errorWhen (not $ iden `notElem` envJoinIdentifiers env)
>                           [AmbiguousIdentifier iden]
>                 return $ (snd . head) types
>     else case lookup correlationName (envIdentifierTypes env) of
>            Nothing -> Left [UnrecognisedCorrelationName correlationName]
>            Just s -> case lookup iden (catPair s) of
>                        Nothing -> Left [UnrecognisedIdentifier $ correlationName ++ "." ++ iden]
>                        Just t -> Right t

> catPair :: ([a],[a]) -> [a]
> catPair = uncurry (++)

================================================================================

= built in stuff

keyword operators, all of these are built in and don't appear in any
postgresql catalog

> keywordOperatorTypes :: [(String,[Type],Type)]
> keywordOperatorTypes = [
>   ("!and", [typeBool, typeBool], typeBool)
>  ,("!or", [typeBool, typeBool], typeBool)
>  ,("!like", [ScalarType "text", ScalarType "text"], typeBool)
>  ,("!not", [typeBool], typeBool)
>  ,("!isNull", [Pseudo AnyElement], typeBool)
>  ,("!isNotNull", [Pseudo AnyElement], typeBool)
>  ,("!arrayCtor", [Pseudo AnyElement], Pseudo AnyArray) -- not quite right,
>                                         -- needs variadic support
>                                         -- currently works via a special case
>                                         -- in the type checking code
>  ,("!between", [Pseudo AnyElement
>               ,Pseudo AnyElement
>               ,Pseudo AnyElement], Pseudo AnyElement)
>  ,("!substring", [ScalarType "text",typeInt,typeInt], ScalarType "text")
>  ,("!arraySub", [Pseudo AnyArray,typeInt], Pseudo AnyElement)
>  ]

these look like functions, but don't appear in the postgresql catalog.

> specialFunctionTypes :: [(String,[Type],Type)]
> specialFunctionTypes = [
>   ("coalesce", [Pseudo AnyElement],
>     Pseudo AnyElement) -- needs variadic support to be correct,
>                        -- uses special case in type checking
>                        -- currently
>  ,("nullif", [Pseudo AnyElement, Pseudo AnyElement], Pseudo AnyElement)
>  ,("greatest", [Pseudo AnyElement], Pseudo AnyElement) --variadic, blah
>  ,("least", [Pseudo AnyElement], Pseudo AnyElement) --also
>  ]

> pseudoTypes :: [Type]
> pseudoTypes = undefined
