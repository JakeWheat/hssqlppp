Copyright 2009 Jake Wheat

This module contains the implementation of the Environment data types
and functions, and provides the api for the other type checking
modules.

> {-# LANGUAGE DeriveDataTypeable #-}
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
>     ,FunFlav(..)
>     ,emptyEnvironment
>     ,defaultEnvironment
>     ,EnvironmentUpdate(..)
>     ,updateEnvironment
>     --,destructEnvironment
>     -- type checker stuff
>     ,envExpandStar
>     ,envLookupID
>     ,envCompositeAttrs
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
> import Debug.Trace
> import Data.Generics

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
>                    --,envAttrSystemColumns :: [CompositeDef]
>                    ,envIdentifierTypes :: [[QualifiedIDs]]
>                    ,envStarTypes :: [QualifiedIDs]}


> -- | Represents an empty environment. This doesn't contain things
> -- like the \'and\' operator, and so if you try to use it it will
> -- almost certainly not work.
> emptyEnvironment :: Environment
> emptyEnvironment = Environment [] [] [] [] [] [] [] [] [] [] [] []

> -- | Represents what you probably want to use as a starting point if
> -- you are building an environment from scratch. It contains
> -- information on built in function like things that aren't in the
> -- PostGreSQL catalog, such as greatest, coalesce, keyword operators
> -- like \'and\', etc..
> defaultEnvironment :: Environment
> defaultEnvironment = emptyEnvironment {
>                       envTypeNames = pseudoTypes
>                      ,envBinaryOperators = keywordOperatorTypes
>                      ,envFunctions = specialFunctionTypes}

> -- | Represents the types of the ids available, currently used for
> -- resolving identifiers inside select expressions. Will probably
> -- change as this is fixed to support more general contexts.  The
> -- components represent the qualifying name (empty string for no
> -- qualifying name), the list of identifier names with their types,
> -- and the list of system column identifier names with their types.
> type QualifiedIDs = (String, [(String,Type)])

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

> -- | Provides the definition of a composite type. The components are
> -- composite (or table or view) name, the flavour of the composite,
> -- the types of the composite attributes, and the types of the
> -- system columns iff the composite represents a table type (the
> -- third and fourth components are always 'UnnamedCompositeType's).
> type CompositeDef = (String, CompositeFlavour, Type, Type)

> -- | The components are: function (or operator) name, argument
> -- types, and return type.
> type FunctionPrototype = (String, [Type], Type)

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
>   | EnvCreateFunction FunFlav String [Type] Type
>     -- | to allow an unqualified identifier reference to work you need to
>     -- supply an extra entry with \"\" as the alias, and all the fields,
>     -- in the case of joins, these unaliased fields need to have the
>     -- duplicates removed and the types resolved
>   | EnvStackIDs [QualifiedIDs]
>     -- | to allow an unqualified star to work you need to
>     -- supply an extra entry with \"\" as the alias, and all the fields
>   | EnvSetStarExpansion [QualifiedIDs]
>     deriving (Eq,Show,Typeable,Data)

> data FunFlav = FunPrefix | FunPostfix | FunBinary
>              | FunName | FunAgg
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
>                 let cat = envTypeCategory env baseTy
>                 return $ (addTypeWithArray env nm ty cat False) {
>                                        envDomainDefs =
>                                          (ty,baseTy):envDomainDefs env
>                                        ,envCasts =
>                                          (ty,baseTy,ImplicitCastContext):envCasts env}
>                 where
>                   allowed = case ty of
>                                     DomainType _ -> True
>                                     _ -> False
>                   baseAllowed = case baseTy of
>                                   ScalarType _ -> True
>                                   _ -> False
>         EnvCreateComposite nm flds -> do
>                 return $ (addTypeWithArray env nm (CompositeType nm) "C" False) {
>                             envAttrDefs =
>                               (nm,Composite,UnnamedCompositeType flds, UnnamedCompositeType [])
>                               : envAttrDefs env}
>         EnvCreateCast src tgt ctx -> return $ env {envCasts = (src,tgt,ctx):envCasts env}
>         EnvCreateTable nm attrs sysAttrs -> do
>                 checkTypeDoesntExist env nm (CompositeType nm)
>                 return $ (addTypeWithArray env nm
>                             (CompositeType nm) "C" False) {
>                             envAttrDefs =
>                               (nm,TableComposite,UnnamedCompositeType attrs, UnnamedCompositeType sysAttrs)
>                               : envAttrDefs env}
>         EnvCreateView nm attrs -> {-trace ("create view:" ++ show nm) $-} do
>                 checkTypeDoesntExist env nm (CompositeType nm)
>                 return $ (addTypeWithArray env nm
>                             (CompositeType nm) "C" False) {
>                             envAttrDefs =
>                               (nm,ViewComposite,UnnamedCompositeType attrs, UnnamedCompositeType [])
>                               : envAttrDefs env}
>         EnvCreateFunction f nm args ret ->
>             return $ case f of
>               FunPrefix -> env {envPrefixOperators=(nm,args,ret):envPrefixOperators env}
>               FunPostfix -> env {envPostfixOperators=(nm,args,ret):envPostfixOperators env}
>               FunBinary -> env {envBinaryOperators=(nm,args,ret):envBinaryOperators env}
>               FunAgg -> env {envAggregates=(nm,args,ret):envAggregates env}
>               FunName -> env {envFunctions=(nm,args,ret):envFunctions env}
>         EnvStackIDs qids -> return $ env {envIdentifierTypes = qids:envIdentifierTypes env}
>         EnvSetStarExpansion sids -> return $ env {envStarTypes = sids}
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
>         errorWhen (any (==nm) $ map fst $ envTypeNames env) $
>             [TypeAlreadyExists ty]
>         errorWhen (any (==ty) $ map snd $ envTypeNames env) $
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

> envCompositeAttrs :: Environment -> [CompositeFlavour] -> Type -> Either [TypeError] (CompositeDef)
> envCompositeAttrs env flvs ty = do
>   let CompositeType nm = ty
>   let c = filter (\(n,t,_,_) -> n == nm && (null flvs || t `elem` flvs)) $ envAttrDefs env
>   errorWhen (length c == 0)
>             [UnrecognisedRelation nm]
>   case c of
>     (_,fl1,r,s):[] -> return (nm,fl1,r,s)
>     _ -> error $ "problem getting attributes for: " ++ show ty ++ ", " ++ show c

> envTypeCategory :: Environment -> Type -> String
> envTypeCategory env ty =
>   let (c,_) = envGetCategoryInfo env ty
>   in c

> envPreferredType :: Environment -> Type -> Bool
> envPreferredType env ty =
>   let (_,p) = envGetCategoryInfo env ty
>   in p

> envCast :: Environment -> CastContext -> Type -> Type -> Bool
> envCast env ctx from to = any (==(from,to,ctx)) (envCasts env)


> envDomainBaseType :: Environment -> Type -> Type
> envDomainBaseType env ty = do
>   --check type is domain, check it exists in main list
>   case lookup ty (envDomainDefs env) of
>       Nothing -> error "domain not found" -- Left [DomainDefNotFound ty]
>       Just t -> t


> envLookupFns :: Environment -> String -> [FunctionPrototype]
> envLookupFns env name =
>     filter (\(nm,_,_) -> nm == name) envGetAllFns
>     where
>     envGetAllFns =
>         concat [envPrefixOperators env
>                ,envPostfixOperators env
>                ,envBinaryOperators env
>                ,envFunctions env
>                ,envAggregates env]

== internal support for type checker fns above

> envGetCategoryInfo :: Environment -> Type -> (String, Bool)
> envGetCategoryInfo env ty =
>   case ty of
>     ArrayType (Pseudo _) -> ("A",False)
>     Pseudo _ -> ("P",False)
>     _ -> let l = filter (\(t,_,_) -> ty == t) $ envTypeCategories env
>          in if null l
>               then error $ "no type category for " ++ show ty
>               else let (_,c,p):_ =l
>                    in (c,p)



= Attribute identifier scoping

The way this scoping works is we have a list of prefixes/namespaces,
which is generally the table/view name, or the alias given to it, and
then a list of identifiers (with no dots) and their types. When we
look up the type of an identifier, if it has an correlation name we
try to match that against a table name or alias in that list, if it is
not present or not unique then throw an error. Similarly with no
correlation name, we look at all the lists, if the id is not present
or not unique then throw an error.

envIdentifierTypes is for expanding *. If we want to access the
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

This system still isn't working right. Subqueries are a
problem. Aspects which don't work right now are:

consider this query:
select relname as relvar_name
    from pg_class
    where ((relnamespace =
           (select oid
              from pg_namespace
              where (nspname = 'public'))) and (relkind = 'r'));

we need to be able to access attributes from pg_class inside the subquery,
but 1) they aren't inserted if you use * in the inner query
2) they can't make an identifier ambiguous, so the oid here in the subquery
is ok even though both the oid from pg_namespace and the oid from pg_class
are in scope.

So there are two problems with the current code:
it's too aggressive at throwing ambiguous identifier errors
it pulls in too many identifiers when expanding star

Solution ideas:
for the ambiguous errors, create a stack of identifiers, then split
the EnvUpdateIDs into two, one to replace the current set, and one to
push a new set on the stack. Then fix the lookup to walk the stack level by level.

for the *, we already have special cases for system columns, and for
join ids. I think the best solution is to provide a separate list of *
columns and types, with a separate env update ctor, and get the type
checker to resolve the list for * expansion rather than doing it here.

This should also handle parameters and variable declarations in plpgsql
functions too, these stack in the same way, with one complication to
do with parameters:

there is an additional complication with plpgsql, which isn't going to
be handled for now: instead of stacking like everything else, for
variable references inside select, insert, update and delete
statements only, which aren't qualified and match a parameter name,
then the parameter is used in lieu of variable declarations or
attributes inside a select expression. This will be handled at some
point.

this is something the lint checker should flag when it's written, it
will also flag any ambiguous identifiers which resolve ok only because
of stacking, this is a standard warning in many flavours of lint
checkers.

One last thing is that we need to make sure identifiers availability doesn't
get inherited too far: e.g. a create function inside a create function
can't access ids from the outer create function. This is pretty easy:
the following things generate identifier bindings:
select expressions, inside the expression
parameter defs
variable defs

since select expressions can't contain statements, we don't need to
worry about e.g. if statements, they want to inherit ids from params
and variable defs, so the default is good.

For environments being updated sequentially: since the environment is
updated in a statement list (i.e. environment updates stack from one
statement to the next within a single statement list), any var defs
can't break out of the containing list, so we are covered e.g. for a
variable def leaking from an inner block to an outer block.

With ids going into select expressions: we want the default which is
parameters, vardefs and ids from containing select expressions to be
inherited. So, in the end the only case to deal with is a create
function inside another create function. This isn't dealt with at the
moment.



> envExpandStar :: Environment -> String -> Either [TypeError] [(String,Type)]
> envExpandStar env correlationName =
>     case lookup correlationName $ envStarTypes env of
>       Nothing -> errorWhen (correlationName == "")
>                            [InternalError "no star expansion found?"] >>
>                  Left [UnrecognisedCorrelationName correlationName]
>       Just l -> Right l

> envLookupID :: Environment -> String -> String -> Either [TypeError] Type
> envLookupID env correlationName iden = {-trace ("lookup " ++ show iden ++ " in " ++ show (envIdentifierTypes env)) $ -}
>   envLookupID' $ envIdentifierTypes env
>   where
>     envLookupID' (its:itss) =
>       case lookup correlationName its of
>         Nothing -> errorWhen (correlationName == "")
>                              [UnrecognisedIdentifier iden] >>
>                    Left [UnrecognisedCorrelationName correlationName]
>         Just s -> case filter (\(n,_) -> n==iden) s of
>                     [] -> envLookupID' itss
>                     (_,t):[] -> Right t
>                     _ -> Left [AmbiguousIdentifier iden]
>     envLookupID' [] = Left [UnrecognisedIdentifier $ if correlationName == "" then iden else correlationName ++ "." ++ iden]

> envTypeExists :: Environment -> Type -> Either [TypeError] Type
> envTypeExists env t =
>     errorWhen (t `notElem` (map snd $ envTypeNames env))
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
>                         | any (\(x,_,_) -> x == s) (envBinaryOperators env) ->
>                             BinaryOp
>                         | any (\(x,_,_) -> x == s ||
>                                            (x=="-" && s=="u-"))
>                               (envPrefixOperators env) ->
>                             PrefixOp
>                         | any (\(x,_,_) -> x == s) (envPostfixOperators env) ->
>                             PostfixOp
>                         | otherwise ->
>                             error $ "don't know flavour of operator " ++ s

> isOperatorName :: String -> Bool
> isOperatorName = any (`elem` "+-*/<>=~!@#%^&|`?")
