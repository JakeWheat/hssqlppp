Copyright 2009 Jake Wheat

This module contains the implementation of the Environment data types and functions, and provides the api for the other type checking modules

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
>     ,envLookupFn
>      --temporary exports, will become internal
>     ,keywordOperatorTypes
>     ,specialFunctionTypes
>     ) where

> import Database.HsSqlPpp.TypeChecking.TypeType


> -- | The main datatype, this holds the catalog and context
> -- information to type check against.
> data Environment = Env String

> -- | Represents an empty environment. This doesn't contain things
> -- like the \'and\' operator, and so if you try to use it it will
> -- almost certainly not work.
> emptyEnvironment :: Environment
> emptyEnvironment = Env ""

> -- | Represents what you probably want to use as a starting point if
> -- you are building an environment from scratch. It contains
> -- information on built in function like things that aren't in the
> -- PostGreSQL catalog, such as greatest, coalesce, keyword operators
> -- like \'and\', etc..
> defaultEnvironment :: Environment
> defaultEnvironment = Env ""

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
> -- and the type of the composite (the third component is always an
> -- 'UnnamedCompositeType').
> type CompositeDef = (String, CompositeFlavour, Type)

> -- | The components are: function (or operator) name, argument
> -- types, and return type.
> type FunctionPrototype = (String, [Type], Type)

> -- | The components are domain type, base type (todo: add check
> -- constraint).
> type DomainDefinition = (Type,Type)

> data EnvironmentUpdate =
>       EnvUpdateIDs
>         [QualifiedIDs] -- ids, system ids, with qualifier names
>         [String] -- join ids
>     | Ev String


> -- | Applies a list of 'EnvironmentUpdate's to an 'Environment' value
> -- to produce a new Environment value.
> updateEnvironment :: Environment -> [EnvironmentUpdate] -> Environment
> updateEnvironment = undefined

> -- | Takes part an 'Environment' value to produce a list of 'EnvironmentUpdate's.
> -- You can use this to look inside the Environment data type e.g. if you want to
> -- examine a catalog. It should be the case that:
> --
> -- @
> -- updateEnvironment emptyEnvironment (destructEnvironment env) = env
> -- @
> destructEnvironment :: Environment -> [EnvironmentUpdate]
> destructEnvironment = undefined

> envExpandStar :: Environment -> String -> Either [TypeError] [(String,Type)]
> envExpandStar = undefined

> envLookupID :: Environment -> String -> String -> Either [TypeError] Type
> envLookupID = undefined

> envCompositeAttrs :: Environment -> [CompositeFlavour] -> String -> Either [TypeError] (CompositeDef,CompositeDef)
> envCompositeAttrs = undefined

> envTypeCategory :: Environment -> Type -> Either [TypeError] String
> envTypeCategory = undefined

> envCast :: Environment -> CastContext -> Type -> Type -> Either [TypeError] Bool
> envCast = undefined

> envDomainBaseType :: Environment -> Type -> Either [TypeError] Type
> envDomainBaseType = undefined

> envLookupFn :: Environment -> String -> [FunctionPrototype]
> envLookupFn = undefined



================================================================================

types for the keyword operators, for use in findCallMatch. Not sure
where these should live but probably not here.

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

special functions, stuck in here at random also, these look like
functions, but don't appear in the postgresql catalog.

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

