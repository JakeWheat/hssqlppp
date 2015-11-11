
Currently this is the base catalog which contains the things which are
needed for typechecking but don't appear in the postgresql
catalog. This needs a rethink once there are a lot more tests.

Because this is currently used for postgresql and ansi, and these
dialects have different canonical names for some of the types, you
have to pass the names of these types in for your specific dialect.

> {-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Internals.Catalog.BaseCatalog
>     (defaultCatalog
>     ,insertOperators
>     ,pseudoTypes
>      ) where
>

> --import Control.Monad
> --import Data.List
> --import Data.Data
> --import Data.Char
> --import Data.Maybe

> import qualified Data.Map as M
> import qualified Data.Set as S
> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Utils.Utils
> import Data.Text (Text)
> --import qualified Data.Text as T
> --import qualified Data.Text.Lazy as LT

> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes





>
> -- | Represents what you probably want to use as a starting point if
> -- you are building an catalog from scratch. It contains
> -- information on built in function like things such as keyword
> -- operators like \'and\', etc..
> defaultCatalog :: Text -> Text -> [Text] -> Catalog
> defaultCatalog boolTypeName intTypeName textTypeNames =
>     -- todo: specify in terms of catalog updates
>   emptyCatalog {catSchemas = S.fromList ["public"]
>                ,catBinaryOps = insertOperators
>                                (systemBinaryOps boolTypeName intTypeName textTypeNames)
>                                M.empty
>                ,catPrefixOps = insertOperators (systemPrefixOps boolTypeName) M.empty
>                ,catPostfixOps = insertOperators (systemPostfixOps boolTypeName) M.empty
>                ,catFunctions = insertOperators systemFunctions M.empty
>                ,catScalarTypeNames = rangeTypes}

> insertOperators :: [(CatName,OperatorPrototype)]
>                 -> M.Map CatName [OperatorPrototype]
>                 -> M.Map CatName [OperatorPrototype]
> insertOperators vs m =
>   foldr i m vs
>   where
>     i (k,v) = M.insertWith (++) k [v]

-------------------------------------------------------------

'system' stuff

bunch of operators which you can use but don't appear in the
postgresql catalog

boolTypeName
textTypeNames
intTypeName

> systemBinaryOps :: Text -> Text -> [Text] -> [(CatName,OperatorPrototype)]
> systemBinaryOps boolTypeName intTypeName textTypeNames =
>    [("=", ("=",[Pseudo AnyElement, Pseudo AnyElement], ScalarType boolTypeName, False))
>    ,("and",("and", [ScalarType boolTypeName, ScalarType boolTypeName], ScalarType boolTypeName, False))
>    ,("or",("or", [ScalarType boolTypeName, ScalarType boolTypeName], ScalarType boolTypeName, False))
>    ] ++ concat
>    [[("like",("like", [ScalarType t,ScalarType t], ScalarType boolTypeName, False))
>     ,("notlike",("notlike", [ScalarType t,ScalarType t], ScalarType boolTypeName, False))
>     ,("rlike",("rlike", [ScalarType t,ScalarType t], ScalarType boolTypeName, False))
>     ,("substring",("substring",[ScalarType t,ScalarType intTypeName,ScalarType intTypeName],ScalarType t,False))]
>     | t <- textTypeNames]
>    ++
>    [("arrayctor",("arrayctor", [ArrayType $ Pseudo AnyElement], Pseudo AnyArray, True))
>    ,("between",("between", [Pseudo AnyElement
>                            ,Pseudo AnyElement
>                            ,Pseudo AnyElement], ScalarType boolTypeName, False))
>    ,("notbetween",("mptbetween", [Pseudo AnyElement
>                                  ,Pseudo AnyElement
>                                  ,Pseudo AnyElement], ScalarType boolTypeName, False))
>    ,("arraysub",("arraysub", [Pseudo AnyArray,ScalarType intTypeName], Pseudo AnyElement, False))
>    ]

> systemPrefixOps :: Text -> [(CatName,OperatorPrototype)]
> systemPrefixOps boolTypeName =
>    [("not",("not", [ScalarType boolTypeName], ScalarType boolTypeName, False))]

> systemPostfixOps :: Text -> [(CatName,OperatorPrototype)]
> systemPostfixOps boolTypeName =
>    [("isnull",("isnull", [Pseudo AnyElement], ScalarType boolTypeName, False))
>    ,("isnotnull",("isnotnull", [Pseudo AnyElement], ScalarType boolTypeName, False))]

> systemFunctions :: [(CatName, OperatorPrototype)]
> systemFunctions =
>  [("coalesce",("coalesce", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement, True))
>  ,("nullif", ("nullif",[Pseudo AnyElement, Pseudo AnyElement], Pseudo AnyElement,False))
>  ,("greatest",("greatest", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement,True))
>  ,("least",("least", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement,True))
>  ]

names to refer to the pseudo types. This is very postgresql
specific. Some of these should be deleted since hssqlppp has nothing
to do with them. Some of them will become postgresql dialect specific
and not appear here, and some we will repurpose to implement features
for non-postgresql dialects as well.

> pseudoTypes :: M.Map CatName Type
> pseudoTypes = M.fromList
>     [("any",Pseudo Any)
>     ,("anyarray",Pseudo AnyArray)
>     ,("anyelement",Pseudo AnyElement)
>     ,("anyenum",Pseudo AnyEnum)
>     ,("anyrange",Pseudo AnyRange)
>     ,("anynonarray",Pseudo AnyNonArray)
>     ,("cstring",Pseudo Cstring)
>     ,("record",Pseudo (Record Nothing))
>     ,("trigger",Pseudo Trigger)
>      -- todo: fix this?
>     ,("event_trigger",Pseudo Trigger)
>     ,("void",Pseudo Void)
>     ,("_cstring",ArrayType $ Pseudo Cstring)
>     ,("_record",ArrayType $ Pseudo (Record Nothing))
>     ,("internal",Pseudo Internal)
>     ,("language_handler", Pseudo LanguageHandler)
>     ,("opaque", Pseudo Opaque)
>     ,("fdw_handler", Pseudo FdwHandler)
>     ]

built in range types in postgresql

todo: maybe these are in the catalog somewhere and should come from
postgres?

> rangeTypes :: S.Set CatName
> rangeTypes = S.fromList ["int4range", "int8range"
>                         ,"numrange","daterange"
>                         ,"tsrange","tstzrange"]

