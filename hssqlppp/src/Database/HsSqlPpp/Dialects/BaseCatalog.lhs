
Currently this is the base catalog which contains the things which are
needed for typechecking but don't appear in the postgresql
catalog. This needs a rethink once there are a lot more tests.

Because this is currently used for postgresql and ansi, and these
dialects have different canonical names for some of the types, you
have to pass the names of these types in for your specific dialect.

> {-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Dialects.BaseCatalog
>     (baseCatalog
>     ,insertOperators
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

todo: get rid of this import by having the base catalog use the
regular catalogupdates instead of accessing the catalog internals

> import Database.HsSqlPpp.Internals.Catalog.CatalogBuilder (insertOperators)





>
> -- | Represents what you probably want to use as a starting point if
> -- you are building an catalog from scratch. It contains
> -- information on built in function like things such as keyword
> -- operators like \'and\', etc..
> baseCatalog :: Text -> Text -> [Text] -> Catalog
> baseCatalog boolTypeName intTypeName textTypeNames =
>     -- todo: specify in terms of catalog updates
>   emptyCatalog {catSchemas = S.fromList ["public"]
>                ,catBinaryOps = insertOperators
>                                (systemBinaryOps boolTypeName intTypeName textTypeNames)
>                                M.empty
>                ,catPrefixOps = insertOperators (systemPrefixOps boolTypeName) M.empty
>                ,catPostfixOps = insertOperators (systemPostfixOps boolTypeName) M.empty
>                ,catFunctions = insertOperators systemFunctions M.empty
>                ,catScalarTypeNames = rangeTypes}

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


built in range types in postgresql

todo: maybe these are in the catalog somewhere and should come from
postgres?

> rangeTypes :: S.Set CatName
> rangeTypes = S.fromList ["int4range", "int8range"
>                         ,"numrange","daterange"
>                         ,"tsrange","tstzrange"]

