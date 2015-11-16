
Currently this is the base catalog which contains the things which are
needed for typechecking but don't appear in the postgresql
catalog. This needs a rethink once there are a lot more tests.

Because this is currently used for postgresql and ansi, and these
dialects have different canonical names for some of the types, you
have to pass the names of these types in for your specific dialect.

TODO:
I think = should not be here, it should be in the dialects
arrayctor and arraysub should be implemented differently, they
  shouldn't appear in the catalog

think about where coalesce, nullif, greatest and least should
appear. Maybe they should be in the regular dialects since they work
and look just like normal functions.

each one of the non standard syntaxes (including keyword operators)
should be enabled/disabled in the dialect, which will handle adding
the appropriate stuff to the catalog, and enable the parse to reject
unsupported syntax at the same time

todo:

create a dialect description datatype which has entries for
each of the special syntaxes

then create a helper function which adds the enabled syntax to a
catalog. This helper function will replace the BaseCatalog.

then use these flags in the parser also
at the same time can replace the syntaxflavour with syntax flags
for each thing which is currently different



notes:

= built in keyword binary operators (regular)

and
or
not
(not) like
(not) rlike
is (not) null
overlaps
is (not) similar to
is (not) { true | false | unknown }
is (not) distinct from

= special operators

array subscript
(not) in
(not) between
substring
cast

coalesce?
nullif?

extract
position
substring
convert
translate
overlay
trim

quantified comparisons

exists
unique
array
multiset

> {-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Dialects.BaseCatalog
>     (baseCatalog
>     --,insertOperators
>      ) where
> import Data.Text (Text)

> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes

> baseCatalog :: Text -> Text -> [Text] -> [CatalogUpdate]
> baseCatalog boolTypeName intTypeName textTypeNames =
>     CatCreateSchema "public"
>     : (binaryOps boolTypeName intTypeName textTypeNames
>        ++ prefixOps boolTypeName
>        ++ postfixOps boolTypeName
>        ++ functions
>        ++ specialOps boolTypeName intTypeName textTypeNames)


> binaryOps :: Text -> Text -> [Text] -> [CatalogUpdate]
> binaryOps bool _int texts =
>    [CatCreateBinaryOp "and" bool bool bool
>    ,CatCreateBinaryOp "or" bool bool bool
>    ] ++
>    concat [ [CatCreateBinaryOp "like" t t bool
>             ,CatCreateBinaryOp "notlike" t t bool]
>           | t <- texts]
>    ++
>    [CatCreateFunction "between" ["anyelement","anyelement","anyelement"] False bool
>    ,CatCreateFunction "notbetween" ["anyelement","anyelement","anyelement"] False bool
>    ]

todo: put these in a separate namespace in catalog

make sure all the special ops appear in this namespace and not
somewhere else (like binary ops or functions)

> specialOps :: Text -> Text -> [Text] -> [CatalogUpdate]
> specialOps _bool int texts =
>        [CatCreateSpecialOp "substring" [t,int,int] False t
>        | t <- texts]


> prefixOps :: Text -> [CatalogUpdate]
> prefixOps bool =
>    [CatCreatePrefixOp "not" bool bool]

> postfixOps :: Text -> [CatalogUpdate]
> postfixOps bool =
>    [CatCreatePostfixOp "isnull" "anyelement" bool
>    ,CatCreatePostfixOp "isnotnull" "anyelement" bool]

These appear here since basically every dialect has them the same. Not
sure if this is a good enough reason.

> functions :: [CatalogUpdate]
> functions =
>     [CatCreateVariadicFunction "coalesce" ["anyelement"] False "anyelement"
>     ,CatCreateFunction "nullif" ["anyelement","anyelement"] False "anyelement"
>     ]


