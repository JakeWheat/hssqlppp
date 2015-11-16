
This file contains special definitions which are used by pretty much
every dialect.

This includes the catalog entries to support:
mixfix:
between and not between

keyword operators:
and, or, not, (not) like, is (not) null, overlaps
is (not) similar to
is (not) { true | false | unknown }
is (not) distinct from

in (list version)

almost-function like operators
position
substring
convert
translate
overlay
trim

coalesce and nullif also appear here

TODO: this isn't complete yet (we cannot even parse some of the above
currently)

todo: not sure how to handle:
array subscript and ctor
cast - very special case
extract - special case because one of the args is a typename not
  a scalar expr
quantified comparisons?
array and multiset ops
other subqueries

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
>              -- todo: rename this to "not like"
>              -- and other operators the same
>             ,CatCreateBinaryOp "notlike" t t bool
>              -- todo: overlaps needs a tuple?
>             --,CatCreateBinaryOp "overlaps" t t bool
>             --is (not) similar to
>              -- is (not) distinct from
>             ]
>           | t <- texts]
>    ++ -- these should go in a separate namespace, not functions
>    [CatCreateFunction "between" ["anyelement","anyelement","anyelement"] False bool
>    ,CatCreateFunction "notbetween" ["anyelement","anyelement","anyelement"] False bool
>    ]

todo: do some writeup on namespaces in the hssqlppp catalog code

> specialOps :: Text -> Text -> [Text] -> [CatalogUpdate]
> specialOps _bool int texts =
>        -- these should go in a separate namespace along with between
>        [CatCreateSpecialOp "substring" [t,int,int] False t
>         -- in (list version) can be treated as a variadic special function
>         -- extract ... takes a typename as one of the args
>         -- position
>         -- convert
>         -- translate
>         -- overlay
>         -- trim

>        | t <- texts]


> prefixOps :: Text -> [CatalogUpdate]
> prefixOps bool =
>    [CatCreatePrefixOp "not" bool bool]

> postfixOps :: Text -> [CatalogUpdate]
> postfixOps bool =
>    [CatCreatePostfixOp "isnull" "anyelement" bool
>    ,CatCreatePostfixOp "isnotnull" "anyelement" bool
>     -- is (not) { true | false | unknown }
>    ]

These appear here since basically every dialect has them the same. Not
sure if this is a good enough reason.

> functions :: [CatalogUpdate]
> functions =
>     [CatCreateVariadicFunction "coalesce" ["anyelement"] False "anyelement"
>     ,CatCreateFunction "nullif" ["anyelement","anyelement"] False "anyelement"
>     ]
