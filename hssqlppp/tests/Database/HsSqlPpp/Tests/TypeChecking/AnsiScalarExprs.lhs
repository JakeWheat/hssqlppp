

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.AnsiScalarExprs
>     (ansiScalarExprs) where

> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Tests.TestTypes
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Dialect
> --import Database.HsSqlPpp.Internals.TypeChecking.Environment
> --import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import qualified Data.Text.Lazy as T

> ansiScalarExprs :: Item
> ansiScalarExprs =
>   Group "ansiScalarExprs"
>   [Group "sanity"
>    [
>      -- todo: these should be unknown
>     se "1" $ Right $ ScalarType "int" -- Right UnknownType
>    ,se "1.1" $ Right $ ScalarType "numeric" -- Right UnknownType
>    ,se "'test'" $ Right UnknownType
>    ,se "null" $ Right UnknownType
>    -- todo: the canonical name in ansi for bool is boolean
>    ,se "true" $ Right $ ScalarType "boolean"
>    ,se "false" $ Right $ ScalarType "boolean"

>    ,se "null" $ Right UnknownType


typedstringlit

>    -- todo: int2 issue
>    ,se "int '3'" $ Right $ ScalarType "int" -- todo: should be int

interval

>    ,se "interval '3' day" $ Right $ ScalarType "interval"

identifier

>    ,see [("a", mkTypeExtra $ ScalarType "int")]
>         "a" $ Right $ ScalarType "int"

star: not here
qstar: not here

positional arg: not supported in this dialect
placeholder,hostparameter

>    ,se "?" $ Right UnknownType

>    -- not sure how this should work
>    -- todo: not currently supported in the parser
>    --,se ":test" $ Right UnknownType


cast


>   -- todo: int2 issue
>   ,see [("a", mkTypeExtra $ ScalarType "float")]
>        "cast(a as real)" $ Right $ ScalarType "real"

implicit cast: not here

case x 2

>   -- todo: should be int, not int4
>   ,see [("a", mkTypeExtra $ ScalarType "int")
>        ,("b", mkTypeExtra $ ScalarType "int")
>        ,("c", mkTypeExtra $ ScalarType "int")]
>    "case a when 1 then b else c end" $ Right $ ScalarType "int"

>   ,see [("a", mkTypeExtra $ ScalarType "int")
>        ,("b", mkTypeExtra $ ScalarType "int")
>        ,("c", mkTypeExtra $ ScalarType "int")]
>    "case when a=1 then b else c end" $ Right $ ScalarType "int"


parens

>    -- todo: should be int
>    ,see [("a", mkTypeExtra $ ScalarType "int")]
>         "(a)" $ Right $ ScalarType "int"


in predicate: just the list version here
  query version tested elsewhere


>    -- todo: should be boolean
>    ,see [("a", mkTypeExtra $ ScalarType "int")]
>     "a in (1,2,3)" $ Right $ ScalarType "boolean"

>    ]

subqueries: test these elsewhere
quantified comparison: test these elsewhere
match: test this elsewhere

TODO: arrays
array select
array ctor

TODO: collate, needs some typechecking on collations, which needs more
catalog stuff

todo: multiset ops
todo: next value for - needs sequences in catalog

>
>

6.9 grouping

6.10 window function stuff

6.11 nested window?

6.12 case

todo: these should be 'polymorphic' and work with non built in types
also

>    ,Group "case abbreviation simple" $
>     [see [("a", mkTypeExtra $ ScalarType t)
>          ,("b", mkTypeExtra $ ScalarType t)]
>      "nullif(a,b)"
>      $ Right $ ScalarType t
>     | t <- allTypes]
>     ++
>     [see [("a", mkTypeExtra $ ScalarType t)
>          ,("b", mkTypeExtra $ ScalarType t)
>          ,("c", mkTypeExtra $ ScalarType t)]
>      (T.pack $ "coalesce(" ++ args ++ ")") $ Right $ ScalarType t
>      | args <- ["a","a,b","a,b,c"]
>      , t <- allTypes ]


todo: normal cases

6.13 cast

6.14 next value for

6.15 field reference

6.24 array element reference

6.25 multiset element reference

6.27 numeric value expression


>    ,Group "numeric value expression simple"
>     [see [("a", mkTypeExtra $ ScalarType t)
>          ,("b", mkTypeExtra $ ScalarType t)]
>      e $ Right $ ScalarType t
>      | e <- ["+a","-a","a+b","a-b","a*b","a/b"]
>      , t <- numericTypes ]

6.28 numeric value function

todo: position

>    {-,Group "numeric value function simple" $
>    [see [("a", mkTypeExtra $ ScalarType t)]
>      e $ Right $ ScalarType "int"
>      | e <- ["char_length(a)", "character_length(a)"
>             ,"octet_length(a)"]
>      , t <- textTypes ] -}

todo: char_length with using

>    {- ++
>    [see [("a", mkTypeExtra $ ScalarType t)]
>      (T.pack $ "extract(" ++ f ++ " from a)")
>       $ Right $ ScalarType "int"
>      | f <- ["TIMEZONE_HOUR","TIMEZONE_MINUTE"
>             ,"SECOND"
>             ,"YEAR"
>             ,"MONTH"
>             ,"DAY"
>             ,"HOUR"
>             ,"MINUTE"]
>      , t <- datetimeTypes ] -}

specialops

special functions

functions

aggs

windows

typealias checking

implicit casting:
case
greatest,least
ops, fns, etc.
in list
how to test these well enough?

anomalies
  bad casts
  interval?
  type match fails in implicit castings





>   ]
>   where
>     se = TCScalExpr (diDefaultCatalog ansiDialect) emptyEnvironment
>                     defaultTypeCheckFlags {tcfDialect=ansiDialect}
>     see env = TCScalExpr (diDefaultCatalog ansiDialect) (selListEnv env)
>               defaultTypeCheckFlags {tcfDialect=ansiDialect}
>     selListEnv env = either (const brokeEnvironment) id
>                      $ envSelectListEnvironment env
>     numericTypes = ["numeric", "decimal", "smallint", "int", "bigint", "float", "real"]
>     textTypes = ["char","varchar","clob","nchar","nvarchar","nclob"]
>     binaryTypes = ["binary","varbinary","blob"]
>     datetimeTypes = ["date","time","timestamp","interval"]
>     allTypes = numericTypes
>                ++ textTypes
>                ++ binaryTypes
>                ++ ["boolean"]
>                ++ datetimeTypes
