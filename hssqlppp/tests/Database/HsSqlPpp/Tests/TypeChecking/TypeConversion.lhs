
> {-# LANGUAGE OverloadedStrings,LambdaCase #-}
> module Database.HsSqlPpp.Tests.TypeChecking.TypeConversion
>        (typeConversionTests) where

> import Data.Text ()
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.SqlDialect
> import Database.HsSqlPpp.Tests.TestTypes


tests:
numeric operators simulator
  choose each pair of number types with the plus operator
    and check the return type
  choose one type with unknown and plus op
  two unknowns and plus -> error

> typeConversionTests :: Item
> typeConversionTests = Group "typeConversionTests"
>     [binaryOperators]


> binaryOperators :: Item
> binaryOperators = Group "binaryOperators"
>     [{-MatchApp PostgreSQLDialect defaultTemplate1Catalog
>      [Nmc "+"] [(intNotNull,Nothing),(intNotNull, Nothing)]
>      (Right ([intNotNull,intNotNull],intNotNull))
>     ,MatchApp PostgreSQLDialect defaultTemplate1Catalog
>      [Nmc "+"] [(intNotNull,Nothing),(unk, Nothing)]
>      (Right ([intNotNull,intNotNull],intNotNull))
>     ,-}MatchApp PostgreSQLDialect defaultTemplate1Catalog
>      [Nmc "+"] [(unk,Nothing),(unk, Nothing)]
>      (Left [NoMatchingOperator "+" [UnknownType,UnknownType]])

different types with implicit casts

>     {-,MatchApp PostgreSQLDialect defaultTemplate1Catalog
>      [Nmc "+"] [(intNotNull,Nothing),(te typeNumeric, Nothing)]
>      (Right ([te typeNumeric,te typeNumeric],te typeNumeric))


nullability

>     ,MatchApp PostgreSQLDialect defaultTemplate1Catalog
>      [Nmc "+"] [(intNull,Nothing),(intNotNull, Nothing)]
>      (Right ([intNull,intNotNull],intNull))-}


precision and scale
not quite right

>     {-,MatchApp PostgreSQLDialect defaultTemplate1Catalog
>      [Nmc "+"] [(numeric,Nothing),(numeric, Nothing)]
>      (Right ([numeric,numeric],numeric))-}


>     ]
>   where
>     intNotNull = TypeExtra typeInt Nothing Nothing False
>     intNull = TypeExtra typeInt Nothing Nothing True
>     unk = TypeExtra UnknownType Nothing Nothing False
>     te s = TypeExtra s Nothing Nothing False
>     numeric = TypeExtra typeNumeric (Just 15) (Just 2) False

text conversions
  do something similar to numeric with pairs of string types
  and the || op
domains
  do a simple test for these
function style cast: test some success and fails
time types
  can do something similar to numeric ops?
binary strings
  don't have these right now

review each branch in the matchapp and fix it

precision tests
nullability tests

what are all the special cases currently:
special cases for precision
special cases for result null
in OldTypeConverion.findCallMatch:
   between, not between, greatest, least
   rowctor
   .
   comparisons for composite/set types
more stuff in TypeConversion.matchApp
  sql server date stuff
  decode
  something to do with datetimes?
  string precisions?:
    ||, substring, replace
  some nullability special cases?
  more stuff
SqlTypeConversions: special case for implicit casts from text types to
  numeric (see if can handle in rule system)
ScalarExprs.ag
  needs implicit cast
  implicit cast type
  check the types (e.g. cast syntax should use this typeconversion
   machinery)
  tcAppLike: more mssql date stuff

  getmaybeintsfromliterals: also suggests only need to support int
    literals in the matchapp function here

