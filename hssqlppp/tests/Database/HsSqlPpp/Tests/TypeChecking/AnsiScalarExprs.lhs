

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.AnsiScalarExprs
>     (ansiScalarExprs) where

> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Tests.TestTypes
> import Database.HsSqlPpp.Internals.TypeChecking.Environment
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> ansiScalarExprs :: Item
> ansiScalarExprs =
>   Group "ansiScalarExprs"
>   [Group "sanity"
>    [
>      -- todo: these should be unknown
>     se "1" $ Right $ ScalarType "int4" -- Right UnknownType
>    ,se "1.1" $ Right $ ScalarType "numeric" -- Right UnknownType
>    ,se "'test'" $ Right UnknownType
>    ,se "null" $ Right UnknownType
>    -- todo: the canonical name in ansi for bool is boolean
>    ,se "true" $ Right $ ScalarType "bool"
>    ,se "false" $ Right $ ScalarType "bool"

typedstringlit

>    -- todo: int2 issue
>    -- ,se "int '3'" $ Right $ ScalarType "int4" -- todo: should be int

interval

>    ,se "interval '3' day" $ Right $ ScalarType "interval"

identifier

>    ,see [("a", mkTypeExtra $ ScalarType "int")]
>         "a" $ Right $ ScalarType "int4"

star: not here
qstar: not here

positional arg: not supported in this dialect
placeholder,hostparameter

>    ,se "?" $ Right UnknownType

>    -- not sure how this should work
>    -- todo: not currently supported in the parser
>    -- ,se ":test" $ Right UnknownType


cast


>   -- todo: int2 issue
>    --,see [("a", mkTypeExtra $ ScalarType "float")]
>    --     "cast(a as real)" $ Right $ ScalarType "real"

implicit cast: not here

case x 2

>   -- todo: should be int, not int4
>   ,see [("a", mkTypeExtra $ ScalarType "int")
>        ,("b", mkTypeExtra $ ScalarType "int")
>        ,("c", mkTypeExtra $ ScalarType "int")]
>    "case a when 1 then b else c end" $ Right $ ScalarType "int4"

>   ,see [("a", mkTypeExtra $ ScalarType "int")
>        ,("b", mkTypeExtra $ ScalarType "int")
>        ,("c", mkTypeExtra $ ScalarType "int")]
>    "case when a=1 then b else c end" $ Right $ ScalarType "int4"


parens

>    -- todo: should be int
>    ,see [("a", mkTypeExtra $ ScalarType "int")]
>         "(a)" $ Right $ ScalarType "int4"


in predicate: just the list version here
  query version tested elsewhere


>    -- todo: should be boolean
>    ,see [("a", mkTypeExtra $ ScalarType "int")]
>     "a in (1,2,3)" $ Right $ ScalarType "bool"


subqueries: test these elsewhere
quantified comparison: test these elsewhere
match: test this elsewhere

array select
array ctor

collate

multiset ops
next value for


in their own sections, covers all variations without casts:
prefix, postfix, binary
specialops
special functions

functions

aggs

windows



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


>    ]



>   ]
>   where
>     se = TCScalExpr ansiCatalog emptyEnvironment
>                     defaultTypeCheckFlags {tcfDialect=ANSI}
>     see env = TCScalExpr ansiCatalog (selListEnv env)
>               defaultTypeCheckFlags {tcfDialect=ANSI}
>     selListEnv env = either (const brokeEnvironment) id
>                      $ envSelectListEnvironment env
