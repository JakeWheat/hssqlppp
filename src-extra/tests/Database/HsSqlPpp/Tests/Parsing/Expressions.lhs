
> {-# LANGUAGE QuasiQuotes #-}
>
> module Database.HsSqlPpp.Tests.Parsing.Expressions (expressionParsingTestData) where
>
> import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> expressionParsingTestData:: Item
> expressionParsingTestData =
>    Group "parse expressions" [
>     Group "numbers" [
>       e "42" (NumberLit ea "42")
>      ,e "3.5" (NumberLit ea "3.5")
>      ,e "4." (NumberLit ea "4.")
>      ,e ".001" (NumberLit ea ".001")
>      ,e "5e2" (NumberLit ea "5e2")
>      ,e "1.925e-3" (NumberLit ea "1.925e-3")

>      ]
>    ,Group "basic expressions" [
>       e "1" (NumberLit ea "1")
>      ,e "-1" (FunCall ea "u-" [NumberLit ea "1"])
>      ,e "1.1" (NumberLit ea "1.1")
>      ,e "-1.1" (FunCall ea "u-" [NumberLit ea "1.1"])
>      ,e " 1 + 1 " (FunCall ea "+" [NumberLit ea "1"
>                                   ,NumberLit ea "1"])
>      ,e "1+1+1" (FunCall ea "+" [FunCall ea "+" [NumberLit ea "1"
>                                                 ,NumberLit ea "1"]
>                                 ,NumberLit ea "1"])
>      ]
>    ,Group "parens" [

check some basic parens use wrt naked values and row constructors
these tests reflect how pg seems to interpret the variants.

>       e "(1)" (NumberLit ea "1")
>      ,e "row ()" (FunCall ea "!rowctor" [])
>      ,e "row (1)" (FunCall ea "!rowctor" [NumberLit ea "1"])
>      ,e "row (1,2)" (FunCall ea "!rowctor" [NumberLit ea "1",NumberLit ea "2"])
>      ,e "(1,2)" (FunCall ea "!rowctor" [NumberLit ea "1",NumberLit ea "2"])
>      ]
>    ,Group "more basic expressions" [

test some more really basic expressions

>       e "'test'" (stringQ "test")
>      ,e "''" (stringQ "")
>      ,e "hello" (Identifier ea "hello")
>      ,e "helloTest" (Identifier ea "helloTest")
>      ,e "hello_test" (Identifier ea "hello_test")
>      ,e "\"this is an identifier\"" (Identifier ea "this is an identifier")
>      ,e "hello1234" (Identifier ea "hello1234")
>      ,e "true" (BooleanLit ea True)
>      ,e "false" (BooleanLit ea False)
>      ,e "null" (NullLit ea)
>      ]
>    ,Group "array ctor and selector" [
>       e "array[1,2]" (FunCall ea "!arrayctor" [NumberLit ea "1", NumberLit ea "2"])
>      ,e "a[1]" (FunCall ea "!arraysub" [Identifier ea "a", NumberLit ea "1"])
>      ]
>    ,Group "simple operators" [
>       e "1 + tst1" (FunCall ea "+" [NumberLit ea "1"
>                                ,Identifier ea "tst1"])
>      ,e "tst1 + 1" (FunCall ea "+" [Identifier ea "tst1"
>                                ,NumberLit ea "1"])
>      ,e "tst + tst1" (FunCall ea "+" [Identifier ea "tst"
>                                  ,Identifier ea "tst1"])
>      ,e "'a' || 'b'" (FunCall ea "||" [stringQ "a"
>                                   ,stringQ "b"])
>      ,e "'stuff'::text" (Cast ea (stringQ "stuff") (SimpleTypeName ea "text"))
>      ,e "245::float(24)" (Cast ea (NumberLit ea "245") (PrecTypeName ea "float" 24))
>      ,e "245.1::numeric(5,3)" (Cast ea (NumberLit ea "245.1") (Prec2TypeName ea "numeric" 5 3))
>      ,e "245::double precision" (Cast ea (NumberLit ea "245") (SimpleTypeName ea "double precision"))
>      ,e "date '1998-12-01'" (TypedStringLit ea (SimpleTypeName ea "date") "1998-12-01")
>      ,e "interval '63' day" (Interval ea "63" IntervalDay Nothing)
>      ,e "interval '63' day (3)" (Interval ea "63" IntervalDay $ Just 3)
>      ,e "extract(year from a)" (Extract ea ExtractYear $ Identifier ea "a")
>      ,e "a between 1 and 3"
>         (FunCall ea "!between" [Identifier ea "a", NumberLit ea "1", NumberLit ea "3"])
>      ,e "a between 7 - 1 and 7 + 1"
>         (FunCall ea "!between" [Identifier ea "a"
>                                ,FunCall ea "-" [NumberLit ea "7"
>                                                ,NumberLit ea "1"]
>                                ,FunCall ea "+" [NumberLit ea "7"
>                                                ,NumberLit ea "1"]])
>      ,e "cast(a as text)"
>         (Cast ea (Identifier ea "a") (SimpleTypeName ea "text"))
>      ,e "@ a"
>         (FunCall ea "@" [Identifier ea "a"])
>      ,e "substring(a from 0 for 3)"
>         (FunCall ea "!substring" [Identifier ea "a", NumberLit ea "0", NumberLit ea "3"])
>      ,e "substring(a from 0 for (5 - 3))"
>         (FunCall ea "!substring" [Identifier ea "a",NumberLit ea "0",
>          FunCall ea "-" [NumberLit ea "5",NumberLit ea "3"]])
>      ,e "substring(a,b,c)"
>         (FunCall ea "substring" [Identifier ea "a"
>                                 ,Identifier ea "b"
>                                 ,Identifier ea "c"])
>      ,e "a like b"
>         (FunCall ea "!like" [Identifier ea "a", Identifier ea "b"])
>      ,e "a not like b"
>         (FunCall ea "!notlike" [Identifier ea "a", Identifier ea "b"])
>      , e "a and b and c and d"
>         (FunCall ea "!and"
>          [FunCall ea "!and"
>           [FunCall ea "!and" [Identifier ea "a"
>                              ,Identifier ea "b"]
>           ,Identifier ea "c"]
>          ,Identifier ea "d"])
>      ]
>    ,Group "function calls" [
>       e "fn()" (FunCall ea "fn" [])
>      ,e "fn(1)" (FunCall ea "fn" [NumberLit ea "1"])
>      ,e "fn('test')" (FunCall ea "fn" [stringQ "test"])
>      ,e "fn(1,'test')" (FunCall ea "fn" [NumberLit ea "1", stringQ "test"])
>      ,e "fn('test')" (FunCall ea "fn" [stringQ "test"])
>      ]
>    ,Group "simple whitespace sanity checks" [
>       e "fn (1)" (FunCall ea "fn" [NumberLit ea "1"])
>      ,e "fn( 1)" (FunCall ea "fn" [NumberLit ea "1"])
>      ,e "fn(1 )" (FunCall ea "fn" [NumberLit ea "1"])
>      ,e "fn(1) " (FunCall ea "fn" [NumberLit ea "1"])
>      ]
>    ,Group "null stuff" [
>       e "not null" (FunCall ea "!not" [NullLit ea])
>      ,e "a is null" (FunCall ea "!isnull" [Identifier ea "a"])
>      ,e "a is not null" (FunCall ea "!isnotnull" [Identifier ea "a"])
>      ,e "not not true" (FunCall ea "!not"
>                          [FunCall ea "!not"
>                           [BooleanLit ea True]])
>      ]

>    ,Group "case expressions" [
>       e {-"case when a,b then 3\n\
>         \     when c then 4\n\
>         \     else 5\n\
>         \end" -}
>         [here|
>          case when a,b then 3
>               when c then 4
>               else 5
>          end
>          |]
>         (Case ea [([Identifier ea "a", Identifier ea "b"], NumberLit ea "3")
>               ,([Identifier ea "c"], NumberLit ea "4")]
>          (Just $ NumberLit ea "5"))
>      ,e  "case 1 when 2 then 3 else 4 end"
>         (CaseSimple ea (NumberLit ea "1")
>            [([NumberLit ea "2"], NumberLit ea "3")]
>          (Just $ NumberLit ea "4"))
>      ]
>    ,Group "positional args" [
>       e "$1" (PositionalArg ea 1)
>      ,e "?" (Placeholder ea)
>      ,e "a = ?" (FunCall ea "=" [Identifier ea "a",Placeholder ea])
>      ]
>    ,Group "exists" [
>       e "exists (select 1 from a)"
>       (Exists ea (selectFrom [SelExp ea (NumberLit ea "1")] (Tref ea (i "a") (NoAlias ea))))
>      ]
>    ,Group "in variants" [
>       e "t in (1,2)"
>       (InPredicate ea (Identifier ea "t") True (InList ea [NumberLit ea "1",NumberLit ea "2"]))
>      ,e "t not in (1,2)"
>       (InPredicate ea (Identifier ea "t") False (InList ea [NumberLit ea "1",NumberLit ea "2"]))
>      ,e "(t,u) in (1,2)"
>       (InPredicate ea (FunCall ea "!rowctor" [Identifier ea "t",Identifier ea "u"]) True
>        (InList ea [NumberLit ea "1",NumberLit ea "2"]))
>      ,e "3 = any (array[1,2])"
>       (LiftOperator ea "=" LiftAny [NumberLit ea "3"
>                                     ,FunCall ea "!arrayctor" [NumberLit ea "1"
>                                                              ,NumberLit ea "2"]])
>      ,e "3 = all (array[1,2,4])"
>       (LiftOperator ea "=" LiftAll [NumberLit ea "3"
>                                     ,FunCall ea "!arrayctor" [NumberLit ea "1"
>                                                              ,NumberLit ea "2"
>                                                              ,NumberLit ea "4"]])
>      ]
>    ,Group "comparison operators" [
>       e "a < b"
>       (FunCall ea "<" [Identifier ea "a", Identifier ea "b"])
>      ,e "a <> b"
>       (FunCall ea "<>" [Identifier ea "a", Identifier ea "b"])
>      ,e "a != b"
>       (FunCall ea "<>" [Identifier ea "a", Identifier ea "b"])
>      ]

test some string parsing, want to check single quote behaviour,
and dollar quoting, including nesting.

>    ,Group "string parsing" [
>       e "''" (stringQ "")
>      ,e "''''" (stringQ "'")
>      ,e "'test'''" (stringQ "test'")
>      ,e "'''test'" (stringQ "'test")
>      ,e "'te''st'" (stringQ "te'st")
>      ,e "$$test$$" (StringLit ea "test")
>      ,e "$$te'st$$" (StringLit ea "te'st")
>      ,e "$st$test$st$" (StringLit ea "test")
>      ,e "$outer$te$$yup$$st$outer$" (StringLit ea "te$$yup$$st")
>      ,e "'spl$$it'" (stringQ "spl$$it")
>      ]
>    ,Group "bracketed things" [
>       e "(p).x" (eqi "p" "x")
>      ,e "(select f(((a).x, y)::z))"
>         (ScalarSubQuery ea
>          (selectE (sl
>                    [SelExp ea
>                     (FunCall ea "f" [Cast ea
>                                      (FunCall ea "!rowctor"
>                                       [eqi "a" "x"
>                                       ,Identifier ea "y"])
>                                      (SimpleTypeName ea "z")])])))
>      ]
>      ]
>  where
>    e = Expr
