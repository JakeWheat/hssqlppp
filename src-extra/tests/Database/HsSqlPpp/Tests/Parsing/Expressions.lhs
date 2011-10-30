
> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
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
>       e "42" (num "42")
>      ,e "3.5" (num "3.5")
>      ,e "4." (num "4.")
>      ,e ".001" (num ".001")
>      ,e "5e2" (num "5e2")
>      ,e "1.925e-3" (num "1.925e-3")

>      ]
>    ,Group "basic expressions" [
>       e "1" (num "1")
>      ,e "-1" (app "u-" [num "1"])
>      ,e "1.1" (num "1.1")
>      ,e "-1.1" (app "u-" [num "1.1"])
>      ,e " 1 + 1 " (app "+" [num "1"
>                                ,num "1"])
>      ,e "1+1+1" (app "+" [app "+" [num "1"
>                                           ,num "1"]
>                              ,num "1"])
>      ]
>    ,Group "parens" [

check some basic parens use wrt naked values and row constructors
these tests reflect how pg seems to interpret the variants.

>       e "(1)" (num "1")
>      ,e "row ()" (app "!rowctor" [])
>      ,e "row (1)" (app "!rowctor" [num "1"])
>      ,e "row (1,2)" (app "!rowctor" [num "1",num "2"])
>      ,e "(1,2)" (app "!rowctor" [num "1",num "2"])
>      ]
>    ,Group "more basic expressions" [

test some more really basic expressions

>       e "'test'" (stringQ "test")
>      ,e "''" (stringQ "")
>      ,e "hello" (ei "hello")
>      ,e "helloTest" (ei "helloTest")
>      ,e "hello_test" (ei "hello_test")
>      ,e "\"this is an identifier\""
>             (Identifier ea (QNmc "this is an identifier"))
>      ,e "hello1234" (ei "hello1234")
>      ,e "true" lTrue
>      ,e "false" lFalse
>      ,e "null" lNull
>      ]
>    ,Group "array ctor and selector" [
>       e "array[1,2]" (app "!arrayctor" [num "1", num "2"])
>      ,e "a[1]" (app "!arraysub" [ei "a", num "1"])
>      ]
>    ,Group "simple operators" [
>       e "1 + tst1" (app "+" [num "1"
>                                 ,ei "tst1"])
>      ,e "tst1 + 1" (app "+" [ei "tst1"
>                                 ,num "1"])
>      ,e "tst + tst1" (app "+" [ei "tst"
>                                   ,ei "tst1"])
>      ,e "'a' || 'b'" (app "||" [stringQ "a"
>                                    ,stringQ "b"])
>      ,e "'stuff'::text"
>       (Cast ea (stringQ "stuff") (SimpleTypeName ea "text"))
>      ,e "245::float(24)" (Cast ea (num "245") (PrecTypeName ea "float" 24))
>      ,e "245.1::numeric(5,3)"
>       (Cast ea (num "245.1") (Prec2TypeName ea "numeric" 5 3))
>      ,e "245::double precision"
>       (Cast ea (num "245") (SimpleTypeName ea "double precision"))
>      ,e "date '1998-12-01'"
>       (TypedStringLit ea (SimpleTypeName ea "date") "1998-12-01")
>      ,e "interval '63' day" (Interval ea "63" IntervalDay Nothing)
>      ,e "interval '63' day (3)" (Interval ea "63" IntervalDay $ Just 3)
>      ,e "extract(year from a)" (Extract ea ExtractYear $ ei "a")
>      ,e "a between 1 and 3"
>         (app "!between" [ei "a", num "1", num "3"])
>      ,e "a between 7 - 1 and 7 + 1"
>         (app "!between" [ei "a"
>                             ,app "-" [num "7"
>                                          ,num "1"]
>                             ,app "+" [num "7"
>                                          ,num "1"]])
>      ,e "cast(a as text)"
>         (Cast ea (ei "a") (SimpleTypeName ea "text"))
>      ,e "@ a"
>         (app "@" [ei "a"])
>      ,e "substring(a from 0 for 3)"
>         (app "!substring" [ei "a", num "0", num "3"])
>      ,e "substring(a from 0 for (5 - 3))"
>         (app "!substring" [ei "a"
>                               ,num "0"
>                               ,app "-" [num "5",num "3"]])
>      ,e "substring(a,b,c)"
>         (app "substring" [ei "a"
>                              ,ei "b"
>                              ,ei "c"])
>      ,e "a like b"
>         (app "!like" [ei "a", ei "b"])
>      ,e "a not like b"
>         (app "!notlike" [ei "a", ei "b"])
>      , e "a and b and c and d"
>         (app "!and"
>          [app "!and"
>           [app "!and" [ei "a"
>                           ,ei "b"]
>           ,ei "c"]
>          ,ei "d"])
>      ]
>    ,Group "function calls" [
>       e "fn()" (app "fn" [])
>      ,e "fn(1)" (app "fn" [num "1"])
>      ,e "fn('test')" (app "fn" [stringQ "test"])
>      ,e "fn(1,'test')" (app "fn" [num "1", stringQ "test"])
>      ,e "fn('test')" (app "fn" [stringQ "test"])
>      ]
>    ,Group "simple whitespace sanity checks" [
>       e "fn (1)" (app "fn" [num "1"])
>      ,e "fn( 1)" (app "fn" [num "1"])
>      ,e "fn(1 )" (app "fn" [num "1"])
>      ,e "fn(1) " (app "fn" [num "1"])
>      ]
>    ,Group "null stuff" [
>       e "not null" (app "!not" [lNull])
>      ,e "a is null" (app "!isnull" [ei "a"])
>      ,e "a is not null" (app "!isnotnull" [ei "a"])
>      ,e "not not true" (app "!not"
>                          [app "!not"
>                           [lTrue]])
>      ]

>    ,Group "case expressions" [
>       e [here|
>          case when a,b then 3
>               when c then 4
>               else 5
>          end
>          |]
>         (Case ea [([ei "a", ei "b"], num "3")
>                  ,([ei "c"], num "4")]
>          (Just $ num "5"))
>      ,e  "case 1 when 2 then 3 else 4 end"
>         (CaseSimple ea (num "1")
>          [([num "2"], num "3")]
>          (Just $ num "4"))
>      ]
>    ,Group "positional args" [
>       e "$1" (PositionalArg ea 1)
>      ,e "?" (Placeholder ea)
>      ,e "a = ?" (app "=" [ei "a",Placeholder ea])
>      ]
>    ,Group "exists" [
>       e "exists (select 1 from a)"
>       (Exists ea (selectFrom [SelExp ea (num "1")]
>                   (Tref ea (i "a") (NoAlias ea))))
>      ]
>    ,Group "in variants" [
>       e "t in (1,2)"
>       (InPredicate ea (ei "t") True (InList ea [num "1",num "2"]))
>      ,e "t not in (1,2)"
>       (InPredicate ea (ei "t") False (InList ea [num "1",num "2"]))
>      ,e "(t,u) in (1,2)"
>       (InPredicate ea (app "!rowctor" [ei "t",ei "u"]) True
>        (InList ea [num "1",num "2"]))
>      ,e "3 = any (array[1,2])"
>       (LiftApp ea "=" LiftAny [num "3"
>                                    ,app "!arrayctor" [num "1"
>                                                          ,num "2"]])
>      ,e "3 = all (array[1,2,4])"
>       (LiftApp ea "=" LiftAll [num "3"
>                                    ,app "!arrayctor" [num "1"
>                                                          ,num "2"
>                                                          ,num "4"]])
>      ]
>    ,Group "comparison operators" [
>       e "a < b"
>       (app "<" [ei "a", ei "b"])
>      ,e "a <> b"
>       (app "<>" [ei "a", ei "b"])
>      ,e "a != b"
>       (app "<>" [ei "a", ei "b"])
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
>                     (app "f" [Cast ea
>                                   (app "!rowctor"
>                                    [eqi "a" "x"
>                                    ,ei "y"])
>                                   (SimpleTypeName ea "z")])])))
>      ]
>      ]
>  where
>    e = Expr
