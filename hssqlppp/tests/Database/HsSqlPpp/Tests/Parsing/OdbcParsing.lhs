
Odbc comes with some small 'extensions' to sql syntax. The basic idea
is to wrap a few places in SQL where you write ansi standard sql, and
delimit it with a {} pair, and then some simple parser in the odbc
driver can convert this into the dialect specific sql for your
non-ansi-sql supporting product circa 1989. In this way, you can
achieve a little bit of SQL DBMS dialect independence.

I think it is sensible to support the odbc syntax directly in your
main SQL parser instead of in the driver, so here it is.

Summary of ODBC syntax extensions:

Here is microsoft's docs which are OK:

https://msdn.microsoft.com/en-us/library/ms715364(v=vs.85).aspx

1. date, time, and timestamp literals
2. scalar function calls
3. escapes for like strings
4. outer joins
5. calling a procedure

= date, time and timestamp literals

These abstract out the particular formats (because everyone has to put
the years and months in different places, and use slashes or dashes or
dots or whatever).

{d 'yyyy-mm-dd'}
{t 'HH:MM:ss[.f...]}
{ts 'yyyy-mm-dd HH:MM:ss[.f...]'}

The point of this is to have a fixed format for dates/times which the
vendor specific driver vendor can convert into the format expected by
the vendor's sql dbms.

One thing that isn't clear to me is is whitespace allowed between {
and d, or ' and } for instance, and is different whitespace other than
a single space between the d and the ' for instance allowed?

= scalar function calls

{fn scalar-function-call}

e.g.
{fn curdate())}

Note: these can nest, e.g.:

INSERT INTO Orders (OrderID, CustID, OpenDate, SalesPerson, Status)
   VALUES (?, ?, {fn CONVERT({fn CURDATE()}, SQL_DATE)}, ?, ?)

The purpose of these is to take the ansi standard functions, and sql
server/access functions, and the extract operator, which isn't quite a
normal function, and then allow the driver to convert the function
names and arguments into the database specific alternative (e.g. with
a different function name, or the argument order changed).

= escape for a like string

this is the ansi standard:

[ NOT ] LIKE <character pattern> [ ESCAPE <escape character> ]

The odbc syntax to abstract this is:

[ NOT ] LIKE <character pattern> {escape 'escape-character'}

e.g.
like '[[' {escape '['}
->
like '[[' escape [

This isn't currently supported in hssqlppp.

= outer join

This is basically a weird wrapper which allows the driver to replace
'ansi' outer join syntax with the old oracle outer join syntax if this
is what the sql dbms supports. You write something like:

select * from {oj t1 left outer join t1 on t1.a = t2.b}

and then in theory, an older dbms which doesn't support this syntax
can conver this to:

select * from t1,t2 where t1.a(+) = t2.b

I may have got the (+) in the wrong place. In hssqlppp, the {oj ... }
is treated like trefparens:

select * from {oj t1 left outer join t1 on t1.a = t2.b}
is basically the same as
select * from (t1 left outer join t1 on t1.a = t2.b)

= calling a procedure

{[?=]call procedure-name[([parameter][,[parameter]]...)]}

this isn't currently supported in hssqlppp.

There is additional stuff in the typechecking, see the odbc
typechecking tests for more info. The type checker doesn't test the
date/time literals inside the literal like other literals in hssqlppp,
and these are treated as sugar, and oj does nothing, but there is a
bit of work to deal with the functions.

The odbc syntax is currently available in all dialects.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.OdbcParsing (odbcParsing) where
>
> import Database.HsSqlPpp.Syntax

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> odbcParsing:: Item
> odbcParsing =
>    Group "odbcParsing" [
>        Group "datetime" [
>            Expr "{d '2000-01-01'}" (OdbcLiteral ea OLDate "2000-01-01")
>           ,Expr "{t '12:00:01.1'}" (OdbcLiteral ea OLTime "12:00:01.1")
>           ,Expr "{ts '2000-01-01 12:00:01.1'}"
>                (OdbcLiteral ea OLTimestamp "2000-01-01 12:00:01.1")
>        ]
>        ,Group "functions" [
>              Expr "{fn CHARACTER_LENGTH(string_exp)}"
>              $ OdbcFunc ea (App ea (name "CHARACTER_LENGTH")
>                                    [ei "string_exp"])
>             ,Expr "{fn EXTRACT(year from my_date)}"
>              $ OdbcFunc ea (Extract ea ExtractYear $ ei "my_date")
>             ,Expr "{fn now()}"
>              $ OdbcFunc ea (App ea (name "now") [])
>             ,Expr "{fn CONVERT('2000-01-01', SQL_DATE)}"
>              $ OdbcFunc ea (App ea (name "CONVERT")
>               [StringLit ea "2000-01-01"
>               ,ei "SQL_DATE"])
>             ,Expr "{fn CONVERT({fn CURDATE()}, SQL_DATE)}"
>              $ OdbcFunc ea (App ea (name "CONVERT")
>               [OdbcFunc ea (App ea (name "CURDATE") [])
>               ,ei "SQL_DATE"])
>             ]
>        ,Group "outer join" [
>              QueryExpr "select * from {oj t1 left outer join t2 on true}"
>              $ makeSelect
>             {selSelectList = sl [si $ Star ea]
>             ,selTref = [OdbcTableRef ea (JoinTref ea (tref "t1") Unnatural LeftOuter Nothing
>                                          (tref "t2") (Just $ JoinOn ea (BooleanLit ea True)))]}]
>        ,Group "check parsing bugs" [
>              QueryExpr "select {fn CONVERT(cint,SQL_BIGINT)} from t;"
>              $ makeSelect
>             {selSelectList = sl [si $ OdbcFunc ea (App ea (name "CONVERT")
>                                                            [ei "cint"
>                                                            ,ei "SQL_BIGINT"])]
>             ,selTref = [tref "t"]}]
>        ,Group "outer join" [
>              TSQL "select * from {oj t1 left outer join t2 on true}"
>              [QueryStatement ea $ makeSelect
>             {selSelectList = sl [si $ Star ea]
>             ,selTref = [OdbcTableRef ea (JoinTref ea (tref "t1") Unnatural LeftOuter Nothing
>                                          (tref "t2") (Just $ JoinOn ea (BooleanLit ea True)))]}]]
>        ,Group "check parsing bugs" [
>              TSQL "select {fn CONVERT(cint,SQL_BIGINT)} from t;"
>              [QueryStatement ea $ makeSelect
>             {selSelectList = sl [si $ OdbcFunc ea (App ea (name "convert")
>                                                            [ei "cint"
>                                                            ,ei "SQL_BIGINT"])]
>             ,selTref = [tref "t"]}]]

>        ]

