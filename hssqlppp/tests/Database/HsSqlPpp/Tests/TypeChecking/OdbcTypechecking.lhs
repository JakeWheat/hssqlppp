
See the odbcparsing for an overview.

1. date, time, and timestamp literals: these are trivially type
checked (and we don't check the format of the literal string
in hssqlppp currently)
2. scalar function calls: see below
3. escapes for like strings: not currently supported
4. outer joins: works like trefparens
5. calling a procedure: not currently supported

scalar functions

if we write:
select _f_(a), {fn _g_(b)} from t

the _f_ function is looked up in the usual catalog
but the _g_ function is looked up in an alternative namespace for
functions. We can't combine these two namespaces because the valid
functions are different in each namespace.

So we have to create a special case in the catalog and type checking
code. Since odbc is old, this is considered a fixed list and is just
implemented as a separate list of function prototypes to check against
when you are directly inside a {fn ... } node.

Here is the 'catalog' for odbc scalar functions:

https://msdn.microsoft.com/en-us/library/ms711813(v=vs.85).aspx



> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.OdbcTypechecking (odbcTypechecking) where
>
> --import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog

> --import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes
> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Internals.TypesInternal hiding (mkTypeExtra,mkTypeExtraNN)

> odbcTypechecking:: Item
> odbcTypechecking =
>    Group "odbcTypechecking" [
>        -- literals
>        scalExpr "{d '2000-01-01'}" $ Right typeDate
>       ,scalExpr "{t '12:00:01.1'}" $ Right $ ScalarType "time"
>       ,scalExpr "{ts '2000-01-01 12:00:01.1'}" $ Right typeTimestamp
>       -- scalar functions
>       ,scalExpr "{fn ascii('test')}" $ Right typeInt
>       ,scalExpr "{fn extract(hour from date 'dt')}" $ Right typeInt
>       ,scalExpr "(extract(hour from date 'dt'))" $ Right typeInt
>       -- position not supported in the parser
>       --,scalExpr "{fn POSITION('aaa' IN 'bbb')}" $ Right typeVarChar
>       ,scalExpr "{fn CONVERT(3, SQL_BIGINT)}" $ Right typeBigInt
>       ,scalExpr "{fn CONVERT(3, SQL_FLOAT)}" $ Right typeFloat8

>       ,scalExpr "{fn timestampadd(SQL_TSI_SECOND,3, {t '12:00:00'})}" $ Right typeTime
>       ,scalExpr "{fn timestampadd(SQL_TSI_MINUTE,3, {ts '2001-01-01 12:00:00'})}" $ Right typeTimestamp
>       ,scalExpr "{fn timestampadd(SQL_TSI_YEAR,3, {d '2001-01-01'})}" $ Right typeDate

>       ,scalExpr "{fn timestampdiff(SQL_TSI_YEAR,{d '2001-01-01'}, {d '2001-01-01'})}" $ Right typeInt



>       ,scalExpr "{fn left('test',3)}" $ Right $ ScalarType "text"

        -- todo: somehow, the generated catalog ended up wrong here
 >       ,scalExpr "left(3,'test')" $ Right $ ScalarType "text"

 >       ,scalExpr "{fn left(3,'test')}" $ Left [NoMatchingOperator "!odbc-left" [ScalarType "int4",UnknownType]]
 >       ,scalExpr "left('test',3)" $ Left [NoMatchingOperator "left" [UnknownType,ScalarType "int4"]]

 >       ,scalExpr "{fn left(left(3,'test'),3)}" $ Right $ ScalarType "text"
 >       ,scalExpr "left(3,{fn left('test',3)})" $ Right $ ScalarType "text"



>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")]]
>         "select {fn ascii('test')} as a, a as b, {d '2000-01-01'} as c,\n\
>         \       {fn CONVERT('text', SQL_VARCHAR)} || {t '12:00:01.1'} as d from t"
>        $ Right $ CompositeType [("a", (mkTypeExtra typeInt) {teNullable=False})
>                                ,("b", mkTypeExtra typeInt)
>                                ,("c", mkTypeExtra typeDate)
>                                ,("d", mkTypeExtra $ ScalarType "text")]

>       -- outer join
>       ,tcQueryExpr [CatCreateTable ("public","t0") [("a", mkCatNameExtra "int4")
>                                         ,("b", mkCatNameExtra "text")]
>                    ,CatCreateTable ("public","t1") [("c", mkCatNameExtra "int4")
>                                         ,("d", mkCatNameExtra "text")]]
>        "select * from {oj t0 left outer join t1 on t0.a=t1.c}"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                ,("b", mkTypeExtra $ ScalarType "text")
>                                ,("c", mkTypeExtra typeInt)
>                                ,("d", mkTypeExtra $ ScalarType "text")]


>   ]
>   where
>     scalExpr = TCScalExpr defaultTemplate1Catalog emptyEnvironment
>                           defaultTypeCheckFlags
>     tcQueryExpr cus =
>         let cat = makeCatalog PostgreSQL cus defaultTemplate1Catalog
>         in TCQueryExpr cat defaultTypeCheckFlags
