
> {-# LANGUAGE QuasiQuotes #-}
>
> module Database.HsSqlPpp.Tests.Parsing.Dml (dmlParsingTestData) where
>
> --import Test.HUnit
> --import Test.Framework
> --import Test.Framework.Providers.HUnit
> --import Data.Generics
>
> import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Annotation
> --import Database.HsSqlPpp.Parser
> --import Database.HsSqlPpp.Pretty

> import Database.HsSqlPpp.Tests.Parsing.Utils

TODO:

from in update, using in delete (+ type check these)

> dmlParsingTestData:: Item
> dmlParsingTestData =
>    Group "dml" [
>      Group "insert" [
>       s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2);\n"
>        [Insert ea
>         (dqi "testtable")
>         ["columna", "columnb"]
>         (Values ea [[NumberLit ea "1", NumberLit ea "2"]])
>         Nothing]

multi row insert, test the stand alone values statement first, maybe
that should be in the select section?

>      ,s "values (1,2), (3,4);"
>      [QueryStatement ea $ Values ea [[NumberLit ea "1", NumberLit ea "2"]
>              ,[NumberLit ea "3", NumberLit ea "4"]]]
>
>      ,s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2), (3,4);\n"
>       [Insert ea
>         (dqi "testtable")
>         ["columna", "columnb"]
>         (Values ea [[NumberLit ea "1", NumberLit ea "2"]
>                 ,[NumberLit ea "3", NumberLit ea "4"]])
>         Nothing]

insert from select

>      ,s "insert into a\n\
>          \    select b from c;"
>       [Insert ea (dqi "a") []
>        (selectFrom [selI "b"] (Tref ea (i "c") (NoAlias ea)))
>        Nothing]
>
>      ,s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2) returning id;\n"
>       [Insert ea
>         (dqi "testtable")
>         ["columna", "columnb"]
>         (Values ea [[NumberLit ea "1", NumberLit ea "2"]])
>         (Just $ sl [selI "id"])]
>      ]
>
>     ,Group "update" [
>       s "update tb\n\
>         \  set x = 1, y = 2;"
>       [Update ea (dqi "tb") [FunCall ea "=" [Identifier ea "x", NumberLit ea "1"]
>                       ,FunCall ea "=" [Identifier ea "y", NumberLit ea "2"]]
>        [] Nothing Nothing]
>      ,s "update tb\n\
>         \  set x = 1, y = 2 where z = true;"
>       [Update ea (dqi "tb") [FunCall ea "=" [Identifier ea "x", NumberLit ea "1"]
>                       ,FunCall ea "=" [Identifier ea "y", NumberLit ea "2"]]
>        []
>        (Just $ FunCall ea "="
>         [Identifier ea "z", BooleanLit ea True])
>        Nothing]
>      ,s "update tb\n\
>         \  set x = 1, y = 2 returning id;"
>       [Update ea (dqi "tb") [FunCall ea "=" [Identifier ea "x", NumberLit ea "1"]
>                       ,FunCall ea "=" [Identifier ea "y", NumberLit ea "2"]]
>        [] Nothing (Just $ sl [selI "id"])]
>      ,s "update tb\n\
>         \  set (x,y) = (1,2);"
>       [Update ea (dqi "tb") [FunCall ea "="
>                        [FunCall ea "!rowctor" [Identifier ea "x"
>                                               ,Identifier ea "y"]
>                        ,FunCall ea "!rowctor" [NumberLit ea "1"
>                                               ,NumberLit ea "2"]]]
>        []
>        Nothing Nothing]
>      ]

FunCall ea "=" [FunCall ea "!rowctor" [Identifier ea "x",Identifier ea "y"],FunCall ea "!rowctor" [NumberLit ea "1",NumberLit ea "2"]])


>
>     ,Group "delete" [
>       s "delete from tbl1 where x = true;"
>       [Delete ea (dqi "tbl1") [] (Just $ FunCall ea "="
>                                [Identifier ea "x", BooleanLit ea True])
>        Nothing]
>      ,s "delete from tbl1 where x = true returning id;"
>       [Delete ea (dqi "tbl1") [] (Just $ FunCall ea "="
>                                [Identifier ea "x", BooleanLit ea True])
>        (Just $ sl [selI "id"])]
>      ]
>
>     ,Group "truncate" [
>       s "truncate test;"
>        [Truncate ea ["test"] ContinueIdentity Restrict]
>
>      ,s "truncate table test, test2 restart identity cascade;"
>        [Truncate ea ["test","test2"] RestartIdentity Cascade]
>      ]

copy, bit crap at the moment

>     ,Group "copy" [
>       s "copy tbl(a,b) from stdin;\n\
>         \bat\tt\n\
>         \bear\tf\n\
>         \\\.\n"
>       [Copy ea "tbl" ["a", "b"] Stdin
>        ,CopyData ea "\
>         \bat\tt\n\
>         \bear\tf\n"]
>      ]]
>  where
>    --e = Expr
>    s = Stmt
>    --f = PgSqlStmt