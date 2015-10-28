

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.Plpgsql (pgplsql) where
>
> import Database.HsSqlPpp.Syntax

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> pgplsql :: Item
> pgplsql =
>   Group "plpgsql" [
>     Group "simple plpgsql statements" [
>       f "success := true;"
>       [Assignment ea (name "success") lTrue]
>      ,f "success = true;"
>       [Assignment ea (name "success") lTrue]
>      ,f "return true;"
>       [Return ea $ Just lTrue]
>      ,f "return;"
>       [Return ea Nothing]
>      ,f "return next 1;"
>       [ReturnNext ea $ num "1"]
>      ,f "return query select a from b;"
>       [ReturnQuery ea $ makeSelect {selSelectList = sl [si $ ei "a"]
>                                    ,selTref = [tref "b"]}]
>      ,f "raise notice 'stuff %', 1;"
>       [Raise ea RNotice "stuff %" [num "1"]]
>      ,f "perform test();"
>       [Perform ea $ app "test" []]
>      ,f "perform test(a,b);"
>       [Perform ea $ app "test" [ei "a", ei "b"]]
>      ,f "perform test(r.relvar_name || '_and_stuff');"
>       [Perform ea $ app "test" [
>                     binop "||" (eqi "r" "relvar_name")
>                                (stringQ "_and_stuff")]]
>      ,f "select into a,b c,d from e;"
>       [Into ea False [name "a", name "b"]
>        $ QueryStatement ea
>        $ makeSelect {selSelectList = sl [si $ ei "c", si $ ei "d"]
>                     ,selTref = [tref "e"]}]
>      ,f "select c,d into a,b from e;"
>       [Into ea False [name "a", name "b"]
>        $ QueryStatement ea
>        $ makeSelect {selSelectList = sl [si $ ei "c", si $ ei "d"]
>                     ,selTref = [tref "e"]}]
>      ,f "delete from pieces where x = 1 and y = 1 returning tag into r.tag;"
>       [Into ea False [qn "r" "tag"]
>        $ Delete ea (name "pieces") []
>          (Just $ binop "and" (binop "=" (ei "x") (num "1"))
>                               (binop "=" (ei "y") (num "1")))
>          (Just $ sl [si $ ei "tag"])]
>      ,f "update pieces\n\
>         \set a=b returning tag into r.tag;"
>       [Into ea False [qn "r" "tag"]
>          $ Update ea (name "pieces")
>                [SetClause ea (Nmc "a") $ ei "b"]
>                []
>                Nothing
>                (Just $ sl $ [si $ ei "tag"])]
>      ,f "insert into t(a) values (1) returning id into x;"
>       [Into ea False [Name ea [Nmc "x"]]
>        $ Insert ea
>         (name "t")
>         [Nmc "a"]
>         (Values ea [[num "1"]])
>         (Just $ sl [si $ ei "id"])]

>      ,f "update t\n\
>         \  set x = 1 returning id into z;"
>       [Into ea False [Name ea [Nmc "z"]]
>       $ Update ea (name "t") [SetClause ea (Nmc "x") $ num "1"]
>         [] Nothing (Just $ sl [si $ ei "id"])]

>      ,f "execute s;"
>       [Execute ea (ei "s")]
>      ,f "execute s into r;"
>       [Into ea False [Name ea [Nmc "r"]] (Execute ea (ei "s"))]
>     ,f "continue;" [ContinueStatement ea Nothing]
>     ]
>
>     ,Group "other plpgsql statements" [
>       f "for r in select a from tbl loop\n\
>         \null;\n\
>         \end loop;"
>       [ForQueryStatement ea Nothing (Nmc "r")
>        (makeSelect {selSelectList = sl [si $ ei "a"]
>                    ,selTref = [tref "tbl"]})
>        [NullStatement ea]]
>      ,f "for r in select a from tbl where true loop\n\
>         \null;\n\
>         \end loop;"
>       [ForQueryStatement ea Nothing (Nmc "r")
>        (makeSelect {selSelectList = sl [si $ ei "a"]
>                    ,selTref = [tref "tbl"]
>                    ,selWhere = Just lTrue})
>        [NullStatement ea]]
>      ,f "for r in 1 .. 10 loop\n\
>         \null;\n\
>         \end loop;"
>       [ForIntegerStatement ea Nothing (Nmc "r")
>        (num "1") (num "10")
>        [NullStatement ea]]
>       -- catch a bug in lexing where 1..10 is parsed as
>       -- num "1.", num ".10", instead of num 1, symbol "..", num 10
>      ,f "for r in 1..10 loop\n\
>         \null;\n\
>         \end loop;"
>       [ForIntegerStatement ea Nothing (Nmc "r")
>        (num "1") (num "10")
>        [NullStatement ea]]
>
>      ,f "if a=b then\n\
>         \  update c set d = e;\n\
>         \end if;"
>       [If ea [(binop "=" (ei "a") (ei "b")
>               ,[Update ea (name "c") [SetClause ea (Nmc "d")
>                                      $ ei "e"] [] Nothing Nothing])]
>        []]
>      ,f "if true then\n\
>         \  null;\n\
>         \else\n\
>         \  null;\n\
>         \end if;"
>       [If ea [(lTrue,[NullStatement ea])]
>        [NullStatement ea]]
>      ,f "if true then\n\
>         \  null;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \end if;"
>       [If ea [(lTrue, [NullStatement ea])
>              ,(lFalse, [Return ea Nothing])]
>        []]
>      ,f "if true then\n\
>         \  null;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \elsif false then\n\
>         \  return;\n\
>         \else\n\
>         \  return;\n\
>         \end if;"
>       [If ea [(lTrue, [NullStatement ea])
>              ,(lFalse, [Return ea Nothing])
>              ,(lFalse, [Return ea Nothing])]
>        [Return ea Nothing]]
>      ,f "case a\n\
>         \  when b then null;\n\
>         \  when c,d then null;\n\
>         \  else null;\n\
>         \end case;"
>      [CaseStatementSimple ea (ei "a")
>       [([ei "b"], [NullStatement ea])
>       ,([ei "c", ei "d"], [NullStatement ea])]
>       [NullStatement ea]]
>     ]]
>  where
>    f = PgSqlStmt
