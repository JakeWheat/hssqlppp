

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.Plpgsql (pgplsql) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> pgplsql :: Item
> pgplsql =
>   Group "plpgsql" [
>     Group "simple plpgsql statements" [
>       f "success := true;"
>       [Assignment ea (Name ea ["success"]) (BooleanLit ea True)]
>      ,f "success = true;"
>       [Assignment ea (Name ea ["success"]) (BooleanLit ea True)]
>      ,f "return true;"
>       [Return ea $ Just (BooleanLit ea True)]
>      ,f "return;"
>       [Return ea Nothing]
>      ,f "return next 1;"
>       [ReturnNext ea $ NumberLit ea "1"]
>      ,f "return query select a from b;"
>       [ReturnQuery ea $ selectFrom [selI "a"] (Tref ea (i "b") (NoAlias ea))]
>      ,f "raise notice 'stuff %', 1;"
>       [Raise ea RNotice "stuff %" [NumberLit ea "1"]]
>      ,f "perform test();"
>       [Perform ea $ App ea (name "test") []]
>      ,f "perform test(a,b);"
>       [Perform ea $ App ea (name "test") [Identifier ea "a", Identifier ea "b"]]
>      ,f "perform test(r.relvar_name || '_and_stuff');"
>       [Perform ea $ App ea (name "test") [
>                     binop "||" (eqi "r" "relvar_name")
>                                (stringQ "_and_stuff")]]
>      ,f "select into a,b c,d from e;"
>       [Into ea False [name "a", name "b"]
>        $ QueryStatement ea $ Select ea Dupes (SelectList ea [selI "c", selI "d"])
>              [Tref ea (i "e") (NoAlias ea)] Nothing [] Nothing [] Nothing Nothing]
>      ,f "select c,d into a,b from e;"
>       [Into ea False [name "a", name "b"]
>        $ QueryStatement ea $ Select ea Dupes (SelectList ea [selI "c", selI "d"])
>          [Tref ea (i "e") (NoAlias ea)] Nothing [] Nothing [] Nothing Nothing]
>      ,f "delete from pieces where x = 1 and y = 1 returning tag into r.tag;"
>       [Into ea False [Name ea [Nmc "r",Nmc "tag"]]
>        $ Delete ea (dqi "pieces") []
>          (Just $ binop "!and" (binop "=" (ei "x") (num "1"))
>                               (binop "=" (ei "y") (num "1")))
>          (Just $ sl [selI "tag"])]
>      ,f "update pieces\n\
>         \set a=b returning tag into r.tag;"
>       [Into ea False [Name ea [Nmc "r",Nmc "tag"]]
>          $ Update ea (dqi "pieces")
>              [SetClause ea (Nmc "a") $ Identifier ea "b"]
>            []
>            Nothing (Just (SelectList ea
>                           [SelExp ea (Identifier ea "tag")]))]
>      ,f "insert into t(a) values (1) returning id into x;"
>       [Into ea False [Name ea [Nmc "x"]]
>        $ Insert ea
>         (dqi "t")
>         [Nmc "a"]
>         (Values ea [[NumberLit ea "1"]])
>         (Just $ sl [selI "id"])]

>      ,f "update t\n\
>         \  set x = 1 returning id into z;"
>       [Into ea False [Name ea [Nmc "z"]]
>       $ Update ea (dqi "t") [SetClause ea (Nmc "x") $ NumberLit ea "1"]
>         [] Nothing (Just $ sl [selI "id"])]

>      ,f "execute s;"
>       [Execute ea (Identifier ea "s")]
>      ,f "execute s into r;"
>       [Into ea False [Name ea [Nmc "r"]] (Execute ea (Identifier ea "s"))]
>     ,f "continue;" [ContinueStatement ea Nothing]
>     ]
>
>     ,Group "other plpgsql statements" [
>       f "for r in select a from tbl loop\n\
>         \null;\n\
>         \end loop;"
>       [ForQueryStatement ea Nothing (Nmc "r") (selectFrom  [selI "a"] (Tref ea (i "tbl") (NoAlias ea)))
>        [NullStatement ea]]
>      ,f "for r in select a from tbl where true loop\n\
>         \null;\n\
>         \end loop;"
>       [ForQueryStatement ea Nothing (Nmc "r")
>        (selectFromWhere [selI "a"] (Tref ea (i "tbl") (NoAlias ea)) lTrue)
>        [NullStatement ea]]
>      ,f "for r in 1 .. 10 loop\n\
>         \null;\n\
>         \end loop;"
>       [ForIntegerStatement ea Nothing (Nmc "r")
>        (NumberLit ea "1") (NumberLit ea "10")
>        [NullStatement ea]]
>       -- catch a bug in lexing where 1..10 is parsed as
>       -- num "1.", num ".10", instead of num 1, symbol "..", num 10
>      ,f "for r in 1..10 loop\n\
>         \null;\n\
>         \end loop;"
>       [ForIntegerStatement ea Nothing (Nmc "r")
>        (NumberLit ea "1") (NumberLit ea "10")
>        [NullStatement ea]]
>
>      ,f "if a=b then\n\
>         \  update c set d = e;\n\
>         \end if;"
>       [If ea [(binop "=" (ei "a") (ei "b")
>               ,[Update ea (dqi "c") [SetClause ea (Nmc "d")
>                                     $ ei "e"] [] Nothing Nothing])]
>        []]
>      ,f "if true then\n\
>         \  null;\n\
>         \else\n\
>         \  null;\n\
>         \end if;"
>       [If ea [(BooleanLit ea True,[NullStatement ea])]
>        [NullStatement ea]]
>      ,f "if true then\n\
>         \  null;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \end if;"
>       [If ea [(BooleanLit ea True, [NullStatement ea])
>              ,(BooleanLit ea False, [Return ea Nothing])]
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
>       [If ea [(BooleanLit ea True, [NullStatement ea])
>              ,(BooleanLit ea False, [Return ea Nothing])
>              ,(BooleanLit ea False, [Return ea Nothing])]
>        [Return ea Nothing]]
>      ,f "case a\n\
>         \  when b then null;\n\
>         \  when c,d then null;\n\
>         \  else null;\n\
>         \end case;"
>      [CaseStatementSimple ea (Identifier ea "a")
>       [([Identifier ea "b"], [NullStatement ea])
>       ,([Identifier ea "c", Identifier ea "d"], [NullStatement ea])]
>       [NullStatement ea]]
>     ]]
>  where
>    f = PgSqlStmt