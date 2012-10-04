

> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.MiscQueryExprs (miscQueryExprs) where
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils

> miscQueryExprs:: Item
> miscQueryExprs =
>    Group "misc select statements"
>    [s "select 1;"
>     $ makeSelect {selSelectList = sl [si $ num "1"]}
>    ,s "select a from t;" $ tblat
>    ,s "select distinct a from t;" $ tblat {selDistinct = Distinct}
>    ,s "select a from t where b=2;"
>       $ tblat {selWhere = Just $ binop "=" (ei "b") (num "2")}
>    ,s "select a from t where b=2 and c=3;"
>       $ tblat {selWhere = Just $ binop "and"
>                           (binop "=" (ei "b") (num "2"))
>                           (binop "=" (ei "c") (num "3"))}
>    ,s "SELECT T.A::INT FROM TBL AS T;"
>     $ makeSelect
>      {selSelectList = sl [si $ Cast ea (eqi "T" "A")
>                                  (st "INT")]
>      ,selTref = [trefa "TBL" "T"]}
>    ,s "select a from t order by a;"
>     $ tblat {selOrderBy = [(ei "a",Asc)]}
>    ,s "select a from t order by a asc;"
>     $ tblat {selOrderBy = [(ei "a",Asc)]}
>    ,s "select a from t order by a desc;"
>     $ tblat {selOrderBy = [(ei "a",Desc)]}
>    ,s "select a from t order by a,b;"
>     $ tblat {selOrderBy = [(ei "a",Asc)
>                           ,(ei "b",Asc)]}
>    ,s "select a from t order by a asc,b;"
>     $ tblat {selOrderBy = [(ei "a",Asc)
>                           ,(ei "b",Asc)]}
>    ,s "select a from t order by a desc,b;"
>     $ tblat {selOrderBy = [(ei "a",Desc)
>                           ,(ei "b",Asc)]}
>    ,s "select a from t order by a desc,b desc;"
>     $ tblat {selOrderBy = [(ei "a",Desc)
>                           ,(ei "b",Desc)]}
>    ,s "select a from t limit 1;"
>     $ tblat {selLimit = Just $ num "1"}
>    ,s "select a from t offset 1;"
>     $ tblat {selOffset = Just $ num "1"}
>    ,s "select a from t order by a limit 1 offset 1;"
>     $ tblat {selOrderBy = [(ei "a",Asc)]
>             ,selLimit = Just $ num "1"
>             ,selOffset = Just $ num "1"}
>    ,s "select (p).x, (p).y from pos;"
>           $ makeSelect
>                   {selSelectList = sl [si $ parenQual (ei "p") (ei "x")
>                                       ,si $ parenQual (ei "p") (ei "y")]
>                   ,selTref = [tref "pos"]}
>    ,s "select ($1).x, ($1).y from pos;"
>           $ makeSelect
>                   {selSelectList = sl
>                    [si $ member (Parens ea $ PositionalArg ea 1) (ei "x")
>                    ,si $ member (Parens ea $ PositionalArg ea 1) (ei "y")]
>                   ,selTref = [tref "pos"]}
>    ]
>  where
>    s = QueryExpr
>    tblat = makeSelect
>            {selSelectList = sl [si $ ei "a"]
>            ,selTref = [tref "t"]}
