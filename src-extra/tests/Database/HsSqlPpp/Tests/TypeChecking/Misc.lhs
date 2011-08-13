
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> module Database.HsSqlPpp.Tests.TypeChecking.Misc
>     (tcMiscTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> --import Database.HsSqlPpp.Catalog

--------------------------------------------------------------------------------


> tcMiscTestData :: Item
> tcMiscTestData = --fixme
>   Group "misc" [
>   {-Group "composite elements" [
>       s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r pg_attrdef;\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  b = r.adsrc;\n\
>         \  r.adnum := a;\n\
>         \  b = r.adsrc;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ]-}

>   {-,Group "positional args" [
>       s "create function distance(int, int, int, int) returns float(24) as $$\n\
>         \  select (point($1, $2) <-> point($3, $4))::float(24) as result;\n\
>         \$$ language sql immutable;"
>         $ Right [Nothing]
>      ,s "create function distance(int, int, int, int) returns float(24) as $$\n\
>         \  select (point($1, $2) <-> point($3, $5))::float(24) as result;\n\
>         \$$ language sql immutable;"
>         $ Left [UnrecognisedIdentifier "$5"]
>      ]

>   ,-}Group "window fns" [
>       s "select *, row_number() over () from pg_attrdef;"
>         $ Right [Just $ ([],
>                   [("adrelid",ScalarType "oid")
>                   ,("adnum",ScalarType "int2")
>                   ,("adbin",ScalarType "text")
>                   ,("adsrc",ScalarType "text")
>                   ,("row_number",ScalarType "int8")])]
>      ]
>   ]

>  where
>    --e = Expr
>    s = StmtType
>    --c = CatStmtType
>    --d = Ddl

