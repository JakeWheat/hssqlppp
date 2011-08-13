
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> module Database.HsSqlPpp.Tests.TypeChecking.Into
>     (tcIntoTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> --import Database.HsSqlPpp.Catalog

--------------------------------------------------------------------------------


> tcIntoTestData :: Item
> tcIntoTestData = --fixme
>   Group "select into" $ drop 1000 [
>       s "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b') returning adnum,adbin;"
>         $ Right [Just $ ([], [("adnum", ScalarType "int2")
>                                          ,("adbin", ScalarType "text")])]
>      ,s "update pg_attrdef set adnum = adnum + 1 returning adnum;"
>         $ Right [Just $ ([], [("adnum", ScalarType "int2")])]
>      ,s "delete from pg_attrdef returning adnum,adbin;"
>         $ Right [Just $ ([], [("adnum", ScalarType "int2")
>                                          ,("adbin", ScalarType "text")])]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into a,b from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into b,a from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [IncompatibleTypes (ScalarType "text") (ScalarType "int2")
>                ,IncompatibleTypes (ScalarType "int4") (ScalarType "text")]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into a,c from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [UnrecognisedIdentifier "c"]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into a from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [WrongNumberOfColumns]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into r from pg_attrdef;\n\
>         \  a := r.adnum;\n\
>         \  b := r.adbin;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into r from pg_attrdef;\n\
>         \  a := r.adnum;\n\
>         \  b := r.adsrc;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [UnrecognisedIdentifier "adsrc"]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r pg_attrdef;\n\
>         \begin\n\
>         \  select * into r from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r pg_class;\n\
>         \begin\n\
>         \  select adnum,adbin into r from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [IncompatibleTypes (NamedCompositeType "pg_class") (AnonymousRecordType [ScalarType "int2",ScalarType "text"])]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \begin\n\
>         \  select adnum,adbin into r from pg_attrdef;\n\
>         \  select relname into r from pg_class;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ]
>  where
>    --e = Expr
>    s = StmtType
>    --c = CatStmtType
>    --d = Ddl

