
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> module Database.HsSqlPpp.Tests.TypeChecking.Plpgsql
>     (tcPlpgsqlTestData) where
>
> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Types
>
> --import Database.HsSqlPpp.Catalog

--------------------------------------------------------------------------------

~~~~
check the identifier resolution for functions:
parameters
variable declarations
select expressions inside these:
refer to param
refer to variable
override var with var
override var with select iden
todo: override var with param, override select iden with param

check var defs:
check type exists
check type of initial values


> tcPlpgsqlTestData :: Item
> tcPlpgsqlTestData = --fixme
>   Group "plpgsql" $ drop 1000 [
>   Group "create function param resolution" [
>       s "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ,s "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return badstuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [UnrecognisedIdentifier "badstuff"])
>      ,s "create function t1() returns text as $$\n\
>         \declare\n\
>         \  stuff text;\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ]


--------------------------------------------------------------------------------

>   ,Group "plpgsqlbits" [
>       s "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ,s "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return badstuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [UnrecognisedIdentifier "badstuff"])
>      ,s "create function t1() returns text as $$\n\
>         \declare\n\
>         \  stuff text;\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a bool;\n\
>         \begin\n\
>         \  a := 3;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [IncompatibleTypes (ScalarType "bool") (ScalarType "int4")])
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a boolean;\n\
>         \begin\n\
>         \  a := true;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ]

--------------------------------------------------------------------------------

>   ,Group "for loops" [
>       s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \  t int;\n\
>         \begin\n\
>         \  for r in select * from pg_attrdef loop\n\
>         \    t := r.adnum;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>
>
>      {-,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \  t int;\n\
>         \begin\n\
>         \  for r in select adnum from pg_attrdef loop\n\
>         \    t := r;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])-}
>
>       -- loop var already declared
>
>      ,s "create function test() returns void as $$\n\
>         \declare\n\
>         \  i int;\n\
>         \  i1 int;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    i1 := i;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>        (Right [Nothing])
>
>        --implicitly created loop var
>
>      ,s "create function test() returns void as $$\n\
>         \declare\n\
>         \  i1 int;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    i1 := i;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>        (Right [Nothing])
>
>        --loop var already declared, wrong type
>
>      ,s "create function test() returns void as $$\n\
>         \declare\n\
>         \  i bool;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    null;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>        (Left [IncompatibleTypes (ScalarType "bool") (ScalarType "int4")])
>
>       --loop var implicit check it's type
>
>      ,s "create function test() returns void as $$\n\
>         \declare\n\
>         \  t bool;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    t := i;\n\
>         \  end loop;\n\
>         \end;\n\
>         \       $$ language plpgsql volatile;"
>         (Left [IncompatibleTypes (ScalarType "bool") (ScalarType "int4")])
>      ]
>      ]
>  where
>    --e = Expr
>    s = StmtType
>    --c = CatStmtType
>    --d = Ddl

