
The test/examples for create assertion

> {-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
> module Database.HsSqlPpp.Extensions.CreateAssertionTests
>     (createAssertionExamples) where
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.PrettyPrinter
> import Database.HsSqlPpp.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote
> import Database.HsSqlPpp.Extensions.CreateAssertion
> import Database.HsSqlPpp.Annotation

>
> createAssertionExamples :: [ExtensionTest]
> createAssertionExamples = [cardinalityExample
>                           ,doubleCardinalityExample
>                           ,simpleViewExample
>                           ,simpleFunctionExample
>                           ,simpleMultiConstraint
>                           ,threewayMultiConstraint]


we don't check the constraints work yet, only that the apparently
correct ddl is generated

cardinality check
-----------------

accesses multiple rows from one table

> cardinalityExample :: ExtensionTest
> cardinalityExample  =
>   ExtensionTest
>     "CreateAssertion cardinality"
>     createAssertion
>     [$sqlStmts|

\begin{code}

create table test_table (
   field text
);

select create_assertion('test_table_count'
                       ,'(select count(*) from test_table) < 10');

\end{code}

>     |]
>     [$sqlStmts|

\begin{code}

create table test_table (
   field text
);

create function check_con_test_table_count() returns bool as $xxx$
begin
  return (select count(*) from test_table) < 10;
end;
$xxx$ language plpgsql stable;

create function test_table_constraint_trigger_operator() returns trigger as $xxx$
begin
  if not check_con_test_table_count() then
    raise exception 'update violates database constraint test_table_count';
  end if;
  return OLD;
end;
$xxx$ language plpgsql stable;

create trigger test_table_constraint_trigger
  after insert or update or delete on test_table
  for each statement
  execute procedure test_table_constraint_trigger_operator();

\end{code}

>      |]

double cardinality
------------------

accesses two tables

> doubleCardinalityExample :: ExtensionTest
> doubleCardinalityExample  =
>   ExtensionTest
>     "createAssertion double cardinality"
>     createAssertion
>     [$sqlStmts|

\begin{code}

create table test_table (
   field text
);

create table test_table1 (
   field text
);

select create_assertion('test_tables_count'
                       ,'((select count(*) from test_table) +
                          (select count(*) from test_table1)) < 10');

\end{code}

>     |]
>     [$sqlStmts|

\begin{code}

create table test_table (
   field text
);

create table test_table1 (
   field text
);

create function check_con_test_tables_count() returns bool as $xxx$
begin
  return ((select count(*) from test_table) +
          (select count(*) from test_table1)) < 10;
end;
$xxx$ language plpgsql stable;

create function test_table_constraint_trigger_operator() returns trigger as $xxx$
begin
  if not check_con_test_tables_count() then
    raise exception 'update violates database constraint test_tables_count';
  end if;
  return OLD;
end;
$xxx$ language plpgsql stable;

create trigger test_table_constraint_trigger
  after insert or update or delete on test_table
  for each statement
  execute procedure test_table_constraint_trigger_operator();

create function test_table1_constraint_trigger_operator() returns trigger as $xxx$
begin
  if not check_con_test_tables_count() then
    raise exception 'update violates database constraint test_tables_count';
  end if;
  return OLD;
end;
$xxx$ language plpgsql stable;

create trigger test_table1_constraint_trigger
  after insert or update or delete on test_table1
  for each statement
  execute procedure test_table1_constraint_trigger_operator();

\end{code}

>      |]

simpleview
----------

constraint on a view rather than a table

> simpleViewExample :: ExtensionTest
> simpleViewExample  =
>   ExtensionTest
>     "createAssertion simpleview"
>     createAssertion
>     [$sqlStmts|

\begin{code}

create table test_table (
   field text
);

create view test_view as
   select * from test_table where field <> "a";

select create_assertion('test_view_count'
                       ,'(select count(*) from test_view) < 10');

\end{code}

>     |]
>     [$sqlStmts|

\begin{code}

create table test_table (
   field text
);

create view test_view as
   select * from test_table where field <> "a";

create function check_con_test_view_count() returns bool as $xxx$
begin
  return (select count(*) from test_view) < 10;
end;
$xxx$ language plpgsql stable;

create function test_table_constraint_trigger_operator() returns trigger as $xxx$
begin
  if not check_con_test_view_count() then
    raise exception 'update violates database constraint test_view_count';
  end if;
  return OLD;
end;
$xxx$ language plpgsql stable;

create trigger test_table_constraint_trigger
  after insert or update or delete on test_table
  for each statement
  execute procedure test_table_constraint_trigger_operator();

\end{code}

>      |]

simpleFunction
--------------

constraint on the result of a function call

> simpleFunctionExample :: ExtensionTest
> simpleFunctionExample  =
>   ExtensionTest
>     "createAssertion simplefunction"
>     createAssertion
>     [$sqlStmts|

\begin{code}

create table test_table (
   field text
);

create function test_table_count() returns int as $$
  select count(*) from test_table;
$$ language sql stable;

select create_assertion('test_function_count'
                       ,'test_table_count() < 10');

\end{code}

>     |]
>     [$sqlStmts|

\begin{code}

create table test_table (
   field text
);

create function test_table_count() returns int as $$
  select count(*) from test_table;
$$ language sql stable;

create function check_con_test_function_count() returns bool as $xxx$
begin
  return test_table_count() < 10;
end;
$xxx$ language plpgsql stable;

create function test_table_constraint_trigger_operator() returns trigger as $xxx$
begin
  if not check_con_test_function_count() then
    raise exception 'update violates database constraint test_function_count';
  end if;
  return OLD;
end;
$xxx$ language plpgsql stable;

create trigger test_table_constraint_trigger
  after insert or update or delete on test_table
  for each statement
  execute procedure test_table_constraint_trigger_operator();

\end{code}

>      |]

multiple constraints one table
------------------------------

check that adding multiple constraints on one table does the right thing

accesses two tables

> simpleMultiConstraint :: ExtensionTest
> simpleMultiConstraint  =
>   ExtensionTest
>     "createAssertion simple multi"
>     createAssertion
>     [$sqlStmts|

\begin{code}

create table test_table (
   field text
);

select create_assertion('test_table_count'
                       ,'(select count(*) from test_table) < 10');

select create_assertion('test_table_stuff_count'
                       ,$$(select count(*) from test_table where field ='stuff') < 5$$);

\end{code}

>     |]
>     [$sqlStmts|

\begin{code}

create table test_table (
   field text
);

create function check_con_test_table_count() returns bool as $xxx$
begin
  return (select count(*) from test_table) < 10;
end;
$xxx$ language plpgsql stable;

create function test_table_constraint_trigger_operator() returns trigger as $xxx$
begin
  if not check_con_test_table_count() then
    raise exception 'update violates database constraint test_table_count';
  end if;
  return OLD;
end;
$xxx$ language plpgsql stable;

create trigger test_table_constraint_trigger
  after insert or update or delete on test_table
  for each statement
  execute procedure test_table_constraint_trigger_operator();

--

create function check_con_test_table_stuff_count() returns bool as $xxx$
begin
  return (select count(*) from test_table where field ='stuff') < 5;
end;
$xxx$ language plpgsql stable;

create or replace function test_table_constraint_trigger_operator() returns trigger as $xxx$
begin
  if not check_con_test_table_stuff_count() then
    raise exception 'update violates database constraint test_table_stuff_count';
  end if;
  if not check_con_test_table_count() then
    raise exception 'update violates database constraint test_table_count';
  end if;
  return OLD;
end;
$xxx$ language plpgsql stable;

\end{code}

>      |]

three constraints three tables
------------------------------

~~~~
table a constraints x y
table b constraints x z
table c constraints z x

createtable a
createtable b
createtable c

add constraint x a b
add constraint y b c
add constraint z c a
~~~~

if the test is harder to understand than the code it's testing, is
that bad?

> threewayMultiConstraint :: ExtensionTest
> threewayMultiConstraint =
>   ExtensionTest
>     "createAssertion three way multi"
>     createAssertion
>     [createTable "a"
>     ,createTable "b"
>     ,createTable "c"
>     ,constraint "x" "a" "b"
>     ,constraint "y" "b" "c"
>     ,constraint "z" "c" "a"]
>     [createTable "a"
>     ,createTable "b"
>     ,createTable "c"
>     ,check "x" "a" "b"
>     ,fn "a" "x"
>     ,trig "a"
>     ,fn "b" "x"
>     ,trig "b"
>     ,check "y" "b" "c"
>     ,fn1 "b" "x" "y"
>     ,fn "c" "y"
>     ,trig "c"
>     ,check "z" "c" "a"
>     ,fn1 "c" "y" "z"
>     ,fn1 "a" "x" "z"]
>     where
>       createTable :: String -> Statement
>       createTable n = let tablename = "table_" ++ n
>                       in [$sqlStmt|
>                         create table $(tablename) (
>                            field text
>                         ); |]
>       constraint :: String -> String -> String -> Statement
>       constraint nm t1 t2 = let cn = conname nm
>                                 exs = exprs t1 t2
>                             in [$sqlStmt|
>                     select create_assertion('$(cn)','$(exs)');
>                                 |]
>       conname nm = "valid_" ++ nm
>       exprs t1 t2 = printScalarExpr $ exprn t1 t2
>       exprn t1 t2 = let t1n = "table_" ++ t1
>                         t2n = "table_" ++ t2
>                     in [$sqlExpr|((select count(*) from $(t1n))
>                                + (select count(*) from $(t2n))) < 10 |]
>       check :: String -> String -> String -> Statement
>       check nm t1 t2 = let cfn = "check_con_valid_" ++ nm
>                            ex = exprn t1 t2
>                        in [$sqlStmt|
>                              create function $(cfn)() returns bool as $xxx$
>                                begin
>                                  return $(ex);
>                                end;
>                              $xxx$ language plpgsql stable;
>                            |]
>       fn :: String -> String -> Statement
>       fn t c = let nm = "table_" ++ t ++ "_constraint_trigger_operator"
>                    cn = FunCall ea ("check_con_valid_" ++ c) []
>                         --"check_con_valid_" ++ c
>                    errMsg = "update violates database constraint valid_" ++ c
>                in [$sqlStmt|
>                         create function $(nm)() returns trigger as $xxx$
>                         begin
>                           if not $(cn) then
>                             raise exception '$(errMsg)';
>                           end if;
>                           return OLD;
>                         end;
>                         $xxx$ language plpgsql stable;
>                    |]
>       fn1 t c1 c2 = let nm = "table_" ++ t ++ "_constraint_trigger_operator"
>                         c1n = FunCall ea ("check_con_valid_" ++ c1) []
>                         c2n = FunCall ea ("check_con_valid_" ++ c2) []
>                         errMsg1 = "update violates database constraint valid_" ++ c1
>                         errMsg2 = "update violates database constraint valid_" ++ c2
>                     in [$sqlStmt|
>                         create or replace function $(nm)() returns trigger as $xxx$
>                         begin
>                           if not $(c2n) then
>                             raise exception '$(errMsg2)';
>                           end if;
>                           if not $(c1n) then
>                             raise exception '$(errMsg1)';
>                          end if;
>                           return OLD;
>                         end;
>                         $xxx$ language plpgsql stable;
>                         |]
>       trig :: String -> Statement
>       trig t = let tn = "table_" ++ t
>                    trn = tn ++ "_constraint_trigger"
>                    cfn = tn ++ "_constraint_trigger_operator"
>                in [$sqlStmt|
>                    create trigger $(trn)
>                      after insert or update or delete on $(tn)
>                      for each statement
>                      execute procedure $(cfn)();
>                    |]

> ea :: Annotation
> ea = emptyAnnotation