Copyright 2010 Jake Wheat

The test/examples for extended constraints

> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables #-}
> module Database.HsSqlPpp.Examples.Extensions.ExtendedConstraintTests
>     (extendedConstraintExamples) where
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote
> import Database.HsSqlPpp.Examples.Extensions.ExtendedConstraints

> extendedConstraintExamples :: [ExtensionTest]
> extendedConstraintExamples = [cardinalityExample
>                              ,doubleCardinalityExample
>                              ,simpleViewExample
>                              ,simpleFunctionExample
>                              ,simpleMultiConstraint]

stage 1: some test cases for general constraints which aren't
implementable as postgresql constraints, we don't check the
constraints work, only that the apparently correct ddl is generated

cardinality check
-----------------

accesses multiple rows from one table

> cardinalityExample :: ExtensionTest
> cardinalityExample  =
>   ExtensionTest
>     "ExtendedConstraints cardinality"
>     extendedConstraints
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
>     "ExtendedConstraints double cardinality"
>     extendedConstraints
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
>     "ExtendedConstraints simpleview"
>     extendedConstraints
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
>     "ExtendedConstraints simplefunction"
>     extendedConstraints
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
>     "ExtendedConstraints simple multi"
>     extendedConstraints
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

drop function test_table_constraint_trigger_operator();
create function test_table_constraint_trigger_operator() returns trigger as $xxx$
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

