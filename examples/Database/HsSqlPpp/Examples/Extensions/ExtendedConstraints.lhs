Copyright 2010 Jake Wheat

Partially working.

Extended constraints
====================

Extended constraint system - idea is to use triggers to roughly create
constraints which can refer to multiple rows in a table or multiple
tables.

The constraints are implemented with:

* a table to hold the constraint names and expressions
* for each table which is involved in a constraint: a function which
  checks all the constraint expressions for that table, excluding ones
  which are implemented as pg constraints directly; and a trigger to
  call that function when the table changes

You also get a check_constraint function for each constraint which can
be run at any time. (including keys and sql fks); these aren't
actually called.

The constraints which are implemented by pg instead of these
functions/triggers are called accelerated constraints below.

issues
------

Since pg has no multiple updates, some sort of hack may be needed for
some updates in combination with some constraints.

I'm pretty sure the constraint system works fine as long as
* you never change the columns on a table after adding a constraint
* you only add constraints, never change or remove them
* all database transactions are run one at a time, serialised
  (actually serialised, not just using sql isolation serializable).

If any of these assumptions are broken, you might break your database,
load bad data in or get weird errors for stuff that should work.

Constraints that can't be implemented by pg are created with the
following syntax: can create a reference to a view or from a non key
column using regular pg syntax, this moves the foreign key constraint
out of the create table during the transform and implements it using a
trigger.

TBC: foreign keys without having to create a view first i.e. a literal
view expression in the constraint.

Primary keys, unique not nulls, regular foreign keys, and regular
check constraints are written in the usual way.

TBC - alternative key syntax to remove the distinction between primary key and unique not null?

TBC - will the extension deal with can check constraints

* referring to multiple rows in same table
* referring to other tables as well as this table
* produce an error if they only refer to other tables

(meaning check constraints inside a create table statement)

To create other sorts of constraints, currently uses a function call
based roughly on the rarely implemented sql syntax:

    create_assertion('constraint_name', 'expression as text');

TODO: maybe add a parser extension to parse:

    create assertion constraint_name check(expression);

Currently, no attempt is made to recognise a constraint that could be
implemented without a trigger when it is added using a
create_assertion call.

other ideas:

* require all constraints to have names
* add some syntax to say:
  * here are some example sets of relation values which should be accepted by the constraint
  * here are some which should not be accepted by the constraint
* add supplemental expressions for error reporting: so instead of
  saying x constraint failed, can run through the expressions one at a
  time and when one passes or fails or something can give a more
  useful error message depending on how it failed.

tests:

check running on empty statement list adds the dependencies:
create table database_constraints (
  constraint_name text,
  expression text
);
create function check_constraint(constraint_name) ...
what else is needed: plpgsql implementation used:
table dbcon_ops - saves the function per constraint to check that constraint
separate function to check each constraint
save the manual relvar list that the constraint references - now will
  generate this list using the parser automatically
-> dbcon_relvars - the relvars that each constraint references
saves information into a system_implementation_objects table
sort_out_constraint_name -> truncate the constraint name and make
  unique if there is an overlap -> this is for auto generated names
have some views to list details about the accelarated constraints from
the pg catalog: check_pg, key_pg, fk_pg

functions to implement accelerated constraints: set_pg_unique,
set_pg_check, set pg_fk

con_pg -> table to list accelerated constraints

db_con_trigger_ops dbcon_triggers: tables to list the triggers and
trigger functions that actually implement the constraints
non accelerated constraints view

regenerate constraint triggers function:
redoes _all_ the triggers and functions
drop all triggers in dbcon_triggers
drop the trigger ops
create a trigger function for each table, include all the constraints mentioning that table
create a trigger for each table

functions to add constraints:
add_key variants
add_foreign_key variants
constraint to zero or one tuple constraint generator

types of constraints: pg key - primary key or unique not null,
?assertion effective key pg foreign key, non pg foreign key (same
syntax, refer to view or from non key), ?assertion effective fk,
inline normal check, check refering to multiple rows, multiple columns
check variants in assertion syntax.
shortcuts for views: key and reference?


for each type of constraint:
check inserts into catalog are generated -> insert into database_constraints
check that the expression is checked at assertion creation time for non accelerated

Use regular extension tests and add a load of tests to exercise a
variety of non pg accelerated constraints and check successul updates
and constraint violation attempts.


/*
== constraints
*/
create table database_constraints (
  constraint_name text,
  expression text
);

=== ghetto test thing
want to write some tests for this constraint system just as a sanity
check for now:
arbitrary check e.g. cardinality < 5
arbitrary check multiple tables e.g. sum cardinality of two tables
check without acceleration?:
fk
fk to view
unique
x,y in board size range from another table

for each check:
check adding constraint to invalid tables throws
check adding constraint to valid tables OK
insert OK data into constrained tables
insert bad data into constrained tables

accelerated checks
fk to view
x,y in board size range
check acceleration for normal checks & fk without pg?

pg accelerated checks:
just check pg catalog to see if inserted
check
fk
unique

all todo: yes, that means there is no direct testing of any of the
constraint stuff...


select add_key('database_constraints', 'constraint_name');

select add_key('dbcon_ops', 'constraint_name');
select add_key('dbcon_ops', 'operator_name');
select add_foreign_key('dbcon_ops', 'constraint_name',
                       'database_constraints');
--select add_foreign_key('dbcon_ops', 'operator_name', 'operators');

select add_key('dbcon_relvars', array['constraint_name', 'relvar_name']);
select add_foreign_key('dbcon_relvars', 'constraint_name',
                       'database_constraints');
select add_foreign_key('dbcon_relvars', 'relvar_name', 'base_relvars');

select add_key('con_pg', 'constraint_name');
select add_foreign_key('con_pg', 'constraint_name', 'database_constraints');
select add_foreign_key('con_pg', 'constraint_name',
  '(select constraint_name from check_pg union
   select constraint_name from key_pg union
   select constraint_name from fk_pg) as x');

select add_key('dbcon_trigger_ops', 'operator_name');
select add_foreign_key('dbcon_trigger_ops', 'operator_name', 'operators');

select add_key('dbcon_triggers', 'trigger_name');
select add_foreign_key('dbcon_triggers', array['trigger_name', 'relvar_name'],
  'triggers');

----------------------------

> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Examples.Extensions.ExtendedConstraints
>     (extendedConstraintExamples
>     ,extendedExtensions) where
>
> --import Data.Generics
> import Data.Generics.Uniplate.Data
> import Debug.Trace
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Parser
> --import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote
> import Database.HsSqlPpp.Examples.Extensions.AstUtils

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
>     extendedExtensions
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
>     extendedExtensions
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
>     extendedExtensions
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
>     extendedExtensions
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
>     extendedExtensions
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


-------------------------------------------

implementation
==============

> extendedExtensions :: [Statement] -> [Statement]
> extendedExtensions ast =
>  flip transformBi ast $ \x ->
>       case x of
>         (funCallView -> FunCallView _
>                                     "create_assertion"
>                                     [StringLit _ _ name
>                                     ,StringLit _ _ exprText]):tl ->
>             makeConstraintDdl name exprText ++ tl
>         x1 -> x1
>  where
>    asti = getAstInfo ast
>    makeConstraintDdl :: String -> String -> [Statement]
>    makeConstraintDdl name exprText =
>      let expr = either (error . show) id
>                   $ parseExpression "" exprText
>      in makeCheckFn name expr
>         ++ extras name expr
>    --expr txt = either (error . show) id
>    --             $ parseExpression "" txt
>    {-makeCheckFn name exprText =
>        let checkfn = "check_con_" ++ name
>            expr1 = expr exprText
>        in [$sqlStmts|
>              create function $(checkfn)() returns bool as $xxx$
>              begin
>                return $(expr1);
>              end;
>              $xxx$ language plpgsql stable;
>            |]-}
>    extras :: String -> Expression -> [Statement]
>    extras name expr = concat $ flip concatMap (tableNames expr) $ \tn ->
>                  let ec = existingConstraints name tn
>                  in if null ec
>                     then [makeTriggerFn tn [name]
>                          ,makeTrigger tn]
>                     else [dropTriggerFn tn
>                          ,makeTriggerFn tn (name:ec)]
>    tableNames expr = let y = getReferencedTableList asti expr
>                      in trace (show y) y
>    existingConstraints name tn = if name == "test_table_stuff_count"
>                                  then ["test_table_count"]
>                                  else []
>
> dropTriggerFn :: String -> [Statement]
> dropTriggerFn tn = let opname = tn ++ "_constraint_trigger_operator"
>                    in [$sqlStmts| drop function $(opname)();|]

> makeCheckFn :: String -> Expression -> [Statement]
> makeCheckFn name expr =
>     let checkfn = "check_con_" ++ name
>     in [$sqlStmts|
>              create function $(checkfn)() returns bool as $xxx$
>              begin
>                return $(expr);
>              end;
>              $xxx$ language plpgsql stable;
>            |]

> makeTriggerFn :: String -> [String] -> [Statement]
> makeTriggerFn tn nms =
>   let trigopname = tn ++ "_constraint_trigger_operator"
>       ifs :: [Statement]
>       ifs = concatMap makeIf nms
>       -- using template approach cos can't get antistatements working
>       template = [$sqlStmts|

\begin{code}

create function $(trigopname)() returns trigger as $xxx$
begin
  null;
  return OLD;
end;
$xxx$ language plpgsql stable;

\end{code}

>      |]
>   in flip transformBi template $ \x ->
>        case x of
>              NullStatement _ : tl -> ifs ++ tl
>              x1 -> x1
>   where
>     makeIf nm = let chk = "check_con_" ++ nm
>                     callcheckfn = FunCall [] chk []
>                     errMsg = "update violates database constraint " ++ nm
>                 in [$pgsqlStmts|
>                    if not $(callcheckfn) then
>                       raise exception '$(errMsg)';
>                    end if;
>                    |]



> makeTrigger :: String -> [Statement]
> makeTrigger tn = let trigname = tn ++ "_constraint_trigger"
>                      opname = tn ++ "_constraint_trigger_operator"
>                  in [$sqlStmts|
>   create trigger $(trigname)
>     after insert or update or delete on $(tn)
>     for each statement
>     execute procedure $(opname)();
>                        |]



>  {-         triggers =
>                  flip concatMap tablenames $ \t ->
>                            let trigopname = t ++ "_constraint_trigger_operator"
>                                trigname = t ++ "_constraint_trigger"
>                            in [$sqlStmts| 

\begin{code}

if not $(callcheckfn) then
    raise exception '$(errMsg)';
  end if;

create function $(trigopname)() returns trigger as $xxx$
begin
  if not $(callcheckfn) then
    raise exception '$(errMsg)';
  end if;
  return OLD;
end;
$xxx$ language plpgsql stable;

create trigger $(trigname)
  after insert or update or delete on $(t)
  for each statement
  execute procedure $(trigopname)();

\end{code}

>                           |] -}


setInferredTypesG :: Data a => [Type] -> a -> a
setInferredTypesG tys x =
  evalState (transformBiM f x) tys
  where
    f (p@(Placeholder _)) = do
         y:ys <- get
         put ys
         return $ updateAnnotation (++ [InferredType y]) p
    f z = return z
