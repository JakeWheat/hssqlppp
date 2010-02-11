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

~~~~~
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
constraints
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
~~~~~

----------------------------

Tests/examples in extendedconstraintstests.lhs

one major thing that is missing is adding stuff to the catalog. This
means that only constraints that are all processed in one transform
will work, if you have two files, transform them seperately, then load
them, any constraints from the first file will be disabled for tables
which also have constraints in the second file.

> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables, TupleSections #-}
>
> module Database.HsSqlPpp.Examples.Extensions.CreateAssertion
>     (createAssertion) where
>
> --import Data.Generics
> import Data.Generics.Uniplate.Data
> --import Debug.Trace
> import Control.Monad.State
>
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.SqlQuote
> import Database.HsSqlPpp.Examples.Extensions.AstUtils

stage 1: some test cases for general constraints which aren't
implementable as postgresql constraints, we don't check the
constraints work, only that the apparently correct ddl is generated

idea: not too sure about how this all works, write a seperate bunch of
code to read all the constraints either out of the catalog or from the
source, then use the final version of the sql code, type check it, and
use the type check information to check all the constraints seem to
have been added ok.

implementation
==============

when we go through, need to record the constraints we've already
seen. using transformBiM with state monad, gives us the constraints in
reverse order of the statement list, so chuck three reverses in there
to make it work right.

> createAssertion :: [Statement] -> [Statement]
> createAssertion ast = reverse $
>  (\f -> evalState (transformBiM f (reverse ast)) ([] :: ConstraintRecord)) $ \x ->
>       case x of
>         [$sqlStmt| select create_assertion($s(name)
>                                           ,$s(exprtext));|] : tl -> do
>             existing <- get
>             let (new, rast) = makeConstraintDdl existing name exprtext
>             put new
>             return $ rast ++ tl
>         x1 -> return x1
>  where
>    asti = getAstInfo ast
>    makeConstraintDdl :: ConstraintRecord -> String -> String -> (ConstraintRecord, [Statement])
>    makeConstraintDdl cons name exprText =
>      let expr = either (error . show) id
>                   $ parseExpression "" exprText
>      in (newcons cons (tableNames expr) name
>         ,reverse (makeCheckFn name expr : extras cons name expr))
>    extras :: ConstraintRecord -> String -> Expression -> [Statement]
>    extras cons name expr = flip concatMap (tableNames expr) $ \tn ->
>                  let ec = existingConstraints tn cons
>                  in if null ec
>                     then [makeTriggerFn tn [name]
>                          ,makeTrigger tn]
>                     else [dropTriggerFn tn
>                          ,makeTriggerFn tn (name:ec)]
>    tableNames expr = let y = getReferencedTableList asti expr
>                      in y
>    newcons cons tns nm = foldr (uncurry (insertWith (++))) cons (map (,[nm]) tns)
>    existingConstraints tn cons = maybe [] id $ lookup tn cons
>
> type ConstraintRecord = [(String,[String])] -- tablename, list of constraint names
>
> dropTriggerFn :: String -> Statement
> dropTriggerFn tn = let opname = tn ++ "_constraint_trigger_operator"
>                    in [$sqlStmt| drop function $(opname)();|]
>
> makeCheckFn :: String -> Expression -> Statement
> makeCheckFn name expr =
>     let checkfn = "check_con_" ++ name
>     in [$sqlStmt|
>              create function $(checkfn)() returns bool as $xxx$
>              begin
>                return $(expr);
>              end;
>              $xxx$ language plpgsql stable;
>            |]
>
> makeTriggerFn :: String -> [String] -> Statement
> makeTriggerFn tn nms =
>   let trigopname = tn ++ "_constraint_trigger_operator"
>       ifs :: [Statement]
>       ifs = map makeIf nms
>       -- using template approach cos can't get antistatement -> [statement] working
>       template = [$sqlStmt|
>                   create function $(trigopname)() returns trigger as $xxx$
>                   begin
>                     null;
>                     return OLD;
>                   end;
>                   $xxx$ language plpgsql stable;
>                   |]
>   in flip transformBi template $ \x ->
>        case x of
>              NullStatement _ : tl -> ifs ++ tl
>              x1 -> x1
>   where
>     makeIf nm = let chk = "check_con_" ++ nm
>                     errMsg = "update violates database constraint " ++ nm
>                 in [$pgsqlStmt|
>                    if not $(chk)() then
>                       raise exception '$(errMsg)';
>                    end if;
>                    |]
>
> makeTrigger :: String -> Statement
> makeTrigger tn = let trigname = tn ++ "_constraint_trigger"
>                      opname = tn ++ "_constraint_trigger_operator"
>                  in [$sqlStmt|
>   create trigger $(trigname)
>     after insert or update or delete on $(tn)
>     for each statement
>     execute procedure $(opname)();
>                        |]
