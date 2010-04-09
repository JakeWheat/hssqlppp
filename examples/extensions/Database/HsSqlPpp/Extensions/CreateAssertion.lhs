Copyright 2010 Jake Wheat

sketch
------

something like an implementation of the rarely implemented create
assertion sql syntax

The basic approach is to analyze the constraint expression to get a
list of ultimately reference tables, then:

create a check function which returns a bool to say whether the
expression is true,

create a trigger and trigger function for each table which calls all
the check functions who reference this table and raise if any are
false

when we add a constraint which references a table with an existing
constraint, we add the new check function, and create or replace the
trigger function with the new check function added to the existing
ones, which avoids having to also drop and create the trigger

could add an optimisation to say that if there are no crud statements
inbetween two constraints, we can skip creating the first trigger
function, and change create or replace to create on the second one,
and move the add trigger down. Also - want to try to limit any crud to
using the table values extension, which may be relevant to this.

todo/ issues
------------

add catalog entries. not null and key constrainsts are part of a
table, but the regular check and foreign key constraints possibly
could appear in assertion catalog along with the constraints added by
this extension

the logic which determines which tables are referenced by an
expression is a bit dodgy: it only copes with all the create
assertions being set in one ast, so we can load one file, then load
another file later and it do the right thing.

this logic is also wrong for functions - it doesn't take into account
function overloading, to fix this need to type check to determine
which exact functions are called, might be a bit tricky, since we run
extensions on asts which only neccessarily type check after all the
extensions have been applied.

Since pg has no multiple updates, some sort of hack may be needed for
some updates in combination with some constraints.

I'm pretty sure the constraint system works fine as long as
* you never change the columns on a table after adding a constraint
* you only add constraints, never change or remove them
* all database transactions are run one at a time, serialised
  (actually serialised, not just using sql isolation serializable).

If any of these assumptions are broken, you might break your database,
load bad data in or get weird errors for stuff that should work.

foreign keys without having to create a view first i.e. a literal
view expression in the constraint.

alternative key syntax to remove the distinction between primary key
and unique not null?

could add support for check constraints referring to multiple rows in
same table? don't want to allow check constraints which refer to other
tables. Maybe go further: following tutorial d, only allow key and not
null constraints in create table or alter table, and move chek and
foreign keys out to create assertions or shorthand wrappers? Inline
constraints can be more readable though?

maybe add a parser extension to parse:

    create assertion constraint_name check(expression);

add some syntax to say:
not too sure about how this all works, write a seperate bunch of
code to read all the constraints either out of the catalog or from the
source, then use the final version of the sql code, type check it, and
use the type check information to check all the constraints seem to
have been added ok.

* here are some example sets of relation values which should be
  accepted by the constraint
* here are some which should not be accepted by the constraint

and have a way to check these (this stuff goes in the client program
using the constraints).

add supplemental expressions for error reporting: so instead of
  saying x constraint failed, can run through the expressions one at a
  time and when one passes or fails or something can give a more
  useful error message depending on how it failed.

~~~~
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
~~~~~

not too sure about how this all works, write a seperate bunch of
code to read all the constraints either out of the catalog or from the
source, then use the final version of the sql code, type check it, and
use the type check information to check all the constraints seem to
have been added ok.

----------------------------

Tests/examples in CreateAssertionTests.lhs

> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables, TupleSections #-}
>
> module Database.HsSqlPpp.Extensions.CreateAssertion
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
> import Database.HsSqlPpp.Extensions.AstUtils
> import Database.HsSqlPpp.Annotation

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
>         s@[$sqlStmt| select create_assertion($s(name)
>                                           ,$s(exprtext));|] : tl -> do
>             existing <- get
>             let (new, rast) = makeConstraintDdl existing name exprtext
>             put new
>             return $ replaceSourcePos s rast ++ tl
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
>                     then [makeTriggerFn False tn [name]
>                          ,makeTrigger tn]
>                     else [makeTriggerFn True tn (name:ec)]
>    tableNames expr = let y = getReferencedTableList asti expr
>                      in y
>    newcons cons tns nm = foldr (uncurry (insertWith (++))) cons (map (,[nm]) tns)
>    existingConstraints tn cons = maybe [] id $ lookup tn cons
>
> type ConstraintRecord = [(String,[String])] -- tablename, list of constraint names
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
> makeTriggerFn :: Bool -> String -> [String] -> Statement
> makeTriggerFn r tn nms =
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
>       rep = if r
>             then transformBi $ \x ->
>                    case x of
>                           NoReplace -> Replace
>                           x1 -> x1
>             else id
>   in flip transformBi (rep template) $ \x ->
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
