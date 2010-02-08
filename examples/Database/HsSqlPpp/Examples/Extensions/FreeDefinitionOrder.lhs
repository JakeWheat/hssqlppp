Copyright 2010 Jake Wheat

~~~~
just some notes atm

idea is to allow writing ddl in a file in any order, then reorder the
statements so that pg will accept them

start with a list of statements

just stick to create table, view, type, function for now (add
sequence, trigger, index, etc.), plus basic crud (to add: copy, truncate)
- but the readonlytables extension should allow treating compile
time constant relations as ddl statements
also have extended constraints to consider.

first thing is to split foreign key constraints from the create
tables, since we may need to add the fk and table at different times.

then create immediate dependences list:
[(ddl type, ddl name, def, [(ddlname,type dependences)])]
then use some lib to turn this into a graph and find some sort of
order. no idea what this is called but pretty sure is a standard and
basic operation.
~~~~
