Copyright 2010 Jake Wheat

Currently, just some notes.

Idea is to support tables with some null columns - maybe extend to
support the creation of these tables and adding constraints to say
which combinations of null are ok, and to add views that don't have
nulls in them. This is like a 'denormalised 6nf' thing, since proper
6nf tables are hard to understand and work with, and require
many gratuitous joins also.

To initialise a table like this/ or insert into it, you can end up
with pretty unreadable code, so create a syntax which is like:
field1: value1
field2: value2
etc., and convert these into inserts or copy statements.

if you look at the copy piece_prototypes_mr statement in server.sql in
the testfiles folder you can see how ugly and und readable a regular
copy looks, there are a few others in there as well.

For the time being, I think this is only going to be used with read
only tables to support the value of a constant relation in the
database, which is set when the database is created and then never
altered.

Maybe combine with readonly tables extension?

