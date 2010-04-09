Copyright 2010 Jake Wheat

just some notes atm

Want to support extensions to sql. At the moment the extensions use
one of the following approaches:

overload function calls, so we write something like

select extension_fn(some, extension, args);

and the extension picks this up after parsing and converts it to
something else in the ast.

overload function calls with internal custom syntax, e.g.

select extension_fn($$

  custom extension syntax which is parsed at ast transform time

$$);

some of the extensions take other regular sql syntax, but change it
around slightly, e.g. the not null extension takes all attribute defs
in create table statements and adds a not null constraint to them.

Would like to support better range of custom syntax.

One approach could be:

Provide hooks in the lexer and parser to call custom syntax parsers at
certain points. E.g. provide an additional parser which can be used
when parsing a statement, so we can add new statement syntaxes. One
lightweight way of doing this is to make the custom syntax parser
output vanilla ast nodes, but this is very limiting since much of the
custom syntax I want to write needs more context to decide what sql to
ultimately generate.

The constraints we want to work with are: allow new custom syntaxes to
be added without changing the hssqlppp library source, and try to make
sure we don't lose any type safety for parsing and working with sql
without constraints.

I think adding lots of hook points in the parser, and exposing its
guts e.g. if someone wants to create a completely new statement
parser, but reuse most of the existing individual statement type
parsers like createtable and selectstatement.

On the ast side, the simple solution would be to parameterize the ast
type with a custom data type or types. If we just want to add new
statement types, we could add a new wrapper statement type which could
hold any data type to support custom syntaxes, e.g.

data Statement a = CreateTable
                 | SelectStatement
                 ...
                 | CustomStatement a

The downside to this is we've lost some type safety, so e.g. when
loading into pg, it's only at runtime when we hit a customstatement
which hasn't been transformed out that we error. But this is cheap and
cheerful and it's not too bad to live with.

Is there a better way? Put some work into avoiding this when adding
the antiquotes, which seemed to turn out quite well - leaving
antiquote nodes in the tree under normal usage causes a compile time
error.

Have vague recollection of someone altering a haskell regular
expression engine using something clever, without access to the
source, see if can find and is relevant.

If there is a better way, might be better for the antinode approach
compared to the slightly ducttapey current approach of generating the
ast antinodes - so parsing antiquote sql will use the extension system
rather than being hardcoded into the parser.
