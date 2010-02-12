Copyright 2010 Jake Wheat

silly name, idea is to match some sort of oo inheritance approach to
create a set of 6nf relvars, and then implement using a single table
with nulls, adding appropriate constraints to restrict which
combinations of nulls are possible, and add views which return subsets
with no nulls in

example idea:

this is the basic table we want to produce

~~~~{.sql}
create table piece_prototypes (
  ptype text primary key,
  flying boolean null,
  speed int null,
  agility int null,
  undead boolean null,
  ridable boolean null,
  ranged_weapon_type text,
  range int null,
  ranged_attack_strength int null,
  attack_strength int null,
  physical_defense int null,
  magic_defense int null
);
~~~~

these fields are group as follows

~~~~
proto
  ptype
creature
  flying
  speed
  agility
attacking
  attack_strength
attackable
  physical_defense
  magic_defense
ranged
  ranged_weapon_type
  range
monster
  undead
  ridable
~~~~

so if a row has a non null in one member of a group, they all must be non null (e.g. flying,speed, agility must all be null or all non null)

we also have group dependencies,e.g. if a row is monster,(i.e.  undead and ridable are non null), it also has to be a creature, attacking and attackable (so all their fields have to be non null)

want to describe the table in these terms and have:

* the full table created
* check constraints on combinations of null with nice error messages
* views which show restrict/projections without the nulls

see the examples file for more details

> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables #-}
> module Database.HsSqlPpp.Examples.Extensions.Denormalized6nf
>     (denormalized6nf) where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.SqlQuote
>
> denormalized6nf :: Data a => a -> a
> denormalized6nf =

approach: gather the relationships:
subclass, superclass, mutually_exclusive
find all the tables which are referred to
-> create the base tables and views
replace the first create table from each group with the create tables and views
wipe out the other create tables and fn calls

>     transformBi $ \x ->
>       case x of
>         [$sqlStmt| select create_var($s(varname), $s(typename)); |]
>             -> let tablename = varname ++ "_table"
>                in [$sqlStmt|
>
>   create table $(tablename) (
>    $(varname) $(typename)
>   );
>
>                    |]
>         x1 -> x1
>
