/*

Copyright 2009 Jake Wheat

= Overview

catalog
constraints
modules
utils

= Introduction

This file contains extensions to support some extra relational theory
stuff that pg doesn't support and also add some other things like
modules.

Probably the only interesting bit is the constraint system.


================================================================================

= Catalog

Some new catalog views to use, supposed to be a bit more straight
forward than the sql or pg catalogs.


== system implementation objects

Quite a lot of the code in this file generates extra functions,
triggers and uses extra tables. When you're browsing the catalog,
e.g. as a designer of a schema, or trying to work out the data
structures for a program, you don't usually want to see all this extra
stuff, so tag them.

Also, objects in this table should not be visible to user code, only
to the code in this file (just have to pretend for now).

Implementation objects include objects which are part of the catalog
and extensions themselves as well as objects which are generated when
user code uses the extensions.

Should probably put the catalog in a separate module to the internal
objects.

*/

create table system_implementation_objects (
  object_name text,
  object_type text check (object_type in(
                   'scalar',
                   'base_relvar',
                   'operator',
                   'view',
                   'trigger',
                   'database_constraint'))
);
insert into system_implementation_objects (object_name, object_type)
  values ('system_implementation_objects', 'base_relvar');

create view base_relvars as
  select relname as relvar_name from pg_class where relnamespace =
    (select oid from pg_namespace where nspname = 'public')
    and relkind = 'r';

create view base_relvar_attributes as
  select attname as attribute_name,
         typname as type_name,
         relname as relvar_name
    from pg_attribute inner join pg_class on (attrelid = pg_class.oid)
    inner join pg_type on (atttypid = pg_type.oid)
    inner join base_relvars on (relname = base_relvars.relvar_name)
    where attnum >= 1;

/*
scalars here since we are using the base relvar attributes table
to try to only show scalar types which are used and not the vast
array that pg comes with. This is a bit of a hack job, probably
a bit inaccurate
*/
create view scalars as
--   select typname as scalar_name from pg_type
--     where typtype in ('b', 'd')
--     and typnamespace =
--     (select oid from pg_namespace where nspname='public')
--   union
  select distinct type_name as scalar_name
    from base_relvar_attributes;

create view base_relvar_keys as
  select conname as constraint_name, relvar_name
    from pg_constraint
  natural inner join
    (select oid as conrelid, relname as relvar_name from pg_class) as b
  where contype in('p', 'u') and connamespace =
    (select oid from pg_namespace where nspname='public');

create view base_relvar_key_attributes as
  select constraint_name, attribute_name from
    (select conname as constraint_name, conrelid,
      conkey[generate_series] as attnum
      from pg_constraint
      cross join generate_series(1,
        (select max(array_upper(conkey, 1)) from pg_constraint))
      where contype in('p', 'u') and connamespace =
        (select oid from pg_namespace where nspname='public')
      and generate_series between
      array_lower(conkey, 1) and
      array_upper(conkey, 1)) as a
  natural inner join
    (select oid as conrelid, relname as relvar_name from pg_class) as b
  natural inner join
    (select attrelid as conrelid, attname as attribute_name,
    attnum from pg_attribute) as c
--  order by constraint_name
  ;

create view operators as
  select proname as operator_name from pg_proc
    where pronamespace = (select oid from pg_namespace
                           where nspname = 'public');

create view operator_source as
  select proname as operator_name, prosrc as source from pg_proc
    where pronamespace = (select oid from pg_namespace
                           where nspname = 'public');

create view triggers as
  select relname as relvar_name, tgname as trigger_name,
    proname as operator_name
    from pg_trigger
    inner join pg_class on (tgrelid = pg_class.oid)
    inner join pg_proc on (tgfoid = pg_proc.oid)
    inner join base_relvars on (relname = base_relvars.relvar_name)
    where not tgisconstraint; -- eliminate pg internal triggers

create view views as
  select viewname as view_name, definition
    from pg_views
    where schemaname = 'public';

create view view_attributes as
  select attname as attribute_name,
         typname as type_name,
         relname as relvar_name
    from pg_attribute inner join pg_class on (attrelid = pg_class.oid)
    inner join pg_type on (atttypid = pg_type.oid)
    inner join views on (relname = view_name)
    where attnum >= 1;

/*
== constraints
*/
create table database_constraints (
  constraint_name text,
  expression text
);
/*
== all database objects
*/
create view all_database_objects as
  select 'scalar' as object_type,
    scalar_name as object_name from scalars
  union select 'base_relvar' as object_type,
    relvar_name as object_name from base_relvars
  union select 'operator' as object_type,
    operator_name as object_name from operators
  union select 'view' as object_type,
    view_name as object_name from views
  union select 'trigger' as object_type,
    trigger_name as object_name from triggers
  union select 'database_constraint' as object_type,
    constraint_name as object_name from database_constraints;
insert into system_implementation_objects
  (object_name, object_type) values
  ('all_database_objects', 'view');
create view public_database_objects as
  select object_name,object_type from all_database_objects
  except
  select object_name,object_type from system_implementation_objects;

create view object_orders as
  select 'scalar'::text as object_type, 0 as object_order
  union select 'database_constraint', 1
  union select 'base_relvar', 2
  union select 'operator', 3
  union select 'view', 4
  union select 'trigger', 5
;
/*
================================================================================

= Constraints

Add some extra stuff to the existing pg constraints for the following:

* to support transition constraints directly

* to support constraints which refer to more than one table or row, or
  to views

Both of these are implemented in the usual way using triggers, but
using some shorthands, and the plumbing is hidden to some extent.

There are shortcuts for creating keys and foreign keys which use
postgresql constraints where possible to speed things up.

You have to supply the list of base tables that the triggers will
attach to, so if a constraint involves a view you have to work out the
base tables by hand. This could easily be automated to some extent
with a parser.

The constraints are implemented with

* a table to hold the constraint names and expressions
* a regenerate constraint function which creates:
  * a function which checks all the constraint expressions for a given
    table (excluding ones which are implemented as pg constraints
    directly)
  * a trigger to call that function when the table changes

The regenerate function is called whenever a constraint is added to
the database.

You also get a check_constraint function for each constraint which can
be run at any time. (including keys and sql fks), but unless you want
to sanity check a constraint these are never actually used.

We load candidate key and sql style foreign keys into this constraint
system, but instead of using the constraint check functions to enforce
these, we just load them in as regular pg unique not null and foreign
keys. When a constraint is implemented this way, it's called an
accelerated constraint in the code below

== issues

Since pg has no multiple updates, it may be necessary to have a hack
to disable constraints temporarily. The reenable function will have to
make sure the current data is good. This isn't implemented here yet
but some constraints in the server code use a hack based on this idea.

I'm pretty sure the constraint system works fine as long as
* you never change the columns on a table after adding a constraint
* you only add constraints, never change or remove them
* all database transactions are run one at a time, serialised
  (actually serialised, not just using sql isolation serializable).

If any of these assumptions are broken, you will probably break the
database, load bad data in or just get weird errors for stuff that
should work.

Plan for supporting database updates in the field is to export the
data in the database being upgraded, recreate the new database from
scratch, then load the export back in.

== public interface
*/

--this is the public function which you call to add a constraint
create function add_constraint(vname text,
                               vexpression text,
                               vrelvars text[]
                               ) returns void as $$
begin
  perform add_constraint_internal(vname,vexpression,vrelvars, false);
end;
$$ language plpgsql volatile;

/*
=== internal
create this function so that we can add accelerated constraints and
normal ones in the same place.

*/
create function add_constraint_internal(vname text, vexpression text,
       vrelvars text[], is_con_pg boolean) returns void as $$
declare
  r boolean;
  i integer;
  t text;
  constraint_operator text;
  s text;
begin
  --check the constraint is valid, cannot add it if it doesn't currently
  --evaluate to true
  --raise notice '******* adding constraint: %****%', vname, vexpression;
  if not is_con_pg then
    execute 'select ' || vexpression into r;
    if not r then
      raise exception
        'attempt to add constraint % which evaluates to false: %',
        vname, vexpression;
    end if;
  end if;
  --make entry in catalog
  insert into database_constraints (constraint_name, expression)
    values (vname, vexpression);
   constraint_operator := 'check_con_' || vname;
  --create implementation and add to catalog
  --we do this even if we actually implement it using
  --an accelerator

  --this function checks that the constraint currently holds
  execute 'create function ' || constraint_operator ||
    '() returns boolean as $a$
begin
  return ' || vexpression || ';
end;
$a$ language plpgsql stable;';
  --hide the check function from the user catalog
  insert into system_implementation_objects(object_name, object_type)
    values (constraint_operator, 'operator');
  --store the check function in the internal constraint implementation catalog
  execute $a$insert into dbcon_ops
    (constraint_name, operator_name) values ('$a$ || vname || $a$', '$a$
      || constraint_operator || $a$');$a$;
  --add entries into constraint_relvars: so we can get a list of
  --constraint implementation functions for a given table
  if not is_con_pg then
    for i in array_lower(vrelvars, 1)..array_upper(vrelvars, 1) loop
     -- todo: if relvar is a view, then need to trigger on
     -- view definition change and on any relvars which the view
     -- depends on change
     -- for now just skip views?
      insert into dbcon_relvars (constraint_name, relvar_name)
        values (vname, vrelvars[i]);
    end loop;
    --now recreate the actual triggers which enforce the constraint
    perform regenerate_constraint_triggers();
  end if;

  --hack: if the constraint mentions any tables which are
  -- system_implementation_objects then add the constraint as a
  -- system_implementation_object
  -- this is also to hide implementation details from the user catalog

  if exists(select 1 from system_implementation_objects
    where object_type = 'base_relvar' and
      object_name = any (vrelvars)) then
    insert into system_implementation_objects (object_name, object_type)
      values (vname, 'database_constraint');
  end if;
end;
$$ language plpgsql volatile;


/*
== constraint shortcuts

=== keys

*/

create function add_key(vtable text, attr text[]) returns void as $$
declare
  cn text;
begin
  cn := vtable || '_' ||
    array_to_string(attr, '_') || '_key';
  cn := sort_out_constraint_name(cn, '_key');
  perform add_constraint_internal(cn,
$a$(select count(*) from $a$ || vtable || $a$ ) =
(select count(distinct $a$ || array_to_string(attr, ', ') || $a$)
from $a$ || vtable || $a$)$a$,
     array[vtable], true);
  perform set_pg_unique(cn, vtable, attr);
end;
$$ language plpgsql volatile;

-- shortcut for a key on one attribute
create function add_key(vtable text, attr text) returns void as $$
begin
  perform add_key(vtable, array[attr]);
end;
$$ language plpgsql volatile;

/*
=== fk/ references

The system generalises foreign keys
to a generic 'the tuples in the select expression must be a subset of
the tuples in this other select expression'. This allows
foreign keys to views, and to non key columns.

*/
-- this function is used in the add foreign key function
create function attrs_are_key(text, text[]) returns boolean as $$
  select exists
  (select constraint_name, count(attribute_name)
    from base_relvar_keys
    natural inner join base_relvar_key_attributes
    where relvar_name = $1
    group by constraint_name
  intersect
  select constraint_name, count(attribute_name)
    from base_relvar_keys
    natural inner join base_relvar_key_attributes
    where relvar_name = $1
      and attribute_name = any ($2)
    group by constraint_name);
$$ language sql stable;
insert into system_implementation_objects
  (object_name,object_type) values
  ('attrs_are_key', 'operator');

-- foreign key to view/expression shortcut
create function add_foreign_key(vtable text,
                                 src_attr text[],
                                reftable text,
                                ref_attr text[])
  returns void as $$
declare
  i int;
  vcn text;
  bt text[];
  accel boolean;
begin
/*

automatically generate a name for this constraint in the catalog at
some point, you'll have to provide a name for every constraint
manually (plus some other stuff, like error message variants and
documentation to display in end user programs for user friendliness)

*/
  vcn := vtable || '_' || array_to_string(src_attr, '_') || '_fkey';
  vcn := sort_out_constraint_name(vcn, '_fkey');
  -- make sure we get a unique name in case there are multiple foreign keys
  -- on one set of attributes
  --TODO: rewrite this to use max instead of looping
  -- if target is a base relvar and the target attributes are a key then
  -- pg accelerate it
  if exists(select 1 from database_constraints where
        constraint_name = vcn) then
    i := 1;
-- I must be going mad cos I can't find the convert
-- int to string in base 10 function in pg...
    while exists(select 1 from database_constraints where
      constraint_name = vcn || to_hex(i)) loop
      i := i + 1;
    end loop;
    vcn := vcn || to_hex(i);
  end if;
  --now try to automatically determine the list of base relvars
  --that this constraint depends on
  -- I think this means that references involving views are a bit
  --broken...
  --TODO: get to the bottom of this, and fix it if necessary
  if exists(select 1 from base_relvars
      where relvar_name = reftable) then
    bt := array[vtable, reftable];
  else
    bt := array[vtable];
    raise notice 'fk on % ignoring view %', vtable, reftable;
  end if;

  accel := attrs_are_key(reftable, ref_attr);
  -- add constraint
  perform add_constraint_internal(vcn, 'not exists
(select ' || array_to_string(src_attr, ', ') ||
  ' from ' || vtable || '
  except
select ' || array_to_string(ref_attr, ', ') ||
  ' from ' || reftable || ')',
bt, accel);
  if accel then
    perform set_pg_fk(vcn, vtable, src_attr, reftable, ref_attr);
  end if;

end;
$$ language plpgsql volatile;


-- add foreign key shortcuts for special cases:
--same attributes
create function add_foreign_key(vtable text, src_attr text[], reftable text)
  returns void as $$
begin
  perform add_foreign_key(vtable, src_attr, reftable, src_attr);
end;
$$ language plpgsql volatile;
-- one attribute
create function add_foreign_key(vtable text,
                                 src_attr text,
                                reftable text,
                                ref_attr text)
  returns void as $$
begin
  perform add_foreign_key(vtable, array[src_attr], reftable, array[ref_attr]);
end;
$$ language plpgsql volatile;

-- same one attribute
create function add_foreign_key(vtable text, src_attr text, reftable text)
  returns void as $$
begin
  perform add_foreign_key(vtable, array[src_attr], reftable, array[src_attr]);
end;
$$ language plpgsql volatile;

/*
=== upto 1 tuple
table cardinality 0 or 1 constraint
don't know if there is a better way to do this

*/

create function constrain_to_zero_or_one_tuple(table_name text)
  returns void as $$
begin
  execute $a$select add_constraint('$a$ || table_name || $a$_01_tuple',
    '(select count(*) from $a$ || table_name || $a$) <= 1',
    array['$a$ || table_name || $a$']);$a$;
end;
$$ language plpgsql volatile;

/*

cascade: add cascade to a fk relationship from one base relvar to
another base relvar (implemented using pg on update cascade on delete
cascade)

== internals

each expression gets wrapped up as a function:
every check including those that are accelerated are
 in this table so they can be sanity checked.
 these operators are not used when checking table updates

so this table contains one function per user constraint
the function can be used to check the constraint holds
(without using any acceleration)
*/

create table dbcon_ops (
  constraint_name text,
  operator_name text
);
insert into system_implementation_objects(object_name, object_type)
  values ('dbcon_ops', 'base_relvar');

/*
this is the catalog bit that tells us which tables are referenced
by which constraints. In particular, this is used when generating
the trigger for a table which enforces all the constraints.

*/
create table dbcon_relvars (
  constraint_name text,
  relvar_name text
);
insert into system_implementation_objects(object_name, object_type)
  values ('dbcon_relvars', 'base_relvar');

create function sort_out_constraint_name(cn text,suffix text)
  returns text as $$
begin
  --truncate the name if too long
  --make sure it's unique
  --preserve the suffix
  if length(cn) > 54 then
    --wtf kind of syntax is this?!
    return substring(cn from 0 for (54 - length(suffix))) || suffix;
  else
    return cn;
  end if;
end;
$$ language plpgsql volatile;

/*
=== acceleration ish
==== views for postgresql stuff
track all the pg accelerated constraints
constraint_name, relvar_name, expression
*/
create view check_pg as
  select conname as constraint_name, relname as relvar_name,
-- currently displayed as 'CHECK((expression))' for some reason.
    pg_get_constraintdef(pg_constraint.oid) as expression
    from pg_constraint
  inner join pg_class on (conrelid = pg_class.oid)
    where contype = 'c' and connamespace =
      (select oid from pg_namespace where nspname='public');
insert into system_implementation_objects(object_name, object_type)
  values ('check_pg', 'view');

/*
This view shows all the pg accelerated key constraints :
constraint_name, relvar_name, attribute_name
*/
create view key_pg as -- not normalised
  select constraint_name, relvar_name, attribute_name from
    (select conname as constraint_name, conrelid,
      conkey[generate_series] as attnum
      from pg_constraint
      cross join generate_series(1,
        (select max(array_upper(conkey, 1)) from pg_constraint))
      where contype in('p', 'u') and connamespace =
        (select oid from pg_namespace where nspname='public')
      and generate_series between
      array_lower(conkey, 1) and
      array_upper(conkey, 1)) as a
      -- 'a' contains what we want, but with oids for
      -- relvar and attribute names
      -- natural join in these using renames
      -- using natural joins and renames makes
      -- the sql code much clearer
  natural inner join
    (select oid as conrelid, relname as relvar_name from pg_class) as b
  natural inner join
    (select attrelid as conrelid, attname as attribute_name,
    attnum from pg_attribute) as c
--  order by constraint_name
  ;
insert into system_implementation_objects(object_name, object_type)
  values ('key_pg', 'view');

/*
pg accelerated foreign keys

constraint_name, relvar_name, attribute_name,
target_relvar_name, target_attribute_name

*/

create view fk_pg as -- not normalised
  select constraint_name, relvar_name, attribute_name,
    target_relvar_name, target_attribute_name from
    (select conname as constraint_name, conrelid, confrelid,
      conkey[generate_series] as attnum,
      confkey[generate_series] as fattnum
      from pg_constraint
      cross join generate_series(1,
        (select max(array_upper(conkey, 1)) from pg_constraint))
      where contype = 'f' and connamespace =
        (select oid from pg_namespace where nspname='public')
      and generate_series between
      array_lower(conkey, 1) and
      array_upper(conkey, 1)) as a
  natural inner join
    (select oid as conrelid,
            relname as relvar_name from pg_class) as b
  natural inner join
    (select oid as confrelid,
            relname as target_relvar_name from pg_class) as c
  natural inner join
    (select attrelid as conrelid, attname as attribute_name,
      attnum from pg_attribute) as d
  natural inner join
    (select attrelid as confrelid, attname as target_attribute_name,
      attnum as fattnum from pg_attribute) as e
--  order by constraint_name
  ;
insert into system_implementation_objects(object_name, object_type)
  values ('fk_pg', 'view');
/*
==== functions to activate pg stuff
These add the pg constraint, and insert into the
accelerated constraint catalog relvar
*/
create function check_constraint_name(cn text) returns void as $$
begin
  if length(cn) > 54 then
    raise exception
      'pg constraint names must be 54 chars or less, you have % (%)',
      length(cn), cn;
  end if;
end;
$$ language plpgsql volatile;

create function set_pg_unique
  (vconstraint_name text, vrelvar_name text, vattributes text[])
  returns void as $$
begin
  perform check_constraint_name(vconstraint_name);
  execute 'alter table ' || vrelvar_name ||
    ' add constraint ' || vconstraint_name || ' unique(' ||
   array_to_string(vattributes, ', ') || ');';
  insert into con_pg(constraint_name)
    values(vconstraint_name);
end;
$$ language plpgsql volatile;

insert into system_implementation_objects(object_name, object_type)
  values ('set_pg_unique', 'operator');

create function set_pg_check
  (vconstraint_name text, vrelvar_name text, vexpression text)
  returns void as $$
begin
  perform check_constraint_name(vconstraint_name);
  execute 'alter table ' || vrelvar_name || 'add constraint '
    || vconstraint_name || ' check(' || vexpression || ');';
  insert into con_pg(constraint_name)
    values(vconstraint_name);
end;
$$ language plpgsql volatile;
insert into system_implementation_objects(object_name, object_type)
  values ('set_pg_check', 'operator');

create function set_pg_fk
  (vconstraint_name text, vrelvar_name text, vattributes text[],
   vtarget_relvar_name text, vtarget_attributes text[]) returns void as $$
begin
  perform check_constraint_name(vconstraint_name);
  execute 'alter table ' || vrelvar_name || ' add constraint '
     || vconstraint_name || ' foreign key(' ||
    array_to_string(vattributes, ',') || ') references ' ||
    vtarget_relvar_name || '(' ||
    array_to_string(vtarget_attributes, ',') ||
--just uses cascade for now, revisit this decision at some point.
    ') on update cascade on delete cascade;';

  insert into con_pg(constraint_name)
    values(vconstraint_name);
end;
$$ language plpgsql volatile;
insert into system_implementation_objects(object_name, object_type)
  values ('set_pg_fk', 'operator');


/*

===== internal catalog for pg ish
*/
-- save which constraints have pg accelerators
create table con_pg (
  constraint_name text
);
insert into system_implementation_objects(object_name, object_type)
  values ('con_pg', 'base_relvar');

/*
==== general constraint implementation

remember trigger functions used to enforce constraints
all trigger functions have same sig so just record name
*/
create table dbcon_trigger_ops (
  operator_name text
);
insert into system_implementation_objects(object_name, object_type)
  values ('dbcon_trigger_ops', 'base_relvar');
-- remember triggers used for constraints
  --triggers have to be dropped with name and table so record both
create table dbcon_triggers (
  trigger_name text,
  relvar_name text
);
insert into system_implementation_objects(object_name, object_type)
  values ('dbcon_triggers', 'base_relvar');

/* trigger code
atm it redoes all of them whenever anything changes.
Constraints are not changed much so this should be ok, possibly it
slows down the reset process.

Take all the constraints
exclude ones implemented by pg accelerators
then for each table referenced by the remaining constraints
* generate a function which checks all the
(non pg accel'd) constraints for that table and raises if any aren't true
* generate a trigger to call the function whenever that
table changes

*/
create view non_accelerated_constraints as
select relvar_name,constraint_name,expression
       from dbcon_relvars
       natural inner join database_constraints
    where constraint_name not in
      (select constraint_name from con_pg);

insert into system_implementation_objects(object_name, object_type)
  values ('non_accelerated_constraints', 'view');

create function regenerate_constraint_triggers() returns void as $$
declare
  r record;
  s record;
  f text;
  table_trigger_function text;
  table_trigger text;
begin
  --clean up old triggers and trigger functions
  --store names in table so they can be dropped
  for r in select * from dbcon_triggers loop
    execute 'drop trigger ' || r.trigger_name || ' on '
      || r.relvar_name || ';';
    delete from system_implementation_objects
      where object_name=r.trigger_name
        and object_type='trigger';
  end loop;
  delete from dbcon_triggers;

  for r in select * from dbcon_trigger_ops loop
    execute 'drop function ' || r.operator_name || '();';
    delete from system_implementation_objects
      where object_name=r.operator_name
        and object_type='operator';
  end loop;
  delete from dbcon_trigger_ops;

/*
if constraints are handled by pg constraints, then we
just ignore them here - don't need any operators or triggers
for them.
*/
  for r in select distinct(relvar_name) from non_accelerated_constraints loop
    -- for this table create a trigger function which calls all the relevant
    -- constraint check operators
    table_trigger_function := r.relvar_name || '_constraint_trigger_operator';
    table_trigger := r.relvar_name || '_constraint_trigger';
    --it's a bit tricky following this
    -- start the function definition
    f := $f$create function $f$ || table_trigger_function ||
$f$() returns trigger as $a$
begin
--    raise notice 'in constraint op for $f$ || r.relvar_name || $f$';
$f$;
    -- loop through the constraints
    for s in select distinct constraint_name, expression
             from non_accelerated_constraints
             where relvar_name = r.relvar_name loop
      -- and add a line for each one which checks if
      --it is true, otherwise raises
      f := f  ||
$f$  if not $f$ || s.expression  || $f$ then
    raise exception
      'value violates database constraint "$f$ || s.constraint_name || $f$"';
  end if;
$f$;
    end loop;
    f := f ||
$f$
--  raise notice 'complete constraint op for $f$ || r.relvar_name || $f$';
  return OLD;
end;
$a$ language plpgsql;$f$;
    --create the function
    execute f;
    insert into system_implementation_objects(object_name, object_type)
      values (table_trigger_function, 'operator');

    --now create the trigger to call the function
    execute
$f$create trigger $f$ || table_trigger ||
$f$ after insert or update or delete on $f$ || r.relvar_name ||
$f$ for each statement execute procedure $f$ || table_trigger_function ||
$f$();$f$;
    insert into system_implementation_objects(object_name, object_type)
      values (table_trigger, 'trigger');
    insert into dbcon_triggers
      (trigger_name, relvar_name)
      values(table_trigger, r.relvar_name);
    insert into dbcon_trigger_ops
      (operator_name)
      values(table_trigger_function);
  end loop;
end;
$$ language plpgsql volatile;
insert into system_implementation_objects(object_name, object_type)
  values ('regenerate_constraint_triggers', 'operator');

/*
todo: static check - if there are constraints that fisher price my first
constraints system doesn't know about then complain, add this as
a sql test i.e. check_blah returns bool
*/
-- add constraints to the tables already mentioned
--which we can't do in the right place since we have to
--wait for the constraint system to be defined

select add_key('system_implementation_objects',
        array['object_name', 'object_type']);

--comment this one out since it triples the reset speed
--select add_foreign_key('system_implementation_objects',
--       array['object_name', 'object_type'],
--       'all_database_objects');
--run it as a test so at least we keep checking it
create function check_code_slow_si_objects_constraints() returns boolean as $$
begin
  if not exists
     (select object_name, object_type from system_implementation_objects
      except
      select object_name, object_type from all_database_objects) then
    return true;
  else
    return false;
  end if;
end;
$$ language plpgsql volatile;


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

/*
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

=== transition constraints

quickly hacked together. at the moment only supports constraints involving
a single tuple at a time from a single table. Separate functions
to create a constraint for updates, inserts and deletes.
I'm still not sure transition constraints like this are useful.
*/

create function create_x_transition_tuple_constraint(
   relvar_name text, constraint_name text, constraint_expression text,
   statement_type text) returns void as $$
declare
  st text;
begin
  st := $f$
create function check_$f$ || constraint_name || $f$() returns trigger as $a$
begin
  if not ($f$ || constraint_expression || $f$) then
    raise exception '$f$ || statement_type || $f$ on $f$ || relvar_name ||
      $f$ violates transition constraint $f$ || constraint_name || $f$';
  end if;
$f$;
  if statement_type = 'update' or statement_type = 'insert' then
    st := st || '  return OLD;';
  else
    st := st || '  return null;';
  end if;
  st := st || $f$
end;
$a$ language plpgsql volatile;$f$;

  execute st;
  insert into system_implementation_objects(object_name,object_type)
    values ('check_' || constraint_name, 'operator');

  execute $f$
    create trigger $f$ || constraint_name ||
    $f$_transition_trigger after $f$ ||
    statement_type || $f$ on $f$ || relvar_name || $f$
      for each row execute procedure check_$f$
      || constraint_name || $f$();$f$;

  insert into system_implementation_objects(object_name,object_type)
    values (constraint_name || '_transition_trigger', 'trigger');

end;
$$ language plpgsql volatile;
insert into system_implementation_objects
  (object_name, object_type) values
  ('create_x_transition_tuple_constraint', 'operator');



create function create_update_transition_tuple_constraint(
   relvar_name text, constraint_name text, constraint_expression text)
   returns void as $$
begin
  perform create_x_transition_tuple_constraint(relvar_name,
    constraint_name, constraint_expression, 'update');
end;
$$ language plpgsql volatile;

create function create_insert_transition_tuple_constraint(
   relvar_name text, constraint_name text, constraint_expression text)
   returns void as $$
begin
  perform create_x_transition_tuple_constraint(relvar_name,
    constraint_name, constraint_expression, 'insert');
end;
$$ language plpgsql volatile;

create function create_delete_transition_tuple_constraint(
   relvar_name text, constraint_name text, constraint_expression text)
   returns void as $$
begin
  perform create_x_transition_tuple_constraint(relvar_name,
    constraint_name, constraint_expression, 'delete');
end;
$$ language plpgsql volatile;

/*


================================================================================

= modules

The module system is designed to break programs up into
parts. At some point would like to add namespacing
(e.g. import and export lists, for modules with
enforcement by the compiler).
At the moment it's mainly just used by the documentation
generators and e.g. could be used to assist when browsing
the database in gui and to organise documentation

documentation here needs a bit more work

module order is a hack to allow a program to get the modules
out in the order they are created, which should match up to
the order in which they appear in the source.
*/
create table modules (
  module_name text,
  module_parent_name text,
  module_order serial
);
select add_key('modules', 'module_name');

--can't add this constraint cos it causes another constraint violation
-- haven't worked out why yet
--select add_foreign_key('modules', 'module_name',
--  'modules', 'module_parent_name');

insert into modules (module_name, module_parent_name)
  values ('root', 'root');

create table all_module_objects (
  object_name text,
  object_type text,
  module_name text
);
insert into system_implementation_objects(object_name, object_type)
  values ('all_module_objects', 'base_relvar');
select add_key('all_module_objects', array['object_name', 'object_type']);
select add_foreign_key('all_module_objects', 'module_name',
  'modules');

/*
todo: when we get recursive with in pg 8.4,
add view to give module paths from the root to a module
*/

create view implementation_module_objects as
  select * from all_module_objects natural inner join
    system_implementation_objects;

insert into system_implementation_objects(object_name, object_type)
  values ('implementation_module_objects', 'view');

create view module_objects as
  select object_name, object_type, module_name
    from all_module_objects
  except
  select object_name, object_type, module_name
    from implementation_module_objects;

/*
hack to only name the module once after all its contents
since everything in my catalog is in a module, can just find
all objects without a module which is the objects which have
just been added, and set their module. This saves having to add
each object to a module individually
*/

create function set_module_for_preceding_objects(vmodule_name text)
  returns void as $$
begin
  insert into all_module_objects(module_name, object_name, object_type)
    select vmodule_name as module_name, object_name, object_type from
      (select object_name, object_type from all_database_objects
        except select object_name, object_type
        from all_module_objects) as a;
end;
$$ language plpgsql volatile;

create function new_module(mname text, mparent text) returns void as $$
begin
  insert into modules (module_name, module_parent_name)
    values(mname, mparent);
end;
$$ language plpgsql volatile;

select new_module('system', 'root');
select new_module('catalog', 'system');

select set_module_for_preceding_objects('catalog');


create function check_code_module_membership() returns boolean as $$
declare
  success boolean := true;
  r record;
begin
  --returns false if any objects with no package
  --also list problems them using raise notice
  for r in select object_name, object_type from all_database_objects
      except select object_name, object_type
      from all_module_objects loop
    success := false;
    raise notice '% % - no module', r.object_type, r.object_name;
  end loop;
  return success;
end;
$$ language plpgsql volatile;


-- add in all internal objects already defined
select set_module_for_preceding_objects('catalog');

/*

================================================================================

= utils

needs some work

*/
select new_module('utils', 'system');

/*
== set all columns to not null

saves us putting not null all over almost every table definition

The _mr suffix was left over from an attempt to do some sort of poor
man's multirelation system.  The _mr tables can contain nullable as
well as non-nullable columns, so we leave them alone when setting non
nulls and ignore them when checking for nullable attributes.

*/
create function set_all_attributes_to_not_null() returns void as $$
begin
  update pg_attribute set attnotnull = true
    where attrelid in
      (select oid from pg_class where relnamespace =
        (select oid from pg_namespace
           where nspname = 'public')
           and relkind = 'r'
           --don't touch 'multirelations'
           and not exists(select 1 from regexp_matches(relname, '_mr$')))
    and attnum >= 1;
end;
$$ language plpgsql volatile;

/*
since we can't put triggers on the catalog, at least make sure the above
function has been run in the tests.
verify all attributes in tables are not null. This doesn't affect views
which may have null values.
*/
create function check_code_no_nullable_table_columns() returns boolean as $$
declare
  r record;
  success boolean;
begin
  success := true;
  --check every table is tagged with one of
  --readonly, data, argument, widget_state
  for r in select table_name, column_name
      from information_schema.columns
      inner join base_relvars
        on (table_name = relvar_name)
      where is_nullable='YES'
        --ignore touch multirelations
        and not exists(select 1 from regexp_matches(table_name, '_mr$')) loop
    success := false;
    raise notice '%.% - nullable column', r.table_name, r.column_name;
  end loop;
  return success;
end;
$$ language plpgsql volatile;

/*

== relvars with 0 or 1 values (i.e. 0-1 tuples, one attribute)
only provide a shortcut for select, the rest might as well be in normal sql
rather than wrapped in a function.
*/
create function create_var(vname text, vtype text) returns void as $$
begin
--  create table name_table(name type primary key)
  execute $f$create table $f$ || vname || $f$_table ($f$ ||
    vname || $f$ $f$ || vtype || $f$);$f$;
  execute $f$select add_key('$f$ || vname || $f$_table',
                            '$f$ || vname || $f$');$f$;
--  adds 0 or 1 tuple constraint to this table
  --execute $f$select notify_on_changed(
  --     '$f$ || vname || $f$_table');$f$;
  execute $f$select constrain_to_zero_or_one_tuple(
    '$f$ || vname || $f$_table');$f$;
--  creates (static) functions:
--    insert_name inserts value into table
--    (will error if table isn't empty)
--  execute 'create function insert_' || vname ||
    -- '(' || vtype || ') returns void as $a$\n ' ||
--    'insert into ' || vname || '_table values($1);\n' ||
--    '$a$ language sql volatile;';
--    update_name updates value in table
--  (will error if table is empty)
--  execute 'create function update_' || vname || '(' || vtype || ')
--    returns void as $a$\n ' ||
--    'update ' || vname || '_table set ' || vname || ' = $1;\n' ||
--    '$a$ language sql volatile;';
--    delete_name deletes from table
--    (doesn't error if table is already empty)
--  execute 'create function delete_' || vname || '()
--    returns void as $a$\n ' ||
--    'delete from ' || vname || '_table;\n' ||
--    '$a$ language sql volatile;';
--    get_name returns select * from table_name as a single value
  execute 'create function get_' || vname || '() returns ' ||
    vtype || E' as $a$\n ' ||
    'select * from ' || vname || E'_table;\n' ||
    '$a$ language sql stable;';
--    drop_var_name - drop all this ish, cleanup
--  execute 'create function dropvar_' || vname || E'()
--      returns void as $a$\n ' ||
--    'drop function dropvar_' || vname || E'();\n' ||
--    'drop function insert_' || vname || '(' || vtype ||');\n' ||
--    'drop function update_' || vname || '(' || vtype ||');\n' ||
--    'drop function delete_' || vname || '();\n' ||
--    'drop function get_' || vname || E'();\n' ||
--    'drop table ' || vname || E'_table;\n' ||
--    '$a$ language sql volatile;';
end;
$$ language plpgsql volatile;
/*
== table changed notifier macro

notifier: this function is used setup a trigger to notify with table
name when a table changes. It should be applied to all the data
tables.

*/

create function notify_on_changed(table_name text) returns void as $$
begin
  execute 'create function ' || table_name
    || E'_changed() returns trigger as $a$\n'
    || E'begin\n'
    || 'notify ' || table_name || E';\n'
    || E'return null;\n'
    || E'end;\n'
    || '$a$ language plpgsql volatile;';
  --perform add_to_package('utils', 'notify_table_' ||
--    table_name || '_changed', 'trigger_function');
  insert into system_implementation_objects
    (object_name, object_type) values
     (table_name || '_changed', 'operator');

  execute 'create trigger ' || table_name || '_changed'
    || ' after insert or update or delete on '
    || table_name || ' execute procedure '
    || table_name || '_changed();';
  insert into system_implementation_objects
    (object_name, object_type) values
     (table_name || '_changed', 'trigger');

end;
$$ language plpgsql volatile;

select set_module_for_preceding_objects('utils');
