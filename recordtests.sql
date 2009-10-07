/*

record tests: try to find out exactly how records work
run a bunch of functions and check the results

run this file by loading psql then issuing \i recordtests.sql

the output from doing this has been manually pasted into this file in
comments

start with something to help understand the psql output

*/
create or replace function doingfn(fn text) returns void as $$
begin
  raise notice '\n===================================================\nFUNCTION: %\n-------------------------------------------',fn;
end;
$$ language plpgsql;



select doingfn('t1');

create or replace function t1() returns void as $$
declare
  r record;
  t pg_attrdef;
begin
  select into r * from pg_attrdef;
  t := r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$ language plpgsql;

select t1();

/*
result from this select is:
*************************
psql:recordtests.sql:20: NOTICE:  r: (258241,3,"{FUNCEXPR :funcid 480 :funcresulttype 23 :funcretset false :funcformat 2 :args ({FUNCEXPR :funcid 1574 :funcresulttype 20 :funcretset false :funcformat 0 :args ({CONST :consttype 2205 :consttypmod -1 :constlen 4 :constbyval true :constisnull false :location -1 :constvalue 4 [ -65 -16 3 0 ]}) :location -1}) :location -1}","nextval('modules_module_order_seq'::regclass)")
psql:recordtests.sql:20: NOTICE:  t: (258241,3,"{FUNCEXPR :funcid 480 :funcresulttype 23 :funcretset false :funcformat 2 :args ({FUNCEXPR :funcid 1574 :funcresulttype 20 :funcretset false :funcformat 0 :args ({CONST :consttype 2205 :consttypmod -1 :constlen 4 :constbyval true :constisnull false :location -1 :constvalue 4 [ -65 -16 3 0 ]}) :location -1}) :location -1}","nextval('modules_module_order_seq'::regclass)")
 t1 
----
 
(1 row)
*************************

the output is a list of values, looks like each has a type but we
don't get any field names, will investigate the field name aspects
below

The output from t and r are exactly the same - looks like the record
is pointing to exactly the same type of value that the composite variable is.

*/

-- test two: build a record out of bits and then try to assign to a composite
select doingfn('t2');

create or replace function t2() returns void as $$
declare
  r record;
  t pg_attrdef;
begin
  r.adrelid := 1;
  --r.adnum := 2;
  --r.adbin = 'adbinval';
  --r.adsrc = 'adsrcval';
  --raise notice 'r: %',t;
  --t := r;
  --raise notice 't: %',t;
end;
$$ language plpgsql;

select t2();
/*
psql:recordtests.sql:37: ERROR:  record "r" is not assigned yet
DETAIL:  The tuple structure of a not-yet-assigned record is indeterminate.
CONTEXT:  PL/pgSQL function "t1" line 5 at assignment

didn't know whether this would work or not, but this is only because I
didn't read the postgresql manual properly, it's noted in
38.3.4. Record Types

*/

-- test three - assign to record from row ctor
select doingfn('t3');

create or replace function t3() returns void as $$
declare
  r record;
  t pg_attrdef;
begin
  r := row(1,2,'adbinval','adsrcval');
  raise notice 'r: %',r;
  t := r;
  raise notice 't: %',t;
end;
$$ language plpgsql;

select t3();
/*
output is:
psql:recordtests.sql:105: NOTICE:  r: (,,,)
psql:recordtests.sql:105: NOTICE:  t: (1,2,adbinval,adsrcval)

so:
the assignment worked, but we can't see the fields with the row
constructor value in the record for some reason. This suggests the row
isn't the same type as the composite or even the same kind of type,
but it is compatible. Next check: see what happens when the row ctor
value types are in the wrong order.
*/

select doingfn('t4');

create or replace function t4() returns void as $$
declare
  r record;
  t pg_attrdef;
begin
  r := row('adbinval',1,2,'adsrcval');
  raise notice 'r: %',t;
  t := r;
  raise notice 't: %',t;
end;
$$ language plpgsql;

select t4();

/*
psql:recordtests.sql:143: NOTICE:  r: (,,,)
psql:recordtests.sql:143: ERROR:  invalid input syntax for type oid: "adbinval"
CONTEXT:  PL/pgSQL function "t4" line 7 at assignment

so this fails. So the the order of values counts. I think this would
mean our row ctor which assign succeeds above produces an anonymous
composite type with the structure:

(a:number,b:number,c:unknownstringlit,d:unknownstringlit)

where a,b,c,d are anonymous name placeholders, so we can't use them to
access the individual parts.

then we use the check assignment cast value one by one over the values to work out if the assign from the record to the composite type works.

next: check the number of fields behaviour
*/


select doingfn('t5');

create or replace function t5() returns void as $$
declare
  r record;
  t pg_attrdef;
  b bool;
begin
  r := row(1,2,'adbinval','adsrcval', 3,null,true,'is this ignored?');
  raise notice 'r: %',t;
  t := r;
  raise notice 't: %',t;
  --todo: investigate equalities in same way as assignments
  b := row(1,2,'adbinval','adsrcval') = row(1,2,'adbinval','a1dsrcval');
  raise notice 'b: %',b;
end;
$$ language plpgsql;

select t5();

/*
psql:recordtests.sql:173: NOTICE:  r: (,,,)
psql:recordtests.sql:173: NOTICE:  t: (1,2,adbinval,adsrcval)

slightly disappointing - it works instead of giving an error, with the
extra fields ignored.

test composite variations on this - see if extra fields are ignored there too

Want to check if the field names are significant for named composite
types with non anonymous field names.
- check composite to composite assignment with
  same names and types
  different names same types
  same names and types but different order
  check if extra fields get ignored or cause error as with row values

*/

select doingfn('t6');
drop type if exists fake_pg_attrdef;
create type fake_pg_attrdef as (
  adrelid oid,
  adnum int2,
  adbin text,
  adsrc text
);

--just test a bunch of stuff which should definitely work as sanity check
create or replace function t6() returns void as $$
declare
  t pg_attrdef;
  u fake_pg_attrdef;
  r record;
begin
  u := (1,2,'adbinval','adsrcval');
  t := u;
  raise notice 'u: %',u;
  raise notice 't: %',t;
  r := u;
  t := r;
  raise notice 'u: %',r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$ language plpgsql;

select t6();

/*
works fine:
psql:recordtests.sql:220: NOTICE:  u: (1,2,adbinval,adsrcval)
psql:recordtests.sql:220: NOTICE:  t: (1,2,adbinval,adsrcval)
psql:recordtests.sql:220: NOTICE:  u: (1,2,adbinval,adsrcval)
psql:recordtests.sql:220: NOTICE:  r: (1,2,adbinval,adsrcval)
psql:recordtests.sql:220: NOTICE:  t: (1,2,adbinval,adsrcval)

check if field names are significant:
*/

select doingfn('t7');
drop type if exists fake_renamed_pg_attrdef;
create type fake_renamed_pg_attrdef as (
  a oid,
  b int2,
  c text,
  d text
);

create or replace function t7() returns void as $$
declare
  t pg_attrdef;
  u fake_renamed_pg_attrdef;
  r record;
begin
  u := (1,2,'adbinval','adsrcval');
  t := u;
  raise notice 'u: %',u;
  raise notice 't: %',t;
  r := u;
  t := r;
  raise notice 'u: %',r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$ language plpgsql;

select t7();

/*
works fine also:
psql:recordtests.sql:261: NOTICE:  u: (1,2,adbinval,adsrcval)
psql:recordtests.sql:261: NOTICE:  t: (1,2,adbinval,adsrcval)
psql:recordtests.sql:261: NOTICE:  u: (1,2,adbinval,adsrcval)
psql:recordtests.sql:261: NOTICE:  r: (1,2,adbinval,adsrcval)
psql:recordtests.sql:261: NOTICE:  t: (1,2,adbinval,adsrcval)

check field order is important:
*/

select doingfn('t8');
drop type if exists fake_renamed2_pg_attrdef;
create type fake_renamed2_pg_attrdef as (
  adnum int2,
  adbin text,
  adsrc text,
  adrelid oid
);

create or replace function t8() returns void as $$
declare
  t pg_attrdef;
  u fake_renamed2_pg_attrdef;
  r record;
begin
  u := (2,'adbinval','adsrcval',1);
  --t := u;
  raise notice 'u: %',u;
  raise notice 't: %',t;
  r := u;
  t := r;
  raise notice 'u: %',r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$ language plpgsql;

select t8();

/*
fails:
psql:recordtests.sql:300: ERROR:  invalid input syntax for integer: "adbinval"
also fails with same error earlier if you uncomment the commented out line
confirmation: pg is just looking at the types in order and ignoring
the names. This seems consistent with the rest of sql (e.g. unions)...

check extra fields
*/

select doingfn('t9');
drop type if exists fake_renamed3_pg_attrdef;
create type fake_renamed3_pg_attrdef as (
  adrelid oid,
  adnum int2,
  adbin text,
  adsrc text,
  hello text
);

create or replace function t9() returns void as $$
declare
  t pg_attrdef;
  u fake_renamed3_pg_attrdef;
  r record;
begin
  u := (1,2,'adbinval','adsrcval','test');
  r := u;
  t := r;
  raise notice 'u: %',r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$ language plpgsql;

select t9();
/*
yes, it works just like the row ctor. ignores the extra field
psql:recordtests.sql:340: NOTICE:  u: (1,2,adbinval,adsrcval,test)
psql:recordtests.sql:340: NOTICE:  r: (1,2,adbinval,adsrcval,test)
psql:recordtests.sql:340: NOTICE:  t: (1,2,adbinval,adsrcval)

check with missing fields:
*/

select doingfn('t10');
create or replace function t10() returns void as $$
declare
  t pg_attrdef;
  u fake_renamed3_pg_attrdef;
  r record;
begin
  u := (1,2,'adbinval');
  r := u;
  t := r;
  raise notice 'u: %',r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$ language plpgsql;

select t10();
/*
yep, works
psql:recordtests.sql:363: NOTICE:  u: (1,2,adbinval,,)
psql:recordtests.sql:363: NOTICE:  r: (1,2,adbinval,,)
psql:recordtests.sql:363: NOTICE:  t: (1,2,adbinval,)

*/