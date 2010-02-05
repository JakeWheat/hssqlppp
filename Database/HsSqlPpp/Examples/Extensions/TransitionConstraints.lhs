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
