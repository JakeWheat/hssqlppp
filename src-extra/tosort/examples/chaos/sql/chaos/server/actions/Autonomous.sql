select module('Chaos.Server.Actions.Autononmous');

/*
== autonomous
*/

create view wizards_in_trees as
select ptype,allegiance,tag from pieces
  where ptype='wizard'
    and (x,y) in (select x,y from pieces
                  where ptype='magic_tree');

create function do_autonomous_phase() returns void as $$
declare
  r piece_key;
  r1 piece_key;
begin
  --castles
  for r in select ptype,allegiance,tag from pieces
    where ptype in ('magic_castle', 'dark_citadel') loop
    if check_random_success('disappear', 20) then
      perform disintegrate(r);
    end if;
  end loop;
  --magic trees
 for r in select ptype,allegiance,tag from wizards_in_trees loop
    if check_random_success('bonus', 20) then
      select into r1 ptype,allegiance,tag
             from pieces
             where ptype ='magic_tree'
               and (x,y) = (select x,y from pieces
                            where (ptype,allegiance,tag)::piece_key = r);
      perform disintegrate(r1);
      insert into spell_books (wizard_name, spell_name)
      values (r.allegiance,
       (select spell_name from spells_mr
         where spell_name <> 'disbelieve'
          order by random() limit 1));
    end if;
  end loop;
  perform do_spreading();
end;
$$ language plpgsql volatile;

/*
can't find this function in the postgresql docs...?
*/

create function array_contains(ar anyarray, e anyelement) returns boolean as $$
declare
  i int;
begin
  if ar = '{}' then
    return false;
  end if;
  for i in (array_lower(ar,1))..(array_upper(ar,1)) loop
    if ar[i] = e then
      return true;
    end if;
  end loop;
  return false;
end;
$$ language plpgsql immutable;


/*
rules:
each piece has 10% chance of disappearing
each piece has 20% chance of spawning a new piece
each piece has 20% chance of spawning two new pieces

can't spread to object squares
can't spread to same allegiance squares
blob spreading over wizard kills wizard
blob spreading on monster leaves monster trapped until blob is killed/recedes
fire spreading onto anything kills & disintegrates it

need hack to prevent blobs trying to spread which are owned by wizards
killed previously during this spreading.  I think we need to keep
track of these manually since the database won't let us read a
partially updated wizards or pieces table during the transaction

insert into spell_books (wizard_name,spell_name)
  select wizard_name, 'magic_wood' from wizards
  where not expired;


insert into spell_books (wizard_name,spell_name)
  select wizard_name, 'gooey_blob' from wizards
  union
  select wizard_name, 'magic_fire' from wizards;

*/

create view spreadable_squares as
  select x,y from generate_series(0, 14) as x
    cross join generate_series(0, 9) as y
  except
  select x,y from pieces natural inner join object_piece_types;

select create_var('disable_spreading', 'boolean');
insert into disable_spreading_table values (false);
select set_relvar_type('disable_spreading_table', 'data');

create function do_spreading() returns void as $$
declare
  r piece_key;
  r1 piece_key;
  p pos;
  sp record;
  i int;
  c int;
  tg int;
  killed_wizards text[] = '{}';
begin
  if get_disable_spreading() then
    return;
  end if;
  for sp in select ptype,allegiance,tag,x,y from pieces
           where ptype in ('gooey_blob', 'magic_fire') loop
    --raise notice 'check % e %', sp.allegiance, killed_wizards;
    if array_contains(killed_wizards, sp.allegiance) then
      --raise notice 'skip piece %', sp.allegiance;
      continue;
    end if;
    c := (random() * 100)::Int;
    if c < 10 then
      --recede
      perform add_history_recede(r);
      perform disintegrate((sp.ptype,sp.allegiance,sp.tag));
    elseif c < 50 then
      for i in 1..(case when c < 30 then 1 else 2 end) loop
        select into p tx,ty from (
          select x, y from one_square_away((sp.x,sp.y))
          natural inner join spreadable_squares s
          except select x as tx,y as ty
                   from pieces
                   where allegiance=sp.allegiance) as a
                     order by random() limit 1;
        if p.x is null then continue; end if;
        if exists(select 1 from pieces
               where (x,y) = (p.x,p.y)
                 and ptype = 'wizard') then
          select into r1 ptype,allegiance,tag from pieces
               where (x,y) = (p.x,p.y)
                 and ptype = 'wizard';
          if r1.allegiance = sp.allegiance then
            raise exception 'spread tried to get friendly piece';
          end if;
          killed_wizards := array_append(killed_wizards, r1.allegiance);
          --raise notice 'killed_wizards %', killed_wizards;
          perform add_chinned_history(p.x,p.y);
          perform kill_piece(r1);
        end if;
        --magic fire removes all pieces
        for r1 in select ptype,allegiance,tag from pieces
            where (x,y) = (p.x,p.y) loop
          if r1.allegiance = sp.allegiance then
            raise exception 'spread tried to get friendly piece';
          end if;
          perform disintegrate(r1);
        end loop;
        --raise notice 'spreading %', sp.allegiance;
        tg := create_object(sp.ptype, sp.allegiance, p.x,p.y);
        perform add_history_spread((sp.ptype,sp.allegiance,tg));
      end loop;
    end if;
  end loop;
end;
$$ language plpgsql volatile;

