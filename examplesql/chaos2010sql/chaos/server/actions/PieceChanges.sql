select module('Chaos.Server.Actions.PieceChanges');

/*
== helpers for piece creation and destruction
*/

/*
help to speed up start game - this allows us to select 19 non unique
random spells quicker than using order by random() limit 1 in a loop
*/

/*create table spell_indexes_no_dis_turm ( -- no disbelieve or turmoil
  row_number serial unique,
  spell_name text
);
select set_relvar_type('spell_indexes_no_dis_turm', 'stack');*/

create type random_entry as (
  line int,
  num int
);

create function makeNRandoms(n int, maxi int) returns setof random_entry as $$
begin
  return query
    select generate_series(0, n - 1),
      (random() * maxi + 0.5)::int as num;
end;
$$ language plpgsql volatile;


/*insert into spell_indexes_no_dis_turm (spell_name)
  select spell_name from spells_mr
    where spell_name not in ('disbelieve', 'turmoil');*/

create function create_object(vptype text, vallegiance text, x int, y int)
  returns int as $$
begin
  --assert ptype is an object ptype
  if not exists(select 1 from object_piece_types where ptype = vptype) then
    raise exception 'called create object on % which is not an object', vptype;
  end if;
  return create_piece_internal(vptype, vallegiance, x, y, false, false);
end
$$ language plpgsql volatile;


create function create_monster(vptype text, allegiance text, x int, y int,
                               imaginary boolean, undead boolean) returns void as $$
begin
  if not exists(select 1 from monster_prototypes where ptype = vptype) then
    raise exception 'called create monster on % which is not a monster', vptype;
  end if;
  perform create_piece_internal(vptype, allegiance, x, y, imaginary, undead);
end
$$ language plpgsql volatile;

create function create_corpse(vptype text, px int, py int, imaginary boolean)
  returns void as $$
declare
  vtag int;
  twiz text;
begin
  if not exists(select count(*) from monster_prototypes
                where ptype = vptype) then
    raise exception 'called create corpse on % which is not a monster', vptype;
  end if;
  vtag := create_piece_internal(vptype,
                                'Buddha',
                                px, py, imaginary, false);
  perform kill_monster((vptype, 'Buddha', vtag));
end
$$ language plpgsql volatile;

-----------------------------------------------------
create function get_next_tag(pptype text, pallegiance text) returns int as $$

  select coalesce(max(tag) + 1, 0) from pieces
  where (ptype,allegiance) = ($1,$2);

$$ language sql stable;

create function create_piece_internal(vptype text, vallegiance text,
                                      vx int, vy int, vimaginary boolean, vundead boolean)
                                      returns int as $$
declare
  vtag int;
begin

  insert into pieces (ptype, allegiance, tag, x, y)
    select vptype, vallegiance, get_next_tag(vptype,vallegiance), vx, vy
    returning tag into vtag;

  insert into imaginary_pieces (ptype,allegiance,tag)
    select vptype,vallegiance,vtag where coalesce(vimaginary,false);
  if vundead then
    perform make_piece_undead(vptype, vallegiance, vtag);
  end if;
  return vtag;
end
$$ language plpgsql volatile;

create function make_piece_undead(vptype text, vallegiance text, vtag int)
  returns void as $$
begin
  if not exists(select 1 from piece_prototypes_mr
                where ptype=vptype and undead)
      and not exists (select 1 from crimes_against_nature
                      where (ptype,allegiance,tag) =
                        (vptype,vallegiance,vtag)) then
    insert into crimes_against_nature
      (ptype,allegiance,tag) values
      (vptype,vallegiance,vtag);
  end if;
end;
$$ language plpgsql volatile;

----------------------------------------------
/*
new plan for piece killing and stuff
disintegrate: removes piece, no corpse even for non undead monster
kill piece: calls appropriate routine:
  kill monster: creates corpse if not undead else calls disintegrate
  kill object: calls disintegrate
  kill wizard: calls disintegrate on army and wizard, and other clean up
*/

create function disintegrate(pk piece_key)
  returns void as $$
begin
  delete from pieces where (ptype, allegiance, tag)::piece_key = pk;
end;
$$ language plpgsql volatile;

create function kill_monster(pk piece_key)
  returns void as $$
begin
  --todo some asserts: monster, non undead
  --  undead cannot be dead - add constraint
  --  non monster cannot be dead: shouldn't be possible, check this
  --  after adding update rule to pieces_view
  --todo: generate update rules automatically for entities
  -- and use a single update here
  -- do the sub ones first since the pieces update changes the key
  update pieces set allegiance = 'dead',
                    tag = get_next_tag(pk.ptype,'dead')
    where (ptype, allegiance, tag) = pk;
end
$$ language plpgsql volatile;

create function disintegrate_wizards_army(pwizard_name text) returns void as $$
declare
  r piece_key;
begin
  for r in select ptype, allegiance, tag from pieces
    where allegiance = pwizard_name loop
    perform disintegrate(r);
  end loop;
end;
$$ language plpgsql volatile;

create function kill_wizard(pwizard_name text) returns void as $$
begin
--if current wizard then next_wizard
  if (select current_wizard = pwizard_name
        from current_wizard_table) then
    perform action_next_phase();
    --check if this is the last wizard, slightly hacky
    /*if (select current_wizard = pwizard_name
        from current_wizard_table) then
      perform game_completed();
      perform add_history_game_drawn();
      delete from current_wizard_table;
    end if;*/
  end if;
 --this should all be handled with cascades...?
  delete from wizard_spell_choices_mr where wizard_name = pwizard_name;
--wipe spell book
  delete from spell_books where wizard_name = pwizard_name;
--kill army
  perform disintegrate_wizards_army(pwizard_name);
--set expired to true
  update wizards set expired = true
    where wizard_name = pwizard_name;
end;
$$ language plpgsql volatile;

create function kill_piece(pk piece_key)
  returns void as $$
begin
  if (select coalesce(undead,false) from pieces_mr
        where (ptype, allegiance, tag)::piece_key = pk)
    or exists(select 1 from object_piece_types
                where ptype = pk.ptype) then
    perform disintegrate(pk);
  elseif exists(select 1 from monster_prototypes where ptype = pk.ptype) then
    perform kill_monster(pk);
  elseif pk.ptype = 'wizard' then
    perform kill_wizard(pk.allegiance);
  else
    raise exception 'don''t know how to kill piece with ptype %', pk.ptype;
  end if;

end;
$$ language plpgsql volatile;

--- testing function
create function kill_top_piece_at(px int, py int) returns void as $$
declare
  r piece_key;
begin
  select into r ptype,allegiance,tag
    from pieces_on_top where (x,y) = (px,py);
  perform kill_piece(r);
end;
$$ language plpgsql volatile;
