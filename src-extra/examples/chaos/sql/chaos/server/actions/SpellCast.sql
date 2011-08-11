select module('Chaos.Server.Actions.SpellCast');

/*
== cast spells
*/
create function skip_spell() returns void as $$
begin
  perform add_history_spell_skipped();
  perform spend_current_wizard_spell();
end;
$$ language plpgsql volatile;

create function action_cast_target_spell(px int, py int) returns void as $$
declare
  vspell_name text;
begin
  perform check_can_run_action('cast_target_spell', px, py);
  perform add_history_attempt_target_spell(px,py);

  if not check_spell_success() then
    return;
  end if;

  if exists(select 1 from current_wizard_spell
      natural inner join monster_spells) then
    perform cast_monster_spell(px, py);
  else

    select into vspell_name spell_name from current_wizard_spell;
    if vspell_name = 'disbelieve' then
      if not cast_disbelieve(px, py) then
        return;
      end if;
    elseif vspell_name = 'subversion' then
      if not cast_subversion(px, py) then
        return;
      end if;
    elseif vspell_name = 'raise_dead' then
      perform cast_raise_dead(px, py);
    elseif vspell_name in ('decree', 'justice', 'vengeance', 'dark_power') then
      perform cast_decree_spell(px, py);
    elseif vspell_name in ('lightning', 'magic_bolt') then
      perform cast_ballistic_spell(px, py);
    elseif vspell_name in ('shadow_wood',
      'magic_fire', 'gooey_blob', 'wall',
      'magic_castle', 'dark_citadel') then
      perform cast_object_spell(px, py);
    else
      raise exception 'unrecognised target spell %', vspell_name;
    end if;
  end if;
  --todo: only update alignment once per spell
  perform update_alignment_from_cast();

  update spell_parts_to_cast_table
    set spell_parts_to_cast = spell_parts_to_cast - 1;
  if get_spell_parts_to_cast() = 0 then
    perform spend_current_wizard_spell();
  end if;
end;
$$ language plpgsql volatile;

create function action_cast_activate_spell() returns void as $$
begin
  perform check_can_run_action('cast_activate_spell');
--  perform check_can_cast_spell_now();
  perform add_history_attempt_activate_spell();
  if not check_spell_success() then
    return;
  end if;
  --call the appropiate function to handle the spell
  if (select spell_category = 'wizard' from spells_mr
      natural inner join current_wizard_spell) then
    perform action_cast_wizard_spell(get_current_wizard(),
      get_current_wizard_spell());
  elseif exists(select 1 from spells_mr
      natural inner join current_wizard_spell
      where spell_name in('law', 'chaos', 'large_law',
      'large_chaos')) then
    perform cast_lawchaos();
  elseif (select spell_name='turmoil' from current_wizard_spell) then
    perform cast_turmoil();
  elseif (select spell_name='magic_wood' from current_wizard_spell) then
    perform cast_magic_wood();
  else
    raise exception 'unrecognised activate spell: %',
      (select spell_name from current_wizard_spell);
  end if;
  perform update_alignment_from_cast();
  perform spend_current_wizard_spell();
end;
$$ language plpgsql volatile;


/*
=== internals

*/
create function spend_current_wizard_spell() returns void as $$
declare
  cw text;
begin
  --remove current wizard's spell from spell book
  --make sure we only remove one shot of the spell
  --don't remove disbelieve
  --update spell_choice_hack_table
  --  set spell_choice_hack = true;

  delete from spell_parts_to_cast_table;
  delete from cast_success_checked_table;
  select into cw current_wizard
    from current_wizard_table;
  delete from spell_books where id =
    (with
      w as
        (select * from wizard_spell_choices
         where wizard_name = cw and spell_name != 'disbelieve')
      select id from w
       natural inner join spell_books
       limit 1);
  -- and wipe it from the wizard_spell_choices_table
  delete from wizard_spell_choices_mr
    where wizard_name = cw;

  --update spell_choice_hack_table
  --  set spell_choice_hack = false;

  --auto move to next wizard
  perform action_next_phase();
end;
$$ language plpgsql volatile;

create view spell_cast_chance as
  select spell_name, base_chance as chance from
    --all spells if world is neutral, neutral spells unirregardless
    -- of world alignment
    (select spell_name, sign(alignment) as salign, base_chance,
      'neutral' as alignment from spells_mr
    union all
    --world alignment same as spell alignment
    --  proportionately more easy
    select spell_name, sign(alignment) as salign,
      limit_chance(base_chance + (@ get_world_alignment()) * 10),
      'same' as alignment from spells_mr
    union all
    --world alignment opposite, spell slightly harder
    select spell_name, sign(alignment) as salign,
      limit_chance(base_chance - 10),
      'opposite' as alignment from spells_mr) as a
  where (salign = 0 and alignment = 'neutral') --neutral spells always
                                               --same alignment
    or (sign(get_world_alignment()) = 0 and alignment = 'neutral')
    or (sign(get_world_alignment()) = 1 and --world law
          ((salign = 1 and alignment = 'same') --law spells benefit
            or salign = -1 and alignment = 'opposite'))
    or (sign(get_world_alignment()) = -1 and -- world chaos
          ((salign = -1 and alignment = 'same') --chaos spells benefit
            or salign = 1 and alignment = 'opposite'));

create function spell_cast_chance(text) returns integer as $$
  select chance from spell_cast_chance where spell_name = $1;
$$ language sql stable;

create function action_cast_wizard_spell(
       pwizard_name text, spell_name text)
  returns void as $$
begin
  --todo: update stats
  if spell_name = 'magic_armour' then
      update wizards
        set magic_armour = true
        where wizard_name = pwizard_name;
  elseif spell_name = 'magic_shield' then
      update wizards
        set magic_shield = true
        where wizard_name = pwizard_name;
  elseif spell_name = 'magic_knife' then
      update wizards
        set magic_knife = true
        where wizard_name = pwizard_name;
  elseif spell_name = 'magic_sword' then
      update wizards
        set magic_sword = true
        where wizard_name = pwizard_name;
  elseif spell_name = 'magic_bow' then
      update wizards
        set magic_bow = true
        where wizard_name = pwizard_name;
  elseif spell_name = 'magic_wings' then
    update wizards set magic_wings = true
      where wizard_name = pwizard_name;
  elseif spell_name = 'shadow_form' then
      update wizards
        set shadow_form = true
        where wizard_name = pwizard_name;
  else
    raise exception 'unrecognised wizard spell %', spell_name;
  end if;
  perform add_history_spell_succeeded();
end;
$$ language plpgsql volatile;

create function cast_lawchaos() returns void as $$
begin
  --don't need to do anything, the effect is
  --restricted to the alignment effect which
  --is handled in the same place for all spells
  perform add_history_spell_succeeded();
end;
$$ language plpgsql volatile;

create function cast_turmoil() returns void as $$
declare
  r record;
  s record;
  tx int;
  ty int;
begin
  --algorithm: similar to the original chaos I think
  -- run through each square in turn, starting at top
  --left across top then along each row till you get to the
  --bottom right
  --move all the pieces in a square to a new random empty
  --square at the time of the move (so if pieces on the
  --same square as each other before turmoil is cast
  --will still be on the same square as each other
  --afterwoods. (since we do one square at a time we
  -- won't get exact random distribution).

  -- the for loop does actually save the full query
  -- at the start so updates in the for loop are not
  -- seen by the for loop so there is no risk of a
  -- piece teleporting twice
  for r in select x,y from pieces_on_top order by x,y loop
    select x,y into tx,ty
    from squares_valid_categories
      where category='empty' order by random() limit 1;
    update pieces set x = tx, y = ty
      where (x,y) = (r.x,r.y);
    --add histories
/*    for s in select ptype, allegiance, tag
      from pieces where x = tx and y = ty loop

  perform einsert(array['action_history',
    'action_history_piece_teleport'],
    array['history_name', 'ptype', 'allegiance', 'tag'],
    array['piece teleport', s.ptype, s.allegiance, s.tag::text]);
    end loop;*/
  end loop;
  perform add_history_spell_succeeded();
end;
$$ language plpgsql volatile;

create function cast_decree_spell(px int, py int) returns void as $$
declare
  r piece_key;
  m int;

begin
  --if cast on wizard then success destroys all wizards objects
  --else if cast on monster then success destroys monster
  --get target magic defense
  --todo: should this take into account the spell/attack?
  m := (select magic_defense
        from pieces_on_top_view
        natural inner join magic_attackable_pieces
        where (x,y)=(px,py));

  if not check_random_success('resist', m * 10) then
    select into r ptype, allegiance, tag
      from pieces_on_top
      where (x,y)=(px,py);
    if r.ptype = 'wizard' then
      for r in select ptype, allegiance, tag from pieces
        where allegiance = r.allegiance and ptype != 'wizard' loop
        perform disintegrate(r);
      end loop;
    else
      perform disintegrate(r);
    end if;
    perform add_history_spell_succeeded();
  end if;
end;
$$ language plpgsql volatile;

create function cast_ballistic_spell(px int, py int) returns void as $$
declare
  r piece_key;
begin
  --todo: should factor in the attack strength?
  if not check_random_success('resist',
      (select physical_defense * 10
       from pieces_on_top_view
       where (x,y) = (px,py))) then
    --need to added the chinned history before the
    --piece is killed or we loose the allegiance
    --need to add the spell successful before the
     --chinned history or the order is wrong
    perform add_history_spell_succeeded();
    perform add_chinned_history(px,py);
    select into r ptype,allegiance,tag
      from pieces_on_top
      where (x,y) = (px,py);
    perform kill_piece(r);
  else
    --spell didn't do any damage
    perform add_history_spell_succeeded();
    perform add_history_shrugged_off(px, py);
  end if;
end;
$$ language plpgsql volatile;

create function cast_raise_dead(px int, py int) returns void as $$
declare
  r piece_key;
  cw text;
begin
  select into cw current_wizard from current_wizard_table;
  --turn dead creature on square to live undead
  select into r ptype,allegiance,tag
    from pieces_on_top
    where (x,y) = (px,py);
  update pieces
    set allegiance = get_current_wizard(),
        tag = get_next_tag(r.ptype,cw)
    where (ptype,allegiance,tag)::piece_key = r
    returning tag into r.tag;
  insert into crimes_against_nature (ptype,allegiance,tag)
    values (r.ptype,cw,r.tag);
  perform add_history_spell_succeeded();
end;
$$ language plpgsql volatile;

create function cast_subversion(px int, py int) returns boolean as $$
declare
  r piece_key;
begin
    if check_random_success('resist',
      (select magic_defense * 10
        from pieces_on_top_view
        where (x,y) = (px, py))) then
    perform add_history_shrugged_off(px, py);
    perform action_cast_failed();
    return false;
  end if;
  select into r ptype,allegiance,tag from pieces_on_top
    where (x,y) = (px, py);
 update pieces
    set allegiance = current_wizard
       ,tag = get_next_tag(r.ptype,current_wizard)
    from current_wizard_table
    where (ptype,allegiance,tag)::piece_key = r;
  perform add_chinned_history(px, py);
  perform add_history_spell_succeeded();
  return true;
end;
$$ language plpgsql volatile;

create function cast_disbelieve(px int, py int) returns boolean as $$
declare
  r piece_key;
begin
  if not (select imaginary from pieces_on_top_view where (x,y) = (px,py)) then
    perform add_history_shrugged_off(px, py);
    perform action_cast_failed();
    return false;
  end if;
  select into r ptype, allegiance, tag
    from pieces_on_top where (x,y) = (px,py);

  perform add_history_spell_succeeded();
  perform add_chinned_history(px, py);
  perform disintegrate(r);
  return true;
end;
$$ language plpgsql volatile;

create function cast_object_spell(px int, py int) returns void as $$
begin
  perform create_object(
    (select ptype from current_wizard_spell
     natural inner join summon_spells),
     get_current_wizard(), px, py);
  perform add_history_spell_succeeded();
end;
$$ language plpgsql volatile;


create function cast_monster_spell(x int, y int) returns void as $$
declare
  cw text;
begin
  select into cw current_wizard
    from current_wizard_table;
  perform create_monster(
    (select ptype from current_wizard_spell
      natural inner join summon_spells),
    cw, x, y, coalesce((
      select imaginary
      from wizard_spell_choices_imaginary
      where wizard_name = cw),false), false);
  perform add_history_spell_succeeded();
end;
$$ language plpgsql volatile;

create function check_spell_success() returns boolean as $$
begin
  -- if already checked then return true
  if (select cast_success_checked
    from cast_success_checked_table) then
    return true;
  end if;

  -- if imaginary monster then always succeed
  if (select coalesce(imaginary, false)
    from wizard_spell_choices_mr
    natural inner join current_wizard) then
    return true;
  end if;

  if not check_random_success('cast',
       (select chance
        from spell_cast_chance
        natural inner join current_wizard_spell)) then
     perform action_cast_failed();
     return false;
  else
     update cast_success_checked_table
       set cast_success_checked = true;
     return true;
  end if;
end;
$$ language plpgsql volatile;

create function update_alignment_from_cast() returns void as $$
begin
  update cast_alignment_table
    set cast_alignment = cast_alignment +
      (select alignment from spells_mr
        natural inner join current_wizard_spell);
  perform adjust_world_alignment();
end;
$$ language plpgsql volatile;

create function action_cast_failed() returns void as $$
begin
  perform add_history_spell_failed();
  perform spend_current_wizard_spell();
end;
$$ language plpgsql volatile;

  /*
  idea is to create a view with all the valid squares in it
  and to start with a square series of squares 1 square away from
  the wizard:
   XXX
   XWX
   XXX
  starting with the top left one, cast trees in the available squares
  then move to 2 squares away:
   XXXXX
   X...X
   X.W.X
   X...X
   XXXXX
  and keep going until we are at the range of the spell (if
  the view takes the range into account then we keep going
  to max(width of board, height of board) if this isn't too slow
  pos_in_square is used to track which square we are looking at e.g.
  at range one:
   123
   4W5
   678
  range two
   12345
   6...7
   8.W.9
   0...1
   23456
   (the 012346 on the second to last and last rows
     represent 10,11,12,13,14,15,16
  */

create function get_square_range(x int, y int, range int)
  returns setof pos as $$
    select x, y from
    (select generate_series($1 - $3, $1 + $3) as x,$2 - $3 as y
     union select $1 - $3,generate_series($2 - $3 + 1, $2 + $3 - 1)
     union select $1 + $3,generate_series($2 - $3 + 1, $2 + $3 - 1)
     union select  generate_series($1 - $3, $1 + $3),$2 + $3) as a
$$ language sql immutable;

--take into account range, line of sight,
--atm only takes into account empty squares
--and trees cannot be next to each other

/*
for some reason when this is set to return pos and not an array,
when there are no results in the final
  select (x,y)::pos from sqs order by y,x limit 1;
the return pos return some weird '(,)' which I don't know how to
test for instead of returning null?
it isn't null, and you can't do r.x on it if you assign it to a record
  since it doesn't have a field called x, and it won't assign to a var
  of type pos

*/
create function next_tree_square(available pos[], added pos[]) returns pos[] as $$
  with
           av as
             (select (p1).x,(p1).y from unnest($1) as p1)
          ,ad as
             (select (p2).x,(p2).y from unnest($2) as p2)
          ,adj_ad as
             (select (p3).x,(p3).y
                from (select one_square_away((x,y)) as p3 from ad) as p4)
          ,sqs as
            (select x,y from av
             except (select x,y from adj_ad
                     union select x,y from ad))
  select array_agg((x,y)::pos) from sqs group by y,x order by y,x limit 1;
$$ language sql immutable;

create function cast_magic_wood() returns void as $$
declare
  range int;
  wpos pos;
  cw text;
  added pos[];
  bavail pos[] := (select array_agg((x,y)::pos)
                    from squares_valid_categories
                    where category = 'empty_and_not_adjacent_to_tree');
  avail pos[];
  npos pos;
  npos1 pos[];
begin
  select into cw current_wizard
    from current_wizard_table;
   wpos := (select (x,y)::pos from pieces
            where ptype='wizard' and allegiance=cw);
  range := 1;
  --raise notice 'here % % %', range, array_length(added,1), bavail;
  -- get the list of squares to cast on
  -- basically, we find the closest next available square
  -- then loop until we have got 8 or run out of range
  -- i'm sure this can be done with one 'with recursive' query.
  <<next_range>>
  while range <= 15 and coalesce(array_length(added,1),0) < 8 loop
    --raise notice 'do range %', range;
    -- fix the range, get the set of available squares at that range
    avail := (select array_agg(((a).x,(a).y)::pos)
              from (select * from unnest(bavail)
                    intersect
                    select *
                    from get_square_range(wpos.x, wpos.y, range)) as a);
    --raise notice 'avail: %', avail;
    <<this_range>>
    loop
      --raise notice 'do another sq %', array_length(added,1);
      -- loop adding one square at a time until there are no available suares at this range
      npos1 := (select * from (select * from next_tree_square(avail, added)) a);
      --raise notice 'next square %', npos1;
      if coalesce(array_length(npos1,1),0) = 0 then
        range := range + 1;
        continue next_range;
      end if;
      --raise notice 'addinf %', npos1[1];
      added := added || (npos1[1].x,npos1[1].y)::pos;
      if coalesce(array_length(added,1),0) >= 8 then
        exit next_range;
      end if;
    end loop;
  end loop;
  -- got the cast positions, create the trees
  for npos in select (p).x,(p).y from unnest(added) as p loop
    perform create_object('magic_tree', cw, npos.x, npos.y);
  end loop;

  perform add_history_spell_succeeded();
end;
$$ language plpgsql volatile;
