/*
================================================================================

= Actions
*/
select module('Chaos.Server.Actions');

--this view contains all the squares with no pieces in them
create view empty_squares as
  select x,y from generate_series(0, 14) as x
                cross join generate_series(0, 9) as y
  except
  select x,y from pieces;

-- this view contains all the squares containing corpses and nothing else
create view corpse_only_squares as
  select x,y from pieces_on_top
    natural inner join dead_monster_pieces;

--empty or corpse only doubles as the list of squares moveable to
--either by walking or flying
create view empty_or_corpse_only_squares as
--empty squares union
  select * from empty_squares
  union
  select * from corpse_only_squares;

-- this view contains all the squares which are exactly one square
-- away from a tree (doesn't include the tree squares themselves)
create view adjacent_to_tree_squares as
  select tx as x, ty as y
    from board_ranges
    natural inner join pieces
    where ptype in ('magic_tree', 'shadow_tree')
      and range = 1;
--
create view empty_and_not_adjacent_to_tree_squares as
  select * from empty_squares
  except
  select * from adjacent_to_tree_squares;

--this view contains squares which the 'top piece' is attackable
create view attackable_squares as
  select x,y from attackable_pieces
    natural inner join pieces_on_top;

--this view contains squares which the 'top piece' is a creature
create view creature_on_top_squares as
  select x,y from creature_pieces
  natural inner join pieces_on_top;

--this view contains squares which the 'top piece' is a monster
create view monster_on_top_squares as
  select x,y from monster_pieces
  natural inner join pieces_on_top;

-- this view contains all the squares which are valid for the
-- different spell target categories. Doesn't take into account range
create view spell_valid_squares as
  select 'empty' as valid_square_category, *
    from empty_squares
  union
  select 'empty_or_corpse_only' as valid_square_category, *
    from empty_or_corpse_only_squares
  union
  select 'attackable' as valid_square_category, *
    from attackable_squares
  union
  select 'creature_on_top' as valid_square_category, *
    from creature_on_top_squares
  union
  select 'monster_on_top' as valid_square_category, *
    from monster_on_top_squares
  union
  select 'corpse_only' as valid_square_category, *
    from corpse_only_squares
  union
  select 'empty_and_not_adjacent_to_tree' as valid_square_category, *
    from empty_and_not_adjacent_to_tree_squares;

--this view contains all the squares which would be valid
--for the current wizard's current spell, not taking into
--account the wizard's position and the spell's range.
create view current_wizard_spell_type_squares as
  select x,y from wizard_spell_choices
       inner join current_wizard_table on (wizard_name = current_wizard)
       natural inner join target_spells
       natural inner join spell_valid_squares;

--rewrote joining to board_ranges as a where for speed purposes
create view current_wizard_spell_range_squares as
  select tx as x, ty as y
  from board_ranges
  where (x,y,range) =
       (select x, y, range
          from pieces
          inner join current_wizard_table
            on (allegiance = current_wizard)
          inner join wizard_spell_choices
            on (wizard_name = current_wizard)
          natural inner join target_spells
          where ptype = 'wizard');

--this view contains all the squares which are valid targets
-- for the current wizard's current spell
--taking into account the spell target category, the wizard's
--position and the range, i.e. the final product
--this is directly used in action valid during spell casting
create view current_wizard_spell_squares as
  select * from current_wizard_spell_type_squares
  intersect
  select * from current_wizard_spell_range_squares
  except
  select x, y from pieces
    inner join current_wizard_table
    on (allegiance = current_wizard) where ptype='wizard';

/*
create a view containing all the squares the selected piece
could move to if they had unlimited speed
*/
create view selected_piece_move_squares as
  select x,y from empty_or_corpse_only_squares
  union
  select x,y from pieces
  natural inner join
    (select ptype from enterable_piece_types
       where (select ptype='wizard' from selected_piece)
     union
     select ptype from ridable_prototypes
       where (select ptype='wizard' from selected_piece)) as a
  where allegiance = (select allegiance from selected_piece);

-- = all squares range one from piece, which
-- dont contain anything but
create view selected_piece_walk_squares as
  select x,y from
    selected_piece_move_squares
  intersect
--adjust for 1 square away from selected piece:
--get the ranges
  select tx as x, ty as y from board_ranges
    natural inner join selected_piece
    natural inner join pieces
--restrict to 1 range
--exclude flying creatures
    where range = 1

      and not (select flying or engaged
               from creature_pieces
               natural inner join selected_piece)
-- only if the selected piece has squares left to walk
    and get_remaining_walk() > 0;

create view squares_within_selected_piece_flight_range as
  select tx as x, ty as y from board_ranges
  natural inner join selected_piece
  natural inner join creature_pieces
  where flying and range <= speed;

--this view is the analogue of selected_piece_walk_squares
-- for flying creatures
create view selected_piece_fly_squares as
select x,y from selected_piece_move_squares
intersect
select x,y from squares_within_selected_piece_flight_range
-- only if the selected piece hasn't moved
  where (select move_phase from selected_piece) = 'motion';

create view selected_piecexy as
  select * from selected_piece
  natural inner join pieces;

--create view selected_piece_shootable_squares as
--  select x,y from pieces_on_top
--  natural inner join attackable_pieces;

create function is_equipped(text) returns boolean as $$

  select magic_sword or magic_knife or magic_bow
    from wizards where wizard_name = $1;

$$ language sql stable;

create view selected_piece_attackable_squares as
  select x,y from pieces_on_top t
  natural inner join pieces_mr p
  cross join selected_piece s
  where physical_defense is not null
    and p.allegiance <> s.allegiance
    and p.allegiance <> 'dead'
  --wizards can't attack magic trees but monsters can
    and not (p.ptype='magic_tree' and s.ptype='wizard')

/*

logic isn't quite right - a wizard can only attack undead with the
magic weapon so e.g. they shouldn't be able to attack h2h if they only
have a magic bow

*/
     and (not coalesce(undead,false)
          or coalesce((select coalesce(undead,false) from pieces_mr
                        natural inner join selected_piece), false)
          or coalesce(s.ptype = 'wizard'
                      and is_equipped(s.allegiance), false));

create view selected_piece_walk_attack_squares as
  select x,y from selected_piece_attackable_squares
  intersect
  select tx,ty from board_ranges r
    natural inner join selected_piecexy
    where range = 1
      and move_phase in ('motion','attack');

create view selected_piece_fly_attack_squares as
  select x,y from selected_piece_attackable_squares
  natural inner join squares_within_selected_piece_flight_range
  where (select move_phase from selected_piece) = 'motion';

create view selected_piece_in_range_squares as
  select tx as x, ty as y from board_ranges b
  natural inner join ranged_weapon_pieces s
  natural inner join selected_piece
  where b.range <= s.range;

create view selected_piece_ranged_attackable_squares as
  select x,y from selected_piece_attackable_squares
  natural inner join selected_piece_in_range_squares;

/*
this view lists all the squares which have pieces which can be
selected. It is empty when:
  not in move phase
  there is a currently selected piece
  the current wizard has no pieces left to select
the pieces to move only has entries for
 the current wizard who's moving, else it's empty
 so only need to switch the contents dependant on
 whether there is a selected piece or not
*/

create view selectable_pieces as
  select * from
    (select row_number() over (partition by (x,y) order by sp) as rn, *
       from selectable_pieces_with_priorities
       natural inner join pieces_to_move
       where not exists(select 1 from selected_piece)
    ) as s where rn = 1;


--  select distinct on (x,y) * from selectable_pieces_with_priorities
--  natural inner join pieces_to_move
--  where not exists(select 1 from selected_piece)
--  order by x,y,sp;

/*
=== valid actions

The end result: two relvars, one with x,y,
to list all the valid actions at any time.
*/
create view valid_target_actions as
select * from (
--target spells
select x,y, 'cast_target_spell'::text as action
  from current_wizard_spell_squares
  where get_turn_phase() = 'cast'
--selecting a piece
union
select x,y,action from (
select x,y, 'select_piece_at_position':: text as action
  from selectable_pieces
--walking
union
select x,y, 'walk'::text as action
  from selected_piece_walk_squares
--flying
union
select x,y, 'fly'::text as action
  from selected_piece_fly_squares
--attacking
union
select x,y, 'attack'::text as action
  from selected_piece_walk_attack_squares
--fly attacking
union
select x,y, 'attack'::text as action
  from selected_piece_fly_attack_squares
--shooting
union
select x,y, 'ranged_attack'::text as action
  from selected_piece_ranged_attackable_squares
)as s1
where get_turn_phase()='move'
) as s
where not exists (select 1 from game_completed_table);

create function current_wizard_replicant() returns bool as $$
  select computer_controlled from wizards
    inner join current_wizard_table
      on wizard_name=current_wizard;
$$ language sql stable;

/*
create a view with the choose spell predicates automatically
*/

create view valid_activate_actions as
select * from (
--next_phase - always valid
select 'next_phase'::text as action
--choose spell - need one for each spell, add programmatically
--set imaginary
union
select 'set_imaginary'::text as action
  from monster_spells
  where get_current_wizard_spell() is not null
    and spell_name = get_current_wizard_spell()
--set real
union
select 'set_real'::text as action
  from monster_spells
  where get_current_wizard_spell() is not null
    and spell_name = get_current_wizard_spell()
--cast activate spell
union
select 'cast_activate_spell'::text as action
  where exists (select 1
         from current_wizard_spell
         natural inner join activate_spells
         where get_turn_phase() = 'cast')
      or (select spell_name ='magic_wood'
          from current_wizard_spell
          where get_turn_phase() = 'cast')
--skip spell
--union
--select 'skip_spell'::text as action
--  where get_turn_phase() = 'cast'
--unselect
union
select 'unselect_piece'::text as action
  from selected_piece
--next subphase
union
select 'cancel'::text as action
  from selected_piece
union
/*

generate a separate choose action wrapper for each spell

without this, we can add a general choose spell action but then we
first check if the current player can choose a spell at this time, and
then check if they have the particular spell they are trying to
choose.

By creating these simple wrappers, we can check both at once, and also
the ui has one simple test to see if a spell choice action is valid
instead of two stages.

*/
select 'choose_' || spell_name || '_spell'::text as action
  from spell_books where wizard_name = get_current_wizard()
  and get_turn_phase()='choose'
union
select 'choose_no_spell'::text as action
  from turn_phase_table where turn_phase ='choose'
union
select 'ai_continue'
  from wizards
  inner join current_wizard_table
    on wizard_name = current_wizard
    where computer_controlled
) as a
where not exists (select 1 from game_completed_table);

/*
==== internals
provide shortcut functions to check if an action can be run using
these views

*/
create function check_can_run_action(action_name text) returns void as $$
begin
  if not exists (select 1 from valid_activate_actions
     where action = action_name) then
    raise exception 'cannot run % here', action_name;
  end if;
end;
$$ language plpgsql stable;

create function check_can_run_action(action_name text, px int, py int)
  returns void as $$
begin
  if not exists (select 1 from valid_target_actions
     where action = action_name and (x,y) = (px,py)) then
    raise exception 'cannot run % on %,% here', action_name, px, py;
  end if;
end;
$$ language plpgsql stable;

/*
== next phase

next phase strings the enter and exits all together in the right order
and provides a simple API for clients

there seems to be some nomenclaturic confusion as to whether a single
phase is all wizards choosing, or all casting, or all moving, or if
it's one wizard casting, i.e. whether there are 3 (choose, cast, move,
or 4 including autonomous) phases per turn or roughly 3 * number of
live wizards ( + 1 for autonomous) phases per turn.

Next_phase implies each wizard is a new phase, but sometimes e.g. the
whole of the cast phase for all wizards is refered to as the phase or
a phase...?

*/

select create_var('dont_nest_ai_next_phase', 'bool');
select set_relvar_type('dont_nest_ai_next_phase_table', 'stack');

create function action_next_phase() returns void as $$
declare
  c int;
  next_phase_again boolean := false;
begin
/*
=== check for game completion
*/
  if (exists (select 1 from game_completed_table)) then
    return;
  end if;
  --check for win or draw
  c := (select count(1) from wizards
         where not expired);
  if c = 1 then --someone has won
    perform game_completed();
    update current_wizard_table set current_wizard =
      (select wizard_name from wizards where not expired);
    perform add_history_game_won();
    return;
  elseif c = 0 then --game is drawn
    perform game_completed();
    perform add_history_game_drawn();
    delete from current_wizard_table;
    return;
  end if;

/*
=== current wizard clean up phase

If the user selects next phase when they have a spell to cast, then we
want to call the usual skip spell action so as not to duplicate the
work. But skip spell will call next_phase itself automatically and we
don't want to do two next phases, so if there is a spell to be
skipped, run that and don't run the rest of the next_phase function
since it will be called via skip spell.

*/
    -- if the current spell isn't completed, then skip it
  if exists(select 1 from wizard_spell_choices
       inner join current_wizard_table
       on (current_wizard = wizard_name)
       where get_turn_phase() = 'cast') then
    perform skip_spell();
    return;
  end if;

  --multiple update hack to get round constraints
  update in_next_phase_hack_table
    set in_next_phase_hack = true;

  --complete current phase:
  if (select turn_phase = 'move' from turn_phase_table) then
    delete from pieces_to_move;
  end if;

/*
=== all wizards clean up phase

clean up if this is the last wizard for this phase, then move to next
phase, if this is autonomous, then do it and move to move phase this
works because all the end phase stuff happens before the autonomous
phase is run in this function, and all the setup runs after it is run.

*/

  if is_last_wizard() then
    --clear the cast alignment which is used to adjust the world
    --alignment when a spell is cast
    if get_turn_phase() = 'cast' then
      delete from cast_alignment_table;
    end if;

    --if this is the end of the move phase then we're on the next turn
    if (select turn_phase = 'move' from turn_phase_table) then
      update turn_number_table
        set turn_number = turn_number + 1;
      perform add_history_new_turn();
    end if;

    --move to the next turn phase
    update turn_phase_table
      set turn_phase = next_turn_phase(turn_phase);

    if (select turn_phase = 'autonomous' from turn_phase_table) then
      perform do_autonomous_phase();
      update turn_phase_table
        set turn_phase = next_turn_phase(turn_phase);
    end if;
  end if;

/*
=== init new current phase
*/
  -- move to the next wizard, this is the meat of this function
  update current_wizard_table
    set current_wizard = next_wizard(current_wizard);

  --setup the cast alignment table if this is the start of the cast
  --phases
  if get_turn_phase() = 'cast' and is_first_wizard() then
    insert into cast_alignment_table values(0);
  end if;

  --initialise the spell for this phase
  if (select turn_phase = 'cast' from turn_phase_table) then
    if exists(select 1 from current_wizard_spell) then
      insert into spell_parts_to_cast_table
        select coalesce(num, 0) from target_spells
        natural inner join current_wizard_spell;
      insert into cast_success_checked_table values (false);
    else
      --skip to the next phase automatically
      next_phase_again := true;
    end if;
  elseif (select turn_phase = 'move' from turn_phase_table) then
    insert into pieces_to_move
      select ptype, allegiance, tag
        from moving_pieces
        inner join current_wizard_table
        on allegiance = current_wizard;
  end if;

  --finished our updates for this next phase
  update in_next_phase_hack_table
    set in_next_phase_hack = false;

  perform add_history_wizard_up();
/*
=== continue
*/
  --if there is nothing to do in the new current phase - continue to
  --next phase automatically
  if next_phase_again then
    perform action_next_phase();
  end if;
end;
$$ language plpgsql volatile;

/*
=== internals
*/
create function is_last_wizard() returns boolean as $$
begin
  return ((select place from live_wizards
        natural inner join current_wizard)
     = (select max(place) from live_wizards));
end;
$$ language plpgsql stable;

create function is_first_wizard() returns boolean as $$
begin
  return ((select place from live_wizards
       natural inner join current_wizard)
     = (select min(place) from live_wizards));
end;
$$ language plpgsql stable;

/*
== spell choice

*/
create function action_choose_spell(vspell_name text)
  returns void as $$
begin
  --create the argumentless action name so we can check the action
  --valid table
  perform check_can_run_action('choose_' || vspell_name || '_spell');

  --do nothing if this is the same as the currently selected spell
  if (select spell_name from wizard_spell_choices
            where wizard_name = get_current_wizard()) = vspell_name then
    null;
  else
    --if wizard already has a chosen spell then remove it
    delete from wizard_spell_choices_mr
      where wizard_name = get_current_wizard();
    insert into wizard_spell_choices_mr (wizard_name, spell_name)
        values
      (get_current_wizard(), vspell_name);
    --
    -- set imaginary to false if this is a monster spell
    if exists(select 1 from monster_spells
      where spell_name = vspell_name) then
      update wizard_spell_choices_mr
        set imaginary = false
        where wizard_name = get_current_wizard();
    else
      update wizard_spell_choices_mr
        set imaginary = null
        where wizard_name = get_current_wizard();
    end if;
  end if;
  perform add_history_choose_spell();
end;
$$ language plpgsql volatile;

create function action_choose_no_spell() returns void as $$
begin
  perform check_can_run_action('choose_no_spell');
  delete from wizard_spell_choices_mr where wizard_name = get_current_wizard();
end;
$$ language plpgsql volatile;

create function action_set_imaginary() returns void as $$
begin
  perform check_can_run_action('set_imaginary');
  update wizard_spell_choices_mr
    set imaginary = true
    where wizard_name = get_current_wizard();
end;
$$ language plpgsql volatile;

create function action_set_real() returns void as $$
begin
  perform check_can_run_action('set_real');
  update wizard_spell_choices_mr
    set imaginary = false
    where wizard_name = get_current_wizard();
end;
$$ language plpgsql volatile;
/*
=== internals
generate the individual spell choice actions

*/

select generate_spell_choice_actions();

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
begin
  --remove current wizard's spell from spell book
  --make sure we only remove one shot of the spell
  --don't remove disbelieve
  update spell_choice_hack_table
    set spell_choice_hack = true;

  delete from spell_parts_to_cast_table;
  delete from cast_success_checked_table;

  delete from spell_books where id =
    (select id from spell_books
       natural inner join wizard_spell_choices
       where wizard_name = get_current_wizard()
         and spell_name != 'disbelieve'
         limit 1);
  -- and wipe it from the wizard_spell_choices_table
  delete from wizard_spell_choices_mr
    where wizard_name = get_current_wizard();

  update spell_choice_hack_table
    set spell_choice_hack = false;

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
    union
    --world alignment same as spell alignment
    --  proportionately more easy
    select spell_name, sign(alignment) as salign,
      limit_chance(base_chance + (@ get_world_alignment()) * 10),
      'same' as alignment from spells_mr
    union
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
    select x,y into tx,ty from empty_squares order by random() limit 1;
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
        from pieces_on_top
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
begin
  --turn dead creature on square to live undead
  select into r ptype,allegiance,tag
    from pieces_on_top
    where (x,y) = (px,py);
  update pieces
    set allegiance = get_current_wizard(),
        tag = get_next_tag(r.ptype,get_current_wizard())
    where (ptype,allegiance,tag)::piece_key = r
    returning tag into r.tag;
  insert into crimes_against_nature (ptype,allegiance,tag)
    values (r.ptype,get_current_wizard(),r.tag);
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
    set allegiance = get_current_wizard(),
        tag = get_next_tag(r.ptype,get_current_wizard())
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
    from pieces_on_top_view where (x,y) = (px,py);

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
begin
  perform create_monster(
    (select ptype from current_wizard_spell
      natural inner join summon_spells),
    get_current_wizard(), x, y, coalesce((
      select imaginary
      from wizard_spell_choices_imaginary
      where wizard_name = get_current_wizard()),false));
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


create table cast_magic_wood_squares (
  x int,
  y int,
  unique (x,y)
);
select set_relvar_type('cast_magic_wood_squares', 'stack');

create view adjacent_to_new_tree_squares as
  select tx as x, ty as y from
    board_ranges natural inner join
    cast_magic_wood_squares
    where range = 1;

--take into account range, line of sight,
--atm only takes into account empty squares
--and trees cannot be next to each other
create view cast_magic_wood_available_squares as
select * from empty_and_not_adjacent_to_tree_squares
except select * from adjacent_to_new_tree_squares;

create type ipos as (
  index int,
  x int,
  y int
);
create function get_square_range(x int, y int, range int)
  returns setof ipos as $$
declare
  p ipos;
begin
  p.index := 0;
  if range < 1 then
    return;
  end if;
  --top row
  p.y = y - range;
  for i in 0 .. (range * 2) loop
    p.x = x - range + i;
    return next p;
    p.index := p.index + 1;
  end loop;
  --sides
  for i in 1 .. (range * 2 + 1) - 2 loop
    p.x = x - range;
    p.y = y - range + i;
    return next p;
    p.index := p.index + 1;
    p.x = x + range;
    return next p;
    p.index := p.index + 1;
  end loop;
  --bottom row
    p.y = y + range;
  for i in 0 .. (range * 2) loop
    p.x = x - range + i;
    return next p;
    p.index := p.index + 1;
  end loop;
end;
$$ language plpgsql immutable;


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

create function cast_magic_wood() returns void as $$
declare
  casted int;
  range int;
  pos_in_square int;
  max_pos_in_square int;
  wx int;
  wy int;
  r record;
  s text;
begin
  casted := 0;
  range := 1;
  pos_in_square := 0;
  max_pos_in_square := 7;
  wx := (select x from pieces
     where ptype = 'wizard'
     and allegiance = get_current_wizard());
  wy := (select y from pieces
     where ptype = 'wizard'
     and allegiance = get_current_wizard());

  while (casted < 8 and range <= 15) loop
    select into r * from get_square_range(wx, wy, range)
      where index = pos_in_square;
--    s := 'checking ' || ip.x || ',' || ip.y;
    if exists(select 1 from cast_magic_wood_available_squares
       where (x,y) = (r.x, r.y)) then
       insert into cast_magic_wood_squares(x,y) values (r.x, r.y);
       casted := casted + 1;
    else
      null;
    end if;
    if pos_in_square = max_pos_in_square then
      range := range + 1;
      pos_in_square = 0;
      max_pos_in_square = (select max(index) from get_square_range(0,0,range));
    else
      pos_in_square := pos_in_square + 1;
    end if;
  end loop;
  for r in select * from cast_magic_wood_squares loop
    perform create_object(
      'magic_tree', get_current_wizard(), r.x, r.y);
  end loop;
  delete from cast_magic_wood_squares;
  perform add_history_spell_succeeded();
end;
$$ language plpgsql volatile;


/*
== move
Individual Piece move notes
A piece may be 'selected' iff it can move or attack or has a ranged attack.

Once a piece is selected it must first move, then attack, then ranged
attack (skipping bits which don't apply).

The piece can forfeit any part of this (and can choose to move only
part of it's move if walking).

The parts must be in this strict order.

If a piece is unselected before it has moved or done anything then it
remains able to move this turn otherwise its move is over for this
turn.

=== selection and subphase
*/

create function select_piece(pk piece_key) returns void as $$
declare
  nextp text;
  p pos;
begin
  nextp:= piece_next_subphase('start', false, 'none', pk);
  if nextp = 'end' then
    --nothing to do
    delete from pieces_to_move
      where (ptype, allegiance, tag)::piece_key = pk;
    if not exists(select 1 from pieces_to_move) then
      perform action_next_phase();
    end if;
    return;
  end if;
  insert into selected_piece (ptype, allegiance, tag, move_phase, engaged) values
    (pk.ptype, pk.allegiance, pk.tag, nextp, false);

  if nextp = 'motion' and
       exists(select 1 from selected_piece
              natural inner join creature_pieces
              where not flying) then
    update remaining_walk_hack_table
      set remaining_walk_hack = true;
    insert into remaining_walk_table
      select speed from creature_pieces
        natural inner join selected_piece;
    update remaining_walk_hack_table
      set remaining_walk_hack = false;
    perform check_engaged();
  end if;
end;
$$ language plpgsql volatile;


--short cut for interface
--this fails silently if the action is not valid
--client may wrap this in select piece at cursor, but the
--server doesn't require that the client iface uses a cursor
create function action_select_piece_at_position(vx int, vy int)
  returns void as $$
declare
  r piece_key;
begin
  perform check_can_run_action('select_piece_at_position', vx,vy);
  select into r ptype,allegiance, tag from selectable_pieces
         where (x,y) = (vx,vy);
  perform select_piece(r);
end;
$$ language plpgsql volatile;

create function action_unselect_piece() returns void as $$
begin
  perform check_can_run_action('unselect_piece');
  --remove piece from pieces to move
  delete from pieces_to_move where (ptype, allegiance, tag) =
    (select ptype, allegiance, tag from selected_piece);
  --empty selected piece, squares left_to_walk
  update remaining_walk_hack_table
    set remaining_walk_hack = true;
  delete from selected_piece;
  delete from remaining_walk_table;
  update remaining_walk_hack_table
    set remaining_walk_hack = false;
  --if there are no more pieces that can be selected then move to next
  --phase automatically, todo: take into account monsters in blob
  if not exists(select 1 from pieces_to_move) then
    perform action_next_phase();
  end if;

  --insert history
end;
$$ language plpgsql volatile;

create function action_cancel() returns void as $$
begin
  perform check_can_run_action('cancel');
  perform do_next_move_subphase(true,'none');
end;
$$ language plpgsql volatile;
/*
==== internals
*/
create function do_next_move_subphase(skip_attack boolean, phase_done text)
    returns void as $$
declare
  r record;
  nextp text;
begin
  if not exists (select 1 from selected_piece) then
    return;
  end if;
  select into r * from selected_piece
    natural inner join pieces;
  nextp := piece_next_subphase((select move_phase from selected_piece),
             skip_attack, phase_done, (r.ptype, r.allegiance, r.tag)::piece_key);
  if r.move_phase = 'motion' then
    update remaining_walk_hack_table
      set remaining_walk_hack = true;
      delete from remaining_walk_table;
    update remaining_walk_hack_table
      set remaining_walk_hack = false;
  end if;

  if nextp = 'end' then
    perform action_unselect_piece();
  else
    update selected_piece set move_phase = nextp;
  end if;
end;
$$ language plpgsql volatile;


/*
=== movement
*/
create function action_walk(px int, py int) returns void as $$
declare
  p pos;
begin
  perform check_can_run_action('walk', px, py);
  select into p x,y from pieces natural inner join selected_piece;
  perform selected_piece_move_to(px, py);
  perform add_history_walked(p.x,p.y);
  if get_remaining_walk() = 0 then
    perform do_next_move_subphase(false, 'motion');
  end if;
end;
$$ language plpgsql volatile;

create function action_fly(px int, py int) returns void as $$
declare
  p pos;
begin
  perform check_can_run_action('fly', px, py);
  select into p x,y from pieces natural inner join selected_piece;
  perform selected_piece_move_to(px, py);
  perform add_history_fly(p.x,p.y);
  perform do_next_move_subphase(false, 'motion');
end;
$$ language plpgsql volatile;

/*
=== attacking
*/
create function action_attack(px int, py int) returns void as $$
declare
  ap piece_key;
  r piece_key;
  att int;
  def int;
begin
  perform check_can_run_action('attack', px, py);

  --if the attacker is a wizard with shadow form, they lose the shadow
  --form when they attack

  att := (select attack_strength
         from attacking_pieces
         natural inner join selected_piece);
  def := (select physical_defense
         from attackable_pieces
         natural inner join pieces_on_top
         where (x,y) = (px,py));

  --check for shadow form

  select into ap ptype, allegiance,tag
    from selected_piece;

  if ap.ptype = 'wizard' and
     exists(select 1 from wizards
            where wizard_name = ap.allegiance
            and shadow_form) then
    update wizards
      set shadow_form = false
      where wizard_name = ap.allegiance;
  end if;

  select into r ptype, allegiance,tag
    from pieces_on_top
    where (x,y) = (px,py);

  perform add_history_attack(r);


  if not check_random_success('attack', greatest((att - def) * 10 + 50, 10)) then
    --failure
    perform add_history_shrugged_off(r);
    perform do_next_move_subphase(true, 'attack');
    return;
  end if;

  perform add_history_chinned(r);
  perform kill_piece(r);

  --move to the square if walker and square empty
  if exists(select 1 from creature_prototypes
              natural inner join selected_piece)
     and exists(select 1 from selected_piece_move_squares
                where (x,y) = (px,py)) then
    perform selected_piece_move_to(px, py);
  end if;
  perform do_next_move_subphase(true, 'attack');
end;
$$ language plpgsql volatile;

create function action_ranged_attack(px int, py int)
    returns void as $$
declare
  r piece_key;
  att int;
  def int;
begin
  perform check_can_run_action('ranged_attack', px, py);

  att := (select ranged_attack_strength
         from ranged_weapon_pieces
         natural inner join selected_piece);
  def := (select physical_defense
         from attackable_pieces
         natural inner join pieces_on_top
         where (x,y) = (px, py));

  select into r ptype, allegiance,tag
    from pieces_on_top
    where (x,y) = (px, py);

  perform add_history_ranged_attack(r);

  if not check_random_success('ranged_attack', greatest((att - def) * 10 + 50, 10)) then
    --failure
    perform add_history_shrugged_off(r);
    perform do_next_move_subphase(false, 'ranged_attack');
    return;
  end if;

  perform add_history_chinned(r);
  perform kill_piece(r);
  perform do_next_move_subphase(false, 'ranged_attack');
end;
$$ language plpgsql volatile;

/*
=== internals

subphase progression

skip attack is used to tell this routine to skip the attack sub
phase. this is if either the motion subphase was just cancelled, or if
the piece attacked when it was in the motion subphase

*/

create function piece_next_subphase(
  current_subphase text, skip_attack boolean, just_done text, pk piece_key)
  returns text as $$
declare
  r record;
begin
  select into r x,y from pieces
    where (ptype,allegiance,tag)::piece_key=pk;
  if current_subphase = 'start'
     and just_done='none'
     and exists(select 1 from creature_pieces
                where (ptype,allegiance,tag)::piece_key = pk) then
    return 'motion';
  elseif current_subphase not in ('attack','ranged_attack')
         and not skip_attack
         and just_done not in ('attack', 'ranged_attack')
         and exists(select 1 from attacking_pieces
                where (ptype,allegiance,tag)::piece_key=pk)
         and exists(select 1 from attackable_pieces ap
           natural inner join pieces_on_top
           --want to keep rows where the attack piece is range 1 from
           --the piece in question, so the board range source x,y is the
           --x,y of the piece in question, and the target x,y is the
           --x,y positions of the enemy attackable pieces
           inner join board_ranges b on (b.x,b.y,tx,ty)=(r.x,r.y,ap.x,ap.y)
           where allegiance <> pk.allegiance
             and range = 1) then
    return 'attack';
  elseif current_subphase not in ('ranged_attack')
    and just_done not in ('ranged_attack')
    and exists(select 1 from ranged_weapon_pieces
                where (ptype,allegiance,tag)::piece_key = pk) then
    return 'ranged_attack';
  else
    return 'end';
  end if;
end;
$$ language plpgsql volatile;

create function add_chinned_history(px int, py int) returns void as $$
declare
  r piece_key;
begin
    select into r ptype, allegiance, tag
      from pieces_on_top where (x,y) = (px,py);
    perform add_history_chinned(r);
end;
$$ language plpgsql volatile;

create function add_history_shrugged_off(px int, py int) returns void as $$
declare
  r piece_key;
begin
    select into r ptype, allegiance, tag
      from pieces_on_top where (x,y) = (px,py);
    perform add_history_shrugged_off(r);
end;
$$ language plpgsql volatile;

create function selected_piece_move_to(px int, py int) returns void as $$
begin
  -- this is used to move a piece when it walks/flies and as part of a
  -- successful attack to keep the logic for moving a wizard piece
  -- along with his mount in one place
  if
     --this is a ridable monster
     exists(select 1 from selected_piece
       natural inner join monster_pieces
       where ridable) and
     --there is also a wizard on this square
     exists(select 1
      from (select x,y from pieces
            natural inner join selected_piece) as a
      natural inner join pieces
      where ptype='wizard') then
     -- move the wizard also
    update pieces
      set x = px,
          y = py
      where (ptype,allegiance,tag) =
        (select ptype, allegiance, tag
         from (select x,y from pieces
               natural inner join selected_piece) as a
         natural inner join pieces
         where ptype='wizard');
  end if;

  update pieces
    set x = px,
        y = py
    where (ptype,allegiance,tag) =
      (select ptype,allegiance,tag from selected_piece);

  --todo: if diagonal, reduce by 1.5
  if exists(select 1 from creature_pieces
            natural inner join selected_piece
            where not flying) and
     (select move_phase from selected_piece)='motion' then
    update remaining_walk_table
    set remaining_walk = remaining_walk - 1;
    perform check_engaged();
  end if;

end;
$$ language plpgsql volatile;

create view selected_piece_adjacent_attacking_squares as
  select x,y from pieces_on_top
  natural inner join pieces_mr
  where attack_strength is not null
    and allegiance <> (select allegiance
                       from selected_piece)
    and allegiance <> 'dead'
  intersect
  select tx,ty from board_ranges r
  natural inner join selected_piecexy
  where range = 1;


create function check_engaged() returns void as $$
declare
  ag int;
begin
  if exists(select 1 from selected_piece_adjacent_attacking_squares) then
    select into ag agility from pieces_mr
    natural inner join selected_piece;
    if check_random_success('break_engaged', ag * 10) then
      update selected_piece set engaged = false;
    else
      update selected_piece set engaged = true;
    end if;
  else
    update selected_piece set engaged = false;
  end if;
end;
$$ language plpgsql volatile;


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
          select tx, ty from board_ranges b
          inner join spreadable_squares s
            on (s.x,s.y) = (b.tx,b.ty)
          where range = 1
            and (b.x,b.y) = (sp.x,sp.y)
          except
          select x as tx,y as ty
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

/*
== helpers for piece creation and destruction
*/

/*
help to speed up start game - this allows us to select 19 non unique
random spells quicker than using order by random() limit 1 in a loop
*/

create table spell_indexes_no_dis_turm (
  row_number serial unique,
  spell_name text
);
select set_relvar_type('spell_indexes_no_dis_turm', 'stack');

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


insert into spell_indexes_no_dis_turm (spell_name)
  select spell_name from spells_mr
    where spell_name not in ('disbelieve', 'turmoil');

create function create_object(vptype text, vallegiance text, x int, y int)
  returns int as $$
begin
  --assert ptype is an object ptype
  if not exists(select 1 from object_piece_types where ptype = vptype) then
    raise exception 'called create object on % which is not an object', vptype;
  end if;
  return create_piece_internal(vptype, vallegiance, x, y, false);
end
$$ language plpgsql volatile;


create function create_monster(vptype text, allegiance text, x int, y int,
                               imaginary boolean) returns void as $$
begin
  if not exists(select 1 from monster_prototypes where ptype = vptype) then
    raise exception 'called create monster on % which is not a monster', vptype;
  end if;
  perform create_piece_internal(vptype, allegiance, x, y, imaginary);
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
                                px, py, imaginary);
  perform kill_monster((vptype, 'Buddha', vtag));
end
$$ language plpgsql volatile;

-----------------------------------------------------
create function get_next_tag(pptype text, pallegiance text) returns int as $$

  select coalesce(max(tag) + 1, 0) from pieces
  where (ptype,allegiance) = ($1,$2);

$$ language sql stable;

create function create_piece_internal(vptype text, vallegiance text,
                                      vx int, vy int, vimaginary boolean)
                                      returns int as $$
declare
  vtag int;
begin

  insert into pieces (ptype, allegiance, tag, x, y)
    select vptype, vallegiance, get_next_tag(vptype,vallegiance), vx, vy
    returning tag into vtag;

  insert into imaginary_pieces (ptype,allegiance,tag)
    select vptype,vallegiance,vtag where coalesce(vimaginary,false);
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
  if get_current_wizard() = pwizard_name then
    perform action_next_phase();
    --check if this is the last wizard, slightly hacky
    if get_current_wizard() = pwizard_name then
      perform game_completed();
      perform add_history_game_drawn();
      delete from current_wizard_table;
    end if;
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
