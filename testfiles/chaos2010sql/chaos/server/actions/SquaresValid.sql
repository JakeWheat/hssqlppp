/*

Action validity
===============

All the updates in this database are via stored procs (um, functions
in postgres, called actions here), which are simple and modular. Each
'public' stored proc has precondition checks, which also double as a
guide to what the ui can do, so they are exact preconditions. So this
is seriously paranoid code - chaos is serious business.

All these views need a rethink, its a massive mess.

*/
select module('Chaos.Server.Actions.SquaresValid');

/*
pieces on top
-------------

The topmost piece on each square is the one you interact with most of
the time, e.g. when selecting, attacking, etc.

The exception to this rule is when you select a wizard that is in a
magic tree or castle or mounted on a monster.

The pieces_on_top view also determines what sprite is shown in a
square in the ui

*/

create view pieces_with_priorities as
  select ptype,allegiance,tag,x,y,
    case
      when allegiance='dead' then 3
      when ptype='wizard' then 2
      when ptype in (select ptype from monster_prototypes) then 1
      else 0
    end as sp
    from pieces;

--restrict this view taking only the top piece from each square to get
--the final result

create view pieces_on_top as
  select x,y,ptype,allegiance,tag,sp from
    (select row_number() over(partition by (x,y) order by sp) as rn,
            x, y, ptype, allegiance, tag, sp
      from pieces_with_priorities) as pwp where rn = 1;

--create a full view to help with updates

-- question: why does pieces_view natural inner join pieces_on_top
-- return too many rows?

create view pieces_on_top_view as
  select p.* from pieces_mr p
    inner join pieces_on_top
    using (ptype,allegiance,tag);

/*
selectable squares and pieces
-----------------------------

We can't use the pieces on top for the selection because of
the exceptions re castles, magic wood and mounted wizards,
so create a similar view so we can determine the piece that
gets selected by square, this part is just the pieces on top
combined with all the wizards even if they are not on top.

This is finished off using the pieces_to_move relvar.

*/
create view moving_pieces as
  select ptype, allegiance, tag,x,y from pieces_mr
    where speed is not null
      or attack_strength is not null
      or ranged_attack_strength is not null;

create view selectable_pieces_with_priorities as
  select ptype,allegiance,tag,x,y,
    case
      when ptype='wizard' then 0
      else 1
    end as sp
    from moving_pieces
    where (x,y) not in(select x,y from pieces
                       where ptype = 'gooey_blob');

/*

internals
---------
*/

create function distance(int, int, int, int) returns float(24) as $$
  select (point($1, $2) <-> point($3, $4))::float(24) as result;
$$ language sql immutable;

create view board_ranges as
--iterate x,y over each square on board
--  iterate d over 0 to 20
--    iterate tx,ty over each square on board
--      include x,y,d, tx, ty iff d(x,y,tx,ty) < d
--so: we include squares <= to the range, not just squares at that
--range

  select * from generate_series(0, 14) as x
                cross join generate_series(0, 9) as y
                cross join generate_series(1, 20) as range
                cross join generate_series(0, 14) as tx
                cross join generate_series(0, 9) as ty
  where
    --we never need the centre square to be included
    (x,y) != (tx,ty) and
     --round to closest int
    distance(x,y,tx,ty) - 0.5 <= range;

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

create view empty_and_not_adjacent_to_tree_squares as
  select * from empty_squares
  except
  select * from adjacent_to_tree_squares;

--this view contains squares which the 'top piece' is attackable
create view attackable_squares as
  select x,y from attackable_pieces
    natural inner join pieces_on_top;

--this view contains squares which the 'top piece' is a creature
--(i.e. a wizard or monster)
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

-- all the squares valid for walking to at this time
-- so we start with a selected piece, and at the bottom
-- we make sure it has squares left to walk
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
    -- wizards can't attack magic trees but monsters can
    and not (p.ptype='magic_tree' and s.ptype='wizard')
    -- logic isn't quite right - a wizard can only attack undead with
    -- the imagic weapon so e.g. they shouldn't be able to attack h2h
    -- if they only have a magic bow
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
/*
valid actions
=============

The end result: two relvars, one with x,y, to list all the valid
actions at any time.

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
--skip spell ** why is this commented out? **
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
-- generate a separate choose action wrapper for each spell
--
-- without this, we can add a general choose spell action but then we
-- first check if the current player can choose a spell at this time,
-- and then check if they have the particular spell they are trying to
-- choose.
--
-- By creating these simple wrappers, we can check both at once, and
-- also the ui has one simple test to see if a spell choice action is
-- valid instead of two stages.
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
