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
/*
valid actions
=============

The end result: two relvars, one with x,y, to list all the valid
actions at any time.

*/


-------------------------------------
/*
ideas:

quite a complicated process, want to try to make it declarative rather
than procedural and see if can make quick

the result needs to be action_name,x,y
where the action name is one of
cast_target_spell
select_piece
walk
fly
attack
ranged_attack

in order to make it quick and also make it understandable, do a two
stage process.

In the first view, we collect together all the information from the
pieces - their position and other attributes

The in the second view, we combine the turn sequence information to
get the final list of valid target squares.

[update: exposing some intermediates which are used by the board
sprites and ai]

what do we need to collect in the first view?
first categorize each square into one or more of the following:
completely empty
corpse only
attackable piece on top
adjacent to tree
wizard
mount_enter

the we add all the extra information from the pieces information that
will be used by the second view

we can get the category of spell that can be cast on a square for
cast_target_spell select_piece: need all pieces not in pieces_moved
that are in the current wizard's army: so need to keep the
ptype,allegiance and tag

walk,fly: pretty straight forward

attack,ranged attack: need the allegiance

we need to be able to implement the
following special cases:

cannot select piece under blob: use attackable on top for selection

dismount, exit: the wizard isn't the piece on top when he is on a
mount or in a castle or tree - won't be in the attackable pieces
squares which is what we're using to feed to selectable pieces, so add
the wizard squares as extra category

mount, enter: need to add these squares in as extra category, so we
can add them to the walk/fly squares iff the selected piece is a
wizard

trees: monsters will attack any tree, wizards will move into an empty
magic tree and attack occupied ones as well as shadow_trees

undead: if a piece is undead, it can only be attacked by another
undead creature or a magic weapon

some spells can be cast on corpses as well as monsters/wizards, so we
include corpses in the attackable on top category, and filter corpses
where neccessary using the allegiance info.

  */

create function one_square_away(pos) returns setof pos as $$
               select $1.x-1 as x,$1.y-1 as y
     union all select $1.x-1,$1.y
     union all select $1.x-1,$1.y+1
     union all select $1.x,$1.y-1
     union all select $1.x,$1.y+1
     union all select $1.x+1,$1.y-1
     union all select $1.x+1,$1.y
     union all select $1.x+1,$1.y+1
$$ language sql immutable;

create view squares_valid_categories as
  with
    squares as (select x,y from generate_series(0, 14) as x
                      cross join generate_series(0, 9) as y)
   ,sq1 as
    (select unnest
       (case when ptype is null then '{empty,empty_or_corpse_only}'::text[]
          else
            case when physical_defense is not null then '{attackable}'::text[]
                 else '{}'::text[] end ||
            case when ridable or ptype in ('magic_tree','magic_castle','dark_citadel')
                   then '{mount-enter}'::text[]
                 else '{}'::text[] end ||
            case when allegiance = 'dead' then '{corpse_only,empty_or_corpse_only}'::text[]
                 else '{}'::text[] end ||
            case when ptype in ('magic_tree', 'shadow_tree') then '{tree}'::text[]
                 else '{}'::text[] end ||
            case when speed is not null then '{creature_on_top}'::text[]
                 else '{}'::text[] end ||
            case when undead is not null then '{monster_on_top}'::text[]
                 else '{}'::text[] end
        end) as category,
         s.x,s.y,v.ptype,v.allegiance,v.tag,v.undead,v.ridable
      from squares s
      natural left outer join pieces_on_top_view v)
   ,tree_pos as (select x,y from sq1
                 where category='tree')
   ,tree_adj as (select (p).x,(p).y from
                    (select one_square_away((x,y)) as p from tree_pos) as a)
   ,wz as (select x,y,ptype,allegiance,tag from pieces where ptype = 'wizard')
  select * from sq1 where category not in('tree')
  union all select 'empty_and_not_adjacent_to_tree' as category
                   ,x,y,null,null,null,null,null from sq1
            where category='empty'
              and (x,y) not in (select x,y from tree_adj)
  union all select 'wizard',x,y,ptype,allegiance,tag,null,null from wz;

---------------------------

create view spell_valid_squares as
 select category as valid_square_category,x,y from squares_valid_categories;

create view current_wizard_spell_squares as
with
   -- put together a relation with x,y position for the current wizard
   -- and the range for his currently chosen spell
   -- so we only get a row iff the current wizard has a target spell
   -- chosen and it's the cast phase
  cwsr as
       (select x, y, range, valid_square_category
          from current_wizard_table
          inner join wizard_spell_choices
            on current_wizard=wizard_name
          inner join pieces
            on ptype='wizard' and allegiance=current_wizard
          natural inner join target_spells)
  select distinct svs.x,svs.y
    from (select valid_square_category from cwsr) as b
    natural inner join (select tx as x, ty as y
                          from board_ranges
                          where (x,y,range) = (select x,y,range from cwsr)) as a
    natural inner join spell_valid_squares svs;

create view selected_piece_move_squares as
    select x,y
       from selected_piece sp
       inner join squares_valid_categories svc
       on (category in ('empty','corpse_only')
           or (sp.ptype = 'wizard'
               and ((sp.allegiance = svc.allegiance
                     and (svc.ptype in ('magic_castle','dark_citadel')
                          or svc.ridable))
                    or svc.ptype = 'magic_tree')));
-- todo: fix for ranged/h-h
create function is_equipped(text) returns boolean as $$

  select magic_sword or magic_knife or magic_bow
    from wizards where wizard_name = $1;

$$ language sql stable;

create view selected_piece_attackable_squares as
  with
    spp as
      (select x,y,ptype,allegiance,
              coalesce(flying,false) as flying,
              speed,move_phase,
              range,
              coalesce(undead,false) as undead
         from selected_piece
         natural inner join pieces_mr)
   ,walk_range as
      (select * from one_square_away((select (x,y)::pos from spp)))
   ,attack_ranges as
      (select tx as x, ty as y
       from board_ranges
         where (select flying and move_phase='motion' from spp)
               and (x,y,range) = (select x,y,speed from spp)
       union all (select x,y from walk_range
              where (select move_phase in ('motion','attack') from spp)))
   ,shoot_ranges as
      (select tx as x, ty as y
       from board_ranges
         where (x,y,range) = (select x,y,range from spp
                              where range is not null))
   ,attackable_squares as
      (select x,y
       from squares_valid_categories svc
       cross join (select ptype,undead,allegiance from spp) as spp
       where category = 'attackable'
          and svc.allegiance not in (spp.allegiance, 'dead')
          and not ((svc.ptype = 'magic_tree')
                   and spp.ptype = 'wizard')
          and (not coalesce(svc.undead,false) or (spp.undead or
                                  (spp.ptype = 'wizard'
                                   and is_equipped(spp.allegiance)))))
    select x,y,'attack'::text as action
         /*,svc.ptype,svc.allegiance, svc.undead,get_current_wizard(),
         spp.ptype,spp.allegiance,spp.undead, is_equipped(spp.allegiance),
         category*/
    from attackable_squares
    natural inner join attack_ranges
    union all
    select x,y,'ranged_attack'::text as action
    from attackable_squares
    natural inner join shoot_ranges;

create view selected_piece_move_squares_2 as
with
   spp as
    (select x,y,ptype,allegiance,tag,flying,speed,move_phase,engaged from pieces_mr
     natural inner join selected_piece)
  ,walk_range as
    (select * from one_square_away((select (x,y)::pos from spp))
              where (select not engaged from spp))
  select x,y,'walk'::text as action
    from selected_piece_move_squares
    natural inner join walk_range
    where get_remaining_walk() > 0
          and x between 0 and 14
          and y between 0 and 9
  union all
  select tx as x, ty as y, 'fly'
    from board_ranges br
    natural inner join spp
    inner join selected_piece_move_squares vms
      on (br.tx,br.ty) = (vms.x,vms.y)
    where flying
        and move_phase='motion'
        and range = speed;

create view selectable_pieces as
with
   good_to_go as
     (select 1 from turn_phase_table
      where turn_phase='move'
            and not exists(select 1 from selected_piece))
  ,potnm as
     (select ptype,allegiance,tag,x,y,1 as rn from good_to_go
      cross join current_wizard_table
      inner join pieces_on_top
        on allegiance = current_wizard)
  ,wnm as
     (select ptype,allegiance,tag,x,y,0 as rn from good_to_go
      cross join current_wizard_table
      inner join pieces
        on ptype = 'wizard' and allegiance = current_wizard)
  ,allnm as
      (select * from (select * from potnm
                     union all select * from wnm) as a
       where (ptype,allegiance,tag)
             not in (select ptype,allegiance,tag from pieces_moved))
select ptype,allegiance,tag,x,y from
(select row_number() over (partition by (x,y) order by rn) as rn,ptype,allegiance,tag,x,y
       from allnm) as s where rn = 1;

create view valid_target_actions as
select * from (
--target spells
  select x,y,'cast_target_spell'::text as action
    from current_wizard_spell_squares
--selecting a piece
union all
select x,y,action from (
select x,y, 'select_piece_at_position':: text as action
  from selectable_pieces
--walking
union all
select x,y,action
  from selected_piece_move_squares_2
--attacking
union all
select x,y,action from selected_piece_attackable_squares
)as s1
where get_turn_phase()='move'
) as s
where not exists (select 1 from game_completed_table);

---------------------------------------------

create view valid_activate_actions as
with
  monster_spell as
    (select 1
     from wizard_spell_choices
    natural inner join current_wizard
    natural inner join monster_spells)
  ,cast_phase as
    (select 1 from turn_phase_table where turn_phase='cast')
  ,choose_phase as
    (select 1 from turn_phase_table where turn_phase='choose')
select * from (
--next_phase - always valid
select 'next_phase'::text as action
--choose spell - need one for each spell, add programmatically
--set imaginary
union all
select 'set_imaginary'::text as action
  from monster_spell
--set real
union all
select 'set_real'::text as action
  from monster_spell
--cast activate spell
union all
select 'cast_activate_spell'::text as action
  where exists (select 1
         from cast_phase
         cross join current_wizard_spell
         natural inner join activate_spells)
      or exists(select 1
         from cast_phase
         cross join current_wizard_spell
         where spell_name ='magic_wood')
--skip spell ** why is this commented out? **
--union all
--select 'skip_spell'::text as action
--  where get_turn_phase() = 'cast'
--unselect
union all
select 'unselect_piece'::text as action
  from selected_piece
--next subphase
union all
select 'cancel'::text as action
  from selected_piece
union all
select 'choose_' || spell_name || '_spell'::text as action
  from choose_phase
  cross join current_wizard_table
  inner join spell_books
    on wizard_name = current_wizard
union all
select 'choose_no_spell'::text as action
  from choose_phase
union all
select 'ai_continue'
  from current_wizard_table
  inner join wizards
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

