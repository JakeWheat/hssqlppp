/*

Copyright 2009 Jake Wheat

= Overview

metadata - the readonly,data,stack tags for relvars
read only data - piece prototypes and spells revlars
game data - mainly wizards, spellbooks and pieces relvars
turn sequence - relvars for turn sequence progression
actions - action valid, actions for turn sequence, casting, moving, etc.
history - relar to record actions
new game - functions to reset data relvars and set up new games
test board support - functions to set up a few board layouts for testing
ai - ai for computer controlled wizards

================================================================================

= metadata
== base relvar tags

This stuff is mainly used to produce some half-baked documentation/
diagrams of the database.

*/
select new_module('chaos', 'root');
select new_module('server', 'chaos');
select new_module('metadata', 'server');

create table base_relvar_metadata (
  relvar_name text,
  type text check (type in('readonly', 'data', 'stack'))
);
select add_key('base_relvar_metadata', 'relvar_name');
select add_foreign_key('base_relvar_metadata', 'relvar_name', 'base_relvars');

create function set_relvar_type(vname text, vtype text) returns void as $$
begin
  insert into base_relvar_metadata (relvar_name, type)
    values (vname, vtype);
end;
$$ language plpgsql volatile;

select set_relvar_type('base_relvar_metadata', 'readonly');

/*
This view is only used in the check_code_some_tags function.
*/

create view chaos_base_relvars as
  select object_name,object_type from public_database_objects
  where object_type = 'base_relvar'
  except
        select object_name,object_type from module_objects
        where module_name = 'catalog' and object_type='base_relvar';
/*
part of the tests, will check all the relvars which aren't defined in
system.sql are tagged.
*/

create function check_code_some_tags() returns boolean as $$
declare
  r record;
  success boolean;
begin
  success := true;
  for r in select object_name from chaos_base_relvars
    except select relvar_name from base_relvar_metadata loop
    success := false;
      raise notice
        'table % is not tagged with one of readonly, data, stack',
        r.object_name;
  end loop;
  return success;
end;
$$ language plpgsql volatile;

/*

After we've loaded the sql, we can protect all the readonly relvars
from being updated again using transition constraints (see below for
how they are implemented). This might catch some programming error.

 */

create function protect_readonly_relvars() returns void as $$
declare
  r record;
begin
  for r in select relvar_name, type
           from base_relvar_metadata
           where type='readonly' loop
    perform create_update_transition_tuple_constraint(
      r.relvar_name, r.relvar_name || '_u_readonly', 'false');
    perform create_delete_transition_tuple_constraint(
      r.relvar_name, r.relvar_name || '_d_readonly', 'false');
    perform create_insert_transition_tuple_constraint(
      r.relvar_name, r.relvar_name || '_i_readonly', 'false');
    -- get module
    perform set_module_for_preceding_objects(
    (select module_name from module_objects
          where object_type = 'base_relvar'
            and object_name = r.relvar_name));
  end loop;
end;
$$ language plpgsql volatile;

/*

todo: find way to enforce stack tables empty outside transaction, or
some sort of partial tests on this

*/

/*
== callback notes

add a notify on each table when it is changed. Haven't worked out how
to listen from haskell yet so is unused at the moment.

*/

create function set_notifies_on_all_data_tables() returns void as $$
declare
  r record;
begin
  for r in select relvar_name from base_relvar_metadata where type='data'
  except
  select relvar_name from triggers where trigger_name like '%_changed' loop
    perform notify_on_changed(r.relvar_name);
  end loop;
end;
$$ language plpgsql volatile;

select set_module_for_preceding_objects('metadata');

/*
================================================================================

= read only data

This section defines all the constant data which doesn't change either
during a game or from one game to the next. These are the piece
prototypes, and the spells.

== piece prototypes

=== ddl

Each type of piece starts with the same stats. Once a piece is on the
board, some of these stats can be changed.

So - use a kind of prototype system.  The template for each creature
is held in a read only table, and when a new creature is created on
the board, its stats are copied from this table, and then they can
change if needed.

*/
select new_module('piece_prototypes', 'server');

--creature ranged weapons can be either projectiles or fireballs
create domain ranged_weapon_type as text
  check (value in ('projectile', 'fire'));

create table piece_prototypes_mr (
  ptype text not null,
  flying boolean null,
  speed int null,
  agility int null,
  undead boolean null,
  ridable boolean null,
  ranged_weapon_type ranged_weapon_type null,
  range int null,
  ranged_attack_strength int null,
  attack_strength int null,
  physical_defense int null,
  magic_defense int null
);
select add_key('piece_prototypes_mr', 'ptype');
select set_relvar_type('piece_prototypes_mr', 'readonly');

create view piece_prototypes as
  select ptype from piece_prototypes_mr;

create view creature_prototypes as
  select ptype, flying, speed, agility
    from piece_prototypes_mr
    where flying is not null
    and speed is not null
     and agility is not null;

create view monster_prototypes as
  select ptype, flying, speed, agility, undead, ridable
    from piece_prototypes_mr
    where undead is not null and ridable is not null;

create view object_piece_types as
  select ptype from piece_prototypes_mr where speed is null;

create view ridable_prototypes as
  select ptype from piece_prototypes_mr
    where ridable;

create view enterable_piece_types as
  select 'magic_tree'::text as ptype
  union
  select 'magic_castle'
  union
  select 'dark_citadel';
/*
=== data

TODO: find a way to represent data like this in the source in a much
more readable format.

*/


copy piece_prototypes_mr(ptype,flying,speed,agility,undead,ridable,
ranged_weapon_type,ranged_attack_strength,range,attack_strength,
physical_defense,magic_defense) from stdin;
bat	t	5	4	f	f	\N	\N	\N	1	1	9
bear	f	2	2	f	f	\N	\N	\N	6	7	6
centaur	f	4	5	f	t	projectile	2	4	1	3	5
crocodile	f	1	2	f	f	\N	\N	\N	5	6	2
dark_citadel	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N
dire_wolf	f	3	2	f	f	\N	\N	\N	3	2	7
eagle	t	6	2	f	f	\N	\N	\N	3	3	8
elf	f	1	7	f	f	projectile	2	6	1	2	5
faun	f	1	8	f	f	\N	\N	\N	3	2	7
ghost	t	2	6	t	f	\N	\N	\N	1	3	9
giant	f	2	5	f	f	\N	\N	\N	9	7	6
giant_rat	f	3	2	f	f	\N	\N	\N	1	1	8
goblin	f	1	4	f	f	\N	\N	\N	2	4	4
golden_dragon	t	3	5	f	f	fire	5	4	9	9	5
gooey_blob	\N	\N	\N	\N	\N	\N	\N	\N	\N	1	\N
gorilla	f	1	2	f	f	\N	\N	\N	6	5	4
green_dragon	t	3	4	f	f	fire	4	6	5	8	4
gryphon	t	5	6	f	t	\N	\N	\N	3	5	5
harpy	t	5	5	f	f	\N	\N	\N	4	2	8
horse	f	4	1	f	t	\N	\N	\N	1	3	8
hydra	f	1	6	f	f	\N	\N	\N	7	8	4
king_cobra	f	1	1	f	f	\N	\N	\N	4	1	6
lion	f	4	3	f	f	\N	\N	\N	6	4	8
magic_castle	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N
magic_fire	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N
magic_tree	\N	\N	\N	\N	\N	\N	\N	\N	\N	5	\N
manticore	t	5	8	f	t	projectile	1	3	3	6	6
ogre	f	1	6	f	f	\N	\N	\N	4	7	3
orc	f	1	4	f	f	\N	\N	\N	2	1	4
pegasus	t	5	7	f	t	\N	\N	\N	2	4	6
red_dragon	t	3	5	f	f	fire	3	5	7	9	4
shadow_tree	\N	\N	\N	\N	\N	\N	\N	\N	2	4	\N
skeleton	f	1	4	t	f	\N	\N	\N	3	2	3
spectre	f	1	4	t	f	\N	\N	\N	4	2	6
unicorn	f	4	7	f	t	\N	\N	\N	5	4	9
vampire	t	4	5	t	f	\N	\N	\N	6	8	6
wall	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N
wizard	f	1	3	\N	\N	\N	\N	\N	3	3	5
wraith	f	2	5	t	f	\N	\N	\N	5	5	4
zombie	f	1	3	t	f	\N	\N	\N	1	1	2
\.

select set_module_for_preceding_objects('piece_prototypes');
/*

== spells

Spells come in a number of flavours, the user interface breaks them
down into the same groups that the original chaos instructions did:

wizard spells: upgrade your wizard in some way, most add weaponry

attacking spells: are cast directly on enemy wizards and their
          monsters to kill them or destroy all of a wizards creations.

object spells: create object pieces

miscellaneous spells: various spells not in the other categories

monster spells: summon monsters for a wizard's army

The code breaks the spells down differently: target spells need a
square to be chosen to cast them on activate spells are all the other
spells.

Target spells are further be broken down into summon spells which
create new pieces on the board, and all the other target spells.

casting chance notes:

Each time you cast a spell it can affect the world alignment, which in
turn affects the spell casting chances.

=== ddl
*/
select new_module('spells', 'server');

create domain spell_category as text
       check (value in ('object', 'attacking',
       'wizard', 'miscellaneous', 'monster'));

--what kind of squares can spells be cast on?

create domain spell_square_category as text
  check (value in (
    'empty',
    'empty_or_corpse_only',
    'attackable', --attackable - not in castle, incidental corpses allowed
    'creature_on_top', --creature on top - no blob,
                       --castle, wood, incidental corpses allowed
    'monster_on_top',
    'corpse_only',
    'empty_and_not_adjacent_to_tree'
));

create table spells_mr (
  spell_name text not null,
  base_chance int not null,
  alignment int not null,
  spell_category spell_category not null,
  description text not null,
  activate boolean null,
  target boolean null,
  range int null,
  num int null,
  ptype text null,
  valid_square_category spell_square_category null
);

select add_key('spells_mr', 'spell_name');
select set_relvar_type('spells_mr', 'readonly');

create view spells as
 select spell_name, base_chance, alignment,
   spell_category, description
 from spells_mr;

create view monster_spells as
  select s.* from spells_mr s
    inner join monster_prototypes m
          on s.ptype=m.ptype;

create view spell_valid_square_types as
 select spell_name, base_chance, alignment, spell_category,
   description, valid_square_category
 from spells_mr
 where valid_square_category is not null;

create view spell_ranges as
  select spell_name, base_chance, alignment, spell_category,
   description, range
  from spells_mr
  where range is not null;

create view summon_spells as
  select spell_name, base_chance, alignment, spell_category,
   description, ptype
  from spells_mr
  where ptype is not null;

create view activate_spells as
  select spell_name
  from spells_mr
  where activate is not null
        and activate;

create view target_spells as
  select spell_name
  from spells_mr
  where target is not null
        and target;

create view spells_with_num_shots as
  select spell_name, base_chance, alignment, spell_category,
   description, num
  from spells_mr
  where num is not null;

/*
=== data

*/
copy spells_mr (spell_name, base_chance, alignment, spell_category, description,
  activate, target, range, num, ptype, valid_square_category) from stdin;
dark_citadel	50	-1	object	Gives wizard building to hide in.	\N	\N	8	1	dark_citadel	empty
dark_power	50	-2	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 3 attacks on enemy creatures	\N	t	20	3	\N	creature_on_top
decree	90	1	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 1 attack on an enemy creature.	\N	t	20	1	\N	creature_on_top
disbelieve	100	0	miscellaneous	Allows illusion creatures to be destroyed. This spell has 100% casting chance, and is always available.	\N	t	20	1	\N	monster_on_top
eagle	70	1	monster	monster	\N	\N	1	1	eagle	empty_or_corpse_only
elf	70	2	monster	monster	\N	\N	1	1	elf	empty_or_corpse_only
chaos	80	-2	miscellaneous	Makes the world more chaos.	t	\N	\N	\N	\N	\N
faun	80	-1	monster	monster	\N	\N	1	1	faun	empty_or_corpse_only
ghost	50	-1	monster	monster	\N	\N	1	1	ghost	empty_or_corpse_only
giant	40	1	monster	monster	\N	\N	1	1	giant	empty_or_corpse_only
giant_rat	100	0	monster	monster	\N	\N	1	1	giant_rat	empty_or_corpse_only
goblin	100	-1	monster	monster	\N	\N	1	1	goblin	empty_or_corpse_only
golden_dragon	10	2	monster	monster	\N	\N	1	1	golden_dragon	empty_or_corpse_only
law	80	2	miscellaneous	Makes the world more law.	t	\N	\N	\N	\N	\N
gooey_blob	100	-1	object	Attacks enemy units it covers and randomly spreads across the map. Any unit covered up by a gooey blob will be able to carry on once it is uncovered (except wizards who are killed by gooey blobs).	\N	\N	6	1	gooey_blob	empty_or_corpse_only
gorilla	70	0	monster	monster	\N	\N	1	1	gorilla	empty_or_corpse_only
green_dragon	10	-1	monster	monster	\N	\N	1	1	green_dragon	empty_or_corpse_only
gryphon	60	1	monster	monster	\N	\N	1	1	gryphon	empty_or_corpse_only
harpy	60	-1	monster	monster	\N	\N	1	1	harpy	empty_or_corpse_only
horse	90	1	monster	monster	\N	\N	1	1	horse	empty_or_corpse_only
hydra	50	-1	monster	monster	\N	\N	1	1	hydra	empty_or_corpse_only
justice	50	2	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 3 attacks.	\N	t	20	3	\N	creature_on_top
king_cobra	90	1	monster	monster	\N	\N	1	1	king_cobra	empty_or_corpse_only
large_chaos	60	-4	miscellaneous	Makes the world more chaos.	t	\N	\N	\N	\N	\N
large_law	60	4	miscellaneous	Makes the world more law.	t	\N	\N	\N	\N	\N
lightning	100	0	attacking	Attacks creature it is cast at (more powerful than magic bolt)	\N	t	4	1	\N	attackable
lion	60	1	monster	monster	\N	\N	1	1	lion	empty_or_corpse_only
magic_armour	50	1	wizard	Gives wizard increased protection from attack.	t	\N	\N	\N	\N	\N
magic_bolt	100	0	attacking	Attacks creature it is cast at.	\N	t	6	1	\N	attackable
magic_bow	50	1	wizard	Gives wizard ranged weapon including undead creatures.	t	\N	\N	\N	\N	\N
magic_castle	50	1	object	Gives wizard building to hide in.	\N	\N	8	1	magic_castle	empty
magic_fire	80	-1	object	Attacks and kills enemy units it covers and randomly spreads across the map.	\N	\N	6	1	magic_fire	empty
magic_knife	70	1	wizard	Gives wizard increase attack power including undead creatures.	t	\N	\N	\N	\N	\N
magic_shield	70	1	wizard	Gives wizard increased protection from attack.	t	\N	\N	\N	\N	\N
magic_sword	40	1	wizard	Gives wizard increase attack power including undead creatures.	t	\N	\N	\N	\N	\N
magic_wings	60	0	wizard	Gives wizard ability to fly.	t	\N	\N	\N	\N	\N
magic_wood	80	1	object	Summons up to eight magic trees near your wizard. If you put your wizard in a magic tree and leave him there, he gets a new spell after a few turns.	\N	\N	8	8	magic_tree	empty_and_not_adjacent_to_tree
manticore	50	-1	monster	monster	\N	\N	1	1	manticore	empty_or_corpse_only
ogre	70	-1	monster	monster	\N	\N	1	1	ogre	empty_or_corpse_only
orc	100	-1	monster	monster	\N	\N	1	1	orc	empty_or_corpse_only
pegasus	60	2	monster	monster	\N	\N	1	1	pegasus	empty_or_corpse_only
raise_dead	60	-1	miscellaneous	Allows reanimation of dead bodies left on screen. Any creatures raised from the dead become undead creatures, able to attack other undeads.	\N	t	4	1	\N	corpse_only
red_dragon	10	-2	monster	monster	\N	\N	1	1	red_dragon	empty_or_corpse_only
shadow_form	80	0	wizard	Gives wizard increased protection and allows movement of 3 spaces per turn. Disappears if wizard attacks anything.	t	\N	\N	\N	\N	\N
shadow_wood	50	-1	object	Allows you to place up to eight shadow trees near your wizard. No two trees can be adjacent, and line of sight is needed in placing. Shadow trees can attack anything in contact with them (except undead).	\N	\N	8	8	shadow_tree	empty_and_not_adjacent_to_tree
skeleton	70	-1	monster	monster	\N	\N	1	1	skeleton	empty_or_corpse_only
spectre	60	-1	monster	monster	\N	\N	1	1	spectre	empty_or_corpse_only
subversion	100	0	miscellaneous	Realigns enemy creature to your side.	\N	t	7	1	\N	monster_on_top
turmoil	100	-2	miscellaneous	Randomly moves all objects onscreen to a different location. Only available from a magic tree.	t	\N	\N	\N	\N	\N
unicorn	70	2	monster	monster	\N	\N	1	1	unicorn	empty_or_corpse_only
vampire	20	-2	monster	monster	\N	\N	1	1	vampire	empty_or_corpse_only
vengeance	90	-1	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 1 attack on an enemy creature.	\N	t	20	1	\N	creature_on_top
wall	80	0	object	Allows four wall blocks to be built near the wizard, which blocks creatures paths, but can be flown over.	\N	\N	8	4	wall	empty
wraith	50	-1	monster	monster	\N	\N	1	1	wraith	empty_or_corpse_only
zombie	90	-1	monster	monster	\N	\N	1	1	zombie	empty_or_corpse_only
\.

select set_module_for_preceding_objects('spells');

/*
================================================================================

= game data

Players should not be able to see each others spells before the cast
phase. This is difficult when they are all on the same computer, but
should be easy when they are not.

Imaginary attribute should only be visible to owning player, same
notes as previous.

== global data
*/
select new_module('global_data', 'server');
/*
=== board size
The playing area is 'width' by 'height' squares.
*/
create table board_size (
  width int,
  height int
);
select add_key('board_size', array['width', 'height']);
select constrain_to_zero_or_one_tuple('board_size');
select set_relvar_type('board_size', 'data');


--update operator out param: board_size
create function init_board_size() returns void as $$
begin
  -- default board size
  insert into board_size (width, height) values (15, 10);
end;
$$ language plpgsql volatile;

/*
=== law/ chaos rating

The world has a law/ chaos rating which can be chaos-N, neutral or
law-N. It starts neutral. When the world is chaos, then chaos spells
become easier to cast, and law spells harder, and vice versa. It
becomes more chaos when chaos spells are cast, and more law when law
spells are cast.

*/

create domain alignment as text check (value in ('law', 'neutral', 'chaos'));

--if world alignment = 0, world is neutral, if -ve world is chaos by that amount
--if +ve world is law by that amount
select create_var('world_alignment', 'int');
select set_relvar_type('world_alignment_table', 'data');

create function init_world_alignment() returns void as $$
begin
  insert into world_alignment_table values (0);
end;
$$ language plpgsql volatile;
select set_module_for_preceding_objects('global_data');

/*
== wizards

*/
select new_module('wizards', 'server');

create table wizards (
  wizard_name text,
  shadow_form boolean default false,
  magic_sword boolean default false,
  magic_knife boolean default false,
  magic_shield boolean default false,
  magic_wings boolean default false,
  magic_armour boolean default false,
  magic_bow boolean default false,
  computer_controlled boolean,
  original_place int, -- 0 <= n < num wizards
  expired boolean default false
);
select add_key('wizards', 'wizard_name');
select set_relvar_type('wizards', 'data');

create view live_wizards as
  select *,
         row_number() over(order by original_place) - 1 as place
  from wizards where not expired;

/*
== spell books
Wizard 'wizard_name' is able to cast spell 'spell_name'.
*/

create table spell_books (
  id serial,
  wizard_name text,
  spell_name text
);
select add_key('spell_books', 'id');
select add_foreign_key('spell_books', 'wizard_name', 'wizards');
select add_constraint('no_spells_for_stiffs',
  $$ not exists(select 1 from spell_books
  natural inner join wizards where expired = true)$$,
  array['spell_books', 'wizards']);
select add_foreign_key('spell_books', 'spell_name', 'spells');
select set_relvar_type('spell_books', 'data');

select set_module_for_preceding_objects('wizards');

/*
== pieces
=== piece natural keys
Keys consist of three parts:
type
allegiance
number

- if piece is a wizard -> type is "wizard", allegiance is wizard name,
  number is 0 for all wizards?

- else if dead -> dead aren't owned, allegiance is "dead". (use
  dead-[monster type]-[n] where n is integer, incremented for each
  corpse type. e.g. you can have dead-giant-0, dead-giant-1 and
  dead-eagle-0. Corpses which existed in current game but no longer do
  leave a gap in the numbering.)

- else -> [owning wizard's name]-[type]-[m] where wizard[n] is the
  owning wizard, m is integer incremented per wizard[n]-type, e.g. you
  can have wizard1-goblin-0, wizard2-goblin-0, wizard1-ogre-0,
  etc. (Assuming wizard names are wizard1, etc.). Pieces which existed
  in current game but no longer do leave a gap in the numbering.

To do this will need to roll own sequence type because there will be
many many sequences? Like to create one table to hold all the
sequences: pieces_sequences (prefix text, current_number integer),
where prefix text is the prefix given above. This might be better as
(allegiance text, type text, current_number integer). Locking: intend
to use a server schema database wide lock every update. fastest
solution may be to use row level locking, probably write a function
like sequences to lock row, increment, get number, unlock and return
number.

For simplicity, just use same serial for all pieces for now.

=== relvars

*/
select new_module('pieces', 'server');

--pieces are either a member of a particular wizard's army or
-- they are dead, in which case they are not a member of
-- any wizard's army
create view allegiances as
   select wizard_name as allegiance from wizards
     where expired = false
   union select 'dead' as allegiance;

/*

TODO: maybe make pieces_mr a table called pieces with just the piece
key and position, and then use a view for an analog to pieces with
the stats. Create this view from the prototype stats, the pieces
table, and have table(s) containing things that can affect the stats
and the view selects from the the piece_prototypes and these tables.

I think all the ways stats can change from the prototypes are listed
here:
monster raised from the dead
wizard with upgrade

err... that's it.

*/

create table pieces (
    ptype text,
    allegiance text,
    tag int,
--Piece is on the board at grid position 'x', 'y'.
    x int,
    y int);

select add_key('pieces', array['ptype', 'allegiance', 'tag']);
select add_foreign_key('pieces', 'ptype', 'piece_prototypes');
--piece must be on the board, not outside it
select add_constraint('piece_coordinates_valid',
  ' not exists(select 1 from pieces
  cross join board_size
  where x >= width or y >= height)',
  array['pieces', 'board_size']);
select add_foreign_key('pieces', 'allegiance', 'allegiances');
--temporary constraint while 'fks' to non base relvars are buggy
select add_constraint('dead_wizard_army_empty',
  $$ not exists(select 1 from pieces
    inner join wizards
    on (allegiance = wizard_name)
    where expired = true)$$,
  array['wizards', 'pieces']);
select set_relvar_type('pieces', 'data');

create type piece_key as (
    ptype text,
    allegiance text,
    tag int
);

--create function piece_key_equals(piece_key, piece_key) returns boolean as $$
--  select $1.ptype = $2.ptype and
--         $1.allegiance = $2.allegiance and
--         $1.tag = $2.tag;
--$$ language sql stable;

-- create operator = (
--     leftarg = piece_key,
--     rightarg = piece_key,
--     procedure = piece_key_equals,
--     commutator = =
-- );

create type pos as (
  x int,
  y int
);

-- create function pos_equals(pos, pos) returns boolean as $$
--   select $1.x = $2.x and
--          $1.y = $2.y;
-- $$ language sql stable;

-- create operator = (
--     leftarg = pos,
--     rightarg = pos,
--     procedure = pos_equals,
--     commutator = =
-- );


/*

add two auxiliary tables to track imaginary monsters and raised
monsters who are now undead

*/
create table imaginary_pieces (
    ptype text,
    allegiance text,
    tag int);

select set_relvar_type('imaginary_pieces', 'data');
select add_key('imaginary_pieces', array['ptype', 'allegiance', 'tag']);
select add_foreign_key('imaginary_pieces',
       array['ptype', 'allegiance', 'tag'], 'pieces');
select add_foreign_key('imaginary_pieces', 'ptype',
       'monster_prototypes');

create table crimes_against_nature (
    ptype text,
    allegiance text,
    tag int
);

select set_relvar_type('crimes_against_nature', 'data');
select add_key('crimes_against_nature', array['ptype', 'allegiance', 'tag']);
select add_foreign_key('crimes_against_nature',
       array['ptype', 'allegiance', 'tag'], 'pieces');
select add_foreign_key('crimes_against_nature', 'ptype',
       'monster_prototypes');

create view wizard_upgrade_stats as
select pp.ptype,
       allegiance,
       tag,
       x,
       y,
       false as imaginary,
       magic_wings as flying,
       case when magic_wings then 6
            when shadow_form then 3
            else speed
       end as speed,
       case when shadow_form then agility + 2
            else agility
       end as agility,
       undead,
       ridable,
       case when magic_bow then 'projectile'
            else null
       end as ranged_weapon_type,
       case when magic_bow then 6
            else null
       end as range,
       case when magic_bow then 6
            else null
       end as ranged_attack_strength,
       case when magic_sword then attack_strength + 4
            when magic_knife then attack_strength + 2
            else attack_strength
       end as attack_strength,
       case when magic_armour and shadow_form then physical_defense + 6
            when magic_shield and shadow_form then physical_defense + 4
            when magic_armour then physical_defense + 4
            when magic_shield then physical_defense + 2
            when shadow_form then physical_defense + 2
            else physical_defense
       end as physical_defense,
       magic_defense
  from pieces p
  inner join wizards
    on allegiance = wizard_name
  inner join piece_prototypes_mr pp
    on pp.ptype='wizard'
  where p.ptype = 'wizard';

-- create view imaginary_or_not_pieces as
-- select ptype,allegiance,tag,x,y,coalesce(imaginary,false) as imaginary
--   from pieces
--   natural left outer join (select *,true as imaginary
--                            from imaginary_pieces) as a;

create view pieces_mr as
select ptype,
       allegiance,
       tag,
       x,
       y,
       coalesce(imaginary,case when not ridable is null then false
                               else null end) as imaginary,
       flying,
       speed,
       agility,
       coalesce(raised, undead) as undead,
       ridable,
       ranged_weapon_type,
       range,
       ranged_attack_strength,
       attack_strength,
       physical_defense,
       magic_defense
  from pieces
  natural inner join piece_prototypes_mr
  natural left outer join (select *,true as imaginary
                           from imaginary_pieces) as a
  natural left outer join (select *,true as raised
                           from crimes_against_nature) as b
  where ptype <> 'wizard'
union
select * from wizard_upgrade_stats;

create view creature_pieces as
  select ptype,allegiance,tag,x,y,
    flying,speed,agility
    from pieces_mr
  where flying is not null
    and speed is not null
    and agility is not null;

create view monster_pieces as
  select ptype,allegiance,tag,x,y,
    flying,speed,agility,
    undead,ridable,imaginary
  from pieces_mr
  where flying is not null
    and speed is not null
    and agility is not null
    and undead is not null
    and ridable is not null;

create view dead_monster_pieces as
  select * from monster_pieces
    where allegiance = 'dead';

create view attacking_pieces as
  select ptype,allegiance,tag,x,y,
    attack_strength
  from pieces_mr
  where attack_strength is not null
    and allegiance <> 'dead';

create view ranged_weapon_pieces as
  select ptype,allegiance,tag,x,y,
    ranged_weapon_type,range,ranged_attack_strength
  from pieces_mr
  where ranged_weapon_type is not null
    and range is not null
    and ranged_attack_strength is not null;

create view attackable_pieces as
  select ptype,allegiance,tag,x,y,physical_defense
  from pieces_mr
  where physical_defense is not null;

create view magic_attackable_pieces as
  select ptype,allegiance,tag,x,y,magic_defense
  from pieces_mr
  where magic_defense is not null;


/*

Rules for multiple pieces on one square: only one piece of each
type may occupy a square in particular you can't have two dead bodies
on one square. These are the traditional chaos rules which may change
for other rulesets.

When multiple pieces occupy one square, one is considered to be 'on
top'. This piece
* is the one piece displayed in the UI currently
* the piece upon which any spell cast on that square hits
* the piece which is attacked when another piece attacks or range
  attacks that square

1 item
any
2 items
creature, stiff : creature on top
wizard, mountable monster: mountable monster on top
wizard, box : box on top
stiff, gooey blob : blob on top
monster, gooey blob : blob on top
3 items
wizard, stiff, mountable monster : mountable on top
stiff, monster, blob : blob on top

*/

select set_module_for_preceding_objects('pieces');
/*

================================================================================

= turn sequence

see readme for overview of turn sequence

For the player, there are three phases, but for the computer there are
four phases, the extra one is the autonomous phase in between casting
and moving. In this phase magic fire and gooey blob spread, castles
may disappear, and wizards may receive a new spell from a magic tree.

There are lots of constraints in this section. For an app like this
where all the updates are through stored procs which carefully check
their preconditions, and there are never any multiple updates, this is
a bit excessive. The main takeaway is that you need deferred
constraints or multiple updates for most constraints that involve more
that one table.

== ddl
*/
select new_module('turn_sequence', 'server');

/*
use this to simulate multiple updates:
for a constraint which refers to multiple tables which get updated
during an action_next_phase call, this will be set to true,
false at all other times, so using this can defer constraint checking
till the end of the action_next_phase call after all the relevant
turn phase relvars have been updated. Don't forget to put
in_next_phase_hack_table in the relvar list for the constraint.

*/
select create_var('in_next_phase_hack', 'boolean');
insert into in_next_phase_hack_table values (false);
select set_relvar_type('in_next_phase_hack_table', 'stack');

select create_var('creating_new_game', 'boolean');
insert into creating_new_game_table values (true);
select set_relvar_type('creating_new_game_table', 'stack');

--Turn number, starts at 0 goes up 1 each full turn, just used to provide
--info on how long the game has been going.
select create_var('turn_number', 'int');
select set_relvar_type('turn_number_table', 'data');

--if not creating new game cardinality = 1

select create_update_transition_tuple_constraint(
  'turn_number_table',
  'turn_number_change_valid',
  '(NEW.turn_number = OLD.turn_number + 1)');

create function no_deletes_inserts_except_new_game(relvar_name text)
  returns void as $$
begin
  perform create_delete_transition_tuple_constraint(
    relvar_name,
    relvar_name || '_no_delete',
    'exists(select 1 from creating_new_game_table
      where creating_new_game = true)');
  perform create_insert_transition_tuple_constraint(
    relvar_name,
    relvar_name || '_no_insert',
    'exists(select 1 from creating_new_game_table
      where creating_new_game = true)');

end;
$$ language plpgsql volatile;

select no_deletes_inserts_except_new_game('turn_number_table');

/*
turn phase
must follow choose-cast-auto-move-choose-etc.

wizard spell choices
added row must be for current wizard, and in current wizard's spell book
  in choose phase
removed row must be for current wizard
  in cast phase

spell parts to cast
pieces to move
squares left to walk

*/

create view next_wizard as
select wizard_name, new_wizard_name from
  (select wizard_name as new_wizard_name, place
     from live_wizards) as a inner join
  (select wizard_name,
     (place + 1) %
       (select max(place) + 1 from live_wizards)
      as old_place from live_wizards) as b
  on (place = old_place);


create function next_wizard(text) returns text as $$
  select new_wizard_name from next_wizard
    where wizard_name = $1;
$$ language sql stable;

/*select next_wizard('Buddha');
select next_wizard('Kong Fuzi');
select next_wizard('Laozi');
select next_wizard('Moshe');
select next_wizard('Muhammad');
select next_wizard('Shiva');
select next_wizard('Yeshua');
select next_wizard('Zarathushthra');
*/

--current wizard is the wizard who's turn it is to do stuff in current phase
select create_var('current_wizard', 'text');
select set_relvar_type('current_wizard_table', 'data');
select add_foreign_key('current_wizard_table', 'current_wizard',
  'wizards', 'wizard_name');
select create_update_transition_tuple_constraint(
  'current_wizard_table',
  'next_wizard_change_valid',
  'NEW.current_wizard = next_wizard(OLD.current_wizard)');
select create_delete_transition_tuple_constraint(
    'current_wizard_table',
    'current_wizard_table_no_delete',
    'exists(select 1 from creating_new_game_table
      where creating_new_game = true)
     or exists (select 1 from game_completed_table)');
select create_insert_transition_tuple_constraint(
    'current_wizard_table',
    'current_wizard_table_no_insert',
    'exists(select 1 from creating_new_game_table
      where creating_new_game = true)');


--select no_deletes_inserts_except_new_game('current_wizard_table');
select add_constraint('current_wizard_must_be_alive',
  $$(select not expired from current_wizard_table
     inner join wizards on current_wizard = wizard_name)$$,
  array['wizards', 'current_wizard_table']);

/*
wizard field in most tables and views is named wizard_name

instead of tediously writing out inner join blah on wizard_name =
current_wizard use the following view to instead write natural inner
join current_wizard . Not that much less tedious though.

*/

create view current_wizard as
  select current_wizard as wizard_name from current_wizard_table;

--turn phase enum: choose spell, cast spell, autonomous, move
create domain turn_phase_enum as text
       check (value in ('choose', 'cast', 'autonomous', 'move'));

create function next_turn_phase(text) returns text as $$
  select case
    when $1='choose' then 'cast'
    when $1='cast' then 'autonomous'
    when $1='autonomous' then 'move'
    when $1='move' then 'choose'
  end as result
$$ language sql immutable;

select create_var('turn_phase', 'turn_phase_enum');
select set_relvar_type('turn_phase_table', 'data');
select create_update_transition_tuple_constraint(
  'turn_phase_table',
  'turn_phase_change_valid',
  'NEW.turn_phase = next_turn_phase(OLD.turn_phase)');
select no_deletes_inserts_except_new_game('turn_phase_table');

create type turn_pos as (
    turn_number int,
    turn_phase turn_phase_enum,
    current_wizard text
);

-- create function turn_pos_equals(turn_pos, turn_pos) returns boolean as $$
--   select $1.turn_number = $2.turn_number and
--          $1.turn_phase = $2.turn_phase and
--          $1.current_wizard = $2.current_wizard;
-- $$ language sql stable;

-- create operator = (
--     leftarg = turn_pos,
--     rightarg = turn_pos,
--     procedure = turn_pos_equals,
--     commutator = =
-- );

create function get_current_turn_pos() returns turn_pos as $$
  select (turn_number, turn_phase, current_wizard)::turn_pos
    from turn_number_table
    cross join turn_phase_table
    cross join current_wizard_table;
$$ language sql stable;


/*

Both spell casting and moving have a bunch of state local to each
wizards turn in the that phase. Wizard spell choices is a piece of
turn phase state which is constructed bit by bit in the choice phase
then read in the cast phase, so this lasts from the start of the
choice phase to the end of the cast phase.

*/
create table wizard_spell_choices_mr (
  wizard_name text not null,
  spell_name text not null,
  imaginary boolean null
);
select add_key('wizard_spell_choices_mr', 'wizard_name');
select add_constraint('dead_wizard_no_spell',
  $$ not exists(select 1 from wizard_spell_choices_mr
    natural inner join wizards
    where expired = true)$$,
  array['wizards', 'pieces']);

create view wizard_spell_choices as
  select wizard_name, spell_name
    from wizard_spell_choices_mr;

create view wizard_spell_choices_imaginary as
  select wizard_name, imaginary
    from wizard_spell_choices_mr
    where imaginary is not null;

/*

todo: add constraint to say imaginary must be set for monsters and
must not be set for non-monsters (will need a multiple update hack to
go with this)

*/

--shortcut for current wizard's spell
create view current_wizard_spell as
  select spell_name from wizard_spell_choices
    natural inner join current_wizard;

create function get_current_wizard_spell() returns text as $$
  select spell_name from current_wizard_spell;
$$ language sql stable;

/*this really needs multiple updates

--select add_foreign_key('wizard_spell_choices', array['wizard_name',
--  'spell_name'], 'spell_books');

the problem is that in the action_next_phase for the end of a wizards
cast phase we want to delete the spell choice from this table, and
also delete the spell from the wizards spell book. The code deletes
the spell from the spell book first, but since the spell choice
references the spell book table, the reference stops the delete.

We can't use a conventional cascade delete since there may be multiple
rows in the spell book for the same spell/wizard combo - this isn't a
foreign key in sql sense.

One alternative is to save the wizard and spell names in a variable so
we can delete the spell choice first then the spell book entry, but
that is pretty inelegant.

We could do it properly with multiple updates, so simulate this by
writing out the fk by hand and adding the in next phase hack.

*/
select create_var('spell_choice_hack', 'boolean');
insert into spell_choice_hack_table values (false);
select set_relvar_type('spell_choice_hack_table', 'stack');

select add_constraint('wizard_spell_choices_wizard_name_spell_name_fkey',
$$((select spell_choice_hack from spell_choice_hack_table) or
not exists(select wizard_name, spell_name from wizard_spell_choices
  except
select wizard_name, spell_name from spell_books))$$,
array['spell_choice_hack_table', 'wizard_spell_choices_mr', 'spell_books']);


/*
if choose phase: only current and previous wizards may have a row
if cast phase: only current and subsequent wizards may have a row
this constraint really needs multiple updates.
*/
select add_constraint('chosen_spell_phase_valid',
$$
((select in_next_phase_hack from in_next_phase_hack_table) or
(((select turn_phase='choose' from turn_phase_table) and
 (select max(place) from wizard_spell_choices
   natural inner join live_wizards) <=
 (select place from live_wizards
   inner join current_wizard_table
     on wizard_name = current_wizard))
or
((select turn_phase='cast' from turn_phase_table) and
 (select min(place) from wizard_spell_choices
    natural inner join live_wizards) >=
  (select place from live_wizards
    inner join current_wizard_table
      on wizard_name = current_wizard))
or not exists(select 1 from wizard_spell_choices)
))$$, array['turn_phase_table', 'current_wizard_table',
    'wizard_spell_choices_mr', 'wizards', 'in_next_phase_hack_table']);

select create_update_transition_tuple_constraint(
  'wizard_spell_choices_mr',
  'update_spell_choice_restricted',
  $$(select turn_phase = 'choose' from turn_phase_table)
    and (NEW.wizard_name = OLD.wizard_name)
    and (select current_wizard = NEW.wizard_name from current_wizard_table)$$);
select create_insert_transition_tuple_constraint(
  'wizard_spell_choices_mr',
  'insert_spell_choice_restricted',
  $$(select turn_phase = 'choose' from turn_phase_table)
    and (select current_wizard = NEW.wizard_name from current_wizard_table)$$);
select create_delete_transition_tuple_constraint(
  'wizard_spell_choices_mr',
  'delete_spell_choice_restricted',
  $$(select turn_phase in ('cast', 'choose') from turn_phase_table)$$);

select set_relvar_type('wizard_spell_choices_mr', 'data');

/*

if wizard is skipping casting a spell then no tuple appears in this
relvar for that wizard

spellparts to cast is local to spell casting phase for each wizard

current wizard has cast amount spell parts in this turn phase

when entering spell cast phase, this is set to 0 if wizard has no
spell or max number of casts otherwise

*/

select create_var('spell_parts_to_cast', 'int');
select set_relvar_type('spell_parts_to_cast_table', 'data');

select add_constraint('parts_to_cast_only', $$
  ((select turn_phase = 'cast' from turn_phase_table)
  or not exists(select 1 from spell_parts_to_cast_table))
$$, array['turn_phase_table', 'spell_parts_to_cast_table']);

/*
If casting multipart spell, only check success on first part.
Store whether current wizard's spell needs a success check here.
make sure to reset it each next phase during cast phase
*/

select create_var('cast_success_checked', 'boolean');
select set_relvar_type('cast_success_checked_table', 'data');
select add_constraint('cast_checked_cast_only', $$
  ((select turn_phase = 'cast' from turn_phase_table)
  or not exists(select 1 from cast_success_checked_table))
$$, array['cast_success_checked_table', 'turn_phase_table']);

/*

casting affecting alignment

how does a successful or unsuccessful spell affect world alignment?
do unsuccessful spells have any effect?
does the current world alignment affect the effect?
is there a limit to how much the alignment can change in a turn?
is each spell's effect independent of what other spells are cast that turn?

what about:
  each spell can affect the world alignment
  spell alignments don't add up, the result is taken by random
    from one of the spells cast that turn
  e.g.
  0, -1, -4, 2, -1: five spells cast with alignments given
    chose one of these at random, each with 1/5 chance
    then adjust alignment by this (align/2 with probability for halfs?)

current plan:
only successful spells affect alignment
keep track of all spells during cast phase
sum up total alignment, divide by 2, each full number affects alignment
the fractional part has probability to affect it
maximum change is 2

this means that law increases alignment by one and large law does it
by two in the absence of any other spells.

*/
select create_var('cast_alignment', 'integer');
select set_relvar_type('cast_alignment_table', 'stack');

select add_constraint('cast_alignment_empty',
  $$((get_turn_phase() = 'cast') or
  not exists(select 1 from cast_alignment_table))$$,
  array['turn_phase_table', 'cast_alignment_table']);

create function adjust_world_alignment() returns void as $$
declare
  abs_change float;
begin
  select into abs_change
    min(abs(get_cast_alignment()) / 2, 2);
  update world_alignment_table
    set world_alignment = world_alignment
      + trunc(abs_change) * sign(get_cast_alignment());
  --get fractional part
  if (random() < abs_change - trunc(abs_change)) then
    update world_alignment_table
      set world_alignment = world_alignment +
        sign(get_cast_alignment());
  end if;
  update cast_alignment_table set cast_alignment = 0;
end;
$$ language plpgsql volatile;



/*

pieces to move and selected piece are local to move phase for each
wizard

Piece in this table from current wizard's army hasn't yet moved
in this turn.

TODO: i think switching this from pieces to move to pieces_moved will
be a bit more straightforward

*/
create table pieces_to_move (
    ptype text,
    allegiance text,
    tag int
);
select add_key('pieces_to_move', array['ptype', 'allegiance', 'tag']);
--cascade delete here:
select add_foreign_key('pieces_to_move', array['ptype', 'allegiance', 'tag'],
                       'pieces');
select add_foreign_key('pieces_to_move', 'allegiance',
                       'current_wizard_table', 'current_wizard');
select set_relvar_type('pieces_to_move', 'data');
select add_constraint('pieces_to_move_empty',
$$((select turn_phase = 'move' from turn_phase_table) or
not exists (select 1 from pieces_to_move))$$,
array['pieces_to_move', 'turn_phase_table']);

create domain move_phase as text
  check (value in ('motion', 'attack', 'ranged_attack'));

create table selected_piece (
  ptype text,
  allegiance text,
  tag int,
  move_phase move_phase,
  engaged boolean
); -- 0 to 1 tuple when in move phase,
-- piece key from current wizards army, empty otherwise
select add_key('selected_piece', array['ptype', 'allegiance', 'tag']);
select add_foreign_key('selected_piece', array['ptype', 'allegiance', 'tag'],
                       'pieces');
select add_foreign_key('selected_piece', 'allegiance',
                       'current_wizard_table', 'current_wizard');
select constrain_to_zero_or_one_tuple('selected_piece');
select set_relvar_type('selected_piece', 'data');


/*

squares left to walk is local to the current moving piece during
its walking phase, not used if piece is not a walker.

TODO: this doesn't take into account e.g. move of 3 squares, move
diagonal, second diagonal move all move used up, can't do three
diagonal moves.

*/
select create_var('remaining_walk', 'int');
select set_relvar_type('remaining_walk_table', 'data');
select create_var('remaining_walk_hack', 'boolean');
select set_relvar_type('remaining_walk_hack_table', 'stack');
insert into remaining_walk_hack_table values (false);

select add_constraint('remaining_walk_only_motion',
$$ ((not exists(select 1 from remaining_walk_table)) or
   exists(select 1 from creating_new_game_table
      where creating_new_game = true) or
   (select remaining_walk_hack
     from remaining_walk_hack_table) or
   (exists(select 1 from selected_piece)
      and (select move_phase = 'motion' from selected_piece)
      and exists (select 1 from creature_pieces
                  natural inner join selected_piece)
      and (select not flying from creature_pieces
           natural inner join selected_piece))) $$,
  array['selected_piece', 'pieces', 'remaining_walk_table',
        'remaining_walk_hack_table', 'creating_new_game_table']);

--this function is used to initialise the turn phase data.
create function init_turn_stuff() returns void as $$
begin
  --this should catch attempts to start a game
  --which has already been started
  if exists(select 1 from turn_number_table) then
    raise exception 'new game started when turn number table not empty';
  end if;
  insert into turn_number_table values (0);
  insert into turn_phase_table
    values ('choose');
  insert into current_wizard_table
    select wizard_name from live_wizards
    order by place limit 1;
end;
$$ language plpgsql volatile;

/*

table to cache if the game is over: someone has one or it's a draw.
(This also makes it possible to have a draw when there are wizards
remaining.)

*/

select create_var('game_completed', 'boolean');
select set_relvar_type('game_completed_table', 'data');
select add_constraint('game_completed_wizards',
       $$(not exists(select 1 from game_completed_table)
           or (select count(1) <= 1 from live_wizards))$$,
       array['game_completed_table']);

create function game_completed() returns void as $$
begin
  insert into game_completed_table
    select true where not exists (select 1 from game_completed_table);
end;
$$ language plpgsql volatile;

-- 1 tuple iff current moving piece walks, empty otherwise


/*
================================================================================

= Actions
*/
select new_module('actions', 'server');
/*
== Testing

for testing purposes sometimes want to make a given nondeterministic
action always fail or always succeed.

The categories are:
castle disappear
gooey blob spread
attack
ranged attack
resist: decree, lightning, subversion
cast spell

you have to set the override each time you want to override something

*/

create domain random_test text check (value in
       ('disappear', 'spread', 'attack',
        'ranged_attack', 'resist', 'cast',
        'bonus','break_engaged'));

create table test_action_overrides (
  override random_test,
  setting bool
);
select set_relvar_type('test_action_overrides', 'data');

create function action_rig_action_success(poverride random_test,
       psetting boolean) returns void as $$
begin
  insert into test_action_overrides (override, setting)
    values (poverride, psetting);
end;
$$ language plpgsql volatile;

select add_key('test_action_overrides', 'override');

/*
== random numbers
run all random tests through this, so that we can hook into them
during testing.
*/
create function check_random_success(t random_test, successPercentage int)
  returns boolean as $$
declare
  o boolean;
begin
  o := (select setting from test_action_overrides
       where override = t);
  if o is null then --normal random
    return (random() * 100) < successPercentage;
  else --overriden
    delete from test_action_overrides
      where override = t;
    return o;
  end if;
end;
$$ language plpgsql volatile;

create function min(integer, integer) returns integer as $$
  select min(n) from (select $1 as n union select $2 as n) as a;
$$ language sql immutable;

create function max(integer, integer) returns integer as $$
  select max(n) from (select $1 as n union select $2 as n) as a;
$$ language sql immutable;

create function limit_chance(integer) returns integer as $$
  select max(10, min($1, 100));
$$ language sql immutable;

/*
== action validity

*/
select new_module('squares_valid', 'actions');

/*
=== pieces on top

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
=== selectable squares and pieces

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

=== internals
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
    --slightly hacky, we never need the centre square to be included so
    --exclude it here even though it's not quite mathematically correct
    (x,y) != (tx,ty) and
    distance(x,y,tx,ty) - 0.5 <= range; --round to closest int

select set_module_for_preceding_objects('squares_valid');

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
       natural inner join spell_valid_square_types
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
          natural inner join spell_ranges
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

--select create_var('dont_nest_ai_next_phase', 'bool');
--select set_relvar_type('dont_nest_ai_next_phase_table', 'stack');

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
        select coalesce(num, 0) from spells_with_num_shots
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
create function generate_spell_choice_actions() returns void as $$
declare
  sn text;
  s text;
begin
  for sn in select spell_name from spells loop
  s := $a$
create function action_choose_$a$ || sn || $a$_spell() returns void as $b$
begin
  perform check_can_run_action('choose_$a$ || sn || $a$_spell');
  perform action_choose_spell('$a$ || sn || $a$');
end;
$b$ language plpgsql volatile;
$a$;
  execute s;
  end loop;
end;
$$ language plpgsql volatile;

select generate_spell_choice_actions();
drop function generate_spell_choice_actions();

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
  if (select spell_category = 'wizard' from spells
      natural inner join current_wizard_spell) then
    perform action_cast_wizard_spell(get_current_wizard(),
      get_current_wizard_spell());
  elseif exists(select 1 from spells
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
      'neutral' as alignment from spells
    union
    --world alignment same as spell alignment
    --  proportionately more easy
    select spell_name, sign(alignment) as salign,
      limit_chance(base_chance + (@ get_world_alignment()) * 10),
      'same' as alignment from spells
    union
    --world alignment opposite, spell slightly harder
    select spell_name, sign(alignment) as salign,
      limit_chance(base_chance - 10),
      'opposite' as alignment from spells) as a
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
  select into r ptype, allegiance, tag, imaginary
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
      (select alignment from spells
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


  if not check_random_success('attack', max((att - def) * 10 + 50, 10)) then
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

  if not check_random_success('ranged_attack', max((att - def) * 10 + 50, 10)) then
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
       (select spell_name from spells
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
  row_number serial,
  spell_name text
);
select set_relvar_type('spell_indexes_no_dis_turm', 'readonly');
select add_key('spell_indexes_no_dis_turm', 'row_number');


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

select set_module_for_preceding_objects('actions');

/*
================================================================================

= history
save a short description of each action completed during play

TODO: create a detailed history that allows a game to be replayed
then create a view to show the player visible log with some events
removed and some combined.
*/

select new_module('action_history', 'server');

create domain history_name_enum as text
       check (value in (
                        'spell_succeeded'
                       ,'spell_failed'
                       ,'chinned'
                       ,'shrugged_off'
                       ,'walked'
                       ,'fly'
                       ,'attack'
                       ,'ranged_attack'
                       ,'set_imaginary'
                       ,'set_real'
                       ,'game_won'
                       ,'game_drawn'
                       ,'spell_skipped'
                       ,'new_turn'
                       ,'wizard_up'
                       ,'choose_spell'
                       ,'spread'
                       ,'recede'
                       ,'disappear'
                       ,'new_game'
                       ,'attempt_target_spell'
                       ));


create table action_history_mr (
  id serial not null,
  history_name history_name_enum not null,
  ptype  text null,
  allegiance text  null,
  tag int null,
  spell_name text null,
  turn_number int null,
  turn_phase  turn_phase_enum null,
  num_wizards int null,
  x int null,
  y int null,
  tx int null,
  ty int null
);
select add_key('action_history_mr', 'id');
select set_relvar_type('action_history_mr', 'data');

--Turns

create function get_current_wizard_pos() returns pos as $$
  select x,y from pieces
    where allegiance=get_current_wizard()
      and ptype = 'wizard';
$$ language sql stable;

create function add_history_new_turn() returns void as $$
begin
  insert into action_history_mr (history_name, turn_number)
    values ('new_turn', get_turn_number());
end;
$$ language plpgsql volatile;

create function add_history_wizard_up() returns void as $$
declare
  w pos;
begin
  w := get_current_wizard_pos();
  insert into action_history_mr (history_name, allegiance, turn_phase, x, y)
    values ('wizard_up', get_current_wizard(), get_turn_phase(), w.x, w.y);
end;
$$ language plpgsql volatile;

create function add_history_new_game() returns void as $$
begin
  insert into action_history_mr (history_name, num_wizards)
    values ('new_game', (select count(1) from wizards));
end;
$$ language plpgsql volatile;

create function add_history_game_won() returns void as $$
declare
  w pos;
begin
  w := get_current_wizard_pos();
  insert into action_history_mr (history_name, allegiance, x, y)
    values ('game_won', (select allegiance
                         from pieces
                         where ptype='wizard'),
                         w.x, w.y);
end;
$$ language plpgsql volatile;

create function add_history_game_drawn() returns void as $$
begin
  insert into action_history_mr (history_name)
    values ('game_drawn');
end;
$$ language plpgsql volatile;

--Choosing

create function add_history_choose_spell() returns void as $$
declare
  w pos;
begin
  w := get_current_wizard_pos();
  insert into action_history_mr (history_name, allegiance, spell_name,x,y)
    values ('choose_spell', get_current_wizard(), get_current_wizard_spell(),
            w.x,w.y);
end;
$$ language plpgsql volatile;

create function add_history_set_imaginary() returns void as $$
begin
  insert into action_history_mr (history_name, allegiance)
    values ('set_imaginary', get_current_wizard());
end;
$$ language plpgsql volatile;

create function add_history_set_real() returns void as $$
begin
  insert into action_history_mr (history_name, allegiance)
    values ('set_real', get_current_wizard());
end;
$$ language plpgsql volatile;

--Casting

create function add_history_attempt_target_spell(px int, py int) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces
    where allegiance=get_current_wizard()
      and ptype = 'wizard';
  insert into action_history_mr (history_name, allegiance, spell_name, x, y, tx, ty)
    values ('attempt_target_spell', get_current_wizard(),
            get_current_wizard_spell(), w.x, w.y, px, py);
end;
$$ language plpgsql volatile;

create function add_history_attempt_activate_spell() returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces
    where allegiance=get_current_wizard()
      and ptype = 'wizard';
  insert into action_history_mr (history_name, allegiance, spell_name, x, y)
    values ('attempt_target_spell', get_current_wizard(),
            get_current_wizard_spell(), w.x, w.y);
end;
$$ language plpgsql volatile;


create function add_history_spell_succeeded() returns void as $$
begin
  insert into action_history_mr (history_name, allegiance, spell_name)
    values ('spell_succeeded', get_current_wizard(), get_current_wizard_spell());
end;
$$ language plpgsql volatile;

create function add_history_spell_failed() returns void as $$
begin
  insert into action_history_mr (history_name, allegiance, spell_name)
    values ('spell_failed', get_current_wizard(), get_current_wizard_spell());
end;
$$ language plpgsql volatile;

create function add_history_spell_skipped() returns void as $$
begin
  insert into action_history_mr (history_name, allegiance, spell_name)
    values ('spell_skipped', get_current_wizard(), get_current_wizard_spell());
end;
$$ language plpgsql volatile;

--chinned and shrugged off

create function add_history_chinned(k piece_key) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype, allegiance,tag, x, y)
    values ('chinned', k.ptype, k.allegiance, k.tag, w.x, w.y);

end;
$$ language plpgsql volatile;

create function add_history_shrugged_off(k piece_key) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype, allegiance,tag, x, y)
    values ('shrugged_off', k.ptype, k.allegiance, k.tag,w.x, w.y);
end;
$$ language plpgsql volatile;

--Autonomous

create function add_history_receive_spell(pwizard_name text, pspell_name text) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces
    where allegiance=pwizard_name
      and ptype = 'wizard';
  insert into action_history_mr (history_name, allegiance, spell_name, x, y)
    values ('choose_spell', pwizard_name, pspell_name, w.x, w.y);
end;
$$ language plpgsql volatile;

create function add_history_spread(k piece_key) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype,allegiance,tag, x, y)
    values ('spread', k.ptype,k.allegiance,k.tag, w.x, w.y);
end;
$$ language plpgsql volatile;

create function add_history_recede(k piece_key) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype,allegiance,tag,x, y)
    values ('recede', k.ptype,k.allegiance,k.tag, w.x, w.y);
end;
$$ language plpgsql volatile;

create function add_history_disappear(k piece_key) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype,allegiance,tag, x, y)
    values ('disappear', k.ptype,k.allegiance,k.tag, w.x, w.y);
end;
$$ language plpgsql volatile;


--Move
create function add_history_walked(sx int, sy int) returns void as $$
declare
  w pos;
  k piece_key;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  select into k ptype,allegiance,tag from selected_piece;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x, y, tx, ty)
    values ('walked', k.ptype, k.allegiance, k.tag, w.x,w.y, sx, sy);
end;
$$ language plpgsql volatile;

create function add_history_fly(sx int, sy int) returns void as $$
declare
  w pos;
  k piece_key;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  select into k ptype,allegiance,tag from selected_piece;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x, y, tx, ty)
    values ('fly', k.ptype, k.allegiance, k.tag, w.x,w.y, sx, sy);
end;
$$ language plpgsql volatile;

create function add_history_attack(t piece_key) returns void as $$
declare
  sp record;
  tp pos;
begin
  select into sp ptype,allegiance,tag,x,y from selected_piece
    natural inner join pieces;
  select into tp x,y from pieces where (ptype,allegiance,tag) = t;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x,y,tx,ty)
    values ('attack', sp.ptype, sp.allegiance, sp.tag, sp.x,sp.y, tp.x,tp.y);
end;
$$ language plpgsql volatile;

create function add_history_ranged_attack(t piece_key) returns void as $$
declare
  sp record;
  tp pos;
begin
  select into sp ptype,allegiance,tag,x,y from selected_piece
    natural inner join pieces;
  select into tp x,y from pieces where (ptype,allegiance,tag) = t;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x,y,tx,ty)
    values ('ranged_attack', sp.ptype, sp.allegiance, sp.tag, sp.x,sp.y, tp.x,tp.y);
end;
$$ language plpgsql volatile;


/*

create a view to hide the details which players shouldn't be able to
see - this is the view that the ui should use:
hide set real, imaginary
hide spell received in tree


*/

select set_module_for_preceding_objects('action_history');

/*
================================================================================

= new game
== wizard starting positions
Wizards start the game in positions set by how many wizards there are in a game:


When there are 'wizard_count' wizards in a game, wizard at place
'place' starts at grid position x, y.

These figures are only valid iff there are 2-8 wizards and the board is
15 x 10. Will have to figure out some other system for more wizards or
different boards.

*/

--in a game with wizard_count wizards, wizard at place 'place' starts
--the game on square x,y.

select new_module('new_game', 'server');

create table wizard_starting_positions (
  wizard_count int,
  place int,
  x int,
  y int
);
select add_key('wizard_starting_positions', array['wizard_count', 'place']);
select add_key('wizard_starting_positions', array['wizard_count', 'x', 'y']);
select add_constraint('wizard_starting_positions_place_valid',
  'not exists(select 1 from wizard_starting_positions
    where place >= wizard_count)',
  array['wizard_starting_positions']);
select set_relvar_type('wizard_starting_positions', 'readonly');

copy wizard_starting_positions (wizard_count, place, x, y) from stdin;
2	0	1	4
2	1	13	4
3	0	7	1
3	1	1	8
3	2	13	8
4	0	1	1
4	1	13	1
4	2	1	8
4	3	13	8
5	0	7	0
5	1	0	3
5	2	14	3
5	3	3	9
5	4	11	9
6	0	7	0
6	1	0	1
6	2	14	1
6	3	0	8
6	4	14	8
6	5	7	9
7	0	7	0
7	1	1	1
7	2	13	1
7	3	0	6
7	4	14	6
7	5	4	9
7	6	10	9
8	0	0	0
8	1	7	0
8	2	14	0
8	3	0	4
8	4	14	4
8	5	0	9
8	6	7	9
8	7	14	9
\.

/*
== new game action
*/

create table action_new_game_argument (
  place int, -- place 0..cardinality
  wizard_name text,
  computer_controlled boolean
);
select add_key('action_new_game_argument', 'place');
select add_key('action_new_game_argument', 'wizard_name');
select add_constraint('action_new_game_argument_place_valid',
  '(select count(*) from action_new_game_argument
    where place >= (select count(*) from action_new_game_argument)) = 0',
  array['action_new_game_argument']);

select set_relvar_type('action_new_game_argument', 'stack');

/*
new game action - fill in action_new_game_argument first
*/
create function action_new_game() returns void as $$
declare
  r record;
  t int;
begin

  update creating_new_game_table set creating_new_game = true;
  --assert: all tables tagged data are in this delete list
  --(only need base table of entities since these cascade)
  -- tables are in order of dependencies so delete sequence works

  -- clear data tables
  delete from action_history_mr;
  perform setval('action_history_mr_id_seq', 1);

  --turn data
  delete from game_completed_table;
  delete from cast_alignment_table;
  delete from remaining_walk_table;
  delete from selected_piece;
  delete from pieces_to_move;
  delete from spell_parts_to_cast_table;
  delete from wizard_spell_choices_mr;
  delete from current_wizard_table;
  delete from turn_phase_table;
  delete from cast_success_checked_table;
  delete from turn_number_table;
  --piece data
  delete from spell_books;
  delete from imaginary_pieces;
  delete from pieces;
  delete from wizards;
  delete from board_size;
  delete from world_alignment_table;

  if not exists(select 1 from disable_spreading_table) then
    insert into disable_spreading_table values(false);
  else
    update disable_spreading_table
       set disable_spreading = false;
  end if;

  --reset the overrides when starting new game
  delete from test_action_overrides;

  --assert: call init_ for each data table, make sure order is good

  perform init_world_alignment();
  perform init_board_size();

  --create wizards
  --  wizard table
  insert into wizards (wizard_name, computer_controlled, original_place)
    select wizard_name, computer_controlled, place
      from action_new_game_argument;
  --  pieces
  t := (select count(*) from action_new_game_argument);
  insert into pieces (ptype, allegiance, tag, x, y)
    select 'wizard', wizard_name, 0, x,y
      from action_new_game_argument
      natural inner join wizard_starting_positions
       where wizard_count = t;
  --  spell books
  --init spell book
  -- disbelieve plus 19 {random spells not disbelieve or turmoil}
  insert into spell_books (wizard_name, spell_name)
    select wizard_name, 'disbelieve'
      from action_new_game_argument;

  insert into spell_books (wizard_name, spell_name)
    select wizard_name,spell_name
      from action_new_game_argument
      inner join (select line, spell_name
                  from spell_indexes_no_dis_turm
                  inner join makeNRandoms(t * 19, 53)
                    on row_number = num) as a
      on (line/19) = place;
  --sanity check that bad boy
  if exists(select 1 from (select wizard_name, count(spell_name)
                           from spell_books group by wizard_name
                          ) as a where count <> 20) then
    raise exception 'miscount in initial spell books';
  end if;
  --turn stuff
  perform init_turn_stuff();

  /*
  data tables with no init because they are empty at start of game
  piece sub entities
  action_history and sub entities
  wizard spell choices, pieces to move, current moving piece
  */
  --TODO: add new game action history
  perform add_history_new_game();

  update creating_new_game_table set creating_new_game = false;

end
$$ language plpgsql volatile;

/*
================================================================================

= test board support
*/
--TODO: make this function dump the current game to unique file for backup
create function action_setup_test_board(flavour text) returns void as $$
declare
  i int;
  rec record;
  vwidth int;
  vname text;
  vx int;
  vy int;
  vallegiance text;
begin
  --assert - new game just created
  --         flavour is one of all_pieces, upgraded_wizards, overlapping
  select into vwidth width from board_size;

  if flavour = 'all_pieces' then
    --create one of each monster
    i:= 0;
    for rec in select ptype from monster_prototypes loop
      perform create_monster(rec.ptype, 'Buddha',
                             i % vwidth, 1 + i / vwidth, false);
      i := i + 1;
    end loop;
    --create one of each corpse
    i := 0;
    for rec in select ptype from monster_prototypes where undead = false loop
      perform create_monster(rec.ptype, 'Buddha',
                             i % vwidth, 5 + i / vwidth, false);
      perform kill_top_piece_at(i % vwidth, 5 + i / vwidth);
      i := i + 1;
    end loop;
    --create one of each (pieces - creatures)
    i := 0;
    for rec in select ptype from object_piece_types loop
      perform create_object(rec.ptype, 'Kong Fuzi', i, 8);
      i := i + 1;
    end loop;
  elseif flavour = 'upgraded_wizards' then
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 0),
      'shadow_form');
     --fix history
    update action_history_mr
      set spell_name = 'shadow_form',
      allegiance='Buddha'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 1),
      'magic_sword');
    update action_history_mr
      set spell_name = 'magic_sword',
      allegiance = 'Kong Fuzi'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 2),
      'magic_knife');
    update action_history_mr
      set spell_name = 'magic_knife',
      allegiance = 'Laozi'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 3),
      'magic_shield');
    update action_history_mr
      set spell_name = 'magic_shield',
      allegiance='Moshe'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 4),
      'magic_wings');
    update action_history_mr
      set spell_name = 'magic_wings',
      allegiance='Muhammad'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 5),
      'magic_armour');
    update action_history_mr
      set spell_name = 'magic_armour',
      allegiance='Shiva'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 6),
      'magic_bow');
    update action_history_mr
      set spell_name = 'magic_bow',
      allegiance = 'Yeshua'
      where spell_name is null;
  elseif flavour = 'overlapping' then
    --assert at least 5 wizards
    --wizard, stiff
    select into vx,vy x,y from pieces
      inner join wizards
        on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 0;
    perform create_monster('goblin', 'Buddha', 1, 0, false);
    perform kill_top_piece_at(1, 0);
    --drop in an extra dead gobbo for testing raise dead
    perform create_monster('goblin', 'Yeshua', vx, vy, false);
    perform kill_top_piece_at(vx, vy);
--wizard, mountable
    select into vx,vy,vallegiance x,y,allegiance
      from pieces inner join wizards
      on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 1;
    perform create_monster('horse', vallegiance, vx, vy, false);
--wizard in magic tree, castle, citadel
    select into vx,vy,vallegiance x,y,allegiance
      from pieces inner join wizards
      on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 2;
    perform create_object('magic_tree', vallegiance, vx, vy);
    select into vx,vy,vallegiance x,y,allegiance
      from pieces inner join wizards
      on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 3;
    perform create_object('magic_castle', vallegiance, vx, vy);
    select into vx,vy,vallegiance x,y,allegiance
      from pieces inner join wizards
      on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 4;
    perform create_object('dark_citadel', vallegiance, vx, vy);
--monster, stiff
    perform create_monster('goblin', 'Buddha', 3, 3, false);
    perform kill_top_piece_at(3, 3);
    perform create_monster('giant', 'Buddha', 3, 3, false);
--stiff, blob
    perform create_monster('goblin', 'Buddha', 4, 3, false);
    perform kill_top_piece_at(4, 3);
    perform create_object('gooey_blob', 'Buddha', 4, 3);
--monster, blob
    perform create_monster('goblin', 'Laozi', 5, 3, false);
    perform create_object('gooey_blob', 'Buddha', 5, 3);
--stiff, monster, blob
    perform create_monster('elf', 'Buddha', 6, 3, false);
    perform kill_top_piece_at(6, 3);
    perform create_monster('goblin', 'Laozi', 6, 3, false);
    perform create_object('gooey_blob', 'Buddha', 6, 3);
  else
    raise exception
    'argument must be one of all_pieces, upgraded_wizards, overlapping, got %',
    flavours;
  end if;
end
$$ language plpgsql volatile;


select set_module_for_preceding_objects('new_game');
/*

================================================================================

= ai

For each stage we compile a list of possible actions using the valid_action views. These are then filtered to remove actions we don't want to run. At some places, the possible action list is reduced by keeping only the actions which are deemed vital (e.g. the wizard needs to run away, or a monster has a chance to attact a wizard). The remaining actions are possibly weighted and one is chosen at random.

Choose spells by weighting them according to casting chance, some spells are never cast, and some will be further weighted by the board layout.

When moving army, the general plan is to move the monsters closest to an enemy first.

choose spell
cast spell
move pieces

option 1: upgrade
weight by probability
don't cast one already have or a weaker one in same category
armour - shield
knife - sword
shadow - wings

decree et al: use: wizard with lots of bad guys, being threatened
magic wood: if range of spells is a bit shit
castle, wall, blob, fire, shadowwood - random
disbelieve: cast when threatened by a hard creature, small chance otherwise
subversion - hard creature nearby, when threatened
raise dead - when can
monsters: weight by chances, decide on imag weighted by chances

assess: defensive: wizard under threat
aggressive: choose a target to send everyone against

casting:
raise: hardest corpse in range
decree: if threatened target monster or wizard, else target hardest
wizard/monster on screen
castle -next to, away from danger
disbelieve - closest monster
subvert - closest monster (or if tougher one next to closest monster?)
monster - toward nearest threat
wall - just randomly put about
blob - want to grow safely, unless under threat then use aggresively
fire - use aggressively
shadow wood: use magic wood layout, bias in directions that have
moving enemy pieces

moving:
if defensive move pieces starting with close

The system for running the ai is to make an action available when the
current wizard is an ai to continue the ai's turn. This will do one
action, and move to the next phase if the ai has completed it's
action. This api allows the client to control how and at what speed
the ai's turns are run, we use this to run one ai action every half
second so you can see what the ai is doing by watching the board
change.

TODO:
--don't attack friendlies
--don't cast, attack corpses
--don't choose spells that can't work
cast spells in sensible place
weight choice by chance
sometimes cast imag when unlikely
disbelieve logic and tracking
move phase:
  keep wizards out of danger
  always move into castles,wood
  move towards wood if near
  stay in castles, wood
send monsters towards closest enemy
always attack if can
choose targets: favour wizards and hardest that likely to kill

*/

/*

== main ai action

*/

create function action_ai_continue() returns void as $$
begin
  perform check_can_run_action('ai_continue');
    if get_turn_phase() = 'choose' then
      perform ai_choose_spell();
      perform action_next_phase();
    elseif get_turn_phase() = 'cast' then
      perform ai_cast_spell();
    elseif get_turn_phase() = 'move' then
      perform ai_move_pieces();
    end if;
end;
$$ language plpgsql volatile;

/*

== spell choice

First, eliminate all the useless target spells - those that have no
target and those that can only be cast on a friendly or corpse.

*/

create view current_wizard_target_spells as
  select spell_name,range from spell_books
    inner join current_wizard_table
      on current_wizard = wizard_name
    natural inner join spell_ranges;

create view current_wizard_square as
  select x,y from pieces
  inner join current_wizard_table
    on allegiance =current_wizard
  where ptype= 'wizard';

/*

take all the target spells and create a list of spell names a squares
that they can be cast on using the range and valid square types of
each spell

*/

create view castable_target_spells as
  select spell_name,svs.x,svs.y
    from current_wizard_target_spells cwts
    natural inner join spell_valid_squares svs
    natural inner join spell_valid_square_types svst
    inner join board_ranges br
      on (br.x,br.y) = (select x,y from current_wizard_square)
        and br.range = cwts.range
        and (br.tx, br.ty) = (svs.x, svs.y);

/*

eliminate the rows which have only corpses or friendlies on top

*/

create view ai_useful_spells as
  select spell_name from spell_books
    inner join current_wizard_table
      on wizard_name = current_wizard
    natural inner join activate_spells
  union
    select spell_name from castable_target_spells
    where (x,y) not in (select x,y from corpse_only_squares
                      union
                      select x,y from pieces_on_top
                      inner join current_wizard_table
                      on current_wizard = allegiance);

create function ai_choose_spell() returns void as $$
declare
  vspell_name text;
begin
  select into vspell_name spell_name
    from ai_useful_spells
    order by random() limit 1;
  if vspell_name is null then
    --skip choosing one
    return;
  else
    perform action_choose_spell(vspell_name);
  end if;
end;
$$ language plpgsql volatile;

/*

== spell casting

first filter out all the targets we don't want to cast on:

*/

create view ai_filtered_target_spells as
  select * from valid_target_actions
  where action='cast_target_spell'
  and (x,y) not in
    (select x,y from pieces_on_top
    where allegiance in (get_current_wizard(), 'dead'));

/*
cast spells in a sensible place:
dark power: don't choose a wizard with no creations
            pick enemy monsters that are close or wizards with lots of shit
lightning, magic bolt: enemy wizard then closest monster
raise dead: choose hardest corpse
subversion: choose hardest enemy
shadow wood: use magic tree layout
fire: next to wizard or monster if can, else towards closest enemy
blob: towards closest enemy in some space
castle: next to wizard away from danger
monster: towards closest enemy
*/

create function ai_cast_spell() returns void as $$
declare
  p pos;
begin
  if exists(select 1 from valid_activate_actions
            where action = 'cast_activate_spell') then
     perform action_cast_activate_spell();
  elseif exists(select 1 from ai_filtered_target_spells) then
     select into p x,y from ai_filtered_target_spells
         order by random() limit 1;
     perform action_cast_target_spell(p.x, p.y);
  else
    perform action_next_phase();
  end if;
end;
$$ language plpgsql volatile;

/*

== move phase

*/

create function ai_move_pieces() returns void as $$
declare
  p pos;
begin
  --if no piece selected and none selectable, go to next phase
  if not exists(select 1 from selected_piece)
     and not exists(select 1 from valid_target_actions
            where action = 'select_piece_at_position') then
     perform action_next_phase();
     return;
  end if;
  --if no piece selected try to select one
  if not exists(select 1 from selected_piece)
     and exists(select 1 from valid_target_actions
            where action = 'select_piece_at_position') then
     select into p x,y from valid_target_actions
       where action = 'select_piece_at_position'
       order by random() limit 1;
     perform action_select_piece_at_position(p.x, p.y);
     --check if it has been immediately unselected
     if not exists(select 1 from selected_piece) then
       return;
     end if;
  end if;
  perform ai_move_selected_piece();
end;
$$ language plpgsql volatile;

create view ai_selected_piece_actions as
select a.x,a.y,action
  from valid_target_actions a
  left outer join pieces_on_top p
    using (x,y)
  where action in('walk', 'fly')
  or ((action in('attack', 'ranged_attack')
     and allegiance not in (get_current_wizard(), 'dead')));


/*
rules:
send monsters towards enemy
always attack if can
choose targets: wizards if can, then hardest creature
*/

create view prefered_targets as
select x,y,action,
  case when ptype = 'wizard' then -500
                else 20 - physical_defense
  end as preference
from valid_target_actions
natural inner join pieces_mr
where action in('attack','ranged_attack');

create view closest_enemy_to_selected_piece as
  select a.x,a.y
  from selected_piece_attackable_squares a
  cross join selected_piece s
    inner join pieces s1
    using(ptype,allegiance,tag)
  order by distance(s1.x,s1.y,a.x,a.y) limit 1;

create view select_best_move as
  select a.action,a.x,a.y from ai_selected_piece_actions a
    cross join closest_enemy_to_selected_piece e
    where action in('walk', 'fly')
    order by distance(a.x,a.y,e.x,e.y) limit 1;

create function ai_move_selected_piece() returns void as $$
declare
  r record;
begin
  if exists(select 1 from ai_selected_piece_actions
            where action = 'attack'
            or (action = 'ranged_attack'
                and (select move_phase='ranged_attack'
                from selected_piece))) then
    select into r x,y,action from prefered_targets
      order by preference limit 1;
    if r.action = 'attack' then
      perform action_attack(r.x, r.y);
    elseif r.action = 'ranged_attack' then
      perform action_ranged_attack(r.x, r.y);
    else
      --raise exception 'bad ai attack action: %', r.action;
      perform action_cancel();
    end if;
  else
    if exists(select 1 from ai_selected_piece_actions
              where action in ('walk','fly')) then
      select into r * from select_best_move;
      if r.action = 'walk' then
        perform action_walk(r.x, r.y);
      elseif r.action = 'fly' then
        perform action_fly(r.x, r.y);
      else
        perform action_cancel();
      end if;
    else
      perform action_cancel();
    end if;
  end if;
end;
$$ language plpgsql volatile;

/*
--------------------------------------------------------------------------------
*/
select set_all_attributes_to_not_null();
select set_notifies_on_all_data_tables();

