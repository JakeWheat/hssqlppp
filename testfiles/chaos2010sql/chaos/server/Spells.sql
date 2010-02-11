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
select module('Chaos.Server.Spells');

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
  spell_name text primary key,
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
