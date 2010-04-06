/*

spells
======

Spells come in a number of flavours, the user interface breaks them
down into the same groups that the original chaos instructions did:

wizard spells: upgrade your wizard in some way, most add weaponry

attacking spells: are cast directly on enemy wizards and their
          monsters to kill them or destroy all of a wizards creations.

object spells: create object pieces, which are pieces which aren't
  moved by the wizard, all the other spells which create pieces are
  monster spells

miscellaneous spells: various spells not in the other categories

monster spells: summon monsters for a wizard's army

The code breaks the spells down differently: target spells need a
square to be chosen to cast them on activate spells are all the other
spells.

Target spells are further be broken down into summon spells which
create new pieces on the board, and all the other target spells.

casting chance notes:

Each time you cast a spell it can affect the world alignment, which in
turn affects the spell casting chances in the future, this is critical
to most playing strategies.

=== ddl
*/
select module('Chaos.Server.Spells');

-- this is the presentation catagories
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

select create6nf ($$

  spells_mr (
    spell_name text primary key,
    base_chance int,
    alignment int,
    spell_category spell_category,
    description text
  );

  target_spells : spells_mr (
    range int,
    numb int,
    valid_square_category spell_square_category
  );

  summon_spells : target_spells (
    ptype text
  );

$$);

select set_relvar_type('spells_mr', 'readonly');

create view monster_spells as
  select s.* from spells_mr s
    inner join monster_prototypes m
          on s.ptype=m.ptype;

create view activate_spells as
  select spell_name
  from spells_mr
  where range is null;

/*
data
----

*/
copy spells_mr (spell_name, base_chance, alignment, spell_category, description,
 range, numb, valid_square_category, ptype) from stdin;
dark_citadel	50	-1	object	Gives wizard building to hide in.	8	1	empty	dark_citadel
dark_power	50	-2	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 3 attacks on enemy creatures	20	3	creature_on_top	\N
decree	90	1	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 1 attack on an enemy creature.	20	1	creature_on_top	\N
disbelieve	100	0	miscellaneous	Allows illusion creatures to be destroyed. This spell has 100% casting chance, and is always available.	20	1	monster_on_top	\N
eagle	70	1	monster	monster	1	1	empty_or_corpse_only	eagle
elf	70	2	monster	monster	1	1	empty_or_corpse_only	elf
chaos	80	-2	miscellaneous	Makes the world more chaos.	\N	\N	\N	\N
faun	80	-1	monster	monster	1	1	empty_or_corpse_only	faun
ghost	50	-1	monster	monster	1	1	empty_or_corpse_only	ghost
giant	40	1	monster	monster	1	1	empty_or_corpse_only	giant
giant_rat	100	0	monster	monster	1	1	empty_or_corpse_only	giant_rat
goblin	100	-1	monster	monster	1	1	empty_or_corpse_only	goblin
golden_dragon	10	2	monster	monster	1	1	empty_or_corpse_only	golden_dragon
law	80	2	miscellaneous	Makes the world more law.	\N	\N	\N	\N
gooey_blob	100	-1	object	Attacks enemy units it covers and randomly spreads across the map. Any unit covered up by a gooey blob will be able to carry on once it is uncovered (except wizards who are killed by gooey blobs).	6	1	empty_or_corpse_only	gooey_blob
gorilla	70	0	monster	monster	1	1	empty_or_corpse_only	gorilla
green_dragon	10	-1	monster	monster	1	1	empty_or_corpse_only	green_dragon
gryphon	60	1	monster	monster	1	1	empty_or_corpse_only	gryphon
harpy	60	-1	monster	monster	1	1	empty_or_corpse_only	harpy
horse	90	1	monster	monster	1	1	empty_or_corpse_only	horse
hydra	50	-1	monster	monster	1	1	empty_or_corpse_only	hydra
justice	50	2	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 3 attacks.	20	3	creature_on_top	\N
king_cobra	90	1	monster	monster	1	1	empty_or_corpse_only	king_cobra
large_chaos	60	-4	miscellaneous	Makes the world more chaos.	\N	\N	\N	\N
large_law	60	4	miscellaneous	Makes the world more law.	\N	\N	\N	\N
lightning	100	0	attacking	Attacks creature it is cast at (more powerful than magic bolt)	4	1	attackable	\N
lion	60	1	monster	monster	1	1	empty_or_corpse_only	lion
magic_armour	50	1	wizard	Gives wizard increased protection from attack.	\N	\N	\N	\N
magic_bolt	100	0	attacking	Attacks creature it is cast at.	6	1	attackable	\N
magic_bow	50	1	wizard	Gives wizard ranged weapon including undead creatures.	\N	\N	\N	\N
magic_castle	50	1	object	Gives wizard building to hide in.	8	1	empty	magic_castle
magic_fire	80	-1	object	Attacks and kills enemy units it covers and randomly spreads across the map.	6	1	empty	magic_fire
magic_knife	70	1	wizard	Gives wizard increase attack power including undead creatures.	\N	\N	\N	\N
magic_shield	70	1	wizard	Gives wizard increased protection from attack.	\N	\N	\N	\N
magic_sword	40	1	wizard	Gives wizard increase attack power including undead creatures.	\N	\N	\N	\N
magic_wings	60	0	wizard	Gives wizard ability to fly.	\N	\N	\N	\N
magic_wood	80	1	object	Summons up to eight magic trees near your wizard. If you put your wizard in a magic tree and leave him there, he gets a new spell after a few turns.	8	8	empty_and_not_adjacent_to_tree	magic_tree
manticore	50	-1	monster	monster	1	1	empty_or_corpse_only	manticore
ogre	70	-1	monster	monster	1	1	empty_or_corpse_only	ogre
orc	100	-1	monster	monster	1	1	empty_or_corpse_only	orc
pegasus	60	2	monster	monster	1	1	empty_or_corpse_only	pegasus
raise_dead	60	-1	miscellaneous	Allows reanimation of dead bodies left on screen. Any creatures raised from the dead become undead creatures, able to attack other undeads.	4	1	corpse_only	\N
red_dragon	10	-2	monster	monster	1	1	empty_or_corpse_only	red_dragon
shadow_form	80	0	wizard	Gives wizard increased protection and allows movement of 3 spaces per turn. Disappears if wizard attacks anything.	\N	\N	\N	\N
shadow_wood	50	-1	object	Allows you to place up to eight shadow trees near your wizard. No two trees can be adjacent, and line of sight is needed in placing. Shadow trees can attack anything in contact with them (except undead).	8	8	empty_and_not_adjacent_to_tree	shadow_tree
skeleton	70	-1	monster	monster	1	1	empty_or_corpse_only	skeleton
spectre	60	-1	monster	monster	1	1	empty_or_corpse_only	spectre
subversion	100	0	miscellaneous	Realigns enemy creature to your side.	7	1	monster_on_top	\N
turmoil	100	-2	miscellaneous	Randomly moves all objects onscreen to a different location. Only available from a magic tree.	\N	\N	\N	\N
unicorn	70	2	monster	monster	1	1	empty_or_corpse_only	unicorn
vampire	20	-2	monster	monster	1	1	empty_or_corpse_only	vampire
vengeance	90	-1	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 1 attack on an enemy creature.	20	1	creature_on_top	\N
wall	80	0	object	Allows four wall blocks to be built near the wizard, which blocks creatures paths, but can be flown over.	8	4	empty	wall
wraith	50	-1	monster	monster	1	1	empty_or_corpse_only	wraith
zombie	90	-1	monster	monster	1	1	empty_or_corpse_only	zombie
\.
