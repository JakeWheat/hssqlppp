/*

piece prototypes
================

ddl
---

Each type of piece starts with the same stats. Once a piece is on the
board, some of these stats can be changed - see the pieces module.

This table holds these starting stats.

*/
select module('Chaos.Server.PiecePrototypes');

--creature ranged weapons can be either projectiles or fireballs
create domain ranged_weapon_type as text
  check (value in ('projectile', 'fire'));

/*

the inheritance here reflects the possible kinds of pieces. The main
thing missing is that the create6nf extension doesn't yet support
abstract components.

TODO: find or rewrite the lost notes on the different kinds of pieces.

*/

select create6nf ($$

  piece_prototypes_mr (
    ptype text primary key
  );

  attacking_prototypes : piece_prototypes (
     attack_strength int
  );

  attackable_prototypes : piece_prototypes (
    physical_defense int
  );

  magic_attackable_prototypes : piece_prototypes (
    magic_defense int
  );

  creature_prototypes : piece_prototypes_mr
                       ,attacking_prototypes
                       ,attackable_prototypes
                       ,magic_attackable_prototypes (
    flying boolean,
    speed int,
    agility int
  );

  monster_prototypes : creature_prototypes (
    undead boolean,
    ridable boolean
  );

  range_attacking_prototypes : creature_prototypes (
    ranged_weapon_type ranged_weapon_type,
    range int,
    ranged_attack_strength int
  );

$$);

create view object_piece_types as
  select ptype from piece_prototypes_mr where speed is null;

create view ridable_prototypes as
  select ptype from piece_prototypes_mr
    where ridable;

create view enterable_piece_types as
  select 'magic_tree'::text as ptype
  union all
  select 'magic_castle'
  union all
  select 'dark_citadel';
/*
data
----

TODO: find a way to represent data like this in the source in a much
more readable format - the alternative insert syntax (an unfinished
extension) seems good enough, just makes inserts look like updates so
each field name is next to its value.

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
