/*

================================================================================

== sprites

just the list of the names of the sprites and their animation speed.
todo: add the png data here

pngs for every sprite listed in this table must exist on the disk to
be loaded or the game will refuse to run

*/
select module('Chaos.Client.Sprites');

create table sprites (
  sprite text primary key, -- name of sprite, also part of the name of the png frames
  animation_speed int
--todo: add sprite data here
);
select set_relvar_type('sprites', 'readonly');

copy sprites (sprite,animation_speed) from stdin;
bat	8
dead_bat	250
bear	23
dead_bear	250
centaur	23
dead_centaur	250
crocodile	34
dead_crocodile	250
dark_citadel	50
dire_wolf	12
dead_dire_wolf	250
eagle	14
dead_eagle	250
elf	26
dead_elf	250
faun	20
dead_faun	250
ghost	15
giant	23
dead_giant	250
giant_rat	13
dead_giant_rat	250
goblin	12
dead_goblin	250
golden_dragon	27
dead_golden_dragon	250
gooey_blob	40
gorilla	18
dead_gorilla	250
green_dragon	32
dead_green_dragon	250
gryphon	10
dead_gryphon	250
harpy	13
dead_harpy	250
horse	21
dead_horse	250
hydra	36
dead_hydra	250
king_cobra	30
dead_king_cobra	250
lion	38
dead_lion	250
magic_castle	50
magic_fire	12
magic_tree	250
manticore	13
dead_manticore	250
ogre	23
dead_ogre	250
orc	21
dead_orc	250
pegasus	16
dead_pegasus	250
red_dragon	34
dead_red_dragon	250
shadow_tree	30
skeleton	17
spectre	15
unicorn	16
dead_unicorn	250
vampire	40
wall	30
wizard0	250
wizard1	250
wizard2	250
wizard3	250
wizard4	250
wizard5	250
wizard6	250
wizard7	250
wizard_magic_armour	250
wizard_magic_bow	50
wizard_magic_knife	50
wizard_magic_shield	250
wizard_magic_sword	50
wizard_magic_wings	50
wizard0_shadow	20
wizard1_shadow	20
wizard2_shadow	20
wizard3_shadow	20
wizard4_shadow	20
wizard5_shadow	20
wizard6_shadow	20
wizard7_shadow	20
wizard_magic_armour_shadow	20
wizard_magic_bow_shadow	20
wizard_magic_knife_shadow	20
wizard_magic_shield_shadow	20
wizard_magic_sword_shadow	20
wizard_magic_wings_shadow	20
wraith	10
zombie	25
magic_bolt	250
lightning	250
law	250
large_law	250
chaos	250
large_chaos	250
vengeance	250
subversion	250
turmoil	250
disbelieve	250
justice	250
dark_power	250
decree	250
raise_dead	250
cursor	250
highlight_cast_target_spell	250
highlight_select_piece_at_position	250
highlight_walk	250
highlight_fly	250
highlight_attack	250
highlight_ranged_attack	250
effect_attack	250
\.
