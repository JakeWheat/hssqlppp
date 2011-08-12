
just an insert syntax a bit like updates, so the fields are next to
  the values

> {-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Examples.Extensions.AlternateInsert
>     (altInsExamples
>     ,altIns) where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Quote
>
> altInsExamples :: [ExtensionTest]
> altInsExamples = [altInsExample1]
>
> altInsExample1 :: ExtensionTest
> altInsExample1 =
>   ExtensionTest
>     "tableValueExample1"
>     altIns
>     [$sqlStmts|
>      select table_insert(spells_mr, $$
>      (spell_name = 'dark_citadel'
>      ,base_chance = 50
>      ,alignment = -1
>      ,spell_category = 'object'
>      ,description = 'Gives wizard building to hide in.'
>      ,range =8
>      ,num = 1
>      ,valid_square_category = empty
>      ,ptype = dark_citadel)
>     ,(spell_name= 'dark_power'
>      ,base_chance = 50)
>       $$);
>      |]
>     [$sqlStmts|
>      insert into spells_mr(spell_name
>                           ,base_chance
>                           ,alignment
>                           ,spell_category
>                           ,description
>                           ,range
>                           ,num
>                           ,valid_square_category
>                           ,ptype)
>                   values ('dark_citadel'
>                          ,50
>                          ,-1
>                          ,'object'
>                          ,'Gives wizard building to hide in.'
>                          ,8
>                          ,1
>                          ,empty
>                          ,dark_citadel);
>      insert into spells_mr(spell_name
>                           ,base_chance)
>                   values ('dark_power'
>                          ,50);
>      |]

> altIns :: Data a => a -> a
> altIns =
>     transformBi $ \x ->
>       case x of
>         [$sqlStmt| select create_var($s(varname), $s(typename)); |]
>             -> let tablename = varname ++ "_table"
>                in [$sqlStmt|
>
>   create table $(tablename) (
>    $(varname) $(typename)
>   );
>
>                    |]
>         x1 -> x1
>












see the copy statements in the chaos sql - ugly, unreadable and
uneditable - this is the motivation
copy spell_sprites(spell_name, sprite) from stdin;
magic_wood	magic_tree
shadow_wood	shadow_tree
magic_fire	magic_fire

copy spell_keys (spell_name, key) from stdin;
magic_knife	1
magic_shield	2

copy sprites (sprite,animation_speed) from stdin;
bat	8
dead_bat	250
bear	23
dead_bear	250

copy history_sounds (history_name,sound_name) from stdin;
walked	walk
fly	fly
attack	attack

copy history_no_visuals (history_name) from stdin;
wizard_up
new_turn
new_game

copy colours (name,red,green,blue) from stdin;
grid	32767	32767	32767
background	0	0	32767
black	0	0	0
blue	0	0	65535
green	0	65535	0
red	65535	0	0
pink	65535	49407	49407
purple	65535	0	65535
cyan	0	65535	65535
yellow	65535	65535	0
orange	65535	41215	0
grey	32767	32767	32767
white	65535	65535	65535
\.

copy key_control_settings(key_code, action_name) from stdin;
Up	move_cursor_up
KP_Up	move_cursor_up
Left	move_cursor_left
KP_Left	move_cursor_left

copy wizard_starting_positions (wizard_count, place, x, y) from stdin;
2	0	1	4
2	1	13	4
3	0	7	1
3	1	1	8
3	2	13	8


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
