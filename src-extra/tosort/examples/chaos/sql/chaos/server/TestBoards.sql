/*

Temporary code to create the test boards to check the board display
and such.

Will be moved to haskell code at some point, using the ascii stuff
from the tests.

*/
select module('Chaos.Server.TestBoards');

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
                             i % vwidth, 1 + i / vwidth, false, false);
      i := i + 1;
    end loop;
    --create one of each corpse
    i := 0;
    for rec in select ptype from monster_prototypes where undead = false loop
      perform create_monster(rec.ptype, 'Buddha',
                             i % vwidth, 5 + i / vwidth, false, false);
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
    perform create_monster('goblin', 'Buddha', 1, 0, false, false);
    perform kill_top_piece_at(1, 0);
    --drop in an extra dead gobbo for testing raise dead
    perform create_monster('goblin', 'Yeshua', vx, vy, false, false);
    perform kill_top_piece_at(vx, vy);
--wizard, mountable
    select into vx,vy,vallegiance x,y,allegiance
      from pieces inner join wizards
      on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 1;
    perform create_monster('horse', vallegiance, vx, vy, false, false);
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
    perform create_monster('goblin', 'Buddha', 3, 3, false, false);
    perform kill_top_piece_at(3, 3);
    perform create_monster('giant', 'Buddha', 3, 3, false, false);
--stiff, blob
    perform create_monster('goblin', 'Buddha', 4, 3, false, false);
    perform kill_top_piece_at(4, 3);
    perform create_object('gooey_blob', 'Buddha', 4, 3);
--monster, blob
    perform create_monster('goblin', 'Laozi', 5, 3, false, false);
    perform create_object('gooey_blob', 'Buddha', 5, 3);
--stiff, monster, blob
    perform create_monster('elf', 'Buddha', 6, 3, false, false);
    perform kill_top_piece_at(6, 3);
    perform create_monster('goblin', 'Laozi', 6, 3, false, false);
    perform create_object('gooey_blob', 'Buddha', 6, 3);
  else
    raise exception
    'argument must be one of all_pieces, upgraded_wizards, overlapping, got %',
    flavour;
  end if;
end
$$ language plpgsql volatile;


create function disable_all_constraints() returns void as $$
declare
  tn text;
begin
  for tn in select object_name from all_module_objects
              where object_type = 'table' loop
    execute 'alter table ' || tn || ' disable trigger user';
  end loop;
end;
$$ language plpgsql volatile;


create function enable_all_constraints() returns void as $$
declare
  t text;
  b bool;
begin
  /*for t in select object_name from all_module_objects
             where object_name like 'check_con_%'
               and object_type = 'function' loop
    execute
  end loop;*/
  for t in select object_name from all_module_objects
              where object_type = 'table' loop
    execute 'alter table ' || t || ' enable trigger user';
  end loop;
end;
$$ language plpgsql volatile;


create function action_skip_to_phase(phase text) returns void as $$
declare
  p text;
begin
  if phase not in ('choose','cast','autonomous','move') then
    raise exception 'phase must be one of choose, cast, autonomous,move; got %' ,phase;
  end if;
  loop
    if get_turn_phase() != phase then
      perform action_client_next_phase();
    else
      exit;
    end if;
  end loop;
end;
$$ language plpgsql volatile;

create table test_spell_books (
  wizard_name text
 ,spell_name text
);

copy test_spell_books(wizard_name,spell_name) from stdin;
Buddha	disbelieve
Kong Fuzi	disbelieve
Laozi	disbelieve
Moshe	disbelieve
Muhammad	disbelieve
Shiva	disbelieve
Yeshua	disbelieve
Zarathushthra	disbelieve
Buddha	unicorn
Buddha	lightning
Buddha	ogre
Buddha	vengeance
Buddha	unicorn
Buddha	ogre
Buddha	raise_dead
Buddha	giant
Buddha	king_cobra
Buddha	elf
Buddha	giant_rat
Buddha	harpy
Buddha	magic_castle
Buddha	large_chaos
Buddha	orc
Buddha	magic_wings
Buddha	magic_shield
Buddha	giant
Buddha	gryphon
Kong Fuzi	zombie
Kong Fuzi	magic_bolt
Kong Fuzi	dark_citadel
Kong Fuzi	ogre
Kong Fuzi	wall
Kong Fuzi	unicorn
Kong Fuzi	law
Kong Fuzi	dark_citadel
Kong Fuzi	lion
Kong Fuzi	faun
Kong Fuzi	decree
Kong Fuzi	magic_wings
Kong Fuzi	dark_citadel
Kong Fuzi	magic_armour
Kong Fuzi	hydra
Kong Fuzi	vengeance
Kong Fuzi	justice
Kong Fuzi	eagle
Kong Fuzi	ogre
Laozi	magic_bow
Laozi	lightning
Laozi	shadow_form
Laozi	orc
Laozi	shadow_form
Laozi	horse
Laozi	ghost
Laozi	magic_bow
Laozi	zombie
Laozi	pegasus
Laozi	manticore
Laozi	gryphon
Laozi	pegasus
Laozi	goblin
Laozi	harpy
Laozi	lightning
Laozi	giant
Laozi	law
Laozi	manticore
Moshe	giant
Moshe	orc
Moshe	shadow_wood
Moshe	golden_dragon
Moshe	hydra
Moshe	shadow_wood
Moshe	ogre
Moshe	orc
Moshe	raise_dead
Moshe	chaos
Moshe	shadow_form
Moshe	lion
Moshe	magic_wings
Moshe	gorilla
Moshe	gorilla
Moshe	hydra
Moshe	elf
Moshe	magic_shield
Moshe	magic_bolt
Muhammad	magic_shield
Muhammad	magic_shield
Muhammad	gorilla
Muhammad	green_dragon
Muhammad	vengeance
Muhammad	dark_citadel
Muhammad	magic_armour
Muhammad	gooey_blob
Muhammad	lion
Muhammad	magic_wood
Muhammad	magic_armour
Muhammad	giant_rat
Muhammad	skeleton
Muhammad	golden_dragon
Muhammad	zombie
Muhammad	decree
Muhammad	magic_fire
Muhammad	shadow_wood
Muhammad	raise_dead
Shiva	gryphon
Shiva	magic_knife
Shiva	spectre
Shiva	chaos
Shiva	eagle
Shiva	magic_armour
Shiva	justice
Shiva	harpy
Shiva	subversion
Shiva	lightning
Shiva	wall
Shiva	justice
Shiva	eagle
Shiva	magic_castle
Shiva	magic_wings
Shiva	hydra
Shiva	magic_armour
Shiva	magic_wood
Shiva	subversion
Yeshua	pegasus
Yeshua	ghost
Yeshua	magic_bow
Yeshua	law
Yeshua	gryphon
Yeshua	hydra
Yeshua	large_law
Yeshua	green_dragon
Yeshua	large_chaos
Yeshua	dark_power
Yeshua	faun
Yeshua	giant_rat
Yeshua	harpy
Yeshua	ogre
Yeshua	decree
Yeshua	large_law
Yeshua	raise_dead
Yeshua	magic_bow
Yeshua	shadow_wood
Zarathushthra	chaos
Zarathushthra	king_cobra
Zarathushthra	gorilla
Zarathushthra	decree
Zarathushthra	shadow_form
Zarathushthra	horse
Zarathushthra	magic_knife
Zarathushthra	large_law
Zarathushthra	orc
Zarathushthra	elf
Zarathushthra	chaos
Zarathushthra	magic_knife
Zarathushthra	skeleton
Zarathushthra	law
Zarathushthra	ghost
Zarathushthra	elf
Zarathushthra	magic_castle
Zarathushthra	magic_armour
Zarathushthra	magic_bow
\.


