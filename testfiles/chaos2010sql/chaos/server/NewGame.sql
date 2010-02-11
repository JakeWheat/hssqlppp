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

select module('Chaos.Server.NewGame');

create table wizard_starting_positions (
  wizard_count int,
  place int,
  x int,
  y int,
  unique (wizard_count, place),
  unique (wizard_count,x,y)
);
select set_relvar_type('wizard_starting_positions', 'readonly');
select create_assertion('wizard_starting_positions_place_valid',
  'not exists(select 1 from wizard_starting_positions
    where place >= wizard_count)');


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
  place int unique, -- place 0..cardinality
  wizard_name text unique,
  computer_controlled boolean
);
select set_relvar_type('action_new_game_argument', 'stack');

select create_assertion('action_new_game_argument_place_valid',
  '(select count(*) from action_new_game_argument
    where place >= (select count(*) from action_new_game_argument)) = 0');


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
    flavour;
  end if;
end
$$ language plpgsql volatile;
