/*
= new game
== wizard starting positions
Wizards start the game in positions set by how many wizards there are in a game:

These figures are only valid iff there are 2-8 wizards and the board is
15 x 10.

*/

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

create type new_game_t as (
  place int,
  wizard_name text,
  computer_controlled boolean
);
--select set_relvar_type('action_new_game_argument', 'stack');

/*select create_assertion('action_new_game_argument_place_valid',
  '(select count(*) from action_new_game_argument
    where place >= (select count(*) from action_new_game_argument)) = 0');
*/

create view spell_indexes_no_dis_turm as
  select row_number() over() as row_number,spell_name
    from spells_mr
    where spell_name not in ('disbelieve', 'turmoil');

/*insert into spell_indexes_no_dis_turm (spell_name)
  select spell_name from spells_mr
    where spell_name not in ('disbelieve', 'turmoil');*/
/*
 row_number serial unique,
  spell_name text
*/
/*
new game action - fill in action_new_game_argument first

we have all these init functions for each table, which naturally sit
next to that table's definition, we then have to put them together
here in the right order without missing any - looking for a better way
to do this. at the very least we could tag the init functions and make
sure they're all called. I wonder if transclusion could help here?
What about situations where you have say 100 tables, and you want to
reinitialized 20 of them, maybe a different non overlapping20 each
time, so we don't just have one set of init functions? Not sure if
these ideas are going anywhere.

*/
create function action_new_game(ar new_game_t[]) returns void as $$
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
  delete from pieces_moved;
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
      from unnest(ar);
  --  pieces
  t := array_length(ar,1); --(select count(*) from action_new_game_argument);
  insert into pieces (ptype, allegiance, tag, x, y)
    select 'wizard', wizard_name, 0, x,y
      from unnest(ar)
      natural inner join wizard_starting_positions
       where wizard_count = t;
  --  spell books
  --init spell book
  -- disbelieve plus 19 {random spells not disbelieve or turmoil}
  insert into spell_books (wizard_name, spell_name)
    select wizard_name, 'disbelieve'
      from unnest(ar);

  insert into spell_books (wizard_name, spell_name)
    select wizard_name,spell_name
      from unnest(ar)
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
  perform add_history_new_game();

  update creating_new_game_table set creating_new_game = false;

end
$$ language plpgsql volatile;
