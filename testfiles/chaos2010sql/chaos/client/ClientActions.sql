/*
================================================================================

= actions
*/
select module('Chaos.Client.ClientActions');

/*
== action valid views

we add this view to cover the actions which are defined
in the client to supplement the action valid views
for the server actions define in the server.
*/
create view client_valid_target_actions as
  select * from valid_target_actions
  where not exists (select 1 from game_completed_table);

create view client_valid_activate_actions as
select * from (
  select * from valid_activate_actions
union select 'move_cursor_up'
union select 'move_cursor_down'
union select 'move_cursor_left'
union select 'move_cursor_right'
union select 'move_cursor_up_left'
union select 'move_cursor_down_left'
union select 'move_cursor_up_right'
union select 'move_cursor_down_right'
union select 'print_widget_info'
union select 'refresh_windows'
union select 'spell_book_show_all_update_on'
union select 'spell_book_show_all_update_off'
union select 'client_next_phase'
union select 'go') as a
  where not exists (select 1 from game_completed_table);

/*
== key controls
create a table to map gtk key descriptions to the
names of the action functions which are called.

*/

--select new_module('key_controls', 'client');

create table key_control_settings (
  key_code text unique,
  action_name text
);
select set_relvar_type('key_control_settings', 'readonly');

copy key_control_settings(key_code, action_name) from stdin;
Up	move_cursor_up
KP_Up	move_cursor_up
Left	move_cursor_left
KP_Left	move_cursor_left
Right	move_cursor_right
KP_Right	move_cursor_right
Down	move_cursor_down
KP_Down	move_cursor_down
KP_Home	move_cursor_up_left
KP_Page_Up	move_cursor_up_right
KP_Page_Down	move_cursor_down_right
KP_End	move_cursor_down_left
End	cancel
F11	print_widget_info
F12	refresh_widgets
0	choose_no_spell
Insert	spell_book_show_all_update_on
Delete	spell_book_show_all_update_off
space	client_next_phase
KP_Begin	go
Return	go
KP_5	go
y	set_imaginary
Y	set_imaginary
n	set_real
N	set_real
\.

/*
== key press actions
*/
create function create_client_action_wrapper(client_action_name text,
                                              action_call text)
  returns void as $$
begin
  execute $f$
create function action_$f$ || client_action_name || $f$() returns void as $a$
begin
  perform action_$f$ || action_call || $f$;
end;
$a$ language plpgsql volatile;$f$;
end;
$$ language plpgsql volatile;

/*
cursor movement action redirections, used to make sense but don't
anymore - todo: split the move_cursor function into separate ones.
*/

select create_client_action_wrapper('move_cursor_down',
       $$move_cursor('down')$$);
select create_client_action_wrapper('move_cursor_up',
       $$move_cursor('up')$$);
select create_client_action_wrapper('move_cursor_left',
       $$move_cursor('left')$$);
select create_client_action_wrapper('move_cursor_right',
       $$move_cursor('right')$$);
select create_client_action_wrapper('move_cursor_up_left',
       $$move_cursor('up-left')$$);
select create_client_action_wrapper('move_cursor_up_right',
       $$move_cursor('up-right')$$);
select create_client_action_wrapper('move_cursor_down_right',
       $$move_cursor('down-right')$$);
select create_client_action_wrapper('move_cursor_down_left',
       $$move_cursor('down-left')$$);
select create_client_action_wrapper('spell_book_show_all_update_on',
       $$spell_book_show_all_update(true)$$);
select create_client_action_wrapper('spell_book_show_all_update_off',
       $$spell_book_show_all_update(false)$$);

create function action_key_pressed(pkeycode text) returns void as $$
declare
  a text;
  cursor_move boolean := false;
begin
/*
basic plan
have a table with key code, and action name
then a strategy of taking an action and
     a. deciding where to get the arguments
     b. deciding if it is allowed

profiling progress: started out about 1 sec to run when using for
loop, got rid of that, got it down to about 0.1 sec but this is for an
unmatched keypress, need to be faster.

*/
  if get_running_effects() then
    return;
  end if;

  if exists(select 1 from valid_activate_actions
            where action='ai_continue')
     and pkeycode = 'space' then
    perform action_client_ai_continue();
  else
    select into a action_name from key_control_settings k
      inner join client_valid_activate_actions v
        on k.action_name = v.action
        where key_code = pkeycode;
    if not a is null then
        if substr(a,0,11) =  'move_cursor' then
          cursor_move := true;
        end if;
        execute 'select action_' || a || '();';
    else
      select into a action_name from key_control_settings k
        inner join client_valid_target_actions v
          on k.action_name = v.action
        natural inner join cursor_position
          where key_code = pkeycode;
      if substr(a,0,11) =  'move_cursor' then
        cursor_move := true;
      end if;
      if not a is null then
        execute 'select action_' || a ||
                '(' || (select x from cursor_position) ||
                ', ' || (select y from cursor_position) || ');';
      else
        null;
      end if;
    end if;
  end if;
  perform update_missing_startticks();
  perform check_for_effects();
  if not cursor_move then
    perform update_board_sprites_cache();
  end if;
end;
$$ language plpgsql volatile;

/*
=== spell choice
*/


  insert into key_control_settings(key_code, action_name)
    select key, 'choose_' || spell_name || '_spell'
    from spell_keys;


/*

================================================================================

== turn phases
*/

create function action_client_next_phase() returns void as $$
begin
  perform action_next_phase();
  if not (select computer_controlled from wizards
          inner join current_wizard_table
          on wizard_name=current_wizard) then
    perform action_move_cursor_to_current_wizard();
  end if;
end;
$$ language plpgsql volatile;

/*
================================================================================

== cursor/go actions
*/

create function action_go() returns void as $$
declare
  r record;
  s text;
begin
  select into r x,y,action from client_valid_target_actions
    natural inner join cursor_position;
  if r is not null then
    s :=  'select action_' || r.action || '(' || r.x || ',' || r.y || ')';
    execute s;
  else
    select into r action
      from client_valid_activate_actions
       where action in ('cast_activate_spell');
    if r is not null then
      s := 'select action_' || r.action || '()';
      execute s;
    end if;
  end if;
  return ;
end;
$$ language plpgsql volatile;

/*
================================================================================

== prompt

use the action valid views to provide the user with information on
what their options are.

*/
create view action_instructions as
select 'cast_target_spell'::text as action,
       'Cast spell: Select a square to cast ' ||
        get_current_wizard_spell() || ' on' as help
union
select 'select_piece_at_position',
       'Select: choose a piece to move by selecting its square'
union
select 'walk',
       'Walk: select a square to move piece to'
union
select 'fly',
       'Fly: select a square to move piece to'
union
select 'attack',
       'Attack: select a square to attack that piece'
union
select 'ranged_attack',
       'Ranged attack: select a square to attack that piece'
union
select 'next_phase',
       'Next phase: press space to finish this wizard''s turn'
union
select 'set_imaginary',
       'Press y to cast an imaginary monster'
union
select 'set_real',
       'Press n to cast a real monster'
union
select 'cast_activate_spell',
       'Cast: Press enter to cast ' || get_current_wizard_spell()
union
select 'cancel',
       'Cancel: press End to cancel move/attack/ranged attack'
union
select 'choose_disbelieve_spell',
       'Press a key from the spell book to choose that spell to cast'
;

create view prompt as
select action, help
  from action_instructions
  natural inner join
  (select action from client_valid_target_actions
   union
   select action from client_valid_activate_actions) as a;

/*

TODO: improve these messages, maybe add in relevant sprites inline,
draw lines onto the playing board, be more specific e.g. the help for
enter could say exactly what options are available, next phase is
context specific (e.g. next phase to decline to move pieces which
haven't moved, or to not use additional shots of the currently casting
spell, or if no parts have been cast, to say cancel spell cast, cancel
also more specific.

TODO: in addition to this help, want to make available a "why can't I
do this" facility, which explains why a particular action can't be run
at this time (for target actions, why a particular action can't be run
at this time on this square).

New idea:
state what activate action is available
or// state what target actions are available for some square
and state what target action will run on the current square

also: for squares with no valid action, try to provide a message
guessing what the user might want to run on that square and explain
why they can't: need to work through some examples to see how obvious
these messages will be to create

*/
