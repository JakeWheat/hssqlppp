/*

================================================================================

== new game action
*/

select module('Chaos.Client.ClientNewGame');

/*create table action_client_new_game_argument (
  place int unique,
  wizard_name text unique,
  sprite text unique references sprites,
  colour text unique, --references colours(name),
  computer_controlled boolean
);
select set_relvar_type('action_client_new_game_argument', 'stack');

select create_assertion('action_client_new_game_place_valid',
'(select count(*) from action_client_new_game_argument
  where place >=
  (select count(*) from action_client_new_game_argument)) = 0');*/

create table client_new_game_t (
  place int,
  wizard_name text,
  sprite text,
  colour text,
  computer_controlled boolean
);

--this calls server new game
create function action_client_new_game(a client_new_game_t []) returns void as $$
begin
  --assert: argument has between 2 and 8 active wizards
  --delete from action_new_game_argument;
  --delete from init_wizard_display_info_argument;
  -- clear data tables
  delete from cursor_position;
  delete from wizard_display_info;

  --delete from last_history_effect_id_table;
  --insert into last_history_effect_id_table values (-1);
  --delete from board_square_effects;
  --delete from board_beam_effects;
  --delete from board_sound_effects;
  --delete from current_effects;

  perform action_new_game(
          (with
            s as
              (select place,wizard_name, computer_controlled
               from unnest(a))
           select array_agg((place,wizard_name,computer_controlled)::new_game_t)
             from s));

  --wizard display_info
  /*delete from init_wizard_display_info_argument;
  insert into init_wizard_display_info_argument
      (wizard_name, sprite, colour)
    select wizard_name, sprite, colour
    from unnest(a);*/
  perform init_wizard_display_info(
    (with s as (select wizard_name,sprite as default_sprite,colour from unnest(a))
     select array_agg((wizard_name,default_sprite,colour)::wizard_display_info)
     from s));

  if not exists(select 1 from spell_book_show_all_table) then
    insert into spell_book_show_all_table values (false);
  end if;

  --perform update_board_sprites_cache();
  --perform check_for_effects();
  perform init_cursor_position();
end
$$ language plpgsql volatile;

/*
select protect_readonly_relvars();
select set_notifies_on_all_data_tables();
*/