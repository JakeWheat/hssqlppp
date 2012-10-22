/*
================================================================================

= board widget

*/
select module('Chaos.Client.BoardWidget');
/*
== cursor position + ops

The cursor is at position x,y

The server code has no concept of the cursor.
In the end, this has just made the code more complicated for no reason.
*/
create table cursor_position (
  x int,
  y int
);
select create_assertion('cursor_position_coordinates_valid',
$$ not exists (select 1 from cursor_position
  cross join board_size
  where x >= width or y >= height)$$);
select set_relvar_type('cursor_position', 'data');

select restrict_cardinality('cursor_position', '1');

/*
=== actions
cursor movement
*/

create function safe_move_cursor(px int, py int) returns void as $$
begin
  update cursor_position
    set x = least(greatest(x + px, 0), (select width from board_size) - 1),
        y = least(greatest(y + py, 0), (select height from board_size) - 1);
end;
$$ language plpgsql volatile;

create function action_move_cursor(direction text) returns void as $$
begin
  case direction
  when 'up' then
    perform safe_move_cursor(0, -1);
  when 'down' then
    perform safe_move_cursor(0, 1);
  when 'left' then
    perform safe_move_cursor(-1, 0);
  when 'right' then
    perform safe_move_cursor(1, 0);
  when 'up-left' then
    perform safe_move_cursor(-1, -1);
  when 'up-right' then
    perform safe_move_cursor(1, -1);
  when 'down-left' then
    perform safe_move_cursor(-1, 1);
  when 'down-right' then
    perform safe_move_cursor(1, 1);
  else
    raise exception
      'asked to move cursor in direction % which isn''t valid',
      direction;
  end case;
end;
$$ language plpgsql volatile;

/*
=== internals
When next phase is called, moved the cursor to that wizard
*/
create function action_move_cursor_to_current_wizard() returns void as $$
declare
 p pos;
begin
  --don't move cursor during autonomous phase
  if get_turn_phase() != 'autonomous' then
    select into p x,y from pieces
         inner join current_wizard_table
         on (current_wizard = allegiance)
         where ptype = 'wizard';
    update cursor_position set (x,y) = (p.x,p.y);
  end if;
end;
$$ language plpgsql volatile;

create function init_cursor_position() returns void as $$
begin
  insert into cursor_position (x,y) values (0,0);
end;
$$ language plpgsql volatile;

/*

the plan is to have a board_sprites view for the board widget. This
contains all the sprites on the board (basically everything drawn on
the board: piece sprites, cursor, highlights, etc.)  all the board
needs is x,y,sprite and order. The order is used to make sure
overlapping sprites e.g. a piece, the cursor and a highlight, are
drawn in the right order

*/


/*
== piece sprites
Want to produce a list of x,y,sprite rows
for the pieces on top, the cursor,
and the highlights for the currently available actions

wizard sprites: look in the action history to find the most recent upgrade
*/
create view wizard_sprites as
  select wizard_name,sprite,colour from
  (select row_number() over(partition by wizard_name order by o desc) as rn,
    wizard_name,
    case when shadow_form then sprite || '_shadow'
         else sprite
    end as sprite, w.colour from
  (select -1 as o, wizard_name, default_sprite as sprite
      from wizard_display_info
    union all
    select id as o, allegiance as wizard_name,
      'wizard_' || spell_name
      from action_history_mr
      natural inner join spells_mr
      where spell_name != 'shadow_form'
      and spell_category = 'wizard'
      and history_name = 'spell_succeeded'
      ) as a
  natural inner join wizard_display_info as w
  natural inner join wizards) as w where rn = 1;

/*

piece ptype-allegiance-tag is at x,y, allegiance colour is 'colour',
sprite is 'sprite', sprite priority is sp.

*/
create view piece_sprite as
  select x,y,ptype,
    case when ptype='wizard' then w.sprite
         when allegiance='dead' then 'dead_' || ptype
         else ptype
    end as sprite,
    ac.colour,tag,allegiance
  from pieces p
  left outer join wizard_sprites w
    on (allegiance = wizard_name and ptype='wizard')
  inner join allegiance_colours ac
    using (allegiance);

/*
== highlights
*/

create view board_highlights as
-- include the squares for the selected spell
-- when still in the choose phase, so the user can
--see what squares are valid for their chosen spell
select x,y,'highlight_cast_target_spell' as sprite
  from current_wizard_spell_squares
  where get_turn_phase() = 'choose'
union all
select x,y,'highlight_' || action as sprite
  from valid_target_actions;

/*
== animation

we save a starting tick against each piece. Not really sure what the
best way to do this, some options are:

these are updated in the action_key_pressed and client_ai_continue fns

*/
create table piece_starting_ticks (
  ptype text,
  allegiance text,
  tag int,
  start_tick int,
  unique (ptype,allegiance,tag),
  foreign key (ptype,allegiance,tag) references pieces
);
select set_relvar_type('piece_starting_ticks', 'data');

create function update_missing_startticks()
  returns void as $$
begin
  insert into piece_starting_ticks (ptype,allegiance,tag,start_tick)
    select ptype,allegiance,tag, random()*2500 from pieces
      where (ptype,allegiance,tag) not in
        (select ptype,allegiance,tag
        from piece_starting_ticks);
end;
$$ language plpgsql volatile;

/*

== board sprites

put the piece sprites, the highlight and the cursor
together to give the full list of sprites

split this up so the cursor movement isn't really laggy, just a hack -
needs some more thought.

*/
create view board_sprites1_view as
  select x,y,ptype,allegiance,tag,sprite,colour,sp,
    start_tick, animation_speed, selected from
    (select x,y,ptype,allegiance,tag,
      sprite,colour,sp,0 as start_tick,
      case when not move_phase is null then true
        else false
      end as selected
      from piece_sprite
    natural inner join pieces_on_top
    --natural inner join piece_starting_ticks
    natural inner join sprites
    natural left outer join selected_piece
    union all
    select x,y, '', '', -1, sprite, 'white', 5,0,false
      from board_highlights) as a
  natural inner join sprites
union all
select x,y, '', '', -1,'cursor', 'white', 6,0, animation_speed, false
  from cursor_position
  inner join sprites on sprite='cursor'
  order by sp;


/*create table board_sprites1_cache as
  select * from board_sprites1_view;
select set_relvar_type('board_sprites1_cache', 'data');

create function update_board_sprites_cache() returns void as $$
begin
  --if get_running_effects() then
  --  return;
  --end if;
  --raise notice 'update bpc';
  delete from board_sprites1_cache;
  insert into board_sprites1_cache
    select * from board_sprites1_view;
end;
$$ language plpgsql volatile;

create view board_sprites as
 select * from board_sprites1_cache
union all
select x,y, '', '', -1,'cursor', 'white', 6,0, animation_speed, false
  from cursor_position
  inner join sprites on sprite='cursor';
*/



/*
== effects

two sorts of effects: beam and square

*/
/*
create table board_square_effects (
  id serial unique,
  subtype text,
  x1 int,
  y1 int,
  queuePos int
);
select set_relvar_type('board_square_effects', 'data');

create table board_beam_effects (
  id serial unique,
  subtype text,
  x1 int,
  y1 int,
  x2 int,
  y2 int,
  queuePos int
);
select set_relvar_type('board_beam_effects', 'data');

create table board_sound_effects (
  id serial unique,
  subtype text,
  sound_name text,
  queuePos int
);
select set_relvar_type('board_sound_effects', 'data');

create function get_running_effects() returns boolean as $$
begin
  return exists (select 1 from board_beam_effects)
      or exists (select 1 from board_square_effects)
      or exists (select 1 from board_sound_effects);
end;
$$ language plpgsql stable;


create table history_sounds (
  history_name text,
  sound_name text,
  unique (history_name,sound_name)
);
select set_relvar_type('history_sounds', 'readonly');

copy history_sounds (history_name,sound_name) from stdin;
walked	walk
fly	fly
attack	attack
ranged_attack	shoot
game_drawn	draw
game_won	win
spell_failed	fail
spell_succeeded	success
shrugged_off	shrugged_off
wizard_up	wizard_up
new_game	new_game
chinned	kill
attempt_target_spell	cast
\.

create table history_no_visuals (
  history_name text unique
);
select set_relvar_type('history_no_visuals', 'readonly');

copy history_no_visuals (history_name) from stdin;
wizard_up
new_turn
new_game
game_won
game_drawn
choose_spell
set_imaginary
set_real
\.

select create_var('last_history_effect_id', 'int');
select set_relvar_type('last_history_effect_id_table', 'data');

create function check_for_effects() returns void as $$
begin
  insert into board_square_effects (subtype, x1, y1, queuePos)
    select history_name,case when tx is null then x else tx end,
                        case when ty is null then y else ty end,id
    from action_history_mr
    where id > get_last_history_effect_id()
         and x is not null and y is not null
         and history_name not in (select history_name from history_no_visuals);
  insert into board_beam_effects (subtype,x1,y1,x2,y2,queuePos)
    select history_name,x,y,tx,ty,id
    from action_history_mr
    where id > get_last_history_effect_id()
         and x is not null and y is not null
         and tx is not null and ty is not null
         and history_name not in (select history_name from history_no_visuals);
  insert into board_sound_effects (subtype, sound_name,queuePos)
    select history_name,sound_name,id
    from action_history_mr
    natural inner join history_sounds
    left outer join wizards on allegiance = wizard_name
    where id > get_last_history_effect_id()
--exclude turn sound for computer controlled wizards choose phase
      and not(history_name='wizard_up'
              and turn_phase='choose'
              and coalesce(computer_controlled,false))
;
  update last_history_effect_id_table set
    last_history_effect_id = (select max(id) from action_history_mr);
end;
$$ language plpgsql volatile;
*/
/*

call this function before reading the current effects table and it
will leave those tables the same if the current effects are still
playing, or it will clear the old effects and fill them with the next
set of effects.

call it after reading the current effects table to clear the current
row of sounds, that way the sounds will only be returned to the ui
once and thus will only be played once.

*/

/*create table current_effects (
  ticks int,
  queuePos int
);
select set_relvar_type('current_effects', 'data');

select restrict_cardinality('current_effects', 1);*/

/*create view current_board_sound_effects as
  select * from board_sound_effects
  natural inner join current_effects;

create view current_board_beam_effects as
  select * from board_beam_effects
  natural inner join current_effects;

create view current_board_square_effects as
  select * from board_square_effects
  natural inner join current_effects;
*/
/*create function action_reset_current_effects() returns void as $$
begin
    delete from board_sound_effects;
    delete from board_beam_effects;
    delete from board_square_effects;
    delete from current_effects;
end;
$$ language plpgsql volatile;*/
/*
create function action_update_effects_ticks(pticks int) returns void as $$
declare
  wasEffects boolean := false;
  nextQp int;
begin
  if exists(select 1 from current_effects) then
    wasEffects := true;
  end if;
  --always delete sound effects after the first time they are returned
  if exists(select 1 from current_board_sound_effects) then
    delete from board_sound_effects
      where queuePos = (select queuePos from current_effects);
  end if;
  --see if we need a new row of effects
  if not exists(select 1 from current_effects)
    or pticks > (select ticks + 6 from current_effects) then
    delete from board_sound_effects
      where queuePos = (select queuePos from current_effects);
    delete from board_beam_effects
      where queuePos = (select queuePos from current_effects);
    delete from board_square_effects
      where queuePos = (select queuePos from current_effects);
    delete from current_effects;
    nextQp := (select min(queuePos) from
                 (select queuePos from board_sound_effects
                  union all
                  select queuePos from board_beam_effects
                  union all
                  select queuePos from board_square_effects) as a);
    if nextQp is not null and nextQp <> 0 then
      insert into current_effects (ticks, queuePos)
        values (pticks, nextQp);
    end if;
  end if;
  if not exists(select 1 from current_effects)
     and wasEffects then
    perform update_board_sprites_cache();
  end if;
end;
$$ language plpgsql volatile;
*/
create function action_client_ai_continue() returns void as $$
begin
  /*if get_running_effects() then
    return;
  end if;*/

  perform action_ai_continue();
  perform update_missing_startticks();
  if (select computer_controlled from wizards
      inner join current_wizard_table on wizard_name=current_wizard)
     and get_turn_phase() = 'choose' then
    perform action_client_ai_continue();
  else
    --perform check_for_effects();
    --perform update_board_sprites_cache();
  end if;
  if not (select computer_controlled from wizards
          inner join current_wizard_table
          on wizard_name=current_wizard) then
    perform action_move_cursor_to_current_wizard();
  end if;
end;
$$ language plpgsql volatile;

create function action_client_ai_continue_if() returns void as $$
begin
  if exists(select 1 from valid_activate_actions
            where action='ai_continue') then
    perform action_client_ai_continue();
  end if;
end;
$$ language plpgsql volatile;

/*

================================================================================

= info widget

create a few views to help with the stuff shown
in the info widget

*/

create view piece_details as
  select * from pieces_mr
            full outer join
      (select 'wizard'::text as wtype,* from wizards where not expired) as a
            on (allegiance = wizard_name and ptype = wtype)
    natural inner join pieces_with_priorities
    natural inner join piece_sprite;

create view cursor_piece_details as
  select * from piece_details
      natural inner join cursor_position;

create view selected_piece_details as
  select * from piece_details
      natural inner join selected_piece
      natural full outer join remaining_walk_table;
