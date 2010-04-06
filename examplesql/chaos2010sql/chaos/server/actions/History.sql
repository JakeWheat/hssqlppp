/*
================================================================================

= history
save a short description of each action completed during play

TODO: create a detailed history that allows a game to be replayed
then create a view to show the player visible log with some events
removed and some combined.
*/

select module('Chaos.Server.Actions.History');

create domain history_name_enum as text
       check (value in (
                        'spell_succeeded'
                       ,'spell_failed'
                       ,'chinned'
                       ,'shrugged_off'
                       ,'walked'
                       ,'fly'
                       ,'attack'
                       ,'ranged_attack'
                       ,'set_imaginary'
                       ,'set_real'
                       ,'game_won'
                       ,'game_drawn'
                       ,'spell_skipped'
                       ,'new_turn'
                       ,'wizard_up'
                       ,'choose_spell'
                       ,'spread'
                       ,'recede'
                       ,'disappear'
                       ,'new_game'
                       ,'attempt_target_spell'
                       ));

select create6nf ($$

  action_history_mr (
    id serial unique,
    history_name history_name_enum
  );

  action_history_allegiance : action_history_mr(
    allegiance text
  );

  action_history_source : action_history_allegiance (
    x int,
    y int
  );

  action_history_target : action_history_source (
    tx int,
    ty int
  );

  action_history_piece : action_history_allegiance (
    ptype text,
    tag int
  );

  action_history_piece_source : action_history_piece, action_history_source;

  action_history_piece_target : action_history_piece, action_history_target;

  action_history_spell : action_history_allegiance (
    spell_name text
  );

  action_history_spell_source : action_history_spell, action_history_source;

  action_history_spell_target : action_history_spell, action_history_target;

  action_history_num_wiz : action_history_mr (
    num_wizards int null
  );

  action_history_turn_num : action_history_mr (
    turn_number int
  );

  action_history_turn_phase : action_history_source (
    turn_phase turn_phase_enum
  );

$$);

select set_relvar_type('action_history_mr', 'data');

--Turns

create function get_current_wizard_pos() returns pos as $$
  select x,y from current_wizard_table
    inner join pieces
      on ptype = 'wizard' and allegiance = current_wizard
$$ language sql stable;

create function add_history_new_turn() returns void as $$
begin
  insert into action_history_mr (history_name, turn_number)
    values ('new_turn', get_turn_number());
end;
$$ language plpgsql volatile;

create function add_history_wizard_up() returns void as $$
declare
  w pos;
begin
  w := get_current_wizard_pos();
  insert into action_history_mr (history_name, allegiance, turn_phase, x, y)
    values ('wizard_up', get_current_wizard(), get_turn_phase(), w.x, w.y);
end;
$$ language plpgsql volatile;

create function add_history_new_game() returns void as $$
begin
  insert into action_history_mr (history_name, num_wizards)
    values ('new_game', (select count(1) from wizards));
end;
$$ language plpgsql volatile;

create function add_history_game_won() returns void as $$
declare
  w pos;
begin
  w := get_current_wizard_pos();
  insert into action_history_mr (history_name, allegiance, x, y)
    values ('game_won', (select allegiance
                         from pieces
                         where ptype='wizard'),
                         w.x, w.y);
end;
$$ language plpgsql volatile;

create function add_history_game_drawn() returns void as $$
begin
  insert into action_history_mr (history_name)
    values ('game_drawn');
end;
$$ language plpgsql volatile;

--Choosing

create function add_history_choose_spell() returns void as $$
declare
  w pos;
begin
  w := get_current_wizard_pos();
  insert into action_history_mr (history_name, allegiance, spell_name,x,y)
    values ('choose_spell', get_current_wizard(), get_current_wizard_spell(),
            w.x,w.y);
end;
$$ language plpgsql volatile;

create function add_history_set_imaginary() returns void as $$
begin
  insert into action_history_mr (history_name, allegiance)
    values ('set_imaginary', get_current_wizard());
end;
$$ language plpgsql volatile;

create function add_history_set_real() returns void as $$
begin
  insert into action_history_mr (history_name, allegiance)
    values ('set_real', get_current_wizard());
end;
$$ language plpgsql volatile;

--Casting

create function add_history_attempt_target_spell(px int, py int) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces
    where allegiance=get_current_wizard()
      and ptype = 'wizard';
  insert into action_history_mr (history_name, allegiance, spell_name, x, y, tx, ty)
    values ('attempt_target_spell', get_current_wizard(),
            get_current_wizard_spell(), w.x, w.y, px, py);
end;
$$ language plpgsql volatile;

create function add_history_attempt_activate_spell() returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces
    where allegiance=get_current_wizard()
      and ptype = 'wizard';
  insert into action_history_mr (history_name, allegiance, spell_name, x, y)
    values ('attempt_target_spell', get_current_wizard(),
            get_current_wizard_spell(), w.x, w.y);
end;
$$ language plpgsql volatile;


create function add_history_spell_succeeded() returns void as $$
begin
  insert into action_history_mr (history_name, allegiance, spell_name)
    values ('spell_succeeded', get_current_wizard(), get_current_wizard_spell());
end;
$$ language plpgsql volatile;

create function add_history_spell_failed() returns void as $$
begin
  insert into action_history_mr (history_name, allegiance, spell_name)
    values ('spell_failed', get_current_wizard(), get_current_wizard_spell());
end;
$$ language plpgsql volatile;

create function add_history_spell_skipped() returns void as $$
begin
  insert into action_history_mr (history_name, allegiance, spell_name)
    values ('spell_skipped', get_current_wizard(), get_current_wizard_spell());
end;
$$ language plpgsql volatile;

--chinned and shrugged off

create function add_history_chinned(k piece_key) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype, allegiance,tag, x, y)
    values ('chinned', k.ptype, k.allegiance, k.tag, w.x, w.y);

end;
$$ language plpgsql volatile;

create function add_history_shrugged_off(k piece_key) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype, allegiance,tag, x, y)
    values ('shrugged_off', k.ptype, k.allegiance, k.tag,w.x, w.y);
end;
$$ language plpgsql volatile;

--Autonomous

create function add_history_receive_spell(pwizard_name text, pspell_name text) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces
    where allegiance=pwizard_name
      and ptype = 'wizard';
  insert into action_history_mr (history_name, allegiance, spell_name, x, y)
    values ('choose_spell', pwizard_name, pspell_name, w.x, w.y);
end;
$$ language plpgsql volatile;

create function add_history_spread(k piece_key) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype,allegiance,tag, x, y)
    values ('spread', k.ptype,k.allegiance,k.tag, w.x, w.y);
end;
$$ language plpgsql volatile;

create function add_history_recede(k piece_key) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype,allegiance,tag,x, y)
    values ('recede', k.ptype,k.allegiance,k.tag, w.x, w.y);
end;
$$ language plpgsql volatile;

create function add_history_disappear(k piece_key) returns void as $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype,allegiance,tag, x, y)
    values ('disappear', k.ptype,k.allegiance,k.tag, w.x, w.y);
end;
$$ language plpgsql volatile;


--Move
create function add_history_walked(sx int, sy int) returns void as $$
declare
  w pos;
  k piece_key;
begin
  select into k ptype,allegiance,tag from selected_piece;
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x, y, tx, ty)
    values ('walked', k.ptype, k.allegiance, k.tag, w.x,w.y, sx, sy);
end;
$$ language plpgsql volatile;

create function add_history_fly(sx int, sy int) returns void as $$
declare
  w pos;
  k piece_key;
begin
  select into k ptype,allegiance,tag from selected_piece;
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x, y, tx, ty)
    values ('fly', k.ptype, k.allegiance, k.tag, w.x,w.y, sx, sy);
end;
$$ language plpgsql volatile;

create function add_history_attack(t piece_key) returns void as $$
declare
  sp record;
  tp pos;
begin
  select into sp ptype,allegiance,tag,x,y from selected_piece
    natural inner join pieces;
  select into tp x,y from pieces where (ptype,allegiance,tag) = t;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x,y,tx,ty)
    values ('attack', sp.ptype, sp.allegiance, sp.tag, sp.x,sp.y, tp.x,tp.y);
end;
$$ language plpgsql volatile;

create function add_history_ranged_attack(t piece_key) returns void as $$
declare
  sp record;
  tp pos;
begin
  select into sp ptype,allegiance,tag,x,y from selected_piece
    natural inner join pieces;
  select into tp x,y from pieces where (ptype,allegiance,tag) = t;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x,y,tx,ty)
    values ('ranged_attack', sp.ptype, sp.allegiance, sp.tag, sp.x,sp.y, tp.x,tp.y);
end;
$$ language plpgsql volatile;


/*

create a view to hide the details which players shouldn't be able to
see - this is the view that the ui should use:
hide set real, imaginary
hide spell received in tree


*/
