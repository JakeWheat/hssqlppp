select module('Chaos.Server.Actions.NextPhase');

/*
== next phase

next phase is the function to end a wizard's turn and move to the next
one. It can be called implicitly by other actions as well as directly,
e.g. once you've cast your spell, there's nothing you can do except
end your turn so next_phase gets called automatically

*/

select create_var('dont_nest_ai_next_phase', 'bool');
select set_relvar_type('dont_nest_ai_next_phase_table', 'stack');

create function action_next_phase() returns void as $$
declare
  c int;
  next_phase_again boolean := false;
begin
/*
=== check for game completion
*/
  if (exists (select 1 from game_completed_table)) then
    return;
  end if;
  --check for win or draw
  c := (select count(1) from wizards
         where not expired);
  if c = 1 then --someone has won
    perform game_completed();
    update current_wizard_table set current_wizard =
      (select wizard_name from wizards where not expired);
    perform add_history_game_won();
    return;
  elseif c = 0 then --game is drawn
    perform game_completed();
    perform add_history_game_drawn();
    delete from current_wizard_table;
    return;
  end if;

/*
=== current wizard clean up phase

If the user selects next phase when they have a spell to cast, then we
want to call the usual skip spell action so as not to duplicate the
work. But skip spell will call next_phase itself automatically and we
don't want to do two next phases, so if there is a spell to be
skipped, run that and don't run the rest of the next_phase function
since it will be called via skip spell.

this might be better if skip spell wasn't used for both explicitly and
implicitly skipping

*/
    -- if the current spell isn't completed, then skip it
  if exists(select 1 from wizard_spell_choices
       inner join current_wizard_table
       on (current_wizard = wizard_name)
       where get_turn_phase() = 'cast') then
    perform skip_spell();
    return;
  end if;

  --multiple update hack to get round constraints
  update in_next_phase_hack_table
    set in_next_phase_hack = true;

  --complete current phase:
  if (select turn_phase = 'move' from turn_phase_table) then
    delete from pieces_moved;
  end if;

/*
=== all wizards clean up phase

clean up if this is the last wizard for this phase, then move to next
phase, if this is autonomous, then do it and move to move phase this
works because all the end phase stuff happens before the autonomous
phase is run in this function, and all the setup runs after it is run.

*/

  if is_last_wizard() then
    --clear the cast alignment which is used to adjust the world
    --alignment when a spell is cast
    if get_turn_phase() = 'cast' then
      delete from cast_alignment_table;
    end if;

    --if this is the end of the move phase then we're on the next turn
    if (select turn_phase = 'move' from turn_phase_table) then
      update turn_number_table
        set turn_number = turn_number + 1;
      perform add_history_new_turn();
    end if;

    --move to the next turn phase
    update turn_phase_table
      set turn_phase = next_turn_phase(turn_phase);

    if (select turn_phase = 'autonomous' from turn_phase_table) then
      perform do_autonomous_phase();
      update turn_phase_table
        set turn_phase = next_turn_phase(turn_phase);
    end if;
  end if;

/*
=== init new current phase
*/
  -- move to the next wizard, this is the meat of this function
  update current_wizard_table
    set current_wizard = next_wizard(current_wizard);

  --setup the cast alignment table if this is the start of the cast
  --phases
  if get_turn_phase() = 'cast' and is_first_wizard() then
    insert into cast_alignment_table values(0);
  end if;

  --initialise the spell for this phase
  if (select turn_phase = 'cast' from turn_phase_table) then
    if exists(select 1 from current_wizard_spell) then
      insert into spell_parts_to_cast_table
        select coalesce(numb, 0) from target_spells
        natural inner join current_wizard_spell;
      insert into cast_success_checked_table values (false);
    else
      --skip to the next phase automatically
      next_phase_again := true;
    end if;
  /*elseif (select turn_phase = 'move' from turn_phase_table) then
    insert into pieces_to_move
      select ptype, allegiance, tag
        from moving_pieces
        inner join current_wizard_table
        on allegiance = current_wizard;*/
  end if;

  --finished our updates for this next phase
  update in_next_phase_hack_table
    set in_next_phase_hack = false;

  perform add_history_wizard_up();
/*
=== continue
*/
  --if there is nothing to do in the new current phase - continue to
  --next phase automatically
  if next_phase_again then
    perform action_next_phase();
  end if;
end;
$$ language plpgsql volatile;

/*
=== internals
*/
create function is_last_wizard() returns boolean as $$
begin
  return ((select place from live_wizards
        natural inner join current_wizard)
     = (select max(place) from live_wizards));
end;
$$ language plpgsql stable;

create function is_first_wizard() returns boolean as $$
begin
  return ((select place from live_wizards
       natural inner join current_wizard)
     = (select min(place) from live_wizards));
end;
$$ language plpgsql stable;
