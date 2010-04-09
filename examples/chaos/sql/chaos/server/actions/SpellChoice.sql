select module('Chaos.Server.Actions.SpellChoice');

/*
== spell choice

*/
create function action_choose_spell(vspell_name text)
  returns void as $$
declare
  cw text;
begin
  --create the argumentless action name so we can check the action
  --valid table
  perform check_can_run_action('choose_' || vspell_name || '_spell');
  select into cw current_wizard from current_wizard_table;
  --do nothing if this is the same as the currently selected spell
  if (select spell_name from wizard_spell_choices
        where wizard_name=cw) = vspell_name then
    null;
  else
    --if wizard already has a chosen spell then remove it
    delete from wizard_spell_choices_mr
      where wizard_name = cw;
    insert into wizard_spell_choices_mr (wizard_name
                                        ,spell_name
                                        ,imaginary)
           values (cw
                  ,vspell_name
                  ,case exists(select 1 from monster_spells
                               where spell_name = vspell_name)
                     when true then false
                     else null
                   end);
  end if;
  perform add_history_choose_spell();
end;
$$ language plpgsql volatile;

create function action_choose_no_spell() returns void as $$
begin
  perform check_can_run_action('choose_no_spell');
  delete from wizard_spell_choices_mr where wizard_name = get_current_wizard();
end;
$$ language plpgsql volatile;

create function action_set_imaginary() returns void as $$
begin
  perform check_can_run_action('set_imaginary');
  update wizard_spell_choices_mr
    set imaginary = true
    where wizard_name = get_current_wizard();
end;
$$ language plpgsql volatile;

create function action_set_real() returns void as $$
begin
  perform check_can_run_action('set_real');
  update wizard_spell_choices_mr
    set imaginary = false
    where wizard_name = get_current_wizard();
end;
$$ language plpgsql volatile;
/*
=== internals
generate the individual spell choice actions

*/

select generate_spell_choice_actions();
