/*
== wizards

*/
select module('Chaos.Server.Wizards');

create table wizards (
  wizard_name text unique,
  shadow_form boolean default false,
  magic_sword boolean default false,
  magic_knife boolean default false,
  magic_shield boolean default false,
  magic_wings boolean default false,
  magic_armour boolean default false,
  magic_bow boolean default false,
  computer_controlled boolean,
  original_place int, -- 0 <= n < num wizards
  expired boolean default false
);
--select add_key('wizards', 'wizard_name');
--select set_relvar_type('wizards', 'data');

create view live_wizards as
  select *,
         row_number() over(order by original_place) - 1 as place
  from wizards where not expired;

/*
== spell books
Wizard 'wizard_name' is able to cast spell 'spell_name'.
*/

create table spell_books (
  id serial unique,
  wizard_name text,
  spell_name text
);
--select add_key('spell_books', 'id');
--select add_foreign_key('spell_books', 'wizard_name', 'wizards');
/*select create_assertion('no_spells_for_stiffs',
  $$ not exists(select 1 from spell_books
  natural inner join wizards where expired = true)$$,
  array['spell_books', 'wizards']);*/
--select add_foreign_key('spell_books', 'spell_name', 'spells');
--select set_relvar_type('spell_books', 'data');

--select set_module_for_preceding_objects('wizards');
