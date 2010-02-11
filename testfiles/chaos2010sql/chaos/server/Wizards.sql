/*
== wizards

*/
select module('Chaos.Server.Wizards');

create table wizards (
  wizard_name text primary key,
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
select set_relvar_type('wizards', 'data');

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
  wizard_name text references wizards,
  spell_name text references spells_mr
);
select set_relvar_type('spell_books', 'data');

select create_assertion('no_spells_for_stiffs',
  $$ not exists(select 1 from spell_books
  natural inner join wizards where expired = true)$$);
