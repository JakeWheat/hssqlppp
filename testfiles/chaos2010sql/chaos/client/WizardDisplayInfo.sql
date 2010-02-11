/*
================================================================================

== wizard display info

This table associates a wizards name (= the allegiance) from the
server with a colour for the wizard and his army and a wizard sprite
for display purposes.

The sprite in this table is what the wizard uses if he doesn't have
any upgrades.

Wizard named 'name' started with sprite default_sprite, his army is
coloured 'colour'.

*/
select module('Chaos.Client.WizardDisplayInfo');

create table wizard_display_info (
  wizard_name text unique references wizards,
  default_sprite text unique references sprites, -- and matches /wizard.*/
  colour text unique
);
select set_relvar_type('wizard_display_info','data');

create table init_wizard_display_info_argument (
  wizard_name text unique references wizards,
  sprite text unique references sprites, -- starts with wizard
  colour text unique --todo: make list of colours
);
select set_relvar_type('init_wizard_display_info_argument', 'stack');

create function init_wizard_display_info() returns void as $$
begin
    insert into wizard_display_info (wizard_name, default_sprite,  colour)
       select wizard_name,sprite,colour
       from init_wizard_display_info_argument;
end;
$$ language plpgsql volatile;

/*
================================================================================

== action history with colours

create a view to supply grey as colour for corpses (corpses don't have
an allegiance)

*/
create view allegiance_colours as
  select wizard_name as allegiance, colour from wizard_display_info union
  select 'dead' as allegiance, 'grey' as colour;

create view action_history_colour_mr as
select a.*, colour
  from action_history_mr a
  natural inner join allegiance_colours;
