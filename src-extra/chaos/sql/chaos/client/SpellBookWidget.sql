/*
================================================================================

= spell book widget

order the spells:
wizard, attack, object, misc, monster
law spells, then neutral, then chaos,
highest to lowest base chance,
alpha by spell name

this is a proper mess

== sprites
*/
select module('Chaos.Client.SpellBookWidget');

create table spell_sprites (
  spell_name text unique references spells_mr,
  sprite text references sprites
);
select set_relvar_type('spell_sprites', 'readonly');

copy spell_sprites(spell_name, sprite) from stdin;
magic_wood	magic_tree
shadow_wood	shadow_tree
magic_fire	magic_fire
gooey_blob	gooey_blob
wall	wall
magic_castle	magic_castle
dark_citadel	dark_citadel
magic_bolt	magic_bolt
lightning	lightning
vengeance	vengeance
justice	justice
dark_power	dark_power
decree	decree
magic_armour	wizard_magic_armour
magic_shield	wizard_magic_shield
magic_knife	wizard_magic_knife
magic_sword	wizard_magic_sword
magic_bow	wizard_magic_bow
magic_wings	wizard_magic_wings
law	law
large_law	large_law
chaos	chaos
large_chaos	large_chaos
raise_dead	raise_dead
subversion	subversion
turmoil	turmoil
disbelieve	disbelieve
eagle	eagle
elf	elf
faun	faun
ghost	ghost
giant	giant
giant_rat	giant_rat
goblin	goblin
golden_dragon	golden_dragon
gorilla	gorilla
green_dragon	green_dragon
gryphon	gryphon
harpy	harpy
horse	horse
hydra	hydra
king_cobra	king_cobra
lion	lion
manticore	manticore
ogre	ogre
orc	orc
pegasus	pegasus
red_dragon	red_dragon
skeleton	skeleton
spectre	spectre
unicorn	unicorn
vampire	vampire
wraith	wraith
zombie	zombie
shadow_form	chaos
\.

/*
== show all setting
*/
select create_var('spell_book_show_all', 'boolean');
select set_relvar_type('spell_book_show_all_table', 'data');

create function action_spell_book_show_all_update(v boolean)
  returns void as $$
begin
  update spell_book_show_all_table set spell_book_show_all=v;
end;
$$ language plpgsql volatile;


/*
=== internals
==== ordering
order the spells by spell category
*/
create view section_orderv as
  select 1 as section_order, 'wizard' as spell_category
    union all
  select 2 as section_order, 'attacking' as spell_category
    union all
  select 3 as section_order, 'object' as spell_category
    union all
  select 4 as section_order, 'miscellaneous' as spell_category
    union all
  select 5 as section_order, 'monster' as spell_category;

create view spells_with_order as
  select *, case
          when alignment > 0 then 0
              when alignment = 0 then 1
        when alignment < 0 then 2
      end as alignment_order
  from spells_mr_base natural inner join section_orderv;
/*
==== spell counts
*/
create view current_wizard_spell_counts as
  select spell_name, 0 as count from
    (select spell_name from spells_mr except
     select spell_name from spell_books
       inner join current_wizard_table
       on (wizard_name = current_wizard)) as a
 union all
  select spell_name, count(spell_name)
  from spell_books
  inner join current_wizard_table
    on (wizard_name = current_wizard)
  group by spell_name;

--create a string to represent the number of copies of each spell
create function count_icons(int) returns text as $$
  select repeat('#', $1) as result;
$$ language sql immutable;

--create a string to represent the alignment of each spell
create function align_icons(int) returns text as $$
  select case
    when $1 < 0 then  repeat('*', -$1)
    when $1 > 0 then  repeat('+', $1)
    else '-'
  end as result
$$ language sql immutable;
/*
==== colours
colour each spell according to the probability of casting success
*/

create function chance_colour(chance int) returns text as $$
begin
  return case
    when chance = 0 then 'grey'
    when chance between 1 and 20 then 'red'
    when chance between 21 and 40 then 'purple'
    when chance between 41 and 60 then 'green'
    when chance between 61 and 80 then 'cyan'
    when chance between 81 and 99 then 'yellow'
    when chance = 100 then 'white'
    else 'blue'
  end;
end;
$$ language plpgsql immutable;

create view spell_colours as
  select spell_name, chance_colour(chance) as colour
    from spell_cast_chance;

create function spell_colour(vspell text, vcount int) returns text as $$
declare
  colour text;
begin
  --if spell is current wizard's selected spell then highlight it
  --if spell count is 0 or we aren't in choose phase then colour is grey
  --else colour spell according to casting chance
  if (exists (select 1 from wizard_spell_choices
             inner join current_wizard_table
        on wizard_name = current_wizard
        where spell_name = vspell)) then
    colour := 'inverse-' || chance_colour(spell_cast_chance(vspell));
  elseif (vcount = 0 or get_turn_phase() != 'choose') then
    colour := 'grey';
  else
    colour := chance_colour(spell_cast_chance(vspell));
  end if;
  return coalesce(colour, 'blue');
end;
$$ language plpgsql stable;

-- format function for alignment
create function format_alignment(alignment int) returns text as $$
begin
  if (alignment < 0) then
    return 'chaos-' || cast(@ alignment as text);
  elseif (alignment > 0) then
    return 'law-' || cast(alignment as text);
  else
    return 'neutral';
  end if;
end;
$$ language plpgsql immutable;
/*
== spell choice controls
*/
create table spell_keys (
  spell_name text unique references spells_mr,
  key text unique
);
 select set_relvar_type('spell_keys', 'readonly');

copy spell_keys (spell_name, key) from stdin;
magic_knife	1
magic_shield	2
magic_armour	3
magic_bow	4
magic_sword	5
shadow_form	6
magic_wings	7
decree	A
justice	B
lightning	C
magic_bolt	D
vengeance	E
dark_power	F
magic_wood	G
magic_castle	H
wall	I
gooey_blob	J
magic_fire	K
dark_citadel	L
shadow_wood	M
law	O
large_law	P
disbelieve	Q
subversion	R
turmoil	S
chaos	T
large_chaos	U
raise_dead	V
horse	a
king_cobra	b
eagle	c
elf	d
unicorn	e
gryphon	f
lion	g
pegasus	h
giant	i
golden_dragon	j
giant_rat	k
gorilla	l
goblin	m
orc	o
zombie	p
faun	q
ogre	r
skeleton	s
harpy	t
spectre	u
ghost	v
hydra	w
manticore	x
wraith	z
vampire	W
green_dragon	X
red_dragon	Z
\.

/*
== stuff
*/
create view spell_book_table as
  select spell_category, spell_name, count,
    spell_cast_chance(spell_name) as chance,
    alignment, format_alignment(alignment) as alignment_string,
    key, sprite, section_order, alignment_order, base_chance,
    count_icons(count::int), align_icons(alignment::int),
    spell_colour(spell_name, count::int) as colour
  from spells_with_order
  natural inner join current_wizard_spell_counts
  natural inner join spell_keys
  natural inner join spell_sprites
  cross join spell_book_show_all_table
  where not (spell_book_show_all = false and count = 0);

create view spell_details as
  select * from spells_mr
  full outer join spell_sprites using (spell_name)
  full outer join (
    select /*spell_category,*/ spell_name, count, chance,
    /*alignment,*/ alignment_string,
    key, /*sprite,*/ section_order, alignment_order, /*base_chance,*/
    count_icons, align_icons,
    colour
    from spell_book_table
    ) as balls using (spell_name);

create view current_wizard_selected_spell_details as
  select spell_name, spell_category, sprite, base_chance, description,
    numb, range, count, chance, alignment_string
  from spell_details
  natural inner join wizard_spell_choices
  inner join current_wizard_table on (wizard_name = current_wizard);
