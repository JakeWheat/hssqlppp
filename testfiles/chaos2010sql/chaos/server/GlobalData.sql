/*
================================================================================

= game data

Players should not be able to see each others spells before the cast
phase. This is difficult when they are all on the same computer, but
should be easy when they are not.

Imaginary attribute should only be visible to owning player, same
notes as previous.

== global data
*/
select module('Chaos.Server.GlobalData');
/*
=== board size
The playing area is 'width' by 'height' squares.
*/
create table board_size (
  width int,
  height int,
  unique (width, height)
);
select set_relvar_type('board_size', 'data');
select restrict_cardinality('board_size', 1);


--update operator out param: board_size
create function init_board_size() returns void as $$
begin
  -- default board size
  insert into board_size (width, height) values (15, 10);
end;
$$ language plpgsql volatile;

/*
=== law/ chaos rating

The world has a law/ chaos rating which can be chaos-N, neutral or
law-N. It starts neutral. When the world is chaos, then chaos spells
become easier to cast, and law spells harder, and vice versa. It
becomes more chaos when chaos spells are cast, and more law when law
spells are cast.

*/

create domain alignment as text check (value in ('law', 'neutral', 'chaos'));

--if world alignment = 0, world is neutral, if -ve world is chaos by that amount
--if +ve world is law by that amount
select create_var('world_alignment', 'int');
select set_relvar_type('world_alignment_table', 'data');

create function init_world_alignment() returns void as $$
begin
  insert into world_alignment_table values (0);
end;
$$ language plpgsql volatile;
