/*
== action validity

*/
select module('Chaos.Server.Actions.SquaresValue');

/*
=== pieces on top

The topmost piece on each square is the one you interact with most of
the time, e.g. when selecting, attacking, etc.

The exception to this rule is when you select a wizard that is in a
magic tree or castle or mounted on a monster.

The pieces_on_top view also determines what sprite is shown in a
square in the ui

*/

create view pieces_with_priorities as
  select ptype,allegiance,tag,x,y,
    case
      when allegiance='dead' then 3
      when ptype='wizard' then 2
      when ptype in (select ptype from monster_prototypes) then 1
      else 0
    end as sp
    from pieces;

--restrict this view taking only the top piece from each square to get
--the final result

create view pieces_on_top as
  select x,y,ptype,allegiance,tag,sp from
    (select row_number() over(partition by (x,y) order by sp) as rn,
            x, y, ptype, allegiance, tag, sp
      from pieces_with_priorities) as pwp where rn = 1;

--create a full view to help with updates

-- question: why does pieces_view natural inner join pieces_on_top
-- return too many rows?

create view pieces_on_top_view as
  select p.* from pieces_mr p
    inner join pieces_on_top
    using (ptype,allegiance,tag);

/*
=== selectable squares and pieces

We can't use the pieces on top for the selection because of
the exceptions re castles, magic wood and mounted wizards,
so create a similar view so we can determine the piece that
gets selected by square, this part is just the pieces on top
combined with all the wizards even if they are not on top.

This is finished off using the pieces_to_move relvar.

*/
create view moving_pieces as
  select ptype, allegiance, tag,x,y from pieces_mr
    where speed is not null
      or attack_strength is not null
      or ranged_attack_strength is not null;

create view selectable_pieces_with_priorities as
  select ptype,allegiance,tag,x,y,
    case
      when ptype='wizard' then 0
      else 1
    end as sp
    from moving_pieces
    where (x,y) not in(select x,y from pieces
                     where ptype = 'gooey_blob');

/*

=== internals
*/

create function distance(int, int, int, int) returns float(24) as $$
  select (point($1, $2) <-> point($3, $4))::float(24) as result;
$$ language sql immutable;

create view board_ranges as
--iterate x,y over each square on board
--  iterate d over 0 to 20
--    iterate tx,ty over each square on board
--      include x,y,d, tx, ty iff d(x,y,tx,ty) < d
--so: we include squares <= to the range, not just squares at that
--range

  select * from generate_series(0, 14) as x
                cross join generate_series(0, 9) as y
                cross join generate_series(1, 20) as range
                cross join generate_series(0, 14) as tx
                cross join generate_series(0, 9) as ty
  where
    --slightly hacky, we never need the centre square to be included so
    --exclude it here even though it's not quite mathematically correct
    (x,y) != (tx,ty) and
    distance(x,y,tx,ty) - 0.5 <= range; --round to closest int
