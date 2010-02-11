/*
== pieces
=== piece natural keys
Keys consist of three parts:
type
allegiance
number

- if piece is a wizard -> type is "wizard", allegiance is wizard name,
  number is 0 for all wizards?

- else if dead -> dead aren't owned, allegiance is "dead". (use
  dead-[monster type]-[n] where n is integer, incremented for each
  corpse type. e.g. you can have dead-giant-0, dead-giant-1 and
  dead-eagle-0. Corpses which existed in current game but no longer do
  leave a gap in the numbering.)

- else -> [owning wizard's name]-[type]-[m] where wizard[n] is the
  owning wizard, m is integer incremented per wizard[n]-type, e.g. you
  can have wizard1-goblin-0, wizard2-goblin-0, wizard1-ogre-0,
  etc. (Assuming wizard names are wizard1, etc.). Pieces which existed
  in current game but no longer do leave a gap in the numbering.

To do this will need to roll own sequence type because there will be
many many sequences? Like to create one table to hold all the
sequences: pieces_sequences (prefix text, current_number integer),
where prefix text is the prefix given above. This might be better as
(allegiance text, type text, current_number integer). Locking: intend
to use a server schema database wide lock every update. fastest
solution may be to use row level locking, probably write a function
like sequences to lock row, increment, get number, unlock and return
number.

For simplicity, just use same serial for all pieces for now.

=== relvars

*/
select module('Chaos.Server.Pieces');

--pieces are either a member of a particular wizard's army or
-- they are dead, in which case they are not a member of
-- any wizard's army
create view allegiances as
   select wizard_name as allegiance from wizards
     where expired = false
   union select 'dead' as allegiance;

/*

TODO: maybe make pieces_mr a table called pieces with just the piece
key and position, and then use a view for an analog to pieces with
the stats. Create this view from the prototype stats, the pieces
table, and have table(s) containing things that can affect the stats
and the view selects from the the piece_prototypes and these tables.

I think all the ways stats can change from the prototypes are listed
here:
monster raised from the dead
wizard with upgrade

err... that's it.

*/

create table pieces (
    ptype text references piece_prototypes_mr,
    allegiance text, -- references allegiances, needs constraint support
    tag int,
--Piece is on the board at grid position 'x', 'y'.
    x int,
    y int,
    primary key (ptype,allegiance,tag)
);
select set_relvar_type('pieces', 'data');

--piece must be on the board, not outside it
select create_assertion('piece_coordinates_valid',
  ' not exists(select 1 from pieces
  cross join board_size
  where x >= width or y >= height)');
--select add_foreign_key('pieces', 'allegiance', 'allegiances');
--temporary constraint while 'fks' to non base relvars are buggy
select create_assertion('dead_wizard_army_empty',
  $$ not exists(select 1 from pieces
    inner join wizards
    on (allegiance = wizard_name)
    where expired = true)$$);

create type piece_key as (
    ptype text,
    allegiance text,
    tag int
);

--create function piece_key_equals(piece_key, piece_key) returns boolean as $$
--  select $1.ptype = $2.ptype and
--         $1.allegiance = $2.allegiance and
--         $1.tag = $2.tag;
--$$ language sql stable;

-- create operator = (
--     leftarg = piece_key,
--     rightarg = piece_key,
--     procedure = piece_key_equals,
--     commutator = =
-- );

create type pos as (
  x int,
  y int
);

-- create function pos_equals(pos, pos) returns boolean as $$
--   select $1.x = $2.x and
--          $1.y = $2.y;
-- $$ language sql stable;

-- create operator = (
--     leftarg = pos,
--     rightarg = pos,
--     procedure = pos_equals,
--     commutator = =
-- );


/*

add two auxiliary tables to track imaginary monsters and raised
monsters who are now undead

*/
create table imaginary_pieces (
    ptype text,
    allegiance text,
    tag int,
    unique (ptype,allegiance,tag),
    foreign key (ptype,allegiance,tag) references pieces
);
select set_relvar_type('imaginary_pieces', 'data');
/*
select add_foreign_key('imaginary_pieces', 'ptype',
       'monster_prototypes'); -- fk to view
*/
create table crimes_against_nature(
    ptype text,
    allegiance text,
    tag int,
    unique (ptype,allegiance,tag),
    foreign key (ptype,allegiance,tag) references pieces
);
select set_relvar_type('crimes_against_nature', 'data');
/*
select add_foreign_key('crimes_against_nature', 'ptype',
       'monster_prototypes'); -- fk to view
*/
create view wizard_upgrade_stats as
select pp.ptype,
       allegiance,
       tag,
       x,
       y,
       false as imaginary,
       magic_wings as flying,
       case when magic_wings then 6
            when shadow_form then 3
            else speed
       end as speed,
       case when shadow_form then agility + 2
            else agility
       end as agility,
       undead,
       ridable,
       case when magic_bow then 'projectile'
            else null
       end as ranged_weapon_type,
       case when magic_bow then 6
            else null
       end as range,
       case when magic_bow then 6
            else null
       end as ranged_attack_strength,
       case when magic_sword then attack_strength + 4
            when magic_knife then attack_strength + 2
            else attack_strength
       end as attack_strength,
       case when magic_armour and shadow_form then physical_defense + 6
            when magic_shield and shadow_form then physical_defense + 4
            when magic_armour then physical_defense + 4
            when magic_shield then physical_defense + 2
            when shadow_form then physical_defense + 2
            else physical_defense
       end as physical_defense,
       magic_defense
  from pieces p
  inner join wizards
    on allegiance = wizard_name
  inner join piece_prototypes_mr pp
    on pp.ptype='wizard'
  where p.ptype = 'wizard';

-- create view imaginary_or_not_pieces as
-- select ptype,allegiance,tag,x,y,coalesce(imaginary,false) as imaginary
--   from pieces
--   natural left outer join (select *,true as imaginary
--                            from imaginary_pieces) as a;

create view pieces_mr as
select ptype,
       allegiance,
       tag,
       x,
       y,
       coalesce(imaginary,case when not ridable is null then false
                               else null end) as imaginary,
       flying,
       speed,
       agility,
       coalesce(raised, undead) as undead,
       ridable,
       ranged_weapon_type,
       range,
       ranged_attack_strength,
       attack_strength,
       physical_defense,
       magic_defense
  from pieces
  natural inner join piece_prototypes_mr
  natural left outer join (select *,true as imaginary
                           from imaginary_pieces) as a
  natural left outer join (select *,true as raised
                           from crimes_against_nature) as b
  where ptype <> 'wizard'
union
select * from wizard_upgrade_stats;

create view creature_pieces as
  select ptype,allegiance,tag,x,y,
    flying,speed,agility
    from pieces_mr
  where flying is not null
    and speed is not null
    and agility is not null;

create view monster_pieces as
  select ptype,allegiance,tag,x,y,
    flying,speed,agility,
    undead,ridable,imaginary
  from pieces_mr
  where flying is not null
    and speed is not null
    and agility is not null
    and undead is not null
    and ridable is not null;

create view dead_monster_pieces as
  select * from monster_pieces
    where allegiance = 'dead';

create view attacking_pieces as
  select ptype,allegiance,tag,x,y,
    attack_strength
  from pieces_mr
  where attack_strength is not null
    and allegiance <> 'dead';

create view ranged_weapon_pieces as
  select ptype,allegiance,tag,x,y,
    ranged_weapon_type,range,ranged_attack_strength
  from pieces_mr
  where ranged_weapon_type is not null
    and range is not null
    and ranged_attack_strength is not null;

create view attackable_pieces as
  select ptype,allegiance,tag,x,y,physical_defense
  from pieces_mr
  where physical_defense is not null;

create view magic_attackable_pieces as
  select ptype,allegiance,tag,x,y,magic_defense
  from pieces_mr
  where magic_defense is not null;


/*

Rules for multiple pieces on one square: only one piece of each
type may occupy a square in particular you can't have two dead bodies
on one square. These are the traditional chaos rules which may change
for other rulesets.

When multiple pieces occupy one square, one is considered to be 'on
top'. This piece
* is the one piece displayed in the UI currently
* the piece upon which any spell cast on that square hits
* the piece which is attacked when another piece attacks or range
  attacks that square

1 item
any
2 items
creature, stiff : creature on top
wizard, mountable monster: mountable monster on top
wizard, box : box on top
stiff, gooey blob : blob on top
monster, gooey blob : blob on top
3 items
wizard, stiff, mountable monster : mountable on top
stiff, monster, blob : blob on top

*/
