/*
Pieces
======

piece natural keys
------------------

We use a three part natural key for pieces, consisting of the ptype
from the piece prototypes table, the allegiance i.e. which wizard they
belong to, and a number to distinguish between pieces with the same
first two values, e.g. if a wizard casts two goblins. All pieces are
owned by a wizard, with the exception of corpses, so we create an
allegiance view which is all the wizard names plus dead.

The numbers, called tags in the code, start at 0, and a piece is
assigned the next number to make their key unique. We don't renumber,
so there can be gaps when creatures die. The spell subversion can
cause a piece to change allegiance, they are always be assigned a new
tag when this happens. When a corpse is created, it is assigned a
fresh tag - its allegiance is changed to dead and its ptype stays the
same.

relvars
-------

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

I think all the ways stats can change from the prototypes are listed
here:
monster raised from the dead
wizard with upgrade

err... that's it.

*/

create table pieces (
    ptype text references piece_prototypes_mr,
    allegiance text, -- references allegiances where wizard is not dead
                     -- needs extended constraint support to 'fk' to view
    tag int,
--Piece is on the board at grid position 'x', 'y'.
    x int,
    y int,
    primary key (ptype,allegiance,tag)
);
select set_relvar_type('pieces', 'data');

-- piece must be on the board
select create_assertion('piece_coordinates_valid',
  ' not exists(select 1 from pieces
  cross join board_size
  where x >= width or y >= height)');

--temporary constraint while 'fks' to non base relvars are buggy
-- this won't be needed once the extension is done and the reference is
-- added to the pieces table above
select create_assertion('dead_wizard_army_empty',
  $$ not exists(select 1 from pieces
    inner join wizards
    on (allegiance = wizard_name)
    where expired = true)$$);

-- these is used in functions for
-- parameters and local variables.
create type piece_key as (
    ptype text,
    allegiance text,
    tag int
);

create type pos as (
  x int,
  y int
);

/*

add two auxiliary tables to track imaginary monsters and raised
monsters who are now undead
(only monster pieces can be imaginary or raised).

*/
create table imaginary_pieces (
    ptype text, -- reference monster_prototypes (fk to view)
    allegiance text,
    tag int,
    unique (ptype,allegiance,tag),
    foreign key (ptype,allegiance,tag) references pieces
);
select set_relvar_type('imaginary_pieces', 'data');

create table crimes_against_nature(
    ptype text,  -- reference monster_prototypes (fk to view)
    allegiance text,
    tag int,
    unique (ptype,allegiance,tag),
    foreign key (ptype,allegiance,tag) references pieces
);
select set_relvar_type('crimes_against_nature', 'data');

/*

This is the auxiliary view which takes the base wizard stats, and
adjusts them according to the upgrade spells that the wizard has
active.

We should try to apply the same null combo constraints that the
piece_prototypes table has.

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

/*

This is the main piece information table, which mirrors the
information in the piece_prototypes table for each actual piece, with
modifications where needed.

*/
create view pieces_mr as

-- first we do all the non wizard pieces, we only need to adjust the
-- piece_prototype data by adding an imaginary attribute, and setting
-- the undead attribute also for raised monsters.

select ptype,
       allegiance,
       tag,
       x,
       y,
       coalesce(imaginary
                -- this makes sure all the monster pieces have their
                -- imaginary set to false if they aren't imaginary
                -- and all non monster pieces have null for imaginary
               ,case when not ridable is null
                     then false
                     else null
                end) as imaginary,
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
-- then we add in the wizards, which need the more involved
-- calculations in the wizard_upgrade_stats table.
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

Rules for multiple pieces on one square

When multiple pieces occupy one square, one is considered to be 'on
top'. This piece
* is the one piece displayed in the UI currently
* the piece upon which any spell cast on that square hits
* the piece which is attacked when another piece attacks or range
  attacks that square

The options are:

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

Can these be made into actual constraints - doesn't seem to difficult
now the extension system gives us some help with the triggers and
stuff. could use some ctes and it might come out alright? or use a
function to procedurally do part of it? want something that can be
understood.

*/
