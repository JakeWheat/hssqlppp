/*

Copyright 2009 Jake Wheat

= Overview

windows manager widget
extra stuff - colours, sprites, wizard display info
              (additional info for each wizard)
board widget
info widget
spell book widget
new game widget
planned widgets

actions
key config
action valid view
turn phase
cursor/go
new game

see chaos.lhs for then main ui docs
*/
select module('Chaos.Client.WindowManagement');

/*
================================================================================

= windows manager
Store window positions, size, maximised/minimised,
  open/close so this is restored when
  you restart the program or if it crashes
*/
/*
windows relvar
*/
create domain window_state as text
       check (value in ('maximised', 'minimised',
                        'hidden', 'normal'));
/*

Window with name name: top left corner of window is at position px, py
and the size of the window is sx, sy.  It is in state 'state'.

*/
create table windows (
  window_name text unique,
  px integer, --position
  py integer,
  sx integer, --size
  sy integer,
  state window_state
); --assert there is a row for every widget type.
select set_relvar_type('windows', 'data');

/*

function to reset the windows to default, can be used if the windows
get too messed up or e.g. the window manager row is deleted

*/
create function action_reset_windows() returns void as $$
begin
  delete from windows;
  insert into windows (window_name, px, py, sx, sy, state) values
    --('window_manager', 0,28, 92,320, 'normal'),
    ('info', 0,371, 579,213, 'normal'),
    ('spell_book', 587,28, 268,556, 'normal'),
    ('new_game', 514, 27, 500, 500, 'hidden'),
    ('board', 99,28, 480,320, 'normal'),
    ('action_history', 843,28, 429,556, 'normal');
end;
$$ language plpgsql volatile;

create function action_hide_window(vname text) returns void as $$
begin
  if vname = 'window_manager' then
    raise exception 'cannot hide window manager';
  end if;
  update windows set state='hidden' where window_name = vname;
end;
$$ language plpgsql volatile;

/*

When another window is closed that window is hidden.  when the window
manager is closed, the app exits

TODO: add window zoom and scroll positions to relvar
*/

create function action_refresh_widgets() returns void as $$
begin
--doesn't do owt at the moment, all in the haskell code,
-- just has this stub here to avoid special casing it in the
--haskell code
end;
$$ language plpgsql volatile;

/*
================================================================================

= extras
== colours
*/
create table colours (
       name text primary key,
       red int,
       green int,
       blue int
);
select set_relvar_type('colours', 'readonly');

copy colours (name,red,green,blue) from stdin;
grid	32767	32767	32767
background	0	0	32767
black	0	0	0
blue	0	0	65535
green	0	65535	0
red	65535	0	0
pink	65535	49407	49407
purple	65535	0	65535
cyan	0	65535	65535
yellow	65535	65535	0
orange	65535	41215	0
grey	32767	32767	32767
white	65535	65535	65535
\.

