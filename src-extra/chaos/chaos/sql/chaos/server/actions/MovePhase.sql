select module('Chaos.Server.Actions.MovePhase');

/*
== move
Individual Piece move notes
A piece may be 'selected' iff it can move or attack or has a ranged attack.

Once a piece is selected it must first move, then attack, then ranged
attack (skipping bits which don't apply).

The piece can forfeit any part of this (and can choose to move only
part of it's move if walking).

The parts must be in this strict order.

If a piece is unselected before it has moved or done anything then it
remains able to move this turn otherwise its move is over for this
turn.

=== selection and subphase
*/

create function select_piece(pk piece_key) returns void as $$
declare
  nextp text;
  p pos;
begin
  nextp:= piece_next_subphase('start', false, 'none', pk);
  if nextp = 'end' then
    --nothing to do
    insert into pieces_moved (ptype,allegiance,tag)
     values (pk.ptype, pk.allegiance,pk.tag);
    /*if not exists(select 1 from pieces_to_move) then
      perform action_next_phase();
    end if;*/
    return;
  end if;
  insert into selected_piece (ptype, allegiance, tag, move_phase, engaged) values
    (pk.ptype, pk.allegiance, pk.tag, nextp, false);

  if nextp = 'motion' and
       exists(select 1 from selected_piece
              natural inner join creature_pieces
              where not flying) then
    update remaining_walk_hack_table
      set remaining_walk_hack = true;
    insert into remaining_walk_table
      select speed from creature_pieces
        natural inner join selected_piece;
    update remaining_walk_hack_table
      set remaining_walk_hack = false;
    perform check_engaged();
  end if;
end;
$$ language plpgsql volatile;


--short cut for interface
--this fails silently if the action is not valid
-- which is totally inconsistent - TODO: fix
create function action_select_piece_at_position(vx int, vy int)
  returns void as $$
declare
  r piece_key;
begin
  perform check_can_run_action('select_piece_at_position', vx,vy);
  select into r ptype,allegiance, tag from selectable_pieces
         where (x,y) = (vx,vy);
  perform select_piece(r);
end;
$$ language plpgsql volatile;

create function action_unselect_piece() returns void as $$
begin
  perform check_can_run_action('unselect_piece');
  --remove piece from pieces to move
  insert into pieces_moved (ptype, allegiance, tag)
    (select ptype, allegiance, tag from selected_piece);
  /*delete from pieces_to_move where (ptype, allegiance, tag) =
    (select ptype, allegiance, tag from selected_piece);*/
  --empty selected piece, squares left_to_walk
  update remaining_walk_hack_table
    set remaining_walk_hack = true;
  delete from selected_piece;
  delete from remaining_walk_table;
  update remaining_walk_hack_table
    set remaining_walk_hack = false;
  --if there are no more pieces that can be selected then move to next
  --phase automatically, todo: take into account monsters in blob
  /*if not exists(select 1 from pieces_to_move) then
    perform action_next_phase();
  end if;*/

  --insert history
end;
$$ language plpgsql volatile;

create function action_cancel() returns void as $$
begin
  perform check_can_run_action('cancel');
  perform do_next_move_subphase(true,'none');
end;
$$ language plpgsql volatile;
/*
==== internals
*/
create function do_next_move_subphase(skip_attack boolean, phase_done text)
    returns void as $$
declare
  r record;
  nextp text;
begin
  if not exists (select 1 from selected_piece) then
    return;
  end if;
  select into r * from selected_piece
    natural inner join pieces;
  nextp := piece_next_subphase((select move_phase from selected_piece),
             skip_attack, phase_done, (r.ptype, r.allegiance, r.tag)::piece_key);
  if r.move_phase = 'motion' then
    update remaining_walk_hack_table
      set remaining_walk_hack = true;
      delete from remaining_walk_table;
    update remaining_walk_hack_table
      set remaining_walk_hack = false;
  end if;

  if nextp = 'end' then
    perform action_unselect_piece();
  else
    update selected_piece set move_phase = nextp;
  end if;
end;
$$ language plpgsql volatile;


/*
=== movement
*/
create function action_walk(px int, py int) returns void as $$
declare
  p pos;
begin
  perform check_can_run_action('walk', px, py);
  select into p x,y from pieces natural inner join selected_piece;
  perform selected_piece_move_to(px, py);
  perform add_history_walked(p.x,p.y);
  if get_remaining_walk() = 0 then
    perform do_next_move_subphase(false, 'motion');
  end if;
end;
$$ language plpgsql volatile;

create function action_fly(px int, py int) returns void as $$
declare
  p pos;
begin
  perform check_can_run_action('fly', px, py);
  select into p x,y from pieces natural inner join selected_piece;
  perform selected_piece_move_to(px, py);
  perform add_history_fly(p.x,p.y);
  perform do_next_move_subphase(false, 'motion');
end;
$$ language plpgsql volatile;

/*
=== attacking
*/
create function action_attack(px int, py int) returns void as $$
declare
  ap piece_key;
  r piece_key;
  att int;
  def int;
begin
  perform check_can_run_action('attack', px, py);

  --if the attacker is a wizard with shadow form, they lose the shadow
  --form when they attack

  att := (select attack_strength
         from attacking_pieces
         natural inner join selected_piece);
  def := (select physical_defense
         from attackable_pieces
         natural inner join pieces_on_top
         where (x,y) = (px,py));

  --check for shadow form

  select into ap ptype, allegiance,tag
    from selected_piece;

  if ap.ptype = 'wizard' and
     exists(select 1 from wizards
            where wizard_name = ap.allegiance
            and shadow_form) then
    update wizards
      set shadow_form = false
      where wizard_name = ap.allegiance;
  end if;

  select into r ptype, allegiance,tag
    from pieces_on_top
    where (x,y) = (px,py);

  perform add_history_attack(r);


  if not check_random_success('attack', greatest((att - def) * 10 + 50, 10)) then
    --failure
    perform add_history_shrugged_off(r);
    perform do_next_move_subphase(true, 'attack');
    return;
  end if;

  perform add_history_chinned(r);
  perform kill_piece(r);

  --move to the square if mover and square empty
  if exists(select 1 from creature_prototypes
              natural inner join selected_piece)
     and exists(select 1 from selected_piece_move_squares
                where (x,y) = (px,py)) then
    perform selected_piece_move_to(px, py);
  end if;
  perform do_next_move_subphase(true, 'attack');
end;
$$ language plpgsql volatile;

create function action_ranged_attack(px int, py int)
    returns void as $$
declare
  r piece_key;
  att int;
  def int;
begin
  perform check_can_run_action('ranged_attack', px, py);

  att := (select ranged_attack_strength
         from ranged_weapon_pieces
         natural inner join selected_piece);
  def := (select physical_defense
         from attackable_pieces
         natural inner join pieces_on_top
         where (x,y) = (px, py));

  select into r ptype, allegiance,tag
    from pieces_on_top
    where (x,y) = (px, py);

  perform add_history_ranged_attack(r);

  if not check_random_success('ranged_attack', greatest((att - def) * 10 + 50, 10)) then
    --failure
    perform add_history_shrugged_off(r);
    perform do_next_move_subphase(false, 'ranged_attack');
    return;
  end if;

  perform add_history_chinned(r);
  perform kill_piece(r);
  perform do_next_move_subphase(false, 'ranged_attack');
end;
$$ language plpgsql volatile;

/*
=== internals

subphase progression

skip attack is used to tell this routine to skip the attack sub
phase. this is if either the motion subphase was just cancelled, or if
the piece attacked when it was in the motion subphase

*/

create function piece_next_subphase(
  current_subphase text, skip_attack boolean, just_done text, pk piece_key)
  returns text as $$
declare
  r record;
begin
  select into r x,y from pieces
    where (ptype,allegiance,tag)::piece_key=pk;
  if current_subphase = 'start'
     and just_done='none'
     and exists(select 1 from creature_pieces
                where (ptype,allegiance,tag)::piece_key = pk) then
    return 'motion';
  elseif current_subphase not in ('attack','ranged_attack')
         and not skip_attack
         and just_done not in ('attack', 'ranged_attack')
         and exists(select 1 from attacking_pieces
                where (ptype,allegiance,tag)::piece_key=pk)
         and exists(select 1 from attackable_pieces ap
           natural inner join one_square_away((r.x,r.y))
           where allegiance not in (pk.allegiance, 'dead')) then
    return 'attack';
  elseif current_subphase not in ('ranged_attack')
    and just_done not in ('ranged_attack')
    and exists(select 1 from ranged_weapon_pieces
                where (ptype,allegiance,tag)::piece_key = pk) then
    return 'ranged_attack';
  else
    return 'end';
  end if;
end;
$$ language plpgsql volatile;

create function add_chinned_history(px int, py int) returns void as $$
declare
  r piece_key;
begin
    select into r ptype, allegiance, tag
      from pieces_on_top where (x,y) = (px,py);
    perform add_history_chinned(r);
end;
$$ language plpgsql volatile;

create function add_history_shrugged_off(px int, py int) returns void as $$
declare
  r piece_key;
begin
    select into r ptype, allegiance, tag
      from pieces_on_top where (x,y) = (px,py);
    perform add_history_shrugged_off(r);
end;
$$ language plpgsql volatile;

create function selected_piece_move_to(px int, py int) returns void as $$
begin
  -- this is used to move a piece when it walks/flies and as part of a
  -- successful attack to keep the logic for moving a wizard piece
  -- along with his mount in one place
  if
     --this is a ridable monster
     exists(select 1 from selected_piece
       natural inner join monster_pieces
       where ridable) and
     --there is also a wizard on this square
     exists(select 1
      from (select x,y from pieces
            natural inner join selected_piece) as a
      natural inner join pieces
      where ptype='wizard') then
     -- move the wizard also
    update pieces
      set x = px,
          y = py
      where (ptype,allegiance,tag) =
        (select ptype, allegiance, tag
         from (select x,y from pieces
               natural inner join selected_piece) as a
         natural inner join pieces
         where ptype='wizard');
  end if;

  update pieces
    set x = px,
        y = py
    where (ptype,allegiance,tag) =
      (select ptype,allegiance,tag from selected_piece);

  --todo: if diagonal, reduce by 1.5
  -- the way chaos worked is that if you had 0.5 moves left,
  -- you could walk a whole square, and if you had 0.5 or 1 move left
  -- you could walk a diagonal
  if exists(select 1 from creature_pieces
            natural inner join selected_piece
            where not flying) and
     (select move_phase from selected_piece)='motion' then
    update remaining_walk_table
    set remaining_walk = remaining_walk - 1;
    perform check_engaged();
  end if;

end;
$$ language plpgsql volatile;

create view selected_piece_adjacent_attacking_squares as
  select * from one_square_away((select (x,y)::pos from selected_piece
                                 natural inner join pieces))
  intersect
  select x,y from pieces_on_top
  natural inner join pieces_mr
  where attack_strength is not null
    and allegiance not in (get_current_wizard(), 'dead');

create function check_engaged() returns void as $$
declare
  ag int;
begin
  if exists(select 1 from selected_piece_adjacent_attacking_squares) then
    select into ag agility from pieces_mr
    natural inner join selected_piece;
    if check_random_success('break_engaged', ag * 10) then
      update selected_piece set engaged = false;
    else
      update selected_piece set engaged = true;
    end if;
  else
    update selected_piece set engaged = false;
  end if;
end;
$$ language plpgsql volatile;
