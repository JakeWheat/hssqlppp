/*

================================================================================

= ai

For each stage we compile a list of possible actions using the
valid_action views. These are then filtered to remove actions we don't
want to run. At some places, the possible action list is reduced by
keeping only the actions which are deemed vital (e.g. the wizard needs
to run away, or a monster has a chance to attact a wizard). The
remaining actions are possibly weighted and one is chosen at random.

Choose spells by weighting them according to casting chance, some
spells are never cast, and some will be further weighted by the board
layout.

When moving army, the general plan is to move the monsters closest to
an enemy first.

choose spell
cast spell
move pieces

option 1: upgrade
weight by probability
don't cast one already have or a weaker one in same category
armour - shield
knife - sword
shadow - wings

decree et al: use: wizard with lots of bad guys, being threatened
magic wood: if range of spells is a bit shit
castle, wall, blob, fire, shadowwood - random
disbelieve: cast when threatened by a hard creature, small chance
otherwise
subversion - hard creature nearby, when threatened
raise dead - when can
monsters: weight by chances, decide on imag weighted by chances

assess: defensive: wizard under threat
aggressive: choose a target to send everyone against

casting:
raise: hardest corpse in range
decree: if threatened target monster or wizard, else target hardest
wizard/monster on screen
castle -next to, away from danger
disbelieve - closest monster
subvert - closest monster (or if tougher one next to closest monster?)
monster - toward nearest threat
wall - just randomly put about
blob - want to grow safely, unless under threat then use aggresively
fire - use aggressively
shadow wood: use magic wood layout, bias in directions that have
moving enemy pieces

moving:
if defensive move pieces starting with close

The system for running the ai is to make an action available when the
current wizard is an ai to continue the ai's turn. This will do one
action, and move to the next phase if the ai has completed it's
action. This api allows the client to control how and at what speed
the ai's turns are run, we use this to run one ai action every half
second so you can see what the ai is doing by watching the board
change.

TODO:
--don't attack friendlies
--don't cast, attack corpses
--don't choose spells that can't work
cast spells in sensible place
weight choice by chance
sometimes cast imag when unlikely
disbelieve logic and tracking
move phase:
  keep wizards out of danger
  always move into castles,wood
  move towards wood if near
  stay in castles, wood
send monsters towards closest enemy
always attack if can
choose targets: favour wizards and hardest that likely to kill

*/

select module('Chaos.Server.AI');

/*

== main ai action

*/

create function action_ai_continue() returns void as $$
begin
  perform check_can_run_action('ai_continue');
    if get_turn_phase() = 'choose' then
      perform ai_choose_spell();
      perform action_next_phase();
    elseif get_turn_phase() = 'cast' then
      perform ai_cast_spell();
    elseif get_turn_phase() = 'move' then
      perform ai_move_pieces();
    end if;
end;
$$ language plpgsql volatile;

/*

== spell choice

First, eliminate all the useless target spells - those that have no
target and those that can only be cast on a friendly or corpse.

*/

create view current_wizard_target_spells as
  select spell_name,range from spell_books
    inner join current_wizard_table
      on current_wizard = wizard_name
    natural inner join target_spells;

create view current_wizard_square as
  select x,y from pieces
  inner join current_wizard_table
    on allegiance =current_wizard
  where ptype= 'wizard';

/*

take all the target spells and create a list of spell names a squares
that they can be cast on using the range and valid square types of
each spell

*/

create view castable_target_spells as
  select spell_name,svs.x,svs.y
    from current_wizard_target_spells cwts
    natural inner join spell_valid_squares svs
    natural inner join target_spells svst
    inner join board_ranges br
      on (br.x,br.y) = (select x,y from current_wizard_square)
        and br.range = cwts.range
        and (br.tx, br.ty) = (svs.x, svs.y);

/*

eliminate the rows which have only corpses or friendlies on top

*/

create view ai_useful_spells as
  select spell_name from spell_books
    inner join current_wizard_table
      on wizard_name = current_wizard
    natural inner join activate_spells
  union
    select spell_name from castable_target_spells
    where (x,y) not in (select x,y from corpse_only_squares
                      union
                      select x,y from pieces_on_top
                      inner join current_wizard_table
                      on current_wizard = allegiance);

create function ai_choose_spell() returns void as $$
declare
  vspell_name text;
begin
  select into vspell_name spell_name
    from ai_useful_spells
    order by random() limit 1;
  if vspell_name is null then
    --skip choosing one
    return;
  else
    perform action_choose_spell(vspell_name);
  end if;
end;
$$ language plpgsql volatile;

/*

== spell casting

first filter out all the targets we don't want to cast on:

*/

create view ai_filtered_target_spells as
  select * from valid_target_actions
  where action='cast_target_spell'
  and (x,y) not in
    (select x,y from pieces_on_top
    where allegiance in (get_current_wizard(), 'dead'));

/*
cast spells in a sensible place:
dark power: don't choose a wizard with no creations
            pick enemy monsters that are close or wizards with lots of shit
lightning, magic bolt: enemy wizard then closest monster
raise dead: choose hardest corpse
subversion: choose hardest enemy
shadow wood: use magic tree layout
fire: next to wizard or monster if can, else towards closest enemy
blob: towards closest enemy in some space
castle: next to wizard away from danger
monster: towards closest enemy
*/

create function ai_cast_spell() returns void as $$
declare
  p pos;
begin
  if exists(select 1 from valid_activate_actions
            where action = 'cast_activate_spell') then
     perform action_cast_activate_spell();
  elseif exists(select 1 from ai_filtered_target_spells) then
     select into p x,y from ai_filtered_target_spells
         order by random() limit 1;
     perform action_cast_target_spell(p.x, p.y);
  else
    perform action_next_phase();
  end if;
end;
$$ language plpgsql volatile;

/*

== move phase

*/

create function ai_move_pieces() returns void as $$
declare
  p pos;
begin
  --if no piece selected and none selectable, go to next phase
  if not exists(select 1 from selected_piece)
     and not exists(select 1 from valid_target_actions
            where action = 'select_piece_at_position') then
     perform action_next_phase();
     return;
  end if;
  --if no piece selected try to select one
  if not exists(select 1 from selected_piece)
     and exists(select 1 from valid_target_actions
            where action = 'select_piece_at_position') then
     select into p x,y from valid_target_actions
       where action = 'select_piece_at_position'
       order by random() limit 1;
     perform action_select_piece_at_position(p.x, p.y);
     --check if it has been immediately unselected
     if not exists(select 1 from selected_piece) then
       return;
     end if;
  end if;
  perform ai_move_selected_piece();
end;
$$ language plpgsql volatile;

create view ai_selected_piece_actions as
select a.x,a.y,action
  from valid_target_actions a
  left outer join pieces_on_top p
    using (x,y)
  where action in('walk', 'fly')
  or ((action in('attack', 'ranged_attack')
     and allegiance not in (get_current_wizard(), 'dead')));


/*
rules:
send monsters towards enemy
always attack if can
choose targets: wizards if can, then hardest creature
*/

create view prefered_targets as
select x,y,action,
  case when ptype = 'wizard' then -500
                else 20 - physical_defense
  end as preference
from valid_target_actions
natural inner join pieces_mr
where action in('attack','ranged_attack');

create view closest_enemy_to_selected_piece as
  select a.x,a.y
  from selected_piece_attackable_squares a
  cross join selected_piece s
    inner join pieces s1
    using(ptype,allegiance,tag)
  order by distance(s1.x,s1.y,a.x,a.y) limit 1;

create view select_best_move as
  select a.action,a.x,a.y from ai_selected_piece_actions a
    cross join closest_enemy_to_selected_piece e
    where action in('walk', 'fly')
    order by distance(a.x,a.y,e.x,e.y) limit 1;

create function ai_move_selected_piece() returns void as $$
declare
  r record;
begin
  if exists(select 1 from ai_selected_piece_actions
            where action = 'attack'
            or (action = 'ranged_attack'
                and (select move_phase='ranged_attack'
                from selected_piece))) then
    select into r x,y,action from prefered_targets
      order by preference limit 1;
    if r.action = 'attack' then
      perform action_attack(r.x, r.y);
    elseif r.action = 'ranged_attack' then
      perform action_ranged_attack(r.x, r.y);
    else
      --raise exception 'bad ai attack action: %', r.action;
      perform action_cancel();
    end if;
  else
    if exists(select 1 from ai_selected_piece_actions
              where action in ('walk','fly')) then
      select into r * from select_best_move;
      if r.action = 'walk' then
        perform action_walk(r.x, r.y);
      elseif r.action = 'fly' then
        perform action_fly(r.x, r.y);
      else
        perform action_cancel();
      end if;
    else
      perform action_cancel();
    end if;
  end if;
end;
$$ language plpgsql volatile;

/*
--------------------------------------------------------------------------------
*/
--select set_all_attributes_to_not_null();
--select set_notifies_on_all_data_tables();

