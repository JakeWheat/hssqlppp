--
-- PostgreSQL database dump
--

-- Started on 2009-10-07 16:57:39 BST

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- TOC entry 1268 (class 2612 OID 258032)
-- Name: plpgsql; Type: PROCEDURAL LANGUAGE; Schema: -; Owner: -
--

CREATE PROCEDURAL LANGUAGE plpgsql;


SET search_path = public, pg_catalog;

--
-- TOC entry 924 (class 1247 OID 258410)
-- Dependencies: 925 6
-- Name: alignment; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN alignment AS text
	CONSTRAINT alignment_check CHECK ((VALUE = ANY (ARRAY['law'::text, 'neutral'::text, 'chaos'::text])));


--
-- TOC entry 1260 (class 1247 OID 269182)
-- Dependencies: 6 2651
-- Name: fake_pg_attrdef; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE fake_pg_attrdef AS (
	adrelid oid,
	adnum smallint,
	adbin text,
	adsrc text
);


--
-- TOC entry 1264 (class 1247 OID 269188)
-- Dependencies: 6 2653
-- Name: fake_renamed2_pg_attrdef; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE fake_renamed2_pg_attrdef AS (
	adnum smallint,
	adbin text,
	adsrc text,
	adrelid oid
);


--
-- TOC entry 1266 (class 1247 OID 269191)
-- Dependencies: 6 2654
-- Name: fake_renamed3_pg_attrdef; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE fake_renamed3_pg_attrdef AS (
	adrelid oid,
	adnum smallint,
	adbin text,
	adsrc text,
	hello text
);


--
-- TOC entry 1262 (class 1247 OID 269185)
-- Dependencies: 6 2652
-- Name: fake_renamed_pg_attrdef; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE fake_renamed_pg_attrdef AS (
	a oid,
	b smallint,
	c text,
	d text
);


--
-- TOC entry 1112 (class 1247 OID 260274)
-- Dependencies: 1113 6
-- Name: history_name_enum; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN history_name_enum AS text
	CONSTRAINT history_name_enum_check CHECK ((VALUE = ANY (ARRAY['spell_succeeded'::text, 'spell_failed'::text, 'chinned'::text, 'shrugged_off'::text, 'walked'::text, 'fly'::text, 'attack'::text, 'ranged_attack'::text, 'set_imaginary'::text, 'set_real'::text, 'game_won'::text, 'game_drawn'::text, 'spell_skipped'::text, 'new_turn'::text, 'wizard_up'::text, 'choose_spell'::text, 'spread'::text, 'recede'::text, 'disappear'::text, 'new_game'::text, 'attempt_target_spell'::text])));


--
-- TOC entry 1096 (class 1247 OID 260149)
-- Dependencies: 6 2580
-- Name: ipos; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE ipos AS (
	index integer,
	x integer,
	y integer
);


--
-- TOC entry 1010 (class 1247 OID 259534)
-- Dependencies: 1011 6
-- Name: move_phase; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN move_phase AS text
	CONSTRAINT move_phase_check CHECK ((VALUE = ANY (ARRAY['motion'::text, 'attack'::text, 'ranged_attack'::text])));


--
-- TOC entry 1236 (class 1247 OID 261193)
-- Dependencies: 1237 6
-- Name: new_wizard_state; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN new_wizard_state AS text
	CONSTRAINT new_wizard_state_check CHECK ((VALUE = ANY (ARRAY['human'::text, 'computer'::text, 'none'::text])));


--
-- TOC entry 942 (class 1247 OID 258611)
-- Dependencies: 6 2510
-- Name: piece_key; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE piece_key AS (
	ptype text,
	allegiance text,
	tag integer
);


--
-- TOC entry 944 (class 1247 OID 258614)
-- Dependencies: 6 2511
-- Name: pos; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE pos AS (
	x integer,
	y integer
);


--
-- TOC entry 1110 (class 1247 OID 260260)
-- Dependencies: 6 2587
-- Name: random_entry; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE random_entry AS (
	line integer,
	num integer
);


--
-- TOC entry 1021 (class 1247 OID 259896)
-- Dependencies: 1022 6
-- Name: random_test; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN random_test AS text
	CONSTRAINT random_test_check CHECK ((VALUE = ANY (ARRAY['disappear'::text, 'spread'::text, 'attack'::text, 'ranged_attack'::text, 'resist'::text, 'cast'::text, 'bonus'::text, 'break_engaged'::text])));


--
-- TOC entry 882 (class 1247 OID 258310)
-- Dependencies: 883 6
-- Name: ranged_weapon_type; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN ranged_weapon_type AS text
	CONSTRAINT ranged_weapon_type_check CHECK ((VALUE = ANY (ARRAY['projectile'::text, 'fire'::text])));


--
-- TOC entry 899 (class 1247 OID 258345)
-- Dependencies: 900 6
-- Name: spell_category; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN spell_category AS text
	CONSTRAINT spell_category_check CHECK ((VALUE = ANY (ARRAY['object'::text, 'attacking'::text, 'wizard'::text, 'miscellaneous'::text, 'monster'::text])));


--
-- TOC entry 901 (class 1247 OID 258347)
-- Dependencies: 902 6
-- Name: spell_square_category; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN spell_square_category AS text
	CONSTRAINT spell_square_category_check CHECK ((VALUE = ANY (ARRAY['empty'::text, 'empty_or_corpse_only'::text, 'attackable'::text, 'creature_on_top'::text, 'monster_on_top'::text, 'corpse_only'::text, 'empty_and_not_adjacent_to_tree'::text])));


--
-- TOC entry 983 (class 1247 OID 258943)
-- Dependencies: 984 6
-- Name: turn_phase_enum; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN turn_phase_enum AS text
	CONSTRAINT turn_phase_enum_check CHECK ((VALUE = ANY (ARRAY['choose'::text, 'cast'::text, 'autonomous'::text, 'move'::text])));


--
-- TOC entry 988 (class 1247 OID 258999)
-- Dependencies: 6 2530
-- Name: turn_pos; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE turn_pos AS (
	turn_number integer,
	turn_phase turn_phase_enum,
	current_wizard text
);


--
-- TOC entry 1141 (class 1247 OID 260538)
-- Dependencies: 1142 6
-- Name: window_state; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN window_state AS text
	CONSTRAINT window_state_check CHECK ((VALUE = ANY (ARRAY['maximised'::text, 'minimised'::text, 'hidden'::text, 'normal'::text])));


--
-- TOC entry 396 (class 1255 OID 260455)
-- Dependencies: 6 1268
-- Name: action_ai_continue(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_ai_continue() RETURNS void
    LANGUAGE plpgsql
    AS $$
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
$$;


--
-- TOC entry 362 (class 1255 OID 260159)
-- Dependencies: 6 1268
-- Name: action_attack(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_attack(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
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


  if not check_random_success('attack', max((att - def) * 10 + 50, 10)) then
    --failure
    perform add_history_shrugged_off(r);
    perform do_next_move_subphase(true, 'attack');
    return;
  end if;

  perform add_history_chinned(r);
  perform kill_piece(r);

  --move to the square if walker and square empty
  if exists(select 1 from creature_prototypes
              natural inner join selected_piece)
     and exists(select 1 from selected_piece_move_squares
                where (x,y) = (px,py)) then
    perform selected_piece_move_to(px, py);
  end if;
  perform do_next_move_subphase(true, 'attack');
end;
$$;


--
-- TOC entry 358 (class 1255 OID 260155)
-- Dependencies: 1268 6
-- Name: action_cancel(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_cancel() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('cancel');
  perform do_next_move_subphase(true,'none');
end;
$$;


--
-- TOC entry 337 (class 1255 OID 260113)
-- Dependencies: 1268 6
-- Name: action_cast_activate_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_cast_activate_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('cast_activate_spell');
--  perform check_can_cast_spell_now();
  perform add_history_attempt_activate_spell();
  if not check_spell_success() then
    return;
  end if;
  --call the appropiate function to handle the spell
  if (select spell_category = 'wizard' from spells
      natural inner join current_wizard_spell) then
    perform action_cast_wizard_spell(get_current_wizard(),
      get_current_wizard_spell());
  elseif exists(select 1 from spells
      natural inner join current_wizard_spell
      where spell_name in('law', 'chaos', 'large_law',
      'large_chaos')) then
    perform cast_lawchaos();
  elseif (select spell_name='turmoil' from current_wizard_spell) then
    perform cast_turmoil();
  elseif (select spell_name='magic_wood' from current_wizard_spell) then
    perform cast_magic_wood();
  else
    raise exception 'unrecognised activate spell: %',
      (select spell_name from current_wizard_spell);
  end if;
  perform update_alignment_from_cast();
  perform spend_current_wizard_spell();
end;
$$;


--
-- TOC entry 352 (class 1255 OID 260133)
-- Dependencies: 1268 6
-- Name: action_cast_failed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_cast_failed() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform add_history_spell_failed();
  perform spend_current_wizard_spell();
end;
$$;


--
-- TOC entry 336 (class 1255 OID 260112)
-- Dependencies: 1268 6
-- Name: action_cast_target_spell(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_cast_target_spell(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  vspell_name text;
begin
  perform check_can_run_action('cast_target_spell', px, py);
  perform add_history_attempt_target_spell(px,py);

  if not check_spell_success() then
    return;
  end if;

  if exists(select 1 from current_wizard_spell
      natural inner join monster_spells) then
    perform cast_monster_spell(px, py);
  else

    select into vspell_name spell_name from current_wizard_spell;
    if vspell_name = 'disbelieve' then
      if not cast_disbelieve(px, py) then
        return;
      end if;
    elseif vspell_name = 'subversion' then
      if not cast_subversion(px, py) then
        return;
      end if;
    elseif vspell_name = 'raise_dead' then
      perform cast_raise_dead(px, py);
    elseif vspell_name in ('decree', 'justice', 'vengeance', 'dark_power') then
      perform cast_decree_spell(px, py);
    elseif vspell_name in ('lightning', 'magic_bolt') then
      perform cast_ballistic_spell(px, py);
    elseif vspell_name in ('shadow_wood',
      'magic_fire', 'gooey_blob', 'wall',
      'magic_castle', 'dark_citadel') then
      perform cast_object_spell(px, py);
    else
      raise exception 'unrecognised target spell %', vspell_name;
    end if;
  end if;
  --todo: only update alignment once per spell
  perform update_alignment_from_cast();

  update spell_parts_to_cast_table
    set spell_parts_to_cast = spell_parts_to_cast - 1;
  if get_spell_parts_to_cast() = 0 then
    perform spend_current_wizard_spell();
  end if;
end;
$$;


--
-- TOC entry 340 (class 1255 OID 260121)
-- Dependencies: 6 1268
-- Name: action_cast_wizard_spell(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_cast_wizard_spell(pwizard_name text, spell_name text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  --todo: update stats
  if spell_name = 'magic_armour' then
      update wizards
        set magic_armour = true
        where wizard_name = pwizard_name;
  elseif spell_name = 'magic_shield' then
      update wizards
        set magic_shield = true
        where wizard_name = pwizard_name;
  elseif spell_name = 'magic_knife' then
      update wizards
        set magic_knife = true
        where wizard_name = pwizard_name;
  elseif spell_name = 'magic_sword' then
      update wizards
        set magic_sword = true
        where wizard_name = pwizard_name;
  elseif spell_name = 'magic_bow' then
      update wizards
        set magic_bow = true
        where wizard_name = pwizard_name;
  elseif spell_name = 'magic_wings' then
    update wizards set magic_wings = true
      where wizard_name = pwizard_name;
  elseif spell_name = 'shadow_form' then
      update wizards
        set shadow_form = true
        where wizard_name = pwizard_name;
  else
    raise exception 'unrecognised wizard spell %', spell_name;
  end if;
  perform add_history_spell_succeeded();
end;
$$;


--
-- TOC entry 315 (class 1255 OID 260062)
-- Dependencies: 1268 6
-- Name: action_choose_chaos_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_chaos_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_chaos_spell');
  perform action_choose_spell('chaos');
end;
$$;


--
-- TOC entry 309 (class 1255 OID 260056)
-- Dependencies: 6 1268
-- Name: action_choose_dark_citadel_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_dark_citadel_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_dark_citadel_spell');
  perform action_choose_spell('dark_citadel');
end;
$$;


--
-- TOC entry 310 (class 1255 OID 260057)
-- Dependencies: 6 1268
-- Name: action_choose_dark_power_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_dark_power_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_dark_power_spell');
  perform action_choose_spell('dark_power');
end;
$$;


--
-- TOC entry 311 (class 1255 OID 260058)
-- Dependencies: 1268 6
-- Name: action_choose_decree_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_decree_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_decree_spell');
  perform action_choose_spell('decree');
end;
$$;


--
-- TOC entry 312 (class 1255 OID 260059)
-- Dependencies: 6 1268
-- Name: action_choose_disbelieve_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_disbelieve_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_disbelieve_spell');
  perform action_choose_spell('disbelieve');
end;
$$;


--
-- TOC entry 313 (class 1255 OID 260060)
-- Dependencies: 1268 6
-- Name: action_choose_eagle_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_eagle_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_eagle_spell');
  perform action_choose_spell('eagle');
end;
$$;


--
-- TOC entry 314 (class 1255 OID 260061)
-- Dependencies: 6 1268
-- Name: action_choose_elf_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_elf_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_elf_spell');
  perform action_choose_spell('elf');
end;
$$;


--
-- TOC entry 316 (class 1255 OID 260063)
-- Dependencies: 6 1268
-- Name: action_choose_faun_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_faun_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_faun_spell');
  perform action_choose_spell('faun');
end;
$$;


--
-- TOC entry 317 (class 1255 OID 260064)
-- Dependencies: 1268 6
-- Name: action_choose_ghost_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_ghost_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_ghost_spell');
  perform action_choose_spell('ghost');
end;
$$;


--
-- TOC entry 319 (class 1255 OID 260066)
-- Dependencies: 1268 6
-- Name: action_choose_giant_rat_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_giant_rat_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_giant_rat_spell');
  perform action_choose_spell('giant_rat');
end;
$$;


--
-- TOC entry 318 (class 1255 OID 260065)
-- Dependencies: 1268 6
-- Name: action_choose_giant_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_giant_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_giant_spell');
  perform action_choose_spell('giant');
end;
$$;


--
-- TOC entry 320 (class 1255 OID 260067)
-- Dependencies: 1268 6
-- Name: action_choose_goblin_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_goblin_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_goblin_spell');
  perform action_choose_spell('goblin');
end;
$$;


--
-- TOC entry 321 (class 1255 OID 260068)
-- Dependencies: 1268 6
-- Name: action_choose_golden_dragon_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_golden_dragon_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_golden_dragon_spell');
  perform action_choose_spell('golden_dragon');
end;
$$;


--
-- TOC entry 323 (class 1255 OID 260070)
-- Dependencies: 6 1268
-- Name: action_choose_gooey_blob_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_gooey_blob_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_gooey_blob_spell');
  perform action_choose_spell('gooey_blob');
end;
$$;


--
-- TOC entry 324 (class 1255 OID 260071)
-- Dependencies: 6 1268
-- Name: action_choose_gorilla_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_gorilla_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_gorilla_spell');
  perform action_choose_spell('gorilla');
end;
$$;


--
-- TOC entry 325 (class 1255 OID 260072)
-- Dependencies: 1268 6
-- Name: action_choose_green_dragon_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_green_dragon_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_green_dragon_spell');
  perform action_choose_spell('green_dragon');
end;
$$;


--
-- TOC entry 326 (class 1255 OID 260073)
-- Dependencies: 6 1268
-- Name: action_choose_gryphon_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_gryphon_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_gryphon_spell');
  perform action_choose_spell('gryphon');
end;
$$;


--
-- TOC entry 327 (class 1255 OID 260074)
-- Dependencies: 6 1268
-- Name: action_choose_harpy_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_harpy_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_harpy_spell');
  perform action_choose_spell('harpy');
end;
$$;


--
-- TOC entry 328 (class 1255 OID 260075)
-- Dependencies: 6 1268
-- Name: action_choose_horse_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_horse_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_horse_spell');
  perform action_choose_spell('horse');
end;
$$;


--
-- TOC entry 329 (class 1255 OID 260076)
-- Dependencies: 6 1268
-- Name: action_choose_hydra_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_hydra_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_hydra_spell');
  perform action_choose_spell('hydra');
end;
$$;


--
-- TOC entry 330 (class 1255 OID 260077)
-- Dependencies: 6 1268
-- Name: action_choose_justice_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_justice_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_justice_spell');
  perform action_choose_spell('justice');
end;
$$;


--
-- TOC entry 331 (class 1255 OID 260078)
-- Dependencies: 6 1268
-- Name: action_choose_king_cobra_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_king_cobra_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_king_cobra_spell');
  perform action_choose_spell('king_cobra');
end;
$$;


--
-- TOC entry 332 (class 1255 OID 260079)
-- Dependencies: 6 1268
-- Name: action_choose_large_chaos_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_large_chaos_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_large_chaos_spell');
  perform action_choose_spell('large_chaos');
end;
$$;


--
-- TOC entry 333 (class 1255 OID 260080)
-- Dependencies: 1268 6
-- Name: action_choose_large_law_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_large_law_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_large_law_spell');
  perform action_choose_spell('large_law');
end;
$$;


--
-- TOC entry 322 (class 1255 OID 260069)
-- Dependencies: 1268 6
-- Name: action_choose_law_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_law_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_law_spell');
  perform action_choose_spell('law');
end;
$$;


--
-- TOC entry 334 (class 1255 OID 260081)
-- Dependencies: 6 1268
-- Name: action_choose_lightning_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_lightning_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_lightning_spell');
  perform action_choose_spell('lightning');
end;
$$;


--
-- TOC entry 335 (class 1255 OID 260082)
-- Dependencies: 1268 6
-- Name: action_choose_lion_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_lion_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_lion_spell');
  perform action_choose_spell('lion');
end;
$$;


--
-- TOC entry 228 (class 1255 OID 260083)
-- Dependencies: 6 1268
-- Name: action_choose_magic_armour_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_magic_armour_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_magic_armour_spell');
  perform action_choose_spell('magic_armour');
end;
$$;


--
-- TOC entry 229 (class 1255 OID 260084)
-- Dependencies: 6 1268
-- Name: action_choose_magic_bolt_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_magic_bolt_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_magic_bolt_spell');
  perform action_choose_spell('magic_bolt');
end;
$$;


--
-- TOC entry 230 (class 1255 OID 260085)
-- Dependencies: 6 1268
-- Name: action_choose_magic_bow_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_magic_bow_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_magic_bow_spell');
  perform action_choose_spell('magic_bow');
end;
$$;


--
-- TOC entry 231 (class 1255 OID 260086)
-- Dependencies: 1268 6
-- Name: action_choose_magic_castle_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_magic_castle_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_magic_castle_spell');
  perform action_choose_spell('magic_castle');
end;
$$;


--
-- TOC entry 232 (class 1255 OID 260087)
-- Dependencies: 6 1268
-- Name: action_choose_magic_fire_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_magic_fire_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_magic_fire_spell');
  perform action_choose_spell('magic_fire');
end;
$$;


--
-- TOC entry 233 (class 1255 OID 260088)
-- Dependencies: 1268 6
-- Name: action_choose_magic_knife_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_magic_knife_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_magic_knife_spell');
  perform action_choose_spell('magic_knife');
end;
$$;


--
-- TOC entry 234 (class 1255 OID 260089)
-- Dependencies: 1268 6
-- Name: action_choose_magic_shield_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_magic_shield_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_magic_shield_spell');
  perform action_choose_spell('magic_shield');
end;
$$;


--
-- TOC entry 235 (class 1255 OID 260090)
-- Dependencies: 1268 6
-- Name: action_choose_magic_sword_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_magic_sword_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_magic_sword_spell');
  perform action_choose_spell('magic_sword');
end;
$$;


--
-- TOC entry 236 (class 1255 OID 260091)
-- Dependencies: 6 1268
-- Name: action_choose_magic_wings_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_magic_wings_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_magic_wings_spell');
  perform action_choose_spell('magic_wings');
end;
$$;


--
-- TOC entry 237 (class 1255 OID 260092)
-- Dependencies: 1268 6
-- Name: action_choose_magic_wood_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_magic_wood_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_magic_wood_spell');
  perform action_choose_spell('magic_wood');
end;
$$;


--
-- TOC entry 238 (class 1255 OID 260093)
-- Dependencies: 1268 6
-- Name: action_choose_manticore_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_manticore_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_manticore_spell');
  perform action_choose_spell('manticore');
end;
$$;


--
-- TOC entry 306 (class 1255 OID 260052)
-- Dependencies: 6 1268
-- Name: action_choose_no_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_no_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_no_spell');
  delete from wizard_spell_choices_mr where wizard_name = get_current_wizard();
end;
$$;


--
-- TOC entry 239 (class 1255 OID 260094)
-- Dependencies: 6 1268
-- Name: action_choose_ogre_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_ogre_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_ogre_spell');
  perform action_choose_spell('ogre');
end;
$$;


--
-- TOC entry 240 (class 1255 OID 260095)
-- Dependencies: 6 1268
-- Name: action_choose_orc_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_orc_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_orc_spell');
  perform action_choose_spell('orc');
end;
$$;


--
-- TOC entry 241 (class 1255 OID 260096)
-- Dependencies: 6 1268
-- Name: action_choose_pegasus_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_pegasus_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_pegasus_spell');
  perform action_choose_spell('pegasus');
end;
$$;


--
-- TOC entry 242 (class 1255 OID 260097)
-- Dependencies: 6 1268
-- Name: action_choose_raise_dead_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_raise_dead_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_raise_dead_spell');
  perform action_choose_spell('raise_dead');
end;
$$;


--
-- TOC entry 243 (class 1255 OID 260098)
-- Dependencies: 1268 6
-- Name: action_choose_red_dragon_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_red_dragon_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_red_dragon_spell');
  perform action_choose_spell('red_dragon');
end;
$$;


--
-- TOC entry 244 (class 1255 OID 260099)
-- Dependencies: 1268 6
-- Name: action_choose_shadow_form_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_shadow_form_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_shadow_form_spell');
  perform action_choose_spell('shadow_form');
end;
$$;


--
-- TOC entry 245 (class 1255 OID 260100)
-- Dependencies: 1268 6
-- Name: action_choose_shadow_wood_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_shadow_wood_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_shadow_wood_spell');
  perform action_choose_spell('shadow_wood');
end;
$$;


--
-- TOC entry 246 (class 1255 OID 260101)
-- Dependencies: 1268 6
-- Name: action_choose_skeleton_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_skeleton_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_skeleton_spell');
  perform action_choose_spell('skeleton');
end;
$$;


--
-- TOC entry 247 (class 1255 OID 260102)
-- Dependencies: 1268 6
-- Name: action_choose_spectre_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_spectre_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_spectre_spell');
  perform action_choose_spell('spectre');
end;
$$;


--
-- TOC entry 305 (class 1255 OID 260051)
-- Dependencies: 6 1268
-- Name: action_choose_spell(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_spell(vspell_name text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  --create the argumentless action name so we can check the action
  --valid table
  perform check_can_run_action('choose_' || vspell_name || '_spell');

  --do nothing if this is the same as the currently selected spell
  if (select spell_name from wizard_spell_choices
            where wizard_name = get_current_wizard()) = vspell_name then
    null;
  else
    --if wizard already has a chosen spell then remove it
    delete from wizard_spell_choices_mr
      where wizard_name = get_current_wizard();
    insert into wizard_spell_choices_mr (wizard_name, spell_name)
        values
      (get_current_wizard(), vspell_name);
    --
    -- set imaginary to false if this is a monster spell
    if exists(select 1 from monster_spells
      where spell_name = vspell_name) then
      update wizard_spell_choices_mr
        set imaginary = false
        where wizard_name = get_current_wizard();
    else
      update wizard_spell_choices_mr
        set imaginary = null
        where wizard_name = get_current_wizard();
    end if;
  end if;
  perform add_history_choose_spell();
end;
$$;


--
-- TOC entry 248 (class 1255 OID 260103)
-- Dependencies: 6 1268
-- Name: action_choose_subversion_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_subversion_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_subversion_spell');
  perform action_choose_spell('subversion');
end;
$$;


--
-- TOC entry 249 (class 1255 OID 260104)
-- Dependencies: 6 1268
-- Name: action_choose_turmoil_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_turmoil_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_turmoil_spell');
  perform action_choose_spell('turmoil');
end;
$$;


--
-- TOC entry 250 (class 1255 OID 260105)
-- Dependencies: 6 1268
-- Name: action_choose_unicorn_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_unicorn_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_unicorn_spell');
  perform action_choose_spell('unicorn');
end;
$$;


--
-- TOC entry 251 (class 1255 OID 260106)
-- Dependencies: 1268 6
-- Name: action_choose_vampire_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_vampire_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_vampire_spell');
  perform action_choose_spell('vampire');
end;
$$;


--
-- TOC entry 252 (class 1255 OID 260107)
-- Dependencies: 6 1268
-- Name: action_choose_vengeance_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_vengeance_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_vengeance_spell');
  perform action_choose_spell('vengeance');
end;
$$;


--
-- TOC entry 253 (class 1255 OID 260108)
-- Dependencies: 1268 6
-- Name: action_choose_wall_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_wall_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_wall_spell');
  perform action_choose_spell('wall');
end;
$$;


--
-- TOC entry 254 (class 1255 OID 260109)
-- Dependencies: 1268 6
-- Name: action_choose_wraith_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_wraith_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_wraith_spell');
  perform action_choose_spell('wraith');
end;
$$;


--
-- TOC entry 255 (class 1255 OID 260110)
-- Dependencies: 6 1268
-- Name: action_choose_zombie_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_choose_zombie_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_zombie_spell');
  perform action_choose_spell('zombie');
end;
$$;


--
-- TOC entry 443 (class 1255 OID 261024)
-- Dependencies: 6 1268
-- Name: action_client_ai_continue(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_client_ai_continue() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  if get_running_effects() then
    return;
  end if;

  perform action_ai_continue();
  perform update_missing_startticks();
  if (select computer_controlled from wizards
      inner join current_wizard_table on wizard_name=current_wizard)
     and get_turn_phase() = 'choose' then
    perform action_client_ai_continue();
  else
    perform check_for_effects();
    perform update_board_sprites_cache();
  end if;
  if not (select computer_controlled from wizards
          inner join current_wizard_table
          on wizard_name=current_wizard) then
    perform action_move_cursor_to_current_wizard();
  end if;
end;
$$;


--
-- TOC entry 444 (class 1255 OID 261025)
-- Dependencies: 1268 6
-- Name: action_client_ai_continue_if(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_client_ai_continue_if() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  if exists(select 1 from valid_activate_actions
            where action='ai_continue') then
    perform action_client_ai_continue();
  end if;
end;
$$;


--
-- TOC entry 508 (class 1255 OID 261431)
-- Dependencies: 6 1268
-- Name: action_client_new_game(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_client_new_game() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  --assert: argument has between 2 and 8 active wizards
  delete from action_new_game_argument;
  delete from init_wizard_display_info_argument;
  -- clear data tables
  delete from cursor_position;
  delete from wizard_display_info;

  delete from last_history_effect_id_table;
  insert into last_history_effect_id_table values (-1);
  delete from board_square_effects;
  delete from board_beam_effects;
  delete from board_sound_effects;
  delete from current_effects;

  -- don't reset windows, see below
  --call server new_game
  --populate argument first
  delete from action_new_game_argument;
  insert into action_new_game_argument
    (wizard_name, computer_controlled, place)
    select wizard_name, computer_controlled, place
      from action_client_new_game_argument;
  perform action_new_game();

  --wizard display_info
  delete from init_wizard_display_info_argument;
  insert into init_wizard_display_info_argument
      (wizard_name, sprite, colour)
    select wizard_name, sprite, colour
    from action_client_new_game_argument;
  perform init_wizard_display_info();

  --populate window data,
  -- preserve settings from previous game if there are some
  if not exists(select 1 from windows) then
    perform action_reset_windows();
  end if;

  if not exists(select 1 from spell_book_show_all_table) then
    insert into spell_book_show_all_table values (false);
  end if;

  perform update_board_sprites_cache();
  perform check_for_effects();
  perform init_cursor_position();
end
$$;


--
-- TOC entry 499 (class 1255 OID 261415)
-- Dependencies: 6 1268
-- Name: action_client_new_game_argument_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_client_new_game_argument_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for action_client_new_game_argument';
  if not (select count(*) from action_client_new_game_argument
  where place >=
  (select count(*) from action_client_new_game_argument)) = 0 then
    raise exception
      'value violates database constraint "action_client_new_game_place_valid"';
  end if;

--  raise notice 'complete constraint op for action_client_new_game_argument';
  return OLD;
end;
$$;


--
-- TOC entry 448 (class 1255 OID 261292)
-- Dependencies: 6 1268
-- Name: action_client_new_game_using_new_game_widget_state(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_client_new_game_using_new_game_widget_state() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  delete from action_client_new_game_argument;
  insert into action_client_new_game_argument
    (place, wizard_name, sprite, colour, computer_controlled)
    select line, wizard_name, sprite, colour,
      case when state = 'computer' then true
           else false end
      from new_game_widget_state
      where state != 'none';
  perform action_client_new_game();
end
$$;


--
-- TOC entry 462 (class 1255 OID 261323)
-- Dependencies: 6 1268
-- Name: action_client_next_phase(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_client_next_phase() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_next_phase();
  if not (select computer_controlled from wizards
          inner join current_wizard_table
          on wizard_name=current_wizard) then
    perform action_move_cursor_to_current_wizard();
  end if;
end;
$$;


--
-- TOC entry 361 (class 1255 OID 260158)
-- Dependencies: 1268 6
-- Name: action_fly(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_fly(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  p pos;
begin
  perform check_can_run_action('fly', px, py);
  select into p x,y from pieces natural inner join selected_piece;
  perform selected_piece_move_to(px, py);
  perform add_history_fly(p.x,p.y);
  perform do_next_move_subphase(false, 'motion');
end;
$$;


--
-- TOC entry 464 (class 1255 OID 261324)
-- Dependencies: 6 1268
-- Name: action_go(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_go() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r record;
  s text;
begin
  select into r x,y,action from client_valid_target_actions
    natural inner join cursor_position;
  if r is not null then
    s :=  'select action_' || r.action || '(' || r.x || ',' || r.y || ')';
    execute s;
  else
    select into r action
      from client_valid_activate_actions
       where action in ('cast_activate_spell');
    if r is not null then
      s := 'select action_' || r.action || '()';
      execute s;
    end if;
  end if;
  return ;
end;
$$;


--
-- TOC entry 177 (class 1255 OID 260550)
-- Dependencies: 6 1268
-- Name: action_hide_window(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_hide_window(vname text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  if vname = 'window_manager' then
    raise exception 'cannot hide window manager';
  end if;
  update windows set state='hidden' where window_name = vname;
end;
$$;


--
-- TOC entry 412 (class 1255 OID 260520)
-- Dependencies: 6 1268
-- Name: action_history_mr_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_history_mr_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify action_history_mr;
return null;
end;
$$;


--
-- TOC entry 461 (class 1255 OID 261322)
-- Dependencies: 1268 6
-- Name: action_key_pressed(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_key_pressed(pkeycode text) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  a text;
  cursor_move boolean := false;
begin
/*
basic plan
have a table with key code, and action name
then a strategy of taking an action and
     a. deciding where to get the arguments
     b. deciding if it is allowed

profiling progress: started out about 1 sec to run when using for
loop, got rid of that, got it down to about 0.1 sec but this is for an
unmatched keypress, need to be faster.

*/
  if get_running_effects() then
    return;
  end if;

  if exists(select 1 from valid_activate_actions
            where action='ai_continue')
     and pkeycode = 'space' then
    perform action_client_ai_continue();
  else
    select into a action_name from key_control_settings k
      inner join client_valid_activate_actions v
        on k.action_name = v.action
        where key_code = pkeycode;
    if not a is null then
        if substr(a,0,11) =  'move_cursor' then
          cursor_move := true;
        end if;
        execute 'select action_' || a || '();';
    else
      select into a action_name from key_control_settings k
        inner join client_valid_target_actions v
          on k.action_name = v.action
        natural inner join cursor_position
          where key_code = pkeycode;
      if substr(a,0,11) =  'move_cursor' then
        cursor_move := true;
      end if;
      if not a is null then
        execute 'select action_' || a ||
                '(' || (select x from cursor_position) ||
                ', ' || (select y from cursor_position) || ');';
      else
        null;
      end if;
    end if;
  end if;
  perform update_missing_startticks();
  perform check_for_effects();
  if not cursor_move then
    perform update_board_sprites_cache();
  end if;
end;
$$;


--
-- TOC entry 419 (class 1255 OID 260763)
-- Dependencies: 1268 6
-- Name: action_move_cursor(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_move_cursor(direction text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  case direction
  when 'up' then
    perform safe_move_cursor(0, -1);
  when 'down' then
    perform safe_move_cursor(0, 1);
  when 'left' then
    perform safe_move_cursor(-1, 0);
  when 'right' then
    perform safe_move_cursor(1, 0);
  when 'up-left' then
    perform safe_move_cursor(-1, -1);
  when 'up-right' then
    perform safe_move_cursor(1, -1);
  when 'down-left' then
    perform safe_move_cursor(-1, 1);
  when 'down-right' then
    perform safe_move_cursor(1, 1);
  else
    raise exception
      'asked to move cursor in direction % which isn''t valid',
      direction;
  end case;
end;
$$;


--
-- TOC entry 451 (class 1255 OID 261312)
-- Dependencies: 6 1268
-- Name: action_move_cursor_down(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_move_cursor_down() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_move_cursor('down');
end;
$$;


--
-- TOC entry 458 (class 1255 OID 261319)
-- Dependencies: 6 1268
-- Name: action_move_cursor_down_left(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_move_cursor_down_left() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_move_cursor('down-left');
end;
$$;


--
-- TOC entry 457 (class 1255 OID 261318)
-- Dependencies: 1268 6
-- Name: action_move_cursor_down_right(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_move_cursor_down_right() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_move_cursor('down-right');
end;
$$;


--
-- TOC entry 453 (class 1255 OID 261314)
-- Dependencies: 6 1268
-- Name: action_move_cursor_left(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_move_cursor_left() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_move_cursor('left');
end;
$$;


--
-- TOC entry 454 (class 1255 OID 261315)
-- Dependencies: 1268 6
-- Name: action_move_cursor_right(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_move_cursor_right() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_move_cursor('right');
end;
$$;


--
-- TOC entry 420 (class 1255 OID 260764)
-- Dependencies: 1268 6
-- Name: action_move_cursor_to_current_wizard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_move_cursor_to_current_wizard() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
 p pos;
begin
  --don't move cursor during autonomous phase
  if get_turn_phase() != 'autonomous' then
    select into p x,y from pieces
         inner join current_wizard_table
         on (current_wizard = allegiance)
         where ptype = 'wizard';
    update cursor_position set (x,y) = (p.x,p.y);
  end if;
end;
$$;


--
-- TOC entry 452 (class 1255 OID 261313)
-- Dependencies: 1268 6
-- Name: action_move_cursor_up(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_move_cursor_up() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_move_cursor('up');
end;
$$;


--
-- TOC entry 455 (class 1255 OID 261316)
-- Dependencies: 6 1268
-- Name: action_move_cursor_up_left(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_move_cursor_up_left() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_move_cursor('up-left');
end;
$$;


--
-- TOC entry 456 (class 1255 OID 261317)
-- Dependencies: 1268 6
-- Name: action_move_cursor_up_right(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_move_cursor_up_right() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_move_cursor('up-right');
end;
$$;


--
-- TOC entry 394 (class 1255 OID 260453)
-- Dependencies: 6 1268
-- Name: action_new_game(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_new_game() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r record;
  t int;
begin

  update creating_new_game_table set creating_new_game = true;
  --assert: all tables tagged data are in this delete list
  --(only need base table of entities since these cascade)
  -- tables are in order of dependencies so delete sequence works

  -- clear data tables
  delete from action_history_mr;
  perform setval('action_history_mr_id_seq', 1);

  --turn data
  delete from game_completed_table;
  delete from cast_alignment_table;
  delete from remaining_walk_table;
  delete from selected_piece;
  delete from pieces_to_move;
  delete from spell_parts_to_cast_table;
  delete from wizard_spell_choices_mr;
  delete from current_wizard_table;
  delete from turn_phase_table;
  delete from cast_success_checked_table;
  delete from turn_number_table;
  --piece data
  delete from spell_books;
  delete from imaginary_pieces;
  delete from pieces;
  delete from wizards;
  delete from board_size;
  delete from world_alignment_table;

  if not exists(select 1 from disable_spreading_table) then
    insert into disable_spreading_table values(false);
  else
    update disable_spreading_table
       set disable_spreading = false;
  end if;

  --reset the overrides when starting new game
  delete from test_action_overrides;

  --assert: call init_ for each data table, make sure order is good

  perform init_world_alignment();
  perform init_board_size();

  --create wizards
  --  wizard table
  insert into wizards (wizard_name, computer_controlled, original_place)
    select wizard_name, computer_controlled, place
      from action_new_game_argument;
  --  pieces
  t := (select count(*) from action_new_game_argument);
  insert into pieces (ptype, allegiance, tag, x, y)
    select 'wizard', wizard_name, 0, x,y
      from action_new_game_argument
      natural inner join wizard_starting_positions
       where wizard_count = t;
  --  spell books
  --init spell book
  -- disbelieve plus 19 {random spells not disbelieve or turmoil}
  insert into spell_books (wizard_name, spell_name)
    select wizard_name, 'disbelieve'
      from action_new_game_argument;

  insert into spell_books (wizard_name, spell_name)
    select wizard_name,spell_name
      from action_new_game_argument
      inner join (select line, spell_name
                  from spell_indexes_no_dis_turm
                  inner join makeNRandoms(t * 19, 53)
                    on row_number = num) as a
      on (line/19) = place;
  --sanity check that bad boy
  if exists(select 1 from (select wizard_name, count(spell_name)
                           from spell_books group by wizard_name
                          ) as a where count <> 20) then
    raise exception 'miscount in initial spell books';
  end if;
  --turn stuff
  perform init_turn_stuff();

  /*
  data tables with no init because they are empty at start of game
  piece sub entities
  action_history and sub entities
  wizard spell choices, pieces to move, current moving piece
  */
  --TODO: add new game action history
  perform add_history_new_game();

  update creating_new_game_table set creating_new_game = false;

end
$$;


--
-- TOC entry 495 (class 1255 OID 261409)
-- Dependencies: 6 1268
-- Name: action_new_game_argument_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_new_game_argument_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for action_new_game_argument';
  if not (select count(*) from action_new_game_argument
    where place >= (select count(*) from action_new_game_argument)) = 0 then
    raise exception
      'value violates database constraint "action_new_game_argument_place_valid"';
  end if;

--  raise notice 'complete constraint op for action_new_game_argument';
  return OLD;
end;
$$;


--
-- TOC entry 302 (class 1255 OID 260048)
-- Dependencies: 6 1268
-- Name: action_next_phase(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_next_phase() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  c int;
  next_phase_again boolean := false;
begin
/*
=== check for game completion
*/
  if (exists (select 1 from game_completed_table)) then
    return;
  end if;
  --check for win or draw
  c := (select count(1) from wizards
         where not expired);
  if c = 1 then --someone has won
    perform game_completed();
    update current_wizard_table set current_wizard =
      (select wizard_name from wizards where not expired);
    perform add_history_game_won();
    return;
  elseif c = 0 then --game is drawn
    perform game_completed();
    perform add_history_game_drawn();
    delete from current_wizard_table;
    return;
  end if;

/*
=== current wizard clean up phase

If the user selects next phase when they have a spell to cast, then we
want to call the usual skip spell action so as not to duplicate the
work. But skip spell will call next_phase itself automatically and we
don't want to do two next phases, so if there is a spell to be
skipped, run that and don't run the rest of the next_phase function
since it will be called via skip spell.

*/
    -- if the current spell isn't completed, then skip it
  if exists(select 1 from wizard_spell_choices
       inner join current_wizard_table
       on (current_wizard = wizard_name)
       where get_turn_phase() = 'cast') then
    perform skip_spell();
    return;
  end if;

  --multiple update hack to get round constraints
  update in_next_phase_hack_table
    set in_next_phase_hack = true;

  --complete current phase:
  if (select turn_phase = 'move' from turn_phase_table) then
    delete from pieces_to_move;
  end if;

/*
=== all wizards clean up phase

clean up if this is the last wizard for this phase, then move to next
phase, if this is autonomous, then do it and move to move phase this
works because all the end phase stuff happens before the autonomous
phase is run in this function, and all the setup runs after it is run.

*/

  if is_last_wizard() then
    --clear the cast alignment which is used to adjust the world
    --alignment when a spell is cast
    if get_turn_phase() = 'cast' then
      delete from cast_alignment_table;
    end if;

    --if this is the end of the move phase then we're on the next turn
    if (select turn_phase = 'move' from turn_phase_table) then
      update turn_number_table
        set turn_number = turn_number + 1;
      perform add_history_new_turn();
    end if;

    --move to the next turn phase
    update turn_phase_table
      set turn_phase = next_turn_phase(turn_phase);

    if (select turn_phase = 'autonomous' from turn_phase_table) then
      perform do_autonomous_phase();
      update turn_phase_table
        set turn_phase = next_turn_phase(turn_phase);
    end if;
  end if;

/*
=== init new current phase
*/
  -- move to the next wizard, this is the meat of this function
  update current_wizard_table
    set current_wizard = next_wizard(current_wizard);

  --setup the cast alignment table if this is the start of the cast
  --phases
  if get_turn_phase() = 'cast' and is_first_wizard() then
    insert into cast_alignment_table values(0);
  end if;

  --initialise the spell for this phase
  if (select turn_phase = 'cast' from turn_phase_table) then
    if exists(select 1 from current_wizard_spell) then
      insert into spell_parts_to_cast_table
        select coalesce(num, 0) from spells_with_num_shots
        natural inner join current_wizard_spell;
      insert into cast_success_checked_table values (false);
    else
      --skip to the next phase automatically
      next_phase_again := true;
    end if;
  elseif (select turn_phase = 'move' from turn_phase_table) then
    insert into pieces_to_move
      select ptype, allegiance, tag
        from moving_pieces
        inner join current_wizard_table
        on allegiance = current_wizard;
  end if;

  --finished our updates for this next phase
  update in_next_phase_hack_table
    set in_next_phase_hack = false;

  perform add_history_wizard_up();
/*
=== continue
*/
  --if there is nothing to do in the new current phase - continue to
  --next phase automatically
  if next_phase_again then
    perform action_next_phase();
  end if;
end;
$$;


--
-- TOC entry 363 (class 1255 OID 260160)
-- Dependencies: 6 1268
-- Name: action_ranged_attack(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_ranged_attack(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
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

  if not check_random_success('ranged_attack', max((att - def) * 10 + 50, 10)) then
    --failure
    perform add_history_shrugged_off(r);
    perform do_next_move_subphase(false, 'ranged_attack');
    return;
  end if;

  perform add_history_chinned(r);
  perform kill_piece(r);
  perform do_next_move_subphase(false, 'ranged_attack');
end;
$$;


--
-- TOC entry 178 (class 1255 OID 260551)
-- Dependencies: 6 1268
-- Name: action_refresh_widgets(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_refresh_widgets() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
--doesn't do owt at the moment, all in the haskell code,
-- just has this stub here to avoid special casing it in the
--haskell code
end;
$$;


--
-- TOC entry 441 (class 1255 OID 261022)
-- Dependencies: 6 1268
-- Name: action_reset_current_effects(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_reset_current_effects() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
    delete from board_sound_effects;
    delete from board_beam_effects;
    delete from board_square_effects;
    delete from current_effects;
end;
$$;


--
-- TOC entry 447 (class 1255 OID 261291)
-- Dependencies: 6 1268
-- Name: action_reset_new_game_widget_state(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_reset_new_game_widget_state() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
    delete from new_game_widget_state;
    insert into new_game_widget_state
      (line, wizard_name, sprite, colour, state) values
      (0, 'Buddha', 'wizard0', 'blue', 'human'),
      (1, 'Kong Fuzi', 'wizard1', 'purple', 'computer'),
      (2, 'Laozi', 'wizard2', 'cyan', 'computer'),
      (3, 'Moshe', 'wizard3', 'yellow', 'computer'),
      (4, 'Muhammad', 'wizard4', 'green', 'computer'),
      (5, 'Shiva', 'wizard5', 'red', 'computer'),
      (6, 'Yeshua', 'wizard6', 'white', 'computer'),
      (7, 'Zarathushthra', 'wizard7', 'orange', 'computer');
end
$$;


--
-- TOC entry 176 (class 1255 OID 260549)
-- Dependencies: 1268 6
-- Name: action_reset_windows(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_reset_windows() RETURNS void
    LANGUAGE plpgsql
    AS $$
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
$$;


--
-- TOC entry 291 (class 1255 OID 259904)
-- Dependencies: 1021 1268 6
-- Name: action_rig_action_success(random_test, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_rig_action_success(poverride random_test, psetting boolean) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into test_action_overrides (override, setting)
    values (poverride, psetting);
end;
$$;


--
-- TOC entry 356 (class 1255 OID 260153)
-- Dependencies: 1268 6
-- Name: action_select_piece_at_position(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_select_piece_at_position(vx integer, vy integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
begin
  perform check_can_run_action('select_piece_at_position', vx,vy);
  select into r ptype,allegiance, tag from selectable_pieces
         where (x,y) = (vx,vy);
  perform select_piece(r);
end;
$$;


--
-- TOC entry 307 (class 1255 OID 260053)
-- Dependencies: 6 1268
-- Name: action_set_imaginary(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_set_imaginary() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('set_imaginary');
  update wizard_spell_choices_mr
    set imaginary = true
    where wizard_name = get_current_wizard();
end;
$$;


--
-- TOC entry 308 (class 1255 OID 260054)
-- Dependencies: 6 1268
-- Name: action_set_real(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_set_real() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('set_real');
  update wizard_spell_choices_mr
    set imaginary = false
    where wizard_name = get_current_wizard();
end;
$$;


--
-- TOC entry 395 (class 1255 OID 260454)
-- Dependencies: 6 1268
-- Name: action_setup_test_board(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_setup_test_board(flavour text) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  i int;
  rec record;
  vwidth int;
  vname text;
  vx int;
  vy int;
  vallegiance text;
begin
  --assert - new game just created
  --         flavour is one of all_pieces, upgraded_wizards, overlapping
  select into vwidth width from board_size;

  if flavour = 'all_pieces' then
    --create one of each monster
    i:= 0;
    for rec in select ptype from monster_prototypes loop
      perform create_monster(rec.ptype, 'Buddha',
                             i % vwidth, 1 + i / vwidth, false);
      i := i + 1;
    end loop;
    --create one of each corpse
    i := 0;
    for rec in select ptype from monster_prototypes where undead = false loop
      perform create_monster(rec.ptype, 'Buddha',
                             i % vwidth, 5 + i / vwidth, false);
      perform kill_top_piece_at(i % vwidth, 5 + i / vwidth);
      i := i + 1;
    end loop;
    --create one of each (pieces - creatures)
    i := 0;
    for rec in select ptype from object_piece_types loop
      perform create_object(rec.ptype, 'Kong Fuzi', i, 8);
      i := i + 1;
    end loop;
  elseif flavour = 'upgraded_wizards' then
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 0),
      'shadow_form');
     --fix history
    update action_history_mr
      set spell_name = 'shadow_form',
      allegiance='Buddha'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 1),
      'magic_sword');
    update action_history_mr
      set spell_name = 'magic_sword',
      allegiance = 'Kong Fuzi'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 2),
      'magic_knife');
    update action_history_mr
      set spell_name = 'magic_knife',
      allegiance = 'Laozi'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 3),
      'magic_shield');
    update action_history_mr
      set spell_name = 'magic_shield',
      allegiance='Moshe'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 4),
      'magic_wings');
    update action_history_mr
      set spell_name = 'magic_wings',
      allegiance='Muhammad'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 5),
      'magic_armour');
    update action_history_mr
      set spell_name = 'magic_armour',
      allegiance='Shiva'
      where spell_name is null;
    perform action_cast_wizard_spell(
      (select wizard_name from wizards where original_place = 6),
      'magic_bow');
    update action_history_mr
      set spell_name = 'magic_bow',
      allegiance = 'Yeshua'
      where spell_name is null;
  elseif flavour = 'overlapping' then
    --assert at least 5 wizards
    --wizard, stiff
    select into vx,vy x,y from pieces
      inner join wizards
        on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 0;
    perform create_monster('goblin', 'Buddha', 1, 0, false);
    perform kill_top_piece_at(1, 0);
    --drop in an extra dead gobbo for testing raise dead
    perform create_monster('goblin', 'Yeshua', vx, vy, false);
    perform kill_top_piece_at(vx, vy);
--wizard, mountable
    select into vx,vy,vallegiance x,y,allegiance
      from pieces inner join wizards
      on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 1;
    perform create_monster('horse', vallegiance, vx, vy, false);
--wizard in magic tree, castle, citadel
    select into vx,vy,vallegiance x,y,allegiance
      from pieces inner join wizards
      on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 2;
    perform create_object('magic_tree', vallegiance, vx, vy);
    select into vx,vy,vallegiance x,y,allegiance
      from pieces inner join wizards
      on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 3;
    perform create_object('magic_castle', vallegiance, vx, vy);
    select into vx,vy,vallegiance x,y,allegiance
      from pieces inner join wizards
      on allegiance = wizard_name
      where ptype = 'wizard' and original_place = 4;
    perform create_object('dark_citadel', vallegiance, vx, vy);
--monster, stiff
    perform create_monster('goblin', 'Buddha', 3, 3, false);
    perform kill_top_piece_at(3, 3);
    perform create_monster('giant', 'Buddha', 3, 3, false);
--stiff, blob
    perform create_monster('goblin', 'Buddha', 4, 3, false);
    perform kill_top_piece_at(4, 3);
    perform create_object('gooey_blob', 'Buddha', 4, 3);
--monster, blob
    perform create_monster('goblin', 'Laozi', 5, 3, false);
    perform create_object('gooey_blob', 'Buddha', 5, 3);
--stiff, monster, blob
    perform create_monster('elf', 'Buddha', 6, 3, false);
    perform kill_top_piece_at(6, 3);
    perform create_monster('goblin', 'Laozi', 6, 3, false);
    perform create_object('gooey_blob', 'Buddha', 6, 3);
  else
    raise exception
    'argument must be one of all_pieces, upgraded_wizards, overlapping, got %',
    flavours;
  end if;
end
$$;


--
-- TOC entry 414 (class 1255 OID 261138)
-- Dependencies: 6 1268
-- Name: action_spell_book_show_all_update(boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_spell_book_show_all_update(v boolean) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  update spell_book_show_all_table set spell_book_show_all=v;
end;
$$;


--
-- TOC entry 460 (class 1255 OID 261321)
-- Dependencies: 1268 6
-- Name: action_spell_book_show_all_update_off(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_spell_book_show_all_update_off() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_spell_book_show_all_update(false);
end;
$$;


--
-- TOC entry 459 (class 1255 OID 261320)
-- Dependencies: 6 1268
-- Name: action_spell_book_show_all_update_on(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_spell_book_show_all_update_on() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform action_spell_book_show_all_update(true);
end;
$$;


--
-- TOC entry 357 (class 1255 OID 260154)
-- Dependencies: 6 1268
-- Name: action_unselect_piece(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_unselect_piece() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('unselect_piece');
  --remove piece from pieces to move
  delete from pieces_to_move where (ptype, allegiance, tag) =
    (select ptype, allegiance, tag from selected_piece);
  --empty selected piece, squares left_to_walk
  update remaining_walk_hack_table
    set remaining_walk_hack = true;
  delete from selected_piece;
  delete from remaining_walk_table;
  update remaining_walk_hack_table
    set remaining_walk_hack = false;
  --if there are no more pieces that can be selected then move to next
  --phase automatically, todo: take into account monsters in blob
  if not exists(select 1 from pieces_to_move) then
    perform action_next_phase();
  end if;

  --insert history
end;
$$;


--
-- TOC entry 442 (class 1255 OID 261023)
-- Dependencies: 1268 6
-- Name: action_update_effects_ticks(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_update_effects_ticks(pticks integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  wasEffects boolean := false;
  nextQp int;
begin
  if exists(select 1 from current_effects) then
    wasEffects := true;
  end if;
  --always delete sound effects after the first time they are returned
  if exists(select 1 from current_board_sound_effects) then
    delete from board_sound_effects
      where queuePos = (select queuePos from current_effects);
  end if;
  --see if we need a new row of effects
  if not exists(select 1 from current_effects)
    or pticks > (select ticks + 6 from current_effects) then
    delete from board_sound_effects
      where queuePos = (select queuePos from current_effects);
    delete from board_beam_effects
      where queuePos = (select queuePos from current_effects);
    delete from board_square_effects
      where queuePos = (select queuePos from current_effects);
    delete from current_effects;
    nextQp := (select min(queuePos) from
                 (select queuePos from board_sound_effects
                  union all
                  select queuePos from board_beam_effects
                  union all
                  select queuePos from board_square_effects) as a);
    if nextQp is not null and nextQp <> 0 then
      insert into current_effects (ticks, queuePos)
        values (pticks, nextQp);
    end if;
  end if;
  if not exists(select 1 from current_effects)
     and wasEffects then
    perform update_board_sprites_cache();
  end if;
end;
$$;


--
-- TOC entry 360 (class 1255 OID 260157)
-- Dependencies: 6 1268
-- Name: action_walk(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION action_walk(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
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
$$;


--
-- TOC entry 365 (class 1255 OID 260162)
-- Dependencies: 6 1268
-- Name: add_chinned_history(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_chinned_history(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
begin
    select into r ptype, allegiance, tag
      from pieces_on_top where (x,y) = (px,py);
    perform add_history_chinned(r);
end;
$$;


--
-- TOC entry 19 (class 1255 OID 258103)
-- Dependencies: 1268 6
-- Name: add_constraint(text, text, text[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_constraint(vname text, vexpression text, vrelvars text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform add_constraint_internal(vname,vexpression,vrelvars, false);
end;
$$;


--
-- TOC entry 22 (class 1255 OID 258104)
-- Dependencies: 1268 6
-- Name: add_constraint_internal(text, text, text[], boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_constraint_internal(vname text, vexpression text, vrelvars text[], is_con_pg boolean) RETURNS void
    LANGUAGE plpgsql
    AS $_$
declare
  r boolean;
  i integer;
  t text;
  constraint_operator text;
  s text;
begin
  --check the constraint is valid, cannot add it if it doesn't currently
  --evaluate to true
  --raise notice '******* adding constraint: %****%', vname, vexpression;
  if not is_con_pg then
    execute 'select ' || vexpression into r;
    if not r then
      raise exception
        'attempt to add constraint % which evaluates to false: %',
        vname, vexpression;
    end if;
  end if;
  --make entry in catalog
  insert into database_constraints (constraint_name, expression)
    values (vname, vexpression);
   constraint_operator := 'check_con_' || vname;
  --create implementation and add to catalog
  --we do this even if we actually implement it using
  --an accelerator

  --this function checks that the constraint currently holds
  execute 'create function ' || constraint_operator ||
    '() returns boolean as $a$
begin
  return ' || vexpression || ';
end;
$a$ language plpgsql stable;';
  --hide the check function from the user catalog
  insert into system_implementation_objects(object_name, object_type)
    values (constraint_operator, 'operator');
  --store the check function in the internal constraint implementation catalog
  execute $a$insert into dbcon_ops
    (constraint_name, operator_name) values ('$a$ || vname || $a$', '$a$
      || constraint_operator || $a$');$a$;
  --add entries into constraint_relvars: so we can get a list of
  --constraint implementation functions for a given table
  if not is_con_pg then
    for i in array_lower(vrelvars, 1)..array_upper(vrelvars, 1) loop
     -- todo: if relvar is a view, then need to trigger on
     -- view definition change and on any relvars which the view
     -- depends on change
     -- for now just skip views?
      insert into dbcon_relvars (constraint_name, relvar_name)
        values (vname, vrelvars[i]);
    end loop;
    --now recreate the actual triggers which enforce the constraint
    perform regenerate_constraint_triggers();
  end if;

  --hack: if the constraint mentions any tables which are
  -- system_implementation_objects then add the constraint as a
  -- system_implementation_object
  -- this is also to hide implementation details from the user catalog

  if exists(select 1 from system_implementation_objects
    where object_type = 'base_relvar' and
      object_name = any (vrelvars)) then
    insert into system_implementation_objects (object_name, object_type)
      values (vname, 'database_constraint');
  end if;
end;
$_$;


--
-- TOC entry 26 (class 1255 OID 258108)
-- Dependencies: 1268 6
-- Name: add_foreign_key(text, text[], text, text[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_foreign_key(vtable text, src_attr text[], reftable text, ref_attr text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  i int;
  vcn text;
  bt text[];
  accel boolean;
begin
/*

automatically generate a name for this constraint in the catalog at
some point, you'll have to provide a name for every constraint
manually (plus some other stuff, like error message variants and
documentation to display in end user programs for user friendliness)

*/
  vcn := vtable || '_' || array_to_string(src_attr, '_') || '_fkey';
  vcn := sort_out_constraint_name(vcn, '_fkey');
  -- make sure we get a unique name in case there are multiple foreign keys
  -- on one set of attributes
  --TODO: rewrite this to use max instead of looping
  -- if target is a base relvar and the target attributes are a key then
  -- pg accelerate it
  if exists(select 1 from database_constraints where
        constraint_name = vcn) then
    i := 1;
-- I must be going mad cos I can't find the convert
-- int to string in base 10 function in pg...
    while exists(select 1 from database_constraints where
      constraint_name = vcn || to_hex(i)) loop
      i := i + 1;
    end loop;
    vcn := vcn || to_hex(i);
  end if;
  --now try to automatically determine the list of base relvars
  --that this constraint depends on
  -- I think this means that references involving views are a bit
  --broken...
  --TODO: get to the bottom of this, and fix it if necessary
  if exists(select 1 from base_relvars
      where relvar_name = reftable) then
    bt := array[vtable, reftable];
  else
    bt := array[vtable];
    raise notice 'fk on % ignoring view %', vtable, reftable;
  end if;

  accel := attrs_are_key(reftable, ref_attr);
  -- add constraint
  perform add_constraint_internal(vcn, 'not exists
(select ' || array_to_string(src_attr, ', ') ||
  ' from ' || vtable || '
  except
select ' || array_to_string(ref_attr, ', ') ||
  ' from ' || reftable || ')',
bt, accel);
  if accel then
    perform set_pg_fk(vcn, vtable, src_attr, reftable, ref_attr);
  end if;

end;
$$;


--
-- TOC entry 27 (class 1255 OID 258109)
-- Dependencies: 6 1268
-- Name: add_foreign_key(text, text[], text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_foreign_key(vtable text, src_attr text[], reftable text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform add_foreign_key(vtable, src_attr, reftable, src_attr);
end;
$$;


--
-- TOC entry 28 (class 1255 OID 258110)
-- Dependencies: 1268 6
-- Name: add_foreign_key(text, text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_foreign_key(vtable text, src_attr text, reftable text, ref_attr text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform add_foreign_key(vtable, array[src_attr], reftable, array[ref_attr]);
end;
$$;


--
-- TOC entry 29 (class 1255 OID 258111)
-- Dependencies: 6 1268
-- Name: add_foreign_key(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_foreign_key(vtable text, src_attr text, reftable text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform add_foreign_key(vtable, array[src_attr], reftable, array[src_attr]);
end;
$$;


--
-- TOC entry 147 (class 1255 OID 260310)
-- Dependencies: 6 1268 942
-- Name: add_history_attack(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_attack(t piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  sp record;
  tp pos;
begin
  select into sp ptype,allegiance,tag,x,y from selected_piece
    natural inner join pieces;
  select into tp x,y from pieces where (ptype,allegiance,tag) = t;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x,y,tx,ty)
    values ('attack', sp.ptype, sp.allegiance, sp.tag, sp.x,sp.y, tp.x,tp.y);
end;
$$;


--
-- TOC entry 113 (class 1255 OID 260298)
-- Dependencies: 6 1268
-- Name: add_history_attempt_activate_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_attempt_activate_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  select into w x,y from pieces
    where allegiance=get_current_wizard()
      and ptype = 'wizard';
  insert into action_history_mr (history_name, allegiance, spell_name, x, y)
    values ('attempt_target_spell', get_current_wizard(),
            get_current_wizard_spell(), w.x, w.y);
end;
$$;


--
-- TOC entry 112 (class 1255 OID 260297)
-- Dependencies: 1268 6
-- Name: add_history_attempt_target_spell(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_attempt_target_spell(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  select into w x,y from pieces
    where allegiance=get_current_wizard()
      and ptype = 'wizard';
  insert into action_history_mr (history_name, allegiance, spell_name, x, y, tx, ty)
    values ('attempt_target_spell', get_current_wizard(),
            get_current_wizard_spell(), w.x, w.y, px, py);
end;
$$;


--
-- TOC entry 139 (class 1255 OID 260302)
-- Dependencies: 6 1268 942
-- Name: add_history_chinned(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_chinned(k piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype, allegiance,tag, x, y)
    values ('chinned', k.ptype, k.allegiance, k.tag, w.x, w.y);

end;
$$;


--
-- TOC entry 109 (class 1255 OID 260294)
-- Dependencies: 1268 6
-- Name: add_history_choose_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_choose_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  w := get_current_wizard_pos();
  insert into action_history_mr (history_name, allegiance, spell_name,x,y)
    values ('choose_spell', get_current_wizard(), get_current_wizard_spell(),
            w.x,w.y);
end;
$$;


--
-- TOC entry 144 (class 1255 OID 260307)
-- Dependencies: 6 1268 942
-- Name: add_history_disappear(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_disappear(k piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype,allegiance,tag, x, y)
    values ('disappear', k.ptype,k.allegiance,k.tag, w.x, w.y);
end;
$$;


--
-- TOC entry 146 (class 1255 OID 260309)
-- Dependencies: 6 1268
-- Name: add_history_fly(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_fly(sx integer, sy integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
  k piece_key;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  select into k ptype,allegiance,tag from selected_piece;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x, y, tx, ty)
    values ('fly', k.ptype, k.allegiance, k.tag, w.x,w.y, sx, sy);
end;
$$;


--
-- TOC entry 108 (class 1255 OID 260293)
-- Dependencies: 6 1268
-- Name: add_history_game_drawn(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_game_drawn() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into action_history_mr (history_name)
    values ('game_drawn');
end;
$$;


--
-- TOC entry 107 (class 1255 OID 260292)
-- Dependencies: 6 1268
-- Name: add_history_game_won(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_game_won() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  w := get_current_wizard_pos();
  insert into action_history_mr (history_name, allegiance, x, y)
    values ('game_won', (select allegiance
                         from pieces
                         where ptype='wizard'),
                         w.x, w.y);
end;
$$;


--
-- TOC entry 106 (class 1255 OID 260291)
-- Dependencies: 6 1268
-- Name: add_history_new_game(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_new_game() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into action_history_mr (history_name, num_wizards)
    values ('new_game', (select count(1) from wizards));
end;
$$;


--
-- TOC entry 104 (class 1255 OID 260289)
-- Dependencies: 1268 6
-- Name: add_history_new_turn(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_new_turn() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into action_history_mr (history_name, turn_number)
    values ('new_turn', get_turn_number());
end;
$$;


--
-- TOC entry 148 (class 1255 OID 260311)
-- Dependencies: 6 1268 942
-- Name: add_history_ranged_attack(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_ranged_attack(t piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  sp record;
  tp pos;
begin
  select into sp ptype,allegiance,tag,x,y from selected_piece
    natural inner join pieces;
  select into tp x,y from pieces where (ptype,allegiance,tag) = t;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x,y,tx,ty)
    values ('ranged_attack', sp.ptype, sp.allegiance, sp.tag, sp.x,sp.y, tp.x,tp.y);
end;
$$;


--
-- TOC entry 143 (class 1255 OID 260306)
-- Dependencies: 6 1268 942
-- Name: add_history_recede(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_recede(k piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype,allegiance,tag,x, y)
    values ('recede', k.ptype,k.allegiance,k.tag, w.x, w.y);
end;
$$;


--
-- TOC entry 141 (class 1255 OID 260304)
-- Dependencies: 6 1268
-- Name: add_history_receive_spell(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_receive_spell(pwizard_name text, pspell_name text) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  select into w x,y from pieces
    where allegiance=pwizard_name
      and ptype = 'wizard';
  insert into action_history_mr (history_name, allegiance, spell_name, x, y)
    values ('choose_spell', pwizard_name, pspell_name, w.x, w.y);
end;
$$;


--
-- TOC entry 110 (class 1255 OID 260295)
-- Dependencies: 1268 6
-- Name: add_history_set_imaginary(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_set_imaginary() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into action_history_mr (history_name, allegiance)
    values ('set_imaginary', get_current_wizard());
end;
$$;


--
-- TOC entry 111 (class 1255 OID 260296)
-- Dependencies: 1268 6
-- Name: add_history_set_real(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_set_real() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into action_history_mr (history_name, allegiance)
    values ('set_real', get_current_wizard());
end;
$$;


--
-- TOC entry 366 (class 1255 OID 260163)
-- Dependencies: 6 1268
-- Name: add_history_shrugged_off(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_shrugged_off(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
begin
    select into r ptype, allegiance, tag
      from pieces_on_top where (x,y) = (px,py);
    perform add_history_shrugged_off(r);
end;
$$;


--
-- TOC entry 140 (class 1255 OID 260303)
-- Dependencies: 6 1268 942
-- Name: add_history_shrugged_off(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_shrugged_off(k piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype, allegiance,tag, x, y)
    values ('shrugged_off', k.ptype, k.allegiance, k.tag,w.x, w.y);
end;
$$;


--
-- TOC entry 137 (class 1255 OID 260300)
-- Dependencies: 6 1268
-- Name: add_history_spell_failed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_spell_failed() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into action_history_mr (history_name, allegiance, spell_name)
    values ('spell_failed', get_current_wizard(), get_current_wizard_spell());
end;
$$;


--
-- TOC entry 138 (class 1255 OID 260301)
-- Dependencies: 6 1268
-- Name: add_history_spell_skipped(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_spell_skipped() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into action_history_mr (history_name, allegiance, spell_name)
    values ('spell_skipped', get_current_wizard(), get_current_wizard_spell());
end;
$$;


--
-- TOC entry 114 (class 1255 OID 260299)
-- Dependencies: 1268 6
-- Name: add_history_spell_succeeded(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_spell_succeeded() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into action_history_mr (history_name, allegiance, spell_name)
    values ('spell_succeeded', get_current_wizard(), get_current_wizard_spell());
end;
$$;


--
-- TOC entry 142 (class 1255 OID 260305)
-- Dependencies: 6 1268 942
-- Name: add_history_spread(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_spread(k piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  insert into action_history_mr (history_name, ptype,allegiance,tag, x, y)
    values ('spread', k.ptype,k.allegiance,k.tag, w.x, w.y);
end;
$$;


--
-- TOC entry 145 (class 1255 OID 260308)
-- Dependencies: 6 1268
-- Name: add_history_walked(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_walked(sx integer, sy integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
  k piece_key;
begin
  select into w x,y from pieces where (ptype,allegiance,tag) = k;
  select into k ptype,allegiance,tag from selected_piece;
  insert into action_history_mr (history_name, ptype, allegiance,tag,x, y, tx, ty)
    values ('walked', k.ptype, k.allegiance, k.tag, w.x,w.y, sx, sy);
end;
$$;


--
-- TOC entry 105 (class 1255 OID 260290)
-- Dependencies: 6 1268
-- Name: add_history_wizard_up(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_history_wizard_up() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  w pos;
begin
  w := get_current_wizard_pos();
  insert into action_history_mr (history_name, allegiance, turn_phase, x, y)
    values ('wizard_up', get_current_wizard(), get_turn_phase(), w.x, w.y);
end;
$$;


--
-- TOC entry 23 (class 1255 OID 258105)
-- Dependencies: 6 1268
-- Name: add_key(text, text[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_key(vtable text, attr text[]) RETURNS void
    LANGUAGE plpgsql
    AS $_$
declare
  cn text;
begin
  cn := vtable || '_' ||
    array_to_string(attr, '_') || '_key';
  cn := sort_out_constraint_name(cn, '_key');
  perform add_constraint_internal(cn,
$a$(select count(*) from $a$ || vtable || $a$ ) =
(select count(distinct $a$ || array_to_string(attr, ', ') || $a$)
from $a$ || vtable || $a$)$a$,
     array[vtable], true);
  perform set_pg_unique(cn, vtable, attr);
end;
$_$;


--
-- TOC entry 24 (class 1255 OID 258106)
-- Dependencies: 6 1268
-- Name: add_key(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_key(vtable text, attr text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform add_key(vtable, array[attr]);
end;
$$;


--
-- TOC entry 270 (class 1255 OID 259465)
-- Dependencies: 1268 6
-- Name: adjust_world_alignment(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION adjust_world_alignment() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  abs_change float;
begin
  select into abs_change
    min(abs(get_cast_alignment()) / 2, 2);
  update world_alignment_table
    set world_alignment = world_alignment
      + trunc(abs_change) * sign(get_cast_alignment());
  --get fractional part
  if (random() < abs_change - trunc(abs_change)) then
    update world_alignment_table
      set world_alignment = world_alignment +
        sign(get_cast_alignment());
  end if;
  update cast_alignment_table set cast_alignment = 0;
end;
$$;


--
-- TOC entry 398 (class 1255 OID 260479)
-- Dependencies: 6 1268
-- Name: ai_cast_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION ai_cast_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
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
$$;


--
-- TOC entry 397 (class 1255 OID 260474)
-- Dependencies: 6 1268
-- Name: ai_choose_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION ai_choose_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
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
$$;


--
-- TOC entry 399 (class 1255 OID 260480)
-- Dependencies: 6 1268
-- Name: ai_move_pieces(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION ai_move_pieces() RETURNS void
    LANGUAGE plpgsql
    AS $$
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
$$;


--
-- TOC entry 400 (class 1255 OID 260497)
-- Dependencies: 6 1268
-- Name: ai_move_selected_piece(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION ai_move_selected_piece() RETURNS void
    LANGUAGE plpgsql
    AS $$
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
$$;


--
-- TOC entry 416 (class 1255 OID 261152)
-- Dependencies: 6
-- Name: align_icons(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION align_icons(integer) RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $_$
  select case
    when $1 < 0 then  repeat('*', -$1)
    when $1 > 0 then  repeat('+', $1)
    else '-'
  end as result
$_$;


--
-- TOC entry 370 (class 1255 OID 260176)
-- Dependencies: 6 1268
-- Name: array_contains(anyarray, anyelement); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION array_contains(ar anyarray, e anyelement) RETURNS boolean
    LANGUAGE plpgsql IMMUTABLE
    AS $$
declare
  i int;
begin
  if ar = '{}' then
    return false;
  end if;
  for i in (array_lower(ar,1))..(array_upper(ar,1)) loop
    if ar[i] = e then
      return true;
    end if;
  end loop;
  return false;
end;
$$;


--
-- TOC entry 25 (class 1255 OID 258107)
-- Dependencies: 6
-- Name: attrs_are_key(text, text[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION attrs_are_key(text, text[]) RETURNS boolean
    LANGUAGE sql STABLE
    AS $_$
  select exists
  (select constraint_name, count(attribute_name)
    from base_relvar_keys
    natural inner join base_relvar_key_attributes
    where relvar_name = $1
    group by constraint_name
  intersect
  select constraint_name, count(attribute_name)
    from base_relvar_keys
    natural inner join base_relvar_key_attributes
    where relvar_name = $1
      and attribute_name = any ($2)
    group by constraint_name);
$_$;


--
-- TOC entry 506 (class 1255 OID 261429)
-- Dependencies: 6 1268
-- Name: base_relvar_metadata_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION base_relvar_metadata_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for base_relvar_metadata';
  if not not exists
(select relvar_name from base_relvar_metadata
  except
select relvar_name from base_relvars) then
    raise exception
      'value violates database constraint "base_relvar_metadata_relvar_name_fkey"';
  end if;

--  raise notice 'complete constraint op for base_relvar_metadata';
  return OLD;
end;
$$;


--
-- TOC entry 405 (class 1255 OID 260506)
-- Dependencies: 6 1268
-- Name: board_size_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION board_size_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify board_size;
return null;
end;
$$;


--
-- TOC entry 493 (class 1255 OID 261405)
-- Dependencies: 6 1268
-- Name: board_size_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION board_size_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for board_size';
  if not (select count(*) from board_size) <= 1 then
    raise exception
      'value violates database constraint "board_size_01_tuple"';
  end if;
  if not  not exists (select 1 from cursor_position
  cross join board_size
  where x >= width or y >= height) then
    raise exception
      'value violates database constraint "cursor_position_coordinates_valid"';
  end if;
  if not  not exists(select 1 from pieces
  cross join board_size
  where x >= width or y >= height) then
    raise exception
      'value violates database constraint "piece_coordinates_valid"';
  end if;

--  raise notice 'complete constraint op for board_size';
  return OLD;
end;
$$;


--
-- TOC entry 494 (class 1255 OID 261407)
-- Dependencies: 6 1268
-- Name: cast_alignment_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_alignment_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for cast_alignment_table';
  if not ((get_turn_phase() = 'cast') or
  not exists(select 1 from cast_alignment_table)) then
    raise exception
      'value violates database constraint "cast_alignment_empty"';
  end if;
  if not (select count(*) from cast_alignment_table) <= 1 then
    raise exception
      'value violates database constraint "cast_alignment_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for cast_alignment_table';
  return OLD;
end;
$$;


--
-- TOC entry 344 (class 1255 OID 260125)
-- Dependencies: 6 1268
-- Name: cast_ballistic_spell(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_ballistic_spell(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
begin
  --todo: should factor in the attack strength?
  if not check_random_success('resist',
      (select physical_defense * 10
       from pieces_on_top_view
       where (x,y) = (px,py))) then
    --need to added the chinned history before the
    --piece is killed or we loose the allegiance
    --need to add the spell successful before the
     --chinned history or the order is wrong
    perform add_history_spell_succeeded();
    perform add_chinned_history(px,py);
    select into r ptype,allegiance,tag
      from pieces_on_top
      where (x,y) = (px,py);
    perform kill_piece(r);
  else
    --spell didn't do any damage
    perform add_history_spell_succeeded();
    perform add_history_shrugged_off(px, py);
  end if;
end;
$$;


--
-- TOC entry 343 (class 1255 OID 260124)
-- Dependencies: 6 1268
-- Name: cast_decree_spell(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_decree_spell(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
  m int;

begin
  --if cast on wizard then success destroys all wizards objects
  --else if cast on monster then success destroys monster
  --get target magic defense
  --todo: should this take into account the spell/attack?
  m := (select magic_defense
        from pieces_on_top
        natural inner join magic_attackable_pieces
        where (x,y)=(px,py));

  if not check_random_success('resist', m * 10) then
    select into r ptype, allegiance, tag
      from pieces_on_top
      where (x,y)=(px,py);
    if r.ptype = 'wizard' then
      for r in select ptype, allegiance, tag from pieces
        where allegiance = r.allegiance and ptype != 'wizard' loop
        perform disintegrate(r);
      end loop;
    else
      perform disintegrate(r);
    end if;
    perform add_history_spell_succeeded();
  end if;
end;
$$;


--
-- TOC entry 347 (class 1255 OID 260128)
-- Dependencies: 1268 6
-- Name: cast_disbelieve(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_disbelieve(px integer, py integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
begin
  if not (select imaginary from pieces_on_top_view where (x,y) = (px,py)) then
    perform add_history_shrugged_off(px, py);
    perform action_cast_failed();
    return false;
  end if;
  select into r ptype, allegiance, tag, imaginary
    from pieces_on_top_view where (x,y) = (px,py);

  perform add_history_spell_succeeded();
  perform add_chinned_history(px, py);
  perform disintegrate(r);
  return true;
end;
$$;


--
-- TOC entry 341 (class 1255 OID 260122)
-- Dependencies: 6 1268
-- Name: cast_lawchaos(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_lawchaos() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  --don't need to do anything, the effect is
  --restricted to the alignment effect which
  --is handled in the same place for all spells
  perform add_history_spell_succeeded();
end;
$$;


--
-- TOC entry 354 (class 1255 OID 260151)
-- Dependencies: 1268 6
-- Name: cast_magic_wood(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_magic_wood() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  casted int;
  range int;
  pos_in_square int;
  max_pos_in_square int;
  wx int;
  wy int;
  r record;
  s text;
begin
  casted := 0;
  range := 1;
  pos_in_square := 0;
  max_pos_in_square := 7;
  wx := (select x from pieces
     where ptype = 'wizard'
     and allegiance = get_current_wizard());
  wy := (select y from pieces
     where ptype = 'wizard'
     and allegiance = get_current_wizard());

  while (casted < 8 and range <= 15) loop
    select into r * from get_square_range(wx, wy, range)
      where index = pos_in_square;
--    s := 'checking ' || ip.x || ',' || ip.y;
    if exists(select 1 from cast_magic_wood_available_squares
       where (x,y) = (r.x, r.y)) then
       insert into cast_magic_wood_squares(x,y) values (r.x, r.y);
       casted := casted + 1;
    else
      null;
    end if;
    if pos_in_square = max_pos_in_square then
      range := range + 1;
      pos_in_square = 0;
      max_pos_in_square = (select max(index) from get_square_range(0,0,range));
    else
      pos_in_square := pos_in_square + 1;
    end if;
  end loop;
  for r in select * from cast_magic_wood_squares loop
    perform create_object(
      'magic_tree', get_current_wizard(), r.x, r.y);
  end loop;
  delete from cast_magic_wood_squares;
  perform add_history_spell_succeeded();
end;
$$;


--
-- TOC entry 349 (class 1255 OID 260130)
-- Dependencies: 1268 6
-- Name: cast_monster_spell(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_monster_spell(x integer, y integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform create_monster(
    (select ptype from current_wizard_spell
      natural inner join summon_spells),
    get_current_wizard(), x, y, coalesce((
      select imaginary
      from wizard_spell_choices_imaginary
      where wizard_name = get_current_wizard()),false));
  perform add_history_spell_succeeded();
end;
$$;


--
-- TOC entry 348 (class 1255 OID 260129)
-- Dependencies: 1268 6
-- Name: cast_object_spell(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_object_spell(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform create_object(
    (select ptype from current_wizard_spell
     natural inner join summon_spells),
     get_current_wizard(), px, py);
  perform add_history_spell_succeeded();
end;
$$;


--
-- TOC entry 345 (class 1255 OID 260126)
-- Dependencies: 6 1268
-- Name: cast_raise_dead(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_raise_dead(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
begin
  --turn dead creature on square to live undead
  select into r ptype,allegiance,tag
    from pieces_on_top
    where (x,y) = (px,py);
  update pieces
    set allegiance = get_current_wizard(),
        tag = get_next_tag(r.ptype,get_current_wizard())
    where (ptype,allegiance,tag)::piece_key = r
    returning tag into r.tag;
  insert into crimes_against_nature (ptype,allegiance,tag)
    values (r.ptype,get_current_wizard(),r.tag);
  perform add_history_spell_succeeded();
end;
$$;


--
-- TOC entry 346 (class 1255 OID 260127)
-- Dependencies: 1268 6
-- Name: cast_subversion(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_subversion(px integer, py integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
begin
    if check_random_success('resist',
      (select magic_defense * 10
        from pieces_on_top_view
        where (x,y) = (px, py))) then
    perform add_history_shrugged_off(px, py);
    perform action_cast_failed();
    return false;
  end if;
  select into r ptype,allegiance,tag from pieces_on_top
    where (x,y) = (px, py);
  update pieces
    set allegiance = get_current_wizard(),
        tag = get_next_tag(r.ptype,get_current_wizard())
    where (ptype,allegiance,tag)::piece_key = r;
  perform add_chinned_history(px, py);
  perform add_history_spell_succeeded();
  return true;
end;
$$;


--
-- TOC entry 161 (class 1255 OID 260522)
-- Dependencies: 1268 6
-- Name: cast_success_checked_table_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_success_checked_table_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify cast_success_checked_table;
return null;
end;
$$;


--
-- TOC entry 500 (class 1255 OID 261417)
-- Dependencies: 6 1268
-- Name: cast_success_checked_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_success_checked_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for cast_success_checked_table';
  if not 
  ((select turn_phase = 'cast' from turn_phase_table)
  or not exists(select 1 from cast_success_checked_table))
 then
    raise exception
      'value violates database constraint "cast_checked_cast_only"';
  end if;
  if not (select count(*) from cast_success_checked_table) <= 1 then
    raise exception
      'value violates database constraint "cast_success_checked_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for cast_success_checked_table';
  return OLD;
end;
$$;


--
-- TOC entry 342 (class 1255 OID 260123)
-- Dependencies: 1268 6
-- Name: cast_turmoil(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cast_turmoil() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r record;
  s record;
  tx int;
  ty int;
begin
  --algorithm: similar to the original chaos I think
  -- run through each square in turn, starting at top
  --left across top then along each row till you get to the
  --bottom right
  --move all the pieces in a square to a new random empty
  --square at the time of the move (so if pieces on the
  --same square as each other before turmoil is cast
  --will still be on the same square as each other
  --afterwoods. (since we do one square at a time we
  -- won't get exact random distribution).

  -- the for loop does actually save the full query
  -- at the start so updates in the for loop are not
  -- seen by the for loop so there is no risk of a
  -- piece teleporting twice
  for r in select x,y from pieces_on_top order by x,y loop
    select x,y into tx,ty from empty_squares order by random() limit 1;
    update pieces set x = tx, y = ty
      where (x,y) = (r.x,r.y);
    --add histories
/*    for s in select ptype, allegiance, tag
      from pieces where x = tx and y = ty loop

  perform einsert(array['action_history',
    'action_history_piece_teleport'],
    array['history_name', 'ptype', 'allegiance', 'tag'],
    array['piece teleport', s.ptype, s.allegiance, s.tag::text]);
    end loop;*/
  end loop;
  perform add_history_spell_succeeded();
end;
$$;


--
-- TOC entry 423 (class 1255 OID 261153)
-- Dependencies: 6 1268
-- Name: chance_colour(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION chance_colour(chance integer) RETURNS text
    LANGUAGE plpgsql IMMUTABLE
    AS $$
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
$$;


--
-- TOC entry 510 (class 1255 OID 261434)
-- Dependencies: 1268 6
-- Name: check_base_relvar_metadata_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_base_relvar_metadata_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on base_relvar_metadata violates transition constraint base_relvar_metadata_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 511 (class 1255 OID 261436)
-- Dependencies: 6 1268
-- Name: check_base_relvar_metadata_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_base_relvar_metadata_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on base_relvar_metadata violates transition constraint base_relvar_metadata_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 509 (class 1255 OID 261432)
-- Dependencies: 1268 6
-- Name: check_base_relvar_metadata_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_base_relvar_metadata_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on base_relvar_metadata violates transition constraint base_relvar_metadata_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 300 (class 1255 OID 260046)
-- Dependencies: 6 1268
-- Name: check_can_run_action(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_can_run_action(action_name text) RETURNS void
    LANGUAGE plpgsql STABLE
    AS $$
begin
  if not exists (select 1 from valid_activate_actions
     where action = action_name) then
    raise exception 'cannot run % here', action_name;
  end if;
end;
$$;


--
-- TOC entry 301 (class 1255 OID 260047)
-- Dependencies: 6 1268
-- Name: check_can_run_action(text, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_can_run_action(action_name text, px integer, py integer) RETURNS void
    LANGUAGE plpgsql STABLE
    AS $$
begin
  if not exists (select 1 from valid_target_actions
     where action = action_name and (x,y) = (px,py)) then
    raise exception 'cannot run % on %,% here', action_name, px, py;
  end if;
end;
$$;


--
-- TOC entry 61 (class 1255 OID 258276)
-- Dependencies: 6 1268
-- Name: check_code_module_membership(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_code_module_membership() RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
  success boolean := true;
  r record;
begin
  --returns false if any objects with no package
  --also list problems them using raise notice
  for r in select object_name, object_type from all_database_objects
      except select object_name, object_type
      from all_module_objects loop
    success := false;
    raise notice '% % - no module', r.object_type, r.object_name;
  end loop;
  return success;
end;
$$;


--
-- TOC entry 65 (class 1255 OID 258278)
-- Dependencies: 6 1268
-- Name: check_code_no_nullable_table_columns(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_code_no_nullable_table_columns() RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
declare
  r record;
  success boolean;
begin
  success := true;
  --check every table is tagged with one of
  --readonly, data, argument, widget_state
  for r in select table_name, column_name
      from information_schema.columns
      inner join base_relvars
        on (table_name = relvar_name)
      where is_nullable='YES'
        --ignore touch multirelations
        and not exists(select 1 from regexp_matches(table_name, '_mr$')) loop
    success := false;
    raise notice '%.% - nullable column', r.table_name, r.column_name;
  end loop;
  return success;
end;
$_$;


--
-- TOC entry 37 (class 1255 OID 258171)
-- Dependencies: 6 1268
-- Name: check_code_slow_si_objects_constraints(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_code_slow_si_objects_constraints() RETURNS boolean
    LANGUAGE plpgsql
    AS $$
begin
  if not exists
     (select object_name, object_type from system_implementation_objects
      except
      select object_name, object_type from all_database_objects) then
    return true;
  else
    return false;
  end if;
end;
$$;


--
-- TOC entry 71 (class 1255 OID 258307)
-- Dependencies: 6 1268
-- Name: check_code_some_tags(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_code_some_tags() RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
  r record;
  success boolean;
begin
  success := true;
  for r in select object_name from chaos_base_relvars
    except select relvar_name from base_relvar_metadata loop
    success := false;
      raise notice
        'table % is not tagged with one of readonly, data, stack',
        r.object_name;
  end loop;
  return success;
end;
$$;


--
-- TOC entry 525 (class 1255 OID 261464)
-- Dependencies: 1268 6
-- Name: check_colours_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_colours_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on colours violates transition constraint colours_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 526 (class 1255 OID 261466)
-- Dependencies: 1268 6
-- Name: check_colours_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_colours_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on colours violates transition constraint colours_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 524 (class 1255 OID 261462)
-- Dependencies: 1268 6
-- Name: check_colours_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_colours_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on colours violates transition constraint colours_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 468 (class 1255 OID 261349)
-- Dependencies: 1268 6
-- Name: check_con_action_client_new_game_argument_colour_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_action_client_new_game_argument_colour_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from action_client_new_game_argument ) =
(select count(distinct colour)
from action_client_new_game_argument);
end;
$$;


--
-- TOC entry 465 (class 1255 OID 261340)
-- Dependencies: 6 1268
-- Name: check_con_action_client_new_game_argument_place_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_action_client_new_game_argument_place_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from action_client_new_game_argument ) =
(select count(distinct place)
from action_client_new_game_argument);
end;
$$;


--
-- TOC entry 469 (class 1255 OID 261352)
-- Dependencies: 6 1268
-- Name: check_con_action_client_new_game_argument_sprite_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_action_client_new_game_argument_sprite_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select sprite from action_client_new_game_argument
  except
select sprite from sprites);
end;
$$;


--
-- TOC entry 467 (class 1255 OID 261346)
-- Dependencies: 1268 6
-- Name: check_con_action_client_new_game_argument_sprite_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_action_client_new_game_argument_sprite_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from action_client_new_game_argument ) =
(select count(distinct sprite)
from action_client_new_game_argument);
end;
$$;


--
-- TOC entry 466 (class 1255 OID 261343)
-- Dependencies: 6 1268
-- Name: check_con_action_client_new_game_argument_wizard_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_action_client_new_game_argument_wizard_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from action_client_new_game_argument ) =
(select count(distinct wizard_name)
from action_client_new_game_argument);
end;
$$;


--
-- TOC entry 470 (class 1255 OID 261358)
-- Dependencies: 1268 6
-- Name: check_con_action_client_new_game_place_valid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_action_client_new_game_place_valid() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from action_client_new_game_argument
  where place >=
  (select count(*) from action_client_new_game_argument)) = 0;
end;
$$;


--
-- TOC entry 374 (class 1255 OID 260285)
-- Dependencies: 1268 6
-- Name: check_con_action_history_mr_id_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_action_history_mr_id_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from action_history_mr ) =
(select count(distinct id)
from action_history_mr);
end;
$$;


--
-- TOC entry 391 (class 1255 OID 260386)
-- Dependencies: 6 1268
-- Name: check_con_action_new_game_argument_place_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_action_new_game_argument_place_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from action_new_game_argument ) =
(select count(distinct place)
from action_new_game_argument);
end;
$$;


--
-- TOC entry 393 (class 1255 OID 260392)
-- Dependencies: 6 1268
-- Name: check_con_action_new_game_argument_place_valid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_action_new_game_argument_place_valid() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from action_new_game_argument
    where place >= (select count(*) from action_new_game_argument)) = 0;
end;
$$;


--
-- TOC entry 392 (class 1255 OID 260389)
-- Dependencies: 6 1268
-- Name: check_con_action_new_game_argument_wizard_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_action_new_game_argument_wizard_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from action_new_game_argument ) =
(select count(distinct wizard_name)
from action_new_game_argument);
end;
$$;


--
-- TOC entry 58 (class 1255 OID 258260)
-- Dependencies: 6 1268
-- Name: check_con_all_module_objects_module_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_all_module_objects_module_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select module_name from all_module_objects
  except
select module_name from modules);
end;
$$;


--
-- TOC entry 57 (class 1255 OID 258257)
-- Dependencies: 6 1268
-- Name: check_con_all_module_objects_object_name_object_type_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_all_module_objects_object_name_object_type_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from all_module_objects ) =
(select count(distinct object_name, object_type)
from all_module_objects);
end;
$$;


--
-- TOC entry 69 (class 1255 OID 258291)
-- Dependencies: 6 1268
-- Name: check_con_base_relvar_metadata_relvar_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_base_relvar_metadata_relvar_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select relvar_name from base_relvar_metadata
  except
select relvar_name from base_relvars);
end;
$$;


--
-- TOC entry 68 (class 1255 OID 258288)
-- Dependencies: 6 1268
-- Name: check_con_base_relvar_metadata_relvar_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_base_relvar_metadata_relvar_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from base_relvar_metadata ) =
(select count(distinct relvar_name)
from base_relvar_metadata);
end;
$$;


--
-- TOC entry 197 (class 1255 OID 260833)
-- Dependencies: 1268 6
-- Name: check_con_board_beam_effects_id_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_board_beam_effects_id_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from board_beam_effects ) =
(select count(distinct id)
from board_beam_effects);
end;
$$;


--
-- TOC entry 80 (class 1255 OID 258396)
-- Dependencies: 6 1268
-- Name: check_con_board_size_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_board_size_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from board_size) <= 1;
end;
$$;


--
-- TOC entry 79 (class 1255 OID 258393)
-- Dependencies: 6 1268
-- Name: check_con_board_size_width_height_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_board_size_width_height_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from board_size ) =
(select count(distinct width, height)
from board_size);
end;
$$;


--
-- TOC entry 198 (class 1255 OID 260845)
-- Dependencies: 6 1268
-- Name: check_con_board_sound_effects_id_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_board_sound_effects_id_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from board_sound_effects ) =
(select count(distinct id)
from board_sound_effects);
end;
$$;


--
-- TOC entry 196 (class 1255 OID 260821)
-- Dependencies: 6 1268
-- Name: check_con_board_square_effects_id_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_board_square_effects_id_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from board_square_effects ) =
(select count(distinct id)
from board_square_effects);
end;
$$;


--
-- TOC entry 258 (class 1255 OID 259420)
-- Dependencies: 6 1268
-- Name: check_con_cast_alignment_empty(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_cast_alignment_empty() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return ((get_turn_phase() = 'cast') or
  not exists(select 1 from cast_alignment_table));
end;
$$;


--
-- TOC entry 226 (class 1255 OID 259374)
-- Dependencies: 1268 6
-- Name: check_con_cast_alignment_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_cast_alignment_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from cast_alignment_table) <= 1;
end;
$$;


--
-- TOC entry 225 (class 1255 OID 259371)
-- Dependencies: 6 1268
-- Name: check_con_cast_alignment_table_cast_alignment_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_cast_alignment_table_cast_alignment_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from cast_alignment_table ) =
(select count(distinct cast_alignment)
from cast_alignment_table);
end;
$$;


--
-- TOC entry 224 (class 1255 OID 259325)
-- Dependencies: 6 1268
-- Name: check_con_cast_checked_cast_only(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_cast_checked_cast_only() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return 
  ((select turn_phase = 'cast' from turn_phase_table)
  or not exists(select 1 from cast_success_checked_table))
;
end;
$$;


--
-- TOC entry 222 (class 1255 OID 259281)
-- Dependencies: 1268 6
-- Name: check_con_cast_success_checked_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_cast_success_checked_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from cast_success_checked_table) <= 1;
end;
$$;


--
-- TOC entry 221 (class 1255 OID 259278)
-- Dependencies: 6 1268
-- Name: check_con_cast_success_checked_table_cast_success_checked_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_cast_success_checked_table_cast_success_checked_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from cast_success_checked_table ) =
(select count(distinct cast_success_checked)
from cast_success_checked_table);
end;
$$;


--
-- TOC entry 174 (class 1255 OID 259141)
-- Dependencies: 1268 6
-- Name: check_con_chosen_spell_phase_valid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_chosen_spell_phase_valid() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return 
((select in_next_phase_hack from in_next_phase_hack_table) or
(((select turn_phase='choose' from turn_phase_table) and
 (select max(place) from wizard_spell_choices
   natural inner join live_wizards) <=
 (select place from live_wizards
   inner join current_wizard_table
     on wizard_name = current_wizard))
or
((select turn_phase='cast' from turn_phase_table) and
 (select min(place) from wizard_spell_choices
    natural inner join live_wizards) >=
  (select place from live_wizards
    inner join current_wizard_table
      on wizard_name = current_wizard))
or not exists(select 1 from wizard_spell_choices)
));
end;
$$;


--
-- TOC entry 179 (class 1255 OID 260558)
-- Dependencies: 6 1268
-- Name: check_con_colours_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_colours_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from colours ) =
(select count(distinct name)
from colours);
end;
$$;


--
-- TOC entry 46 (class 1255 OID 258202)
-- Dependencies: 6 1268
-- Name: check_con_con_pg_constraint_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_con_pg_constraint_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select constraint_name from con_pg
  except
select constraint_name from database_constraints);
end;
$$;


--
-- TOC entry 47 (class 1255 OID 258208)
-- Dependencies: 6 1268
-- Name: check_con_con_pg_constraint_name_fkey1(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_con_pg_constraint_name_fkey1() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select constraint_name from con_pg
  except
select constraint_name from (select constraint_name from check_pg union
   select constraint_name from key_pg union
   select constraint_name from fk_pg) as x);
end;
$$;


--
-- TOC entry 45 (class 1255 OID 258199)
-- Dependencies: 6 1268
-- Name: check_con_con_pg_constraint_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_con_pg_constraint_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from con_pg ) =
(select count(distinct constraint_name)
from con_pg);
end;
$$;


--
-- TOC entry 95 (class 1255 OID 258771)
-- Dependencies: 1268 6
-- Name: check_con_creating_new_game_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_creating_new_game_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from creating_new_game_table) <= 1;
end;
$$;


--
-- TOC entry 94 (class 1255 OID 258768)
-- Dependencies: 6 1268
-- Name: check_con_creating_new_game_table_creating_new_game_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_creating_new_game_table_creating_new_game_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from creating_new_game_table ) =
(select count(distinct creating_new_game)
from creating_new_game_table);
end;
$$;


--
-- TOC entry 123 (class 1255 OID 258662)
-- Dependencies: 6 1268
-- Name: check_con_crimes_against_nature_ptype_allegiance_tag_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_crimes_against_nature_ptype_allegiance_tag_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select ptype, allegiance, tag from crimes_against_nature
  except
select ptype, allegiance, tag from pieces);
end;
$$;


--
-- TOC entry 122 (class 1255 OID 258659)
-- Dependencies: 6 1268
-- Name: check_con_crimes_against_nature_ptype_allegiance_tag_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_crimes_against_nature_ptype_allegiance_tag_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from crimes_against_nature ) =
(select count(distinct ptype, allegiance, tag)
from crimes_against_nature);
end;
$$;


--
-- TOC entry 124 (class 1255 OID 258668)
-- Dependencies: 6 1268
-- Name: check_con_crimes_against_nature_ptype_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_crimes_against_nature_ptype_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select ptype from crimes_against_nature
  except
select ptype from monster_prototypes);
end;
$$;


--
-- TOC entry 437 (class 1255 OID 260943)
-- Dependencies: 6 1268
-- Name: check_con_current_effects_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_current_effects_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from current_effects) <= 1;
end;
$$;


--
-- TOC entry 63 (class 1255 OID 258906)
-- Dependencies: 1268 6
-- Name: check_con_current_wizard_must_be_alive(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_current_wizard_must_be_alive() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select not expired from current_wizard_table
     inner join wizards on current_wizard = wizard_name);
end;
$$;


--
-- TOC entry 132 (class 1255 OID 258860)
-- Dependencies: 1268 6
-- Name: check_con_current_wizard_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_current_wizard_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from current_wizard_table) <= 1;
end;
$$;


--
-- TOC entry 153 (class 1255 OID 258894)
-- Dependencies: 1268 6
-- Name: check_con_current_wizard_table_current_wizard_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_current_wizard_table_current_wizard_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select current_wizard from current_wizard_table
  except
select wizard_name from wizards);
end;
$$;


--
-- TOC entry 131 (class 1255 OID 258857)
-- Dependencies: 6 1268
-- Name: check_con_current_wizard_table_current_wizard_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_current_wizard_table_current_wizard_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from current_wizard_table ) =
(select count(distinct current_wizard)
from current_wizard_table);
end;
$$;


--
-- TOC entry 417 (class 1255 OID 260699)
-- Dependencies: 6 1268
-- Name: check_con_cursor_position_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_cursor_position_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from cursor_position) <= 1;
end;
$$;


--
-- TOC entry 192 (class 1255 OID 260636)
-- Dependencies: 6 1268
-- Name: check_con_cursor_position_coordinates_valid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_cursor_position_coordinates_valid() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return  not exists (select 1 from cursor_position
  cross join board_size
  where x >= width or y >= height);
end;
$$;


--
-- TOC entry 38 (class 1255 OID 258172)
-- Dependencies: 1268 6
-- Name: check_con_database_constraints_constraint_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_database_constraints_constraint_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from database_constraints ) =
(select count(distinct constraint_name)
from database_constraints);
end;
$$;


--
-- TOC entry 41 (class 1255 OID 258181)
-- Dependencies: 6 1268
-- Name: check_con_dbcon_ops_constraint_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dbcon_ops_constraint_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select constraint_name from dbcon_ops
  except
select constraint_name from database_constraints);
end;
$$;


--
-- TOC entry 39 (class 1255 OID 258175)
-- Dependencies: 6 1268
-- Name: check_con_dbcon_ops_constraint_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dbcon_ops_constraint_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from dbcon_ops ) =
(select count(distinct constraint_name)
from dbcon_ops);
end;
$$;


--
-- TOC entry 40 (class 1255 OID 258178)
-- Dependencies: 6 1268
-- Name: check_con_dbcon_ops_operator_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dbcon_ops_operator_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from dbcon_ops ) =
(select count(distinct operator_name)
from dbcon_ops);
end;
$$;


--
-- TOC entry 43 (class 1255 OID 258190)
-- Dependencies: 6 1268
-- Name: check_con_dbcon_relvars_constraint_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dbcon_relvars_constraint_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select constraint_name from dbcon_relvars
  except
select constraint_name from database_constraints);
end;
$$;


--
-- TOC entry 42 (class 1255 OID 258187)
-- Dependencies: 6 1268
-- Name: check_con_dbcon_relvars_constraint_name_relvar_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dbcon_relvars_constraint_name_relvar_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from dbcon_relvars ) =
(select count(distinct constraint_name, relvar_name)
from dbcon_relvars);
end;
$$;


--
-- TOC entry 44 (class 1255 OID 258196)
-- Dependencies: 1268 6
-- Name: check_con_dbcon_relvars_relvar_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dbcon_relvars_relvar_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select relvar_name from dbcon_relvars
  except
select relvar_name from base_relvars);
end;
$$;


--
-- TOC entry 49 (class 1255 OID 258216)
-- Dependencies: 6 1268
-- Name: check_con_dbcon_trigger_ops_operator_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dbcon_trigger_ops_operator_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select operator_name from dbcon_trigger_ops
  except
select operator_name from operators);
end;
$$;


--
-- TOC entry 48 (class 1255 OID 258213)
-- Dependencies: 1268 6
-- Name: check_con_dbcon_trigger_ops_operator_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dbcon_trigger_ops_operator_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from dbcon_trigger_ops ) =
(select count(distinct operator_name)
from dbcon_trigger_ops);
end;
$$;


--
-- TOC entry 50 (class 1255 OID 258223)
-- Dependencies: 6 1268
-- Name: check_con_dbcon_triggers_trigger_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dbcon_triggers_trigger_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from dbcon_triggers ) =
(select count(distinct trigger_name)
from dbcon_triggers);
end;
$$;


--
-- TOC entry 51 (class 1255 OID 258226)
-- Dependencies: 1268 6
-- Name: check_con_dbcon_triggers_trigger_name_relvar_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dbcon_triggers_trigger_name_relvar_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select trigger_name, relvar_name from dbcon_triggers
  except
select trigger_name, relvar_name from triggers);
end;
$$;


--
-- TOC entry 115 (class 1255 OID 258588)
-- Dependencies: 6 1268
-- Name: check_con_dead_wizard_army_empty(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dead_wizard_army_empty() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return  not exists(select 1 from pieces
    inner join wizards
    on (allegiance = wizard_name)
    where expired = true);
end;
$$;


--
-- TOC entry 120 (class 1255 OID 259010)
-- Dependencies: 6 1268
-- Name: check_con_dead_wizard_no_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_dead_wizard_no_spell() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return  not exists(select 1 from wizard_spell_choices_mr
    natural inner join wizards
    where expired = true);
end;
$$;


--
-- TOC entry 372 (class 1255 OID 260187)
-- Dependencies: 6 1268
-- Name: check_con_disable_spreading_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_disable_spreading_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from disable_spreading_table) <= 1;
end;
$$;


--
-- TOC entry 371 (class 1255 OID 260184)
-- Dependencies: 1268 6
-- Name: check_con_disable_spreading_table_disable_spreading_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_disable_spreading_table_disable_spreading_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from disable_spreading_table ) =
(select count(distinct disable_spreading)
from disable_spreading_table);
end;
$$;


--
-- TOC entry 287 (class 1255 OID 259784)
-- Dependencies: 6 1268
-- Name: check_con_game_completed_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_game_completed_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from game_completed_table) <= 1;
end;
$$;


--
-- TOC entry 286 (class 1255 OID 259781)
-- Dependencies: 6 1268
-- Name: check_con_game_completed_table_game_completed_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_game_completed_table_game_completed_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from game_completed_table ) =
(select count(distinct game_completed)
from game_completed_table);
end;
$$;


--
-- TOC entry 289 (class 1255 OID 259840)
-- Dependencies: 1268 6
-- Name: check_con_game_completed_wizards(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_game_completed_wizards() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (not exists(select 1 from game_completed_table)
           or (select count(1) <= 1 from live_wizards));
end;
$$;


--
-- TOC entry 203 (class 1255 OID 260864)
-- Dependencies: 1268 6
-- Name: check_con_history_no_visuals_history_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_history_no_visuals_history_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from history_no_visuals ) =
(select count(distinct history_name)
from history_no_visuals);
end;
$$;


--
-- TOC entry 202 (class 1255 OID 260855)
-- Dependencies: 6 1268
-- Name: check_con_history_sounds_history_name_sound_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_history_sounds_history_name_sound_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from history_sounds ) =
(select count(distinct history_name, sound_name)
from history_sounds);
end;
$$;


--
-- TOC entry 117 (class 1255 OID 258624)
-- Dependencies: 6 1268
-- Name: check_con_imaginary_pieces_ptype_allegiance_tag_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_imaginary_pieces_ptype_allegiance_tag_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select ptype, allegiance, tag from imaginary_pieces
  except
select ptype, allegiance, tag from pieces);
end;
$$;


--
-- TOC entry 116 (class 1255 OID 258621)
-- Dependencies: 6 1268
-- Name: check_con_imaginary_pieces_ptype_allegiance_tag_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_imaginary_pieces_ptype_allegiance_tag_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from imaginary_pieces ) =
(select count(distinct ptype, allegiance, tag)
from imaginary_pieces);
end;
$$;


--
-- TOC entry 118 (class 1255 OID 258630)
-- Dependencies: 1268 6
-- Name: check_con_imaginary_pieces_ptype_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_imaginary_pieces_ptype_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select ptype from imaginary_pieces
  except
select ptype from monster_prototypes);
end;
$$;


--
-- TOC entry 91 (class 1255 OID 258737)
-- Dependencies: 1268 6
-- Name: check_con_in_next_phase_hack_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_in_next_phase_hack_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from in_next_phase_hack_table) <= 1;
end;
$$;


--
-- TOC entry 125 (class 1255 OID 258734)
-- Dependencies: 1268 6
-- Name: check_con_in_next_phase_hack_table_in_next_phase_hack_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_in_next_phase_hack_table_in_next_phase_hack_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from in_next_phase_hack_table ) =
(select count(distinct in_next_phase_hack)
from in_next_phase_hack_table);
end;
$$;


--
-- TOC entry 188 (class 1255 OID 260609)
-- Dependencies: 1268 6
-- Name: check_con_init_wizard_display_info_argument_colour_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_init_wizard_display_info_argument_colour_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from init_wizard_display_info_argument ) =
(select count(distinct colour)
from init_wizard_display_info_argument);
end;
$$;


--
-- TOC entry 190 (class 1255 OID 260618)
-- Dependencies: 1268 6
-- Name: check_con_init_wizard_display_info_argument_sprite_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_init_wizard_display_info_argument_sprite_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select sprite from init_wizard_display_info_argument
  except
select sprite from sprites);
end;
$$;


--
-- TOC entry 187 (class 1255 OID 260606)
-- Dependencies: 6 1268
-- Name: check_con_init_wizard_display_info_argument_sprite_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_init_wizard_display_info_argument_sprite_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from init_wizard_display_info_argument ) =
(select count(distinct sprite)
from init_wizard_display_info_argument);
end;
$$;


--
-- TOC entry 189 (class 1255 OID 260612)
-- Dependencies: 1268 6
-- Name: check_con_init_wizard_display_info_argument_wizard_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_init_wizard_display_info_argument_wizard_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select wizard_name from init_wizard_display_info_argument
  except
select wizard_name from wizards);
end;
$$;


--
-- TOC entry 186 (class 1255 OID 260603)
-- Dependencies: 1268 6
-- Name: check_con_init_wizard_display_info_argument_wizard_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_init_wizard_display_info_argument_wizard_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from init_wizard_display_info_argument ) =
(select count(distinct wizard_name)
from init_wizard_display_info_argument);
end;
$$;


--
-- TOC entry 449 (class 1255 OID 261308)
-- Dependencies: 1268 6
-- Name: check_con_key_control_settings_key_code_action_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_key_control_settings_key_code_action_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from key_control_settings ) =
(select count(distinct key_code, action_name)
from key_control_settings);
end;
$$;


--
-- TOC entry 205 (class 1255 OID 260873)
-- Dependencies: 6 1268
-- Name: check_con_last_history_effect_id_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_last_history_effect_id_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from last_history_effect_id_table) <= 1;
end;
$$;


--
-- TOC entry 204 (class 1255 OID 260870)
-- Dependencies: 6 1268
-- Name: check_con_last_history_effect_id_table_last_history_effect__key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_last_history_effect_id_table_last_history_effect__key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from last_history_effect_id_table ) =
(select count(distinct last_history_effect_id)
from last_history_effect_id_table);
end;
$$;


--
-- TOC entry 56 (class 1255 OID 258248)
-- Dependencies: 6 1268
-- Name: check_con_modules_module_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_modules_module_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from modules ) =
(select count(distinct module_name)
from modules);
end;
$$;


--
-- TOC entry 432 (class 1255 OID 261210)
-- Dependencies: 6 1268
-- Name: check_con_new_game_widget_state_colour_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_new_game_widget_state_colour_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from new_game_widget_state ) =
(select count(distinct colour)
from new_game_widget_state);
end;
$$;


--
-- TOC entry 429 (class 1255 OID 261201)
-- Dependencies: 1268 6
-- Name: check_con_new_game_widget_state_line_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_new_game_widget_state_line_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from new_game_widget_state ) =
(select count(distinct line)
from new_game_widget_state);
end;
$$;


--
-- TOC entry 434 (class 1255 OID 261219)
-- Dependencies: 6 1268
-- Name: check_con_new_game_widget_state_line_valid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_new_game_widget_state_line_valid() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return  not exists(select 1 from new_game_widget_state
  where line >= 8);
end;
$$;


--
-- TOC entry 433 (class 1255 OID 261213)
-- Dependencies: 6 1268
-- Name: check_con_new_game_widget_state_sprite_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_new_game_widget_state_sprite_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select sprite from new_game_widget_state
  except
select sprite from sprites);
end;
$$;


--
-- TOC entry 431 (class 1255 OID 261207)
-- Dependencies: 1268 6
-- Name: check_con_new_game_widget_state_sprite_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_new_game_widget_state_sprite_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from new_game_widget_state ) =
(select count(distinct sprite)
from new_game_widget_state);
end;
$$;


--
-- TOC entry 430 (class 1255 OID 261204)
-- Dependencies: 1268 6
-- Name: check_con_new_game_widget_state_wizard_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_new_game_widget_state_wizard_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from new_game_widget_state ) =
(select count(distinct wizard_name)
from new_game_widget_state);
end;
$$;


--
-- TOC entry 86 (class 1255 OID 258474)
-- Dependencies: 6 1268
-- Name: check_con_no_spells_for_stiffs(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_no_spells_for_stiffs() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return  not exists(select 1 from spell_books
  natural inner join wizards where expired = true);
end;
$$;


--
-- TOC entry 201 (class 1255 OID 259234)
-- Dependencies: 6 1268
-- Name: check_con_parts_to_cast_only(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_parts_to_cast_only() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return 
  ((select turn_phase = 'cast' from turn_phase_table)
  or not exists(select 1 from spell_parts_to_cast_table))
;
end;
$$;


--
-- TOC entry 90 (class 1255 OID 258546)
-- Dependencies: 6 1268
-- Name: check_con_piece_coordinates_valid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_piece_coordinates_valid() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return  not exists(select 1 from pieces
  cross join board_size
  where x >= width or y >= height);
end;
$$;


--
-- TOC entry 77 (class 1255 OID 258318)
-- Dependencies: 6 1268
-- Name: check_con_piece_prototypes_mr_ptype_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_piece_prototypes_mr_ptype_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from piece_prototypes_mr ) =
(select count(distinct ptype)
from piece_prototypes_mr);
end;
$$;


--
-- TOC entry 193 (class 1255 OID 260788)
-- Dependencies: 1268 6
-- Name: check_con_piece_starting_ticks_ptype_allegiance_tag_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_piece_starting_ticks_ptype_allegiance_tag_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select ptype, allegiance, tag from piece_starting_ticks
  except
select ptype, allegiance, tag from pieces);
end;
$$;


--
-- TOC entry 422 (class 1255 OID 260785)
-- Dependencies: 1268 6
-- Name: check_con_piece_starting_ticks_ptype_allegiance_tag_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_piece_starting_ticks_ptype_allegiance_tag_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from piece_starting_ticks ) =
(select count(distinct ptype, allegiance, tag)
from piece_starting_ticks);
end;
$$;


--
-- TOC entry 92 (class 1255 OID 258567)
-- Dependencies: 6 1268
-- Name: check_con_pieces_allegiance_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_pieces_allegiance_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select allegiance from pieces
  except
select allegiance from allegiances);
end;
$$;


--
-- TOC entry 88 (class 1255 OID 258522)
-- Dependencies: 6 1268
-- Name: check_con_pieces_ptype_allegiance_tag_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_pieces_ptype_allegiance_tag_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from pieces ) =
(select count(distinct ptype, allegiance, tag)
from pieces);
end;
$$;


--
-- TOC entry 89 (class 1255 OID 258525)
-- Dependencies: 6 1268
-- Name: check_con_pieces_ptype_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_pieces_ptype_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select ptype from pieces
  except
select ptype from piece_prototypes);
end;
$$;


--
-- TOC entry 273 (class 1255 OID 259481)
-- Dependencies: 6 1268
-- Name: check_con_pieces_to_move_allegiance_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_pieces_to_move_allegiance_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select allegiance from pieces_to_move
  except
select current_wizard from current_wizard_table);
end;
$$;


--
-- TOC entry 121 (class 1255 OID 259487)
-- Dependencies: 6 1268
-- Name: check_con_pieces_to_move_empty(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_pieces_to_move_empty() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return ((select turn_phase = 'move' from turn_phase_table) or
not exists (select 1 from pieces_to_move));
end;
$$;


--
-- TOC entry 272 (class 1255 OID 259475)
-- Dependencies: 1268 6
-- Name: check_con_pieces_to_move_ptype_allegiance_tag_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_pieces_to_move_ptype_allegiance_tag_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select ptype, allegiance, tag from pieces_to_move
  except
select ptype, allegiance, tag from pieces);
end;
$$;


--
-- TOC entry 271 (class 1255 OID 259472)
-- Dependencies: 6 1268
-- Name: check_con_pieces_to_move_ptype_allegiance_tag_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_pieces_to_move_ptype_allegiance_tag_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from pieces_to_move ) =
(select count(distinct ptype, allegiance, tag)
from pieces_to_move);
end;
$$;


--
-- TOC entry 282 (class 1255 OID 259670)
-- Dependencies: 6 1268
-- Name: check_con_remaining_walk_hack_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_remaining_walk_hack_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from remaining_walk_hack_table) <= 1;
end;
$$;


--
-- TOC entry 281 (class 1255 OID 259667)
-- Dependencies: 6 1268
-- Name: check_con_remaining_walk_hack_table_remaining_walk_hack_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_remaining_walk_hack_table_remaining_walk_hack_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from remaining_walk_hack_table ) =
(select count(distinct remaining_walk_hack)
from remaining_walk_hack_table);
end;
$$;


--
-- TOC entry 284 (class 1255 OID 259724)
-- Dependencies: 6 1268
-- Name: check_con_remaining_walk_only_motion(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_remaining_walk_only_motion() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return  ((not exists(select 1 from remaining_walk_table)) or
   exists(select 1 from creating_new_game_table
      where creating_new_game = true) or
   (select remaining_walk_hack
     from remaining_walk_hack_table) or
   (exists(select 1 from selected_piece)
      and (select move_phase = 'motion' from selected_piece)
      and exists (select 1 from creature_pieces
                  natural inner join selected_piece)
      and (select not flying from creature_pieces
           natural inner join selected_piece))) ;
end;
$$;


--
-- TOC entry 280 (class 1255 OID 259612)
-- Dependencies: 1268 6
-- Name: check_con_remaining_walk_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_remaining_walk_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from remaining_walk_table) <= 1;
end;
$$;


--
-- TOC entry 279 (class 1255 OID 259609)
-- Dependencies: 6 1268
-- Name: check_con_remaining_walk_table_remaining_walk_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_remaining_walk_table_remaining_walk_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from remaining_walk_table ) =
(select count(distinct remaining_walk)
from remaining_walk_table);
end;
$$;


--
-- TOC entry 277 (class 1255 OID 259557)
-- Dependencies: 6 1268
-- Name: check_con_selected_piece_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_selected_piece_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from selected_piece) <= 1;
end;
$$;


--
-- TOC entry 276 (class 1255 OID 259551)
-- Dependencies: 6 1268
-- Name: check_con_selected_piece_allegiance_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_selected_piece_allegiance_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select allegiance from selected_piece
  except
select current_wizard from current_wizard_table);
end;
$$;


--
-- TOC entry 275 (class 1255 OID 259545)
-- Dependencies: 6 1268
-- Name: check_con_selected_piece_ptype_allegiance_tag_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_selected_piece_ptype_allegiance_tag_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select ptype, allegiance, tag from selected_piece
  except
select ptype, allegiance, tag from pieces);
end;
$$;


--
-- TOC entry 274 (class 1255 OID 259542)
-- Dependencies: 6 1268
-- Name: check_con_selected_piece_ptype_allegiance_tag_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_selected_piece_ptype_allegiance_tag_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from selected_piece ) =
(select count(distinct ptype, allegiance, tag)
from selected_piece);
end;
$$;


--
-- TOC entry 21 (class 1255 OID 261068)
-- Dependencies: 6 1268
-- Name: check_con_spell_book_show_all_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_book_show_all_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_book_show_all_table) <= 1;
end;
$$;


--
-- TOC entry 440 (class 1255 OID 261065)
-- Dependencies: 1268 6
-- Name: check_con_spell_book_show_all_table_spell_book_show_all_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_book_show_all_table_spell_book_show_all_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_book_show_all_table ) =
(select count(distinct spell_book_show_all)
from spell_book_show_all_table);
end;
$$;


--
-- TOC entry 84 (class 1255 OID 258465)
-- Dependencies: 6 1268
-- Name: check_con_spell_books_id_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_books_id_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_books ) =
(select count(distinct id)
from spell_books);
end;
$$;


--
-- TOC entry 87 (class 1255 OID 258493)
-- Dependencies: 6 1268
-- Name: check_con_spell_books_spell_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_books_spell_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select spell_name from spell_books
  except
select spell_name from spells);
end;
$$;


--
-- TOC entry 85 (class 1255 OID 258468)
-- Dependencies: 6 1268
-- Name: check_con_spell_books_wizard_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_books_wizard_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select wizard_name from spell_books
  except
select wizard_name from wizards);
end;
$$;


--
-- TOC entry 171 (class 1255 OID 259064)
-- Dependencies: 1268 6
-- Name: check_con_spell_choice_hack_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_choice_hack_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_choice_hack_table) <= 1;
end;
$$;


--
-- TOC entry 170 (class 1255 OID 259061)
-- Dependencies: 6 1268
-- Name: check_con_spell_choice_hack_table_spell_choice_hack_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_choice_hack_table_spell_choice_hack_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_choice_hack_table ) =
(select count(distinct spell_choice_hack)
from spell_choice_hack_table);
end;
$$;


--
-- TOC entry 378 (class 1255 OID 260255)
-- Dependencies: 6 1268
-- Name: check_con_spell_indexes_no_dis_turm_row_number_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_indexes_no_dis_turm_row_number_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_indexes_no_dis_turm ) =
(select count(distinct row_number)
from spell_indexes_no_dis_turm);
end;
$$;


--
-- TOC entry 427 (class 1255 OID 261169)
-- Dependencies: 1268 6
-- Name: check_con_spell_keys_key_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_keys_key_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_keys ) =
(select count(distinct key)
from spell_keys);
end;
$$;


--
-- TOC entry 428 (class 1255 OID 261172)
-- Dependencies: 1268 6
-- Name: check_con_spell_keys_spell_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_keys_spell_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select spell_name from spell_keys
  except
select spell_name from spells_mr);
end;
$$;


--
-- TOC entry 426 (class 1255 OID 261166)
-- Dependencies: 1268 6
-- Name: check_con_spell_keys_spell_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_keys_spell_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_keys ) =
(select count(distinct spell_name)
from spell_keys);
end;
$$;


--
-- TOC entry 103 (class 1255 OID 259192)
-- Dependencies: 6 1268
-- Name: check_con_spell_parts_to_cast_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_parts_to_cast_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_parts_to_cast_table) <= 1;
end;
$$;


--
-- TOC entry 102 (class 1255 OID 259189)
-- Dependencies: 1268 6
-- Name: check_con_spell_parts_to_cast_table_spell_parts_to_cast_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_parts_to_cast_table_spell_parts_to_cast_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_parts_to_cast_table ) =
(select count(distinct spell_parts_to_cast)
from spell_parts_to_cast_table);
end;
$$;


--
-- TOC entry 439 (class 1255 OID 261056)
-- Dependencies: 1268 6
-- Name: check_con_spell_sprites_spell_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_sprites_spell_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select spell_name from spell_sprites
  except
select spell_name from spells_mr);
end;
$$;


--
-- TOC entry 445 (class 1255 OID 261047)
-- Dependencies: 1268 6
-- Name: check_con_spell_sprites_spell_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_sprites_spell_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spell_sprites ) =
(select count(distinct spell_name)
from spell_sprites);
end;
$$;


--
-- TOC entry 438 (class 1255 OID 261050)
-- Dependencies: 1268 6
-- Name: check_con_spell_sprites_sprite_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spell_sprites_sprite_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select sprite from spell_sprites
  except
select sprite from sprites);
end;
$$;


--
-- TOC entry 78 (class 1255 OID 258355)
-- Dependencies: 6 1268
-- Name: check_con_spells_mr_spell_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_spells_mr_spell_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from spells_mr ) =
(select count(distinct spell_name)
from spells_mr);
end;
$$;


--
-- TOC entry 180 (class 1255 OID 260567)
-- Dependencies: 1268 6
-- Name: check_con_sprites_sprite_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_sprites_sprite_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from sprites ) =
(select count(distinct sprite)
from sprites);
end;
$$;


--
-- TOC entry 36 (class 1255 OID 258168)
-- Dependencies: 1268 6
-- Name: check_con_system_implementation_objects_object_name_object__key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_system_implementation_objects_object_name_object__key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from system_implementation_objects ) =
(select count(distinct object_name, object_type)
from system_implementation_objects);
end;
$$;


--
-- TOC entry 292 (class 1255 OID 259905)
-- Dependencies: 1268 6
-- Name: check_con_test_action_overrides_override_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_test_action_overrides_override_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from test_action_overrides ) =
(select count(distinct override)
from test_action_overrides);
end;
$$;


--
-- TOC entry 128 (class 1255 OID 258807)
-- Dependencies: 1268 6
-- Name: check_con_turn_number_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_turn_number_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from turn_number_table) <= 1;
end;
$$;


--
-- TOC entry 127 (class 1255 OID 258804)
-- Dependencies: 6 1268
-- Name: check_con_turn_number_table_turn_number_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_turn_number_table_turn_number_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from turn_number_table ) =
(select count(distinct turn_number)
from turn_number_table);
end;
$$;


--
-- TOC entry 98 (class 1255 OID 258955)
-- Dependencies: 6 1268
-- Name: check_con_turn_phase_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_turn_phase_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from turn_phase_table) <= 1;
end;
$$;


--
-- TOC entry 97 (class 1255 OID 258952)
-- Dependencies: 6 1268
-- Name: check_con_turn_phase_table_turn_phase_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_turn_phase_table_turn_phase_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from turn_phase_table ) =
(select count(distinct turn_phase)
from turn_phase_table);
end;
$$;


--
-- TOC entry 175 (class 1255 OID 260546)
-- Dependencies: 1268 6
-- Name: check_con_windows_window_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_windows_window_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from windows ) =
(select count(distinct window_name)
from windows);
end;
$$;


--
-- TOC entry 183 (class 1255 OID 260582)
-- Dependencies: 1268 6
-- Name: check_con_wizard_display_info_colour_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizard_display_info_colour_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from wizard_display_info ) =
(select count(distinct colour)
from wizard_display_info);
end;
$$;


--
-- TOC entry 185 (class 1255 OID 260591)
-- Dependencies: 1268 6
-- Name: check_con_wizard_display_info_default_sprite_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizard_display_info_default_sprite_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select default_sprite from wizard_display_info
  except
select sprite from sprites);
end;
$$;


--
-- TOC entry 182 (class 1255 OID 260579)
-- Dependencies: 1268 6
-- Name: check_con_wizard_display_info_default_sprite_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizard_display_info_default_sprite_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from wizard_display_info ) =
(select count(distinct default_sprite)
from wizard_display_info);
end;
$$;


--
-- TOC entry 184 (class 1255 OID 260585)
-- Dependencies: 1268 6
-- Name: check_con_wizard_display_info_wizard_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizard_display_info_wizard_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists
(select wizard_name from wizard_display_info
  except
select wizard_name from wizards);
end;
$$;


--
-- TOC entry 181 (class 1255 OID 260576)
-- Dependencies: 6 1268
-- Name: check_con_wizard_display_info_wizard_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizard_display_info_wizard_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from wizard_display_info ) =
(select count(distinct wizard_name)
from wizard_display_info);
end;
$$;


--
-- TOC entry 119 (class 1255 OID 259007)
-- Dependencies: 6 1268
-- Name: check_con_wizard_spell_choices_mr_wizard_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizard_spell_choices_mr_wizard_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from wizard_spell_choices_mr ) =
(select count(distinct wizard_name)
from wizard_spell_choices_mr);
end;
$$;


--
-- TOC entry 173 (class 1255 OID 259102)
-- Dependencies: 6 1268
-- Name: check_con_wizard_spell_choices_wizard_name_spell_name_fkey(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizard_spell_choices_wizard_name_spell_name_fkey() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return ((select spell_choice_hack from spell_choice_hack_table) or
not exists(select wizard_name, spell_name from wizard_spell_choices
  except
select wizard_name, spell_name from spell_books));
end;
$$;


--
-- TOC entry 151 (class 1255 OID 260321)
-- Dependencies: 6 1268
-- Name: check_con_wizard_starting_positions_place_valid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizard_starting_positions_place_valid() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return not exists(select 1 from wizard_starting_positions
    where place >= wizard_count);
end;
$$;


--
-- TOC entry 149 (class 1255 OID 260315)
-- Dependencies: 6 1268
-- Name: check_con_wizard_starting_positions_wizard_count_place_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizard_starting_positions_wizard_count_place_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from wizard_starting_positions ) =
(select count(distinct wizard_count, place)
from wizard_starting_positions);
end;
$$;


--
-- TOC entry 150 (class 1255 OID 260318)
-- Dependencies: 6 1268
-- Name: check_con_wizard_starting_positions_wizard_count_x_y_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizard_starting_positions_wizard_count_x_y_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from wizard_starting_positions ) =
(select count(distinct wizard_count, x, y)
from wizard_starting_positions);
end;
$$;


--
-- TOC entry 74 (class 1255 OID 258449)
-- Dependencies: 1268 6
-- Name: check_con_wizards_wizard_name_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_wizards_wizard_name_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from wizards ) =
(select count(distinct wizard_name)
from wizards);
end;
$$;


--
-- TOC entry 83 (class 1255 OID 258418)
-- Dependencies: 1268 6
-- Name: check_con_world_alignment_table_01_tuple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_world_alignment_table_01_tuple() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from world_alignment_table) <= 1;
end;
$$;


--
-- TOC entry 82 (class 1255 OID 258415)
-- Dependencies: 1268 6
-- Name: check_con_world_alignment_table_world_alignment_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_con_world_alignment_table_world_alignment_key() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return (select count(*) from world_alignment_table ) =
(select count(distinct world_alignment)
from world_alignment_table);
end;
$$;


--
-- TOC entry 32 (class 1255 OID 258141)
-- Dependencies: 6 1268
-- Name: check_constraint_name(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_constraint_name(cn text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  if length(cn) > 54 then
    raise exception
      'pg constraint names must be 54 chars or less, you have % (%)',
      length(cn), cn;
  end if;
end;
$$;


--
-- TOC entry 155 (class 1255 OID 258902)
-- Dependencies: 6 1268
-- Name: check_current_wizard_table_no_delete(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_current_wizard_table_no_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (exists(select 1 from creating_new_game_table
      where creating_new_game = true)
     or exists (select 1 from game_completed_table)) then
    raise exception 'delete on current_wizard_table violates transition constraint current_wizard_table_no_delete';
  end if;
  return null;
end;
$$;


--
-- TOC entry 62 (class 1255 OID 258904)
-- Dependencies: 1268 6
-- Name: check_current_wizard_table_no_insert(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_current_wizard_table_no_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (exists(select 1 from creating_new_game_table
      where creating_new_game = true)) then
    raise exception 'insert on current_wizard_table violates transition constraint current_wizard_table_no_insert';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 101 (class 1255 OID 259184)
-- Dependencies: 1268 6
-- Name: check_delete_spell_choice_restricted(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_delete_spell_choice_restricted() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not ((select turn_phase in ('cast', 'choose') from turn_phase_table)) then
    raise exception 'delete on wizard_spell_choices_mr violates transition constraint delete_spell_choice_restricted';
  end if;
  return null;
end;
$$;


--
-- TOC entry 368 (class 1255 OID 260170)
-- Dependencies: 6 1268
-- Name: check_engaged(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_engaged() RETURNS void
    LANGUAGE plpgsql
    AS $$
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
$$;


--
-- TOC entry 436 (class 1255 OID 260939)
-- Dependencies: 6 1268
-- Name: check_for_effects(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_for_effects() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into board_square_effects (subtype, x1, y1, queuePos)
    select history_name,case when tx is null then x else tx end,
                        case when ty is null then y else ty end,id
    from action_history_mr
    where id > get_last_history_effect_id()
         and x is not null and y is not null
         and history_name not in (select history_name from history_no_visuals);
  insert into board_beam_effects (subtype,x1,y1,x2,y2,queuePos)
    select history_name,x,y,tx,ty,id
    from action_history_mr
    where id > get_last_history_effect_id()
         and x is not null and y is not null
         and tx is not null and ty is not null
         and history_name not in (select history_name from history_no_visuals);
  insert into board_sound_effects (subtype, sound_name,queuePos)
    select history_name,sound_name,id
    from action_history_mr
    natural inner join history_sounds
    left outer join wizards on allegiance = wizard_name
    where id > get_last_history_effect_id()
--exclude turn sound for computer controlled wizards choose phase
      and not(history_name='wizard_up'
              and turn_phase='choose'
              and coalesce(computer_controlled,false))
;
  update last_history_effect_id_table set
    last_history_effect_id = (select max(id) from action_history_mr);
end;
$$;


--
-- TOC entry 210 (class 1255 OID 261482)
-- Dependencies: 1268 6
-- Name: check_history_no_visuals_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_history_no_visuals_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on history_no_visuals violates transition constraint history_no_visuals_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 211 (class 1255 OID 261484)
-- Dependencies: 6 1268
-- Name: check_history_no_visuals_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_history_no_visuals_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on history_no_visuals violates transition constraint history_no_visuals_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 209 (class 1255 OID 261480)
-- Dependencies: 6 1268
-- Name: check_history_no_visuals_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_history_no_visuals_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on history_no_visuals violates transition constraint history_no_visuals_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 207 (class 1255 OID 261476)
-- Dependencies: 1268 6
-- Name: check_history_sounds_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_history_sounds_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on history_sounds violates transition constraint history_sounds_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 208 (class 1255 OID 261478)
-- Dependencies: 6 1268
-- Name: check_history_sounds_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_history_sounds_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on history_sounds violates transition constraint history_sounds_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 206 (class 1255 OID 261474)
-- Dependencies: 6 1268
-- Name: check_history_sounds_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_history_sounds_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on history_sounds violates transition constraint history_sounds_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 100 (class 1255 OID 259182)
-- Dependencies: 6 1268
-- Name: check_insert_spell_choice_restricted(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_insert_spell_choice_restricted() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not ((select turn_phase = 'choose' from turn_phase_table)
    and (select current_wizard = NEW.wizard_name from current_wizard_table)) then
    raise exception 'insert on wizard_spell_choices_mr violates transition constraint insert_spell_choice_restricted';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 219 (class 1255 OID 261500)
-- Dependencies: 6 1268
-- Name: check_key_control_settings_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_key_control_settings_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on key_control_settings violates transition constraint key_control_settings_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 220 (class 1255 OID 261502)
-- Dependencies: 6 1268
-- Name: check_key_control_settings_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_key_control_settings_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on key_control_settings violates transition constraint key_control_settings_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 218 (class 1255 OID 261498)
-- Dependencies: 6 1268
-- Name: check_key_control_settings_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_key_control_settings_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on key_control_settings violates transition constraint key_control_settings_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 154 (class 1255 OID 258900)
-- Dependencies: 6 1268
-- Name: check_next_wizard_change_valid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_next_wizard_change_valid() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (NEW.current_wizard = next_wizard(OLD.current_wizard)) then
    raise exception 'update on current_wizard_table violates transition constraint next_wizard_change_valid';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 513 (class 1255 OID 261440)
-- Dependencies: 1268 6
-- Name: check_piece_prototypes_mr_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_piece_prototypes_mr_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on piece_prototypes_mr violates transition constraint piece_prototypes_mr_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 514 (class 1255 OID 261442)
-- Dependencies: 1268 6
-- Name: check_piece_prototypes_mr_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_piece_prototypes_mr_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on piece_prototypes_mr violates transition constraint piece_prototypes_mr_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 512 (class 1255 OID 261438)
-- Dependencies: 1268 6
-- Name: check_piece_prototypes_mr_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_piece_prototypes_mr_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on piece_prototypes_mr violates transition constraint piece_prototypes_mr_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 293 (class 1255 OID 259908)
-- Dependencies: 6 1021 1268
-- Name: check_random_success(random_test, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_random_success(t random_test, successpercentage integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
  o boolean;
begin
  o := (select setting from test_action_overrides
       where override = t);
  if o is null then --normal random
    return (random() * 100) < successPercentage;
  else --overriden
    delete from test_action_overrides
      where override = t;
    return o;
  end if;
end;
$$;


--
-- TOC entry 519 (class 1255 OID 261452)
-- Dependencies: 1268 6
-- Name: check_spell_indexes_no_dis_turm_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spell_indexes_no_dis_turm_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on spell_indexes_no_dis_turm violates transition constraint spell_indexes_no_dis_turm_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 520 (class 1255 OID 261454)
-- Dependencies: 6 1268
-- Name: check_spell_indexes_no_dis_turm_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spell_indexes_no_dis_turm_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on spell_indexes_no_dis_turm violates transition constraint spell_indexes_no_dis_turm_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 518 (class 1255 OID 261450)
-- Dependencies: 1268 6
-- Name: check_spell_indexes_no_dis_turm_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spell_indexes_no_dis_turm_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on spell_indexes_no_dis_turm violates transition constraint spell_indexes_no_dis_turm_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 216 (class 1255 OID 261494)
-- Dependencies: 1268 6
-- Name: check_spell_keys_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spell_keys_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on spell_keys violates transition constraint spell_keys_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 217 (class 1255 OID 261496)
-- Dependencies: 6 1268
-- Name: check_spell_keys_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spell_keys_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on spell_keys violates transition constraint spell_keys_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 215 (class 1255 OID 261492)
-- Dependencies: 6 1268
-- Name: check_spell_keys_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spell_keys_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on spell_keys violates transition constraint spell_keys_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 213 (class 1255 OID 261488)
-- Dependencies: 6 1268
-- Name: check_spell_sprites_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spell_sprites_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on spell_sprites violates transition constraint spell_sprites_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 214 (class 1255 OID 261490)
-- Dependencies: 6 1268
-- Name: check_spell_sprites_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spell_sprites_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on spell_sprites violates transition constraint spell_sprites_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 212 (class 1255 OID 261486)
-- Dependencies: 1268 6
-- Name: check_spell_sprites_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spell_sprites_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on spell_sprites violates transition constraint spell_sprites_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 350 (class 1255 OID 260131)
-- Dependencies: 1268 6
-- Name: check_spell_success(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spell_success() RETURNS boolean
    LANGUAGE plpgsql
    AS $$
begin
  -- if already checked then return true
  if (select cast_success_checked
    from cast_success_checked_table) then
    return true;
  end if;

  -- if imaginary monster then always succeed
  if (select coalesce(imaginary, false)
    from wizard_spell_choices_mr
    natural inner join current_wizard) then
    return true;
  end if;

  if not check_random_success('cast',
       (select chance
        from spell_cast_chance
        natural inner join current_wizard_spell)) then
     perform action_cast_failed();
     return false;
  else
     update cast_success_checked_table
       set cast_success_checked = true;
     return true;
  end if;
end;
$$;


--
-- TOC entry 516 (class 1255 OID 261446)
-- Dependencies: 1268 6
-- Name: check_spells_mr_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spells_mr_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on spells_mr violates transition constraint spells_mr_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 517 (class 1255 OID 261448)
-- Dependencies: 1268 6
-- Name: check_spells_mr_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spells_mr_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on spells_mr violates transition constraint spells_mr_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 515 (class 1255 OID 261444)
-- Dependencies: 1268 6
-- Name: check_spells_mr_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_spells_mr_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on spells_mr violates transition constraint spells_mr_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 496 (class 1255 OID 261470)
-- Dependencies: 6 1268
-- Name: check_sprites_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_sprites_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on sprites violates transition constraint sprites_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 507 (class 1255 OID 261472)
-- Dependencies: 6 1268
-- Name: check_sprites_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_sprites_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on sprites violates transition constraint sprites_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 527 (class 1255 OID 261468)
-- Dependencies: 1268 6
-- Name: check_sprites_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_sprites_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on sprites violates transition constraint sprites_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 134 (class 1255 OID 258839)
-- Dependencies: 6 1268
-- Name: check_turn_number_change_valid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_turn_number_change_valid() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not ((NEW.turn_number = OLD.turn_number + 1)) then
    raise exception 'update on turn_number_table violates transition constraint turn_number_change_valid';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 136 (class 1255 OID 258842)
-- Dependencies: 6 1268
-- Name: check_turn_number_table_no_delete(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_turn_number_table_no_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (exists(select 1 from creating_new_game_table
      where creating_new_game = true)) then
    raise exception 'delete on turn_number_table violates transition constraint turn_number_table_no_delete';
  end if;
  return null;
end;
$$;


--
-- TOC entry 129 (class 1255 OID 258844)
-- Dependencies: 6 1268
-- Name: check_turn_number_table_no_insert(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_turn_number_table_no_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (exists(select 1 from creating_new_game_table
      where creating_new_game = true)) then
    raise exception 'insert on turn_number_table violates transition constraint turn_number_table_no_insert';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 157 (class 1255 OID 258991)
-- Dependencies: 1268 6
-- Name: check_turn_phase_change_valid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_turn_phase_change_valid() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (NEW.turn_phase = next_turn_phase(OLD.turn_phase)) then
    raise exception 'update on turn_phase_table violates transition constraint turn_phase_change_valid';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 158 (class 1255 OID 258993)
-- Dependencies: 1268 6
-- Name: check_turn_phase_table_no_delete(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_turn_phase_table_no_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (exists(select 1 from creating_new_game_table
      where creating_new_game = true)) then
    raise exception 'delete on turn_phase_table violates transition constraint turn_phase_table_no_delete';
  end if;
  return null;
end;
$$;


--
-- TOC entry 159 (class 1255 OID 258995)
-- Dependencies: 6 1268
-- Name: check_turn_phase_table_no_insert(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_turn_phase_table_no_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (exists(select 1 from creating_new_game_table
      where creating_new_game = true)) then
    raise exception 'insert on turn_phase_table violates transition constraint turn_phase_table_no_insert';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 99 (class 1255 OID 259180)
-- Dependencies: 6 1268
-- Name: check_update_spell_choice_restricted(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_update_spell_choice_restricted() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not ((select turn_phase = 'choose' from turn_phase_table)
    and (NEW.wizard_name = OLD.wizard_name)
    and (select current_wizard = NEW.wizard_name from current_wizard_table)) then
    raise exception 'update on wizard_spell_choices_mr violates transition constraint update_spell_choice_restricted';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 522 (class 1255 OID 261458)
-- Dependencies: 1268 6
-- Name: check_wizard_starting_positions_d_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_wizard_starting_positions_d_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'delete on wizard_starting_positions violates transition constraint wizard_starting_positions_d_readonly';
  end if;
  return null;
end;
$$;


--
-- TOC entry 523 (class 1255 OID 261460)
-- Dependencies: 1268 6
-- Name: check_wizard_starting_positions_i_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_wizard_starting_positions_i_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'insert on wizard_starting_positions violates transition constraint wizard_starting_positions_i_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 521 (class 1255 OID 261456)
-- Dependencies: 1268 6
-- Name: check_wizard_starting_positions_u_readonly(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION check_wizard_starting_positions_u_readonly() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not (false) then
    raise exception 'update on wizard_starting_positions violates transition constraint wizard_starting_positions_u_readonly';
  end if;
  return OLD;
end;
$$;


--
-- TOC entry 482 (class 1255 OID 261383)
-- Dependencies: 6 1268
-- Name: con_pg_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION con_pg_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for con_pg';
  if not not exists
(select constraint_name from con_pg
  except
select constraint_name from (select constraint_name from check_pg union
   select constraint_name from key_pg union
   select constraint_name from fk_pg) as x) then
    raise exception
      'value violates database constraint "con_pg_constraint_name_fkey1"';
  end if;

--  raise notice 'complete constraint op for con_pg';
  return OLD;
end;
$$;


--
-- TOC entry 30 (class 1255 OID 258112)
-- Dependencies: 1268 6
-- Name: constrain_to_zero_or_one_tuple(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION constrain_to_zero_or_one_tuple(table_name text) RETURNS void
    LANGUAGE plpgsql
    AS $_X$
begin
  execute $a$select add_constraint('$a$ || table_name || $a$_01_tuple',
    '(select count(*) from $a$ || table_name || $a$) <= 1',
    array['$a$ || table_name || $a$']);$a$;
end;
$_X$;


--
-- TOC entry 415 (class 1255 OID 261151)
-- Dependencies: 6
-- Name: count_icons(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION count_icons(integer) RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $_$
  select repeat('#', $1) as result;
$_$;


--
-- TOC entry 450 (class 1255 OID 261311)
-- Dependencies: 6 1268
-- Name: create_client_action_wrapper(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION create_client_action_wrapper(client_action_name text, action_call text) RETURNS void
    LANGUAGE plpgsql
    AS $_$
begin
  execute $f$
create function action_$f$ || client_action_name || $f$() returns void as $a$
begin
  perform action_$f$ || action_call || $f$;
end;
$a$ language plpgsql volatile;$f$;
end;
$_$;


--
-- TOC entry 382 (class 1255 OID 260264)
-- Dependencies: 1268 6
-- Name: create_corpse(text, integer, integer, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION create_corpse(vptype text, px integer, py integer, imaginary boolean) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  vtag int;
  twiz text;
begin
  if not exists(select count(*) from monster_prototypes
                where ptype = vptype) then
    raise exception 'called create corpse on % which is not a monster', vptype;
  end if;
  vtag := create_piece_internal(vptype,
                                'Buddha',
                                px, py, imaginary);
  perform kill_monster((vptype, 'Buddha', vtag));
end
$$;


--
-- TOC entry 55 (class 1255 OID 258238)
-- Dependencies: 1268 6
-- Name: create_delete_transition_tuple_constraint(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION create_delete_transition_tuple_constraint(relvar_name text, constraint_name text, constraint_expression text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform create_x_transition_tuple_constraint(relvar_name,
    constraint_name, constraint_expression, 'delete');
end;
$$;


--
-- TOC entry 54 (class 1255 OID 258237)
-- Dependencies: 1268 6
-- Name: create_insert_transition_tuple_constraint(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION create_insert_transition_tuple_constraint(relvar_name text, constraint_name text, constraint_expression text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform create_x_transition_tuple_constraint(relvar_name,
    constraint_name, constraint_expression, 'insert');
end;
$$;


--
-- TOC entry 381 (class 1255 OID 260263)
-- Dependencies: 6 1268
-- Name: create_monster(text, text, integer, integer, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION create_monster(vptype text, allegiance text, x integer, y integer, imaginary boolean) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  if not exists(select 1 from monster_prototypes where ptype = vptype) then
    raise exception 'called create monster on % which is not a monster', vptype;
  end if;
  perform create_piece_internal(vptype, allegiance, x, y, imaginary);
end
$$;


--
-- TOC entry 380 (class 1255 OID 260262)
-- Dependencies: 6 1268
-- Name: create_object(text, text, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION create_object(vptype text, vallegiance text, x integer, y integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
  --assert ptype is an object ptype
  if not exists(select 1 from object_piece_types where ptype = vptype) then
    raise exception 'called create object on % which is not an object', vptype;
  end if;
  return create_piece_internal(vptype, vallegiance, x, y, false);
end
$$;


--
-- TOC entry 384 (class 1255 OID 260266)
-- Dependencies: 1268 6
-- Name: create_piece_internal(text, text, integer, integer, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION create_piece_internal(vptype text, vallegiance text, vx integer, vy integer, vimaginary boolean) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  vtag int;
begin

  insert into pieces (ptype, allegiance, tag, x, y)
    select vptype, vallegiance, get_next_tag(vptype,vallegiance), vx, vy
    returning tag into vtag;

  insert into imaginary_pieces (ptype,allegiance,tag)
    select vptype,vallegiance,vtag where coalesce(vimaginary,false);
  return vtag;
end
$$;


--
-- TOC entry 53 (class 1255 OID 258236)
-- Dependencies: 6 1268
-- Name: create_update_transition_tuple_constraint(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION create_update_transition_tuple_constraint(relvar_name text, constraint_name text, constraint_expression text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform create_x_transition_tuple_constraint(relvar_name,
    constraint_name, constraint_expression, 'update');
end;
$$;


--
-- TOC entry 66 (class 1255 OID 258279)
-- Dependencies: 1268 6
-- Name: create_var(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION create_var(vname text, vtype text) RETURNS void
    LANGUAGE plpgsql
    AS $_X$
begin
--  create table name_table(name type primary key)
  execute $f$create table $f$ || vname || $f$_table ($f$ ||
    vname || $f$ $f$ || vtype || $f$);$f$;
  execute $f$select add_key('$f$ || vname || $f$_table',
                            '$f$ || vname || $f$');$f$;
--  adds 0 or 1 tuple constraint to this table
  --execute $f$select notify_on_changed(
  --     '$f$ || vname || $f$_table');$f$;
  execute $f$select constrain_to_zero_or_one_tuple(
    '$f$ || vname || $f$_table');$f$;
--  creates (static) functions:
--    insert_name inserts value into table
--    (will error if table isn't empty)
--  execute 'create function insert_' || vname ||
    -- '(' || vtype || ') returns void as $a$\n ' ||
--    'insert into ' || vname || '_table values($1);\n' ||
--    '$a$ language sql volatile;';
--    update_name updates value in table
--  (will error if table is empty)
--  execute 'create function update_' || vname || '(' || vtype || ')
--    returns void as $a$\n ' ||
--    'update ' || vname || '_table set ' || vname || ' = $1;\n' ||
--    '$a$ language sql volatile;';
--    delete_name deletes from table
--    (doesn't error if table is already empty)
--  execute 'create function delete_' || vname || '()
--    returns void as $a$\n ' ||
--    'delete from ' || vname || '_table;\n' ||
--    '$a$ language sql volatile;';
--    get_name returns select * from table_name as a single value
  execute 'create function get_' || vname || '() returns ' ||
    vtype || E' as $a$\n ' ||
    'select * from ' || vname || E'_table;\n' ||
    '$a$ language sql stable;';
--    drop_var_name - drop all this ish, cleanup
--  execute 'create function dropvar_' || vname || E'()
--      returns void as $a$\n ' ||
--    'drop function dropvar_' || vname || E'();\n' ||
--    'drop function insert_' || vname || '(' || vtype ||');\n' ||
--    'drop function update_' || vname || '(' || vtype ||');\n' ||
--    'drop function delete_' || vname || '();\n' ||
--    'drop function get_' || vname || E'();\n' ||
--    'drop table ' || vname || E'_table;\n' ||
--    '$a$ language sql volatile;';
end;
$_X$;


--
-- TOC entry 52 (class 1255 OID 258235)
-- Dependencies: 6 1268
-- Name: create_x_transition_tuple_constraint(text, text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION create_x_transition_tuple_constraint(relvar_name text, constraint_name text, constraint_expression text, statement_type text) RETURNS void
    LANGUAGE plpgsql
    AS $_X$
declare
  st text;
begin
  st := $f$
create function check_$f$ || constraint_name || $f$() returns trigger as $a$
begin
  if not ($f$ || constraint_expression || $f$) then
    raise exception '$f$ || statement_type || $f$ on $f$ || relvar_name ||
      $f$ violates transition constraint $f$ || constraint_name || $f$';
  end if;
$f$;
  if statement_type = 'update' or statement_type = 'insert' then
    st := st || '  return OLD;';
  else
    st := st || '  return null;';
  end if;
  st := st || $f$
end;
$a$ language plpgsql volatile;$f$;

  execute st;
  insert into system_implementation_objects(object_name,object_type)
    values ('check_' || constraint_name, 'operator');

  execute $f$
    create trigger $f$ || constraint_name ||
    $f$_transition_trigger after $f$ ||
    statement_type || $f$ on $f$ || relvar_name || $f$
      for each row execute procedure check_$f$
      || constraint_name || $f$();$f$;

  insert into system_implementation_objects(object_name,object_type)
    values (constraint_name || '_transition_trigger', 'trigger');

end;
$_X$;


--
-- TOC entry 484 (class 1255 OID 261387)
-- Dependencies: 6 1268
-- Name: creating_new_game_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION creating_new_game_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for creating_new_game_table';
  if not (select count(*) from creating_new_game_table) <= 1 then
    raise exception
      'value violates database constraint "creating_new_game_table_01_tuple"';
  end if;
  if not  ((not exists(select 1 from remaining_walk_table)) or
   exists(select 1 from creating_new_game_table
      where creating_new_game = true) or
   (select remaining_walk_hack
     from remaining_walk_hack_table) or
   (exists(select 1 from selected_piece)
      and (select move_phase = 'motion' from selected_piece)
      and exists (select 1 from creature_pieces
                  natural inner join selected_piece)
      and (select not flying from creature_pieces
           natural inner join selected_piece)))  then
    raise exception
      'value violates database constraint "remaining_walk_only_motion"';
  end if;

--  raise notice 'complete constraint op for creating_new_game_table';
  return OLD;
end;
$$;


--
-- TOC entry 409 (class 1255 OID 260514)
-- Dependencies: 6 1268
-- Name: crimes_against_nature_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION crimes_against_nature_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify crimes_against_nature;
return null;
end;
$$;


--
-- TOC entry 477 (class 1255 OID 261373)
-- Dependencies: 6 1268
-- Name: crimes_against_nature_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION crimes_against_nature_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for crimes_against_nature';
  if not not exists
(select ptype from crimes_against_nature
  except
select ptype from monster_prototypes) then
    raise exception
      'value violates database constraint "crimes_against_nature_ptype_fkey"';
  end if;

--  raise notice 'complete constraint op for crimes_against_nature';
  return OLD;
end;
$$;


--
-- TOC entry 479 (class 1255 OID 261377)
-- Dependencies: 1268 6
-- Name: current_effects_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION current_effects_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for current_effects';
  if not (select count(*) from current_effects) <= 1 then
    raise exception
      'value violates database constraint "current_effects_01_tuple"';
  end if;

--  raise notice 'complete constraint op for current_effects';
  return OLD;
end;
$$;


--
-- TOC entry 299 (class 1255 OID 260040)
-- Dependencies: 6
-- Name: current_wizard_replicant(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION current_wizard_replicant() RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select computer_controlled from wizards
    inner join current_wizard_table
      on wizard_name=current_wizard;
$$;


--
-- TOC entry 167 (class 1255 OID 260534)
-- Dependencies: 6 1268
-- Name: current_wizard_table_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION current_wizard_table_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify current_wizard_table;
return null;
end;
$$;


--
-- TOC entry 490 (class 1255 OID 261399)
-- Dependencies: 6 1268
-- Name: current_wizard_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION current_wizard_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for current_wizard_table';
  if not 
((select in_next_phase_hack from in_next_phase_hack_table) or
(((select turn_phase='choose' from turn_phase_table) and
 (select max(place) from wizard_spell_choices
   natural inner join live_wizards) <=
 (select place from live_wizards
   inner join current_wizard_table
     on wizard_name = current_wizard))
or
((select turn_phase='cast' from turn_phase_table) and
 (select min(place) from wizard_spell_choices
    natural inner join live_wizards) >=
  (select place from live_wizards
    inner join current_wizard_table
      on wizard_name = current_wizard))
or not exists(select 1 from wizard_spell_choices)
)) then
    raise exception
      'value violates database constraint "chosen_spell_phase_valid"';
  end if;
  if not (select not expired from current_wizard_table
     inner join wizards on current_wizard = wizard_name) then
    raise exception
      'value violates database constraint "current_wizard_must_be_alive"';
  end if;
  if not (select count(*) from current_wizard_table) <= 1 then
    raise exception
      'value violates database constraint "current_wizard_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for current_wizard_table';
  return OLD;
end;
$$;


--
-- TOC entry 485 (class 1255 OID 261389)
-- Dependencies: 6 1268
-- Name: cursor_position_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION cursor_position_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for cursor_position';
  if not (select count(*) from cursor_position) <= 1 then
    raise exception
      'value violates database constraint "cursor_position_01_tuple"';
  end if;
  if not  not exists (select 1 from cursor_position
  cross join board_size
  where x >= width or y >= height) then
    raise exception
      'value violates database constraint "cursor_position_coordinates_valid"';
  end if;

--  raise notice 'complete constraint op for cursor_position';
  return OLD;
end;
$$;


--
-- TOC entry 481 (class 1255 OID 261381)
-- Dependencies: 6 1268
-- Name: dbcon_relvars_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION dbcon_relvars_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for dbcon_relvars';
  if not not exists
(select relvar_name from dbcon_relvars
  except
select relvar_name from base_relvars) then
    raise exception
      'value violates database constraint "dbcon_relvars_relvar_name_fkey"';
  end if;

--  raise notice 'complete constraint op for dbcon_relvars';
  return OLD;
end;
$$;


--
-- TOC entry 486 (class 1255 OID 261391)
-- Dependencies: 6 1268
-- Name: dbcon_trigger_ops_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION dbcon_trigger_ops_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for dbcon_trigger_ops';
  if not not exists
(select operator_name from dbcon_trigger_ops
  except
select operator_name from operators) then
    raise exception
      'value violates database constraint "dbcon_trigger_ops_operator_name_fkey"';
  end if;

--  raise notice 'complete constraint op for dbcon_trigger_ops';
  return OLD;
end;
$$;


--
-- TOC entry 483 (class 1255 OID 261385)
-- Dependencies: 6 1268
-- Name: dbcon_triggers_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION dbcon_triggers_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for dbcon_triggers';
  if not not exists
(select trigger_name, relvar_name from dbcon_triggers
  except
select trigger_name, relvar_name from triggers) then
    raise exception
      'value violates database constraint "dbcon_triggers_trigger_name_relvar_name_fkey"';
  end if;

--  raise notice 'complete constraint op for dbcon_triggers';
  return OLD;
end;
$$;


--
-- TOC entry 410 (class 1255 OID 260516)
-- Dependencies: 6 1268
-- Name: disable_spreading_table_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION disable_spreading_table_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify disable_spreading_table;
return null;
end;
$$;


--
-- TOC entry 478 (class 1255 OID 261375)
-- Dependencies: 6 1268
-- Name: disable_spreading_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION disable_spreading_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for disable_spreading_table';
  if not (select count(*) from disable_spreading_table) <= 1 then
    raise exception
      'value violates database constraint "disable_spreading_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for disable_spreading_table';
  return OLD;
end;
$$;


--
-- TOC entry 386 (class 1255 OID 260268)
-- Dependencies: 6 942 1268
-- Name: disintegrate(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION disintegrate(pk piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  delete from pieces where (ptype, allegiance, tag)::piece_key = pk;
end;
$$;


--
-- TOC entry 388 (class 1255 OID 260270)
-- Dependencies: 6 1268
-- Name: disintegrate_wizards_army(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION disintegrate_wizards_army(pwizard_name text) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
begin
  for r in select ptype, allegiance, tag from pieces
    where allegiance = pwizard_name loop
    perform disintegrate(r);
  end loop;
end;
$$;


--
-- TOC entry 297 (class 1255 OID 259932)
-- Dependencies: 6
-- Name: distance(integer, integer, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION distance(integer, integer, integer, integer) RETURNS real
    LANGUAGE sql IMMUTABLE
    AS $_$
  select (point($1, $2) <-> point($3, $4))::float(24) as result;
$_$;


--
-- TOC entry 369 (class 1255 OID 260175)
-- Dependencies: 1268 6
-- Name: do_autonomous_phase(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION do_autonomous_phase() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
  r1 piece_key;
begin
  --castles
  for r in select ptype,allegiance,tag from pieces
    where ptype in ('magic_castle', 'dark_citadel') loop
    if check_random_success('disappear', 20) then
      perform disintegrate(r);
    end if;
  end loop;
  --magic trees
 for r in select ptype,allegiance,tag from wizards_in_trees loop
    if check_random_success('bonus', 20) then
      select into r1 ptype,allegiance,tag
             from pieces
             where ptype ='magic_tree'
               and (x,y) = (select x,y from pieces
                            where (ptype,allegiance,tag)::piece_key = r);
      perform disintegrate(r1);
      insert into spell_books (wizard_name, spell_name)
      values (r.allegiance,
       (select spell_name from spells
         where spell_name <> 'disbelieve'
          order by random() limit 1));
    end if;
  end loop;
  perform do_spreading();
end;
$$;


--
-- TOC entry 359 (class 1255 OID 260156)
-- Dependencies: 1268 6
-- Name: do_next_move_subphase(boolean, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION do_next_move_subphase(skip_attack boolean, phase_done text) RETURNS void
    LANGUAGE plpgsql
    AS $$
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
$$;


--
-- TOC entry 377 (class 1255 OID 260245)
-- Dependencies: 1268 6
-- Name: do_spreading(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION do_spreading() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
  r1 piece_key;
  p pos;
  sp record;
  i int;
  c int;
  tg int;
  killed_wizards text[] = '{}';
begin
  if get_disable_spreading() then
    return;
  end if;
  for sp in select ptype,allegiance,tag,x,y from pieces
           where ptype in ('gooey_blob', 'magic_fire') loop
    --raise notice 'check % e %', sp.allegiance, killed_wizards;
    if array_contains(killed_wizards, sp.allegiance) then
      --raise notice 'skip piece %', sp.allegiance;
      continue;
    end if;
    c := (random() * 100)::Int;
    if c < 10 then
      --recede
      perform add_history_recede(r);
      perform disintegrate((sp.ptype,sp.allegiance,sp.tag));
    elseif c < 50 then
      for i in 1..(case when c < 30 then 1 else 2 end) loop
        select into p tx,ty from (
          select tx, ty from board_ranges b
          inner join spreadable_squares s
            on (s.x,s.y) = (b.tx,b.ty)
          where range = 1
            and (b.x,b.y) = (sp.x,sp.y)
          except
          select x as tx,y as ty
            from pieces
            where allegiance=sp.allegiance) as a
              order by random() limit 1;
        if p.x is null then continue; end if;
        if exists(select 1 from pieces
               where (x,y) = (p.x,p.y)
                 and ptype = 'wizard') then
          select into r1 ptype,allegiance,tag from pieces
               where (x,y) = (p.x,p.y)
                 and ptype = 'wizard';
          if r1.allegiance = sp.allegiance then
            raise exception 'spread tried to get friendly piece';
          end if;
          killed_wizards := array_append(killed_wizards, r1.allegiance);
          --raise notice 'killed_wizards %', killed_wizards;
          perform add_chinned_history(p.x,p.y);
          perform kill_piece(r1);
        end if;
        --magic fire removes all pieces
        for r1 in select ptype,allegiance,tag from pieces
            where (x,y) = (p.x,p.y) loop
          if r1.allegiance = sp.allegiance then
            raise exception 'spread tried to get friendly piece';
          end if;
          perform disintegrate(r1);
        end loop;
        --raise notice 'spreading %', sp.allegiance;
        tg := create_object(sp.ptype, sp.allegiance, p.x,p.y);
        perform add_history_spread((sp.ptype,sp.allegiance,tg));
      end loop;
    end if;
  end loop;
end;
$$;


--
-- TOC entry 269 (class 1255 OID 269064)
-- Dependencies: 6 1268
-- Name: doingfn(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION doingfn(fn text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  raise notice '\n===================================================\nFUNCTION: %\n-------------------------------------------',fn;
end;
$$;


--
-- TOC entry 446 (class 1255 OID 261290)
-- Dependencies: 6 1268
-- Name: extract_wizard_state(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION extract_wizard_state(state text) RETURNS boolean
    LANGUAGE plpgsql IMMUTABLE
    AS $$
declare
  ret boolean;
begin
  if state = 'human' then
    ret = false;
  elseif state = 'computer' then
    ret = true;
  else
    raise exception
      'argument must be human or computer, called with %', state;
  end if;
  return ret;
end
$$;


--
-- TOC entry 425 (class 1255 OID 261159)
-- Dependencies: 6 1268
-- Name: format_alignment(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION format_alignment(alignment integer) RETURNS text
    LANGUAGE plpgsql IMMUTABLE
    AS $$
begin
  if (alignment < 0) then
    return 'chaos-' || cast(@ alignment as text);
  elseif (alignment > 0) then
    return 'law-' || cast(alignment as text);
  else
    return 'neutral';
  end if;
end;
$$;


--
-- TOC entry 290 (class 1255 OID 259895)
-- Dependencies: 6 1268
-- Name: game_completed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION game_completed() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into game_completed_table
    select true where not exists (select 1 from game_completed_table);
end;
$$;


--
-- TOC entry 166 (class 1255 OID 260532)
-- Dependencies: 1268 6
-- Name: game_completed_table_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION game_completed_table_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify game_completed_table;
return null;
end;
$$;


--
-- TOC entry 489 (class 1255 OID 261397)
-- Dependencies: 6 1268
-- Name: game_completed_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION game_completed_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for game_completed_table';
  if not (select count(*) from game_completed_table) <= 1 then
    raise exception
      'value violates database constraint "game_completed_table_01_tuple"';
  end if;
  if not (not exists(select 1 from game_completed_table)
           or (select count(1) <= 1 from live_wizards)) then
    raise exception
      'value violates database constraint "game_completed_wizards"';
  end if;

--  raise notice 'complete constraint op for game_completed_table';
  return OLD;
end;
$$;


--
-- TOC entry 257 (class 1255 OID 259419)
-- Dependencies: 6
-- Name: get_cast_alignment(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_cast_alignment() RETURNS integer
    LANGUAGE sql STABLE
    AS $$
 select * from cast_alignment_table;
$$;


--
-- TOC entry 223 (class 1255 OID 259324)
-- Dependencies: 6
-- Name: get_cast_success_checked(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_cast_success_checked() RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
 select * from cast_success_checked_table;
$$;


--
-- TOC entry 126 (class 1255 OID 258800)
-- Dependencies: 6
-- Name: get_creating_new_game(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_creating_new_game() RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
 select * from creating_new_game_table;
$$;


--
-- TOC entry 160 (class 1255 OID 259000)
-- Dependencies: 6 988
-- Name: get_current_turn_pos(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_current_turn_pos() RETURNS turn_pos
    LANGUAGE sql STABLE
    AS $$
  select (turn_number, turn_phase, current_wizard)::turn_pos
    from turn_number_table
    cross join turn_phase_table
    cross join current_wizard_table;
$$;


--
-- TOC entry 152 (class 1255 OID 258893)
-- Dependencies: 6
-- Name: get_current_wizard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_current_wizard() RETURNS text
    LANGUAGE sql STABLE
    AS $$
 select * from current_wizard_table;
$$;


--
-- TOC entry 376 (class 1255 OID 260288)
-- Dependencies: 944 6
-- Name: get_current_wizard_pos(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_current_wizard_pos() RETURNS pos
    LANGUAGE sql STABLE
    AS $$
  select x,y from pieces
    where allegiance=get_current_wizard()
      and ptype = 'wizard';
$$;


--
-- TOC entry 169 (class 1255 OID 259057)
-- Dependencies: 6
-- Name: get_current_wizard_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_current_wizard_spell() RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select spell_name from current_wizard_spell;
$$;


--
-- TOC entry 375 (class 1255 OID 260244)
-- Dependencies: 6
-- Name: get_disable_spreading(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_disable_spreading() RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
 select * from disable_spreading_table;
$$;


--
-- TOC entry 288 (class 1255 OID 259839)
-- Dependencies: 6
-- Name: get_game_completed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_game_completed() RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
 select * from game_completed_table;
$$;


--
-- TOC entry 93 (class 1255 OID 258764)
-- Dependencies: 6
-- Name: get_in_next_phase_hack(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_in_next_phase_hack() RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
 select * from in_next_phase_hack_table;
$$;


--
-- TOC entry 435 (class 1255 OID 260938)
-- Dependencies: 6
-- Name: get_last_history_effect_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_last_history_effect_id() RETURNS integer
    LANGUAGE sql STABLE
    AS $$
 select * from last_history_effect_id_table;
$$;


--
-- TOC entry 383 (class 1255 OID 260265)
-- Dependencies: 6
-- Name: get_next_tag(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_next_tag(pptype text, pallegiance text) RETURNS integer
    LANGUAGE sql STABLE
    AS $_$

  select coalesce(max(tag) + 1, 0) from pieces
  where (ptype,allegiance) = ($1,$2);

$_$;


--
-- TOC entry 278 (class 1255 OID 259663)
-- Dependencies: 6
-- Name: get_remaining_walk(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_remaining_walk() RETURNS integer
    LANGUAGE sql STABLE
    AS $$
 select * from remaining_walk_table;
$$;


--
-- TOC entry 283 (class 1255 OID 259723)
-- Dependencies: 6
-- Name: get_remaining_walk_hack(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_remaining_walk_hack() RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
 select * from remaining_walk_hack_table;
$$;


--
-- TOC entry 199 (class 1255 OID 260848)
-- Dependencies: 6 1268
-- Name: get_running_effects(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_running_effects() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return exists (select 1 from board_beam_effects)
      or exists (select 1 from board_square_effects)
      or exists (select 1 from board_sound_effects);
end;
$$;


--
-- TOC entry 413 (class 1255 OID 261137)
-- Dependencies: 6
-- Name: get_spell_book_show_all(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_spell_book_show_all() RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
 select * from spell_book_show_all_table;
$$;


--
-- TOC entry 172 (class 1255 OID 259101)
-- Dependencies: 6
-- Name: get_spell_choice_hack(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_spell_choice_hack() RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
 select * from spell_choice_hack_table;
$$;


--
-- TOC entry 200 (class 1255 OID 259233)
-- Dependencies: 6
-- Name: get_spell_parts_to_cast(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_spell_parts_to_cast() RETURNS integer
    LANGUAGE sql STABLE
    AS $$
 select * from spell_parts_to_cast_table;
$$;


--
-- TOC entry 353 (class 1255 OID 260150)
-- Dependencies: 1268 6 1096
-- Name: get_square_range(integer, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_square_range(x integer, y integer, range integer) RETURNS SETOF ipos
    LANGUAGE plpgsql IMMUTABLE
    AS $$
declare
  p ipos;
begin
  p.index := 0;
  if range < 1 then
    return;
  end if;
  --top row
  p.y = y - range;
  for i in 0 .. (range * 2) loop
    p.x = x - range + i;
    return next p;
    p.index := p.index + 1;
  end loop;
  --sides
  for i in 1 .. (range * 2 + 1) - 2 loop
    p.x = x - range;
    p.y = y - range + i;
    return next p;
    p.index := p.index + 1;
    p.x = x + range;
    return next p;
    p.index := p.index + 1;
  end loop;
  --bottom row
    p.y = y + range;
  for i in 0 .. (range * 2) loop
    p.x = x - range + i;
    return next p;
    p.index := p.index + 1;
  end loop;
end;
$$;


--
-- TOC entry 133 (class 1255 OID 258838)
-- Dependencies: 6
-- Name: get_turn_number(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_turn_number() RETURNS integer
    LANGUAGE sql STABLE
    AS $$
 select * from turn_number_table;
$$;


--
-- TOC entry 156 (class 1255 OID 258990)
-- Dependencies: 983 6
-- Name: get_turn_phase(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_turn_phase() RETURNS turn_phase_enum
    LANGUAGE sql STABLE
    AS $$
 select * from turn_phase_table;
$$;


--
-- TOC entry 72 (class 1255 OID 258433)
-- Dependencies: 6
-- Name: get_world_alignment(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_world_alignment() RETURNS integer
    LANGUAGE sql STABLE
    AS $$
 select * from world_alignment_table;
$$;


--
-- TOC entry 403 (class 1255 OID 260502)
-- Dependencies: 6 1268
-- Name: imaginary_pieces_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION imaginary_pieces_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify imaginary_pieces;
return null;
end;
$$;


--
-- TOC entry 505 (class 1255 OID 261427)
-- Dependencies: 6 1268
-- Name: imaginary_pieces_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION imaginary_pieces_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for imaginary_pieces';
  if not not exists
(select ptype from imaginary_pieces
  except
select ptype from monster_prototypes) then
    raise exception
      'value violates database constraint "imaginary_pieces_ptype_fkey"';
  end if;

--  raise notice 'complete constraint op for imaginary_pieces';
  return OLD;
end;
$$;


--
-- TOC entry 473 (class 1255 OID 261363)
-- Dependencies: 6 1268
-- Name: in_next_phase_hack_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION in_next_phase_hack_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for in_next_phase_hack_table';
  if not 
((select in_next_phase_hack from in_next_phase_hack_table) or
(((select turn_phase='choose' from turn_phase_table) and
 (select max(place) from wizard_spell_choices
   natural inner join live_wizards) <=
 (select place from live_wizards
   inner join current_wizard_table
     on wizard_name = current_wizard))
or
((select turn_phase='cast' from turn_phase_table) and
 (select min(place) from wizard_spell_choices
    natural inner join live_wizards) >=
  (select place from live_wizards
    inner join current_wizard_table
      on wizard_name = current_wizard))
or not exists(select 1 from wizard_spell_choices)
)) then
    raise exception
      'value violates database constraint "chosen_spell_phase_valid"';
  end if;
  if not (select count(*) from in_next_phase_hack_table) <= 1 then
    raise exception
      'value violates database constraint "in_next_phase_hack_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for in_next_phase_hack_table';
  return OLD;
end;
$$;


--
-- TOC entry 81 (class 1255 OID 258409)
-- Dependencies: 6 1268
-- Name: init_board_size(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION init_board_size() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  -- default board size
  insert into board_size (width, height) values (15, 10);
end;
$$;


--
-- TOC entry 421 (class 1255 OID 260765)
-- Dependencies: 1268 6
-- Name: init_cursor_position(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION init_cursor_position() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into cursor_position (x,y) values (0,0);
end;
$$;


--
-- TOC entry 285 (class 1255 OID 259777)
-- Dependencies: 6 1268
-- Name: init_turn_stuff(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION init_turn_stuff() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  --this should catch attempts to start a game
  --which has already been started
  if exists(select 1 from turn_number_table) then
    raise exception 'new game started when turn number table not empty';
  end if;
  insert into turn_number_table values (0);
  insert into turn_phase_table
    values ('choose');
  insert into current_wizard_table
    select wizard_name from live_wizards
    order by place limit 1;
end;
$$;


--
-- TOC entry 191 (class 1255 OID 260624)
-- Dependencies: 1268 6
-- Name: init_wizard_display_info(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION init_wizard_display_info() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
    insert into wizard_display_info (wizard_name, default_sprite,  colour)
       select wizard_name,sprite,colour
       from init_wizard_display_info_argument;
end;
$$;


--
-- TOC entry 73 (class 1255 OID 258434)
-- Dependencies: 6 1268
-- Name: init_world_alignment(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION init_world_alignment() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into world_alignment_table values (0);
end;
$$;


--
-- TOC entry 298 (class 1255 OID 260008)
-- Dependencies: 6
-- Name: is_equipped(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION is_equipped(text) RETURNS boolean
    LANGUAGE sql STABLE
    AS $_$

  select magic_sword or magic_knife or magic_bow
    from wizards where wizard_name = $1;

$_$;


--
-- TOC entry 304 (class 1255 OID 260050)
-- Dependencies: 6 1268
-- Name: is_first_wizard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION is_first_wizard() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return ((select place from live_wizards
       natural inner join current_wizard)
     = (select min(place) from live_wizards));
end;
$$;


--
-- TOC entry 303 (class 1255 OID 260049)
-- Dependencies: 6 1268
-- Name: is_last_wizard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION is_last_wizard() RETURNS boolean
    LANGUAGE plpgsql STABLE
    AS $$
begin
  return ((select place from live_wizards
        natural inner join current_wizard)
     = (select max(place) from live_wizards));
end;
$$;


--
-- TOC entry 387 (class 1255 OID 260269)
-- Dependencies: 6 1268 942
-- Name: kill_monster(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION kill_monster(pk piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  --todo some asserts: monster, non undead
  --  undead cannot be dead - add constraint
  --  non monster cannot be dead: shouldn't be possible, check this
  --  after adding update rule to pieces_view
  --todo: generate update rules automatically for entities
  -- and use a single update here
  -- do the sub ones first since the pieces update changes the key
  update pieces set allegiance = 'dead',
                    tag = get_next_tag(pk.ptype,'dead')
    where (ptype, allegiance, tag) = pk;
end
$$;


--
-- TOC entry 390 (class 1255 OID 260272)
-- Dependencies: 942 6 1268
-- Name: kill_piece(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION kill_piece(pk piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  if (select coalesce(undead,false) from pieces_mr
        where (ptype, allegiance, tag)::piece_key = pk)
    or exists(select 1 from object_piece_types
                where ptype = pk.ptype) then
    perform disintegrate(pk);
  elseif exists(select 1 from monster_prototypes where ptype = pk.ptype) then
    perform kill_monster(pk);
  elseif pk.ptype = 'wizard' then
    perform kill_wizard(pk.allegiance);
  else
    raise exception 'don''t know how to kill piece with ptype %', pk.ptype;
  end if;

end;
$$;


--
-- TOC entry 373 (class 1255 OID 260273)
-- Dependencies: 6 1268
-- Name: kill_top_piece_at(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION kill_top_piece_at(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r piece_key;
begin
  select into r ptype,allegiance,tag
    from pieces_on_top where (x,y) = (px,py);
  perform kill_piece(r);
end;
$$;


--
-- TOC entry 389 (class 1255 OID 260271)
-- Dependencies: 6 1268
-- Name: kill_wizard(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION kill_wizard(pwizard_name text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
--if current wizard then next_wizard
  if get_current_wizard() = pwizard_name then
    perform action_next_phase();
    --check if this is the last wizard, slightly hacky
    if get_current_wizard() = pwizard_name then
      perform game_completed();
      perform add_history_game_drawn();
      delete from current_wizard_table;
    end if;
  end if;
 --this should all be handled with cascades...?
  delete from wizard_spell_choices_mr where wizard_name = pwizard_name;
--wipe spell book
  delete from spell_books where wizard_name = pwizard_name;
--kill army
  perform disintegrate_wizards_army(pwizard_name);
--set expired to true
  update wizards set expired = true
    where wizard_name = pwizard_name;
end;
$$;


--
-- TOC entry 472 (class 1255 OID 261361)
-- Dependencies: 6 1268
-- Name: last_history_effect_id_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION last_history_effect_id_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for last_history_effect_id_table';
  if not (select count(*) from last_history_effect_id_table) <= 1 then
    raise exception
      'value violates database constraint "last_history_effect_id_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for last_history_effect_id_table';
  return OLD;
end;
$$;


--
-- TOC entry 296 (class 1255 OID 259911)
-- Dependencies: 6
-- Name: limit_chance(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION limit_chance(integer) RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $_$
  select max(10, min($1, 100));
$_$;


--
-- TOC entry 385 (class 1255 OID 260267)
-- Dependencies: 1268 6
-- Name: make_piece_undead(text, text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION make_piece_undead(vptype text, vallegiance text, vtag integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  if not exists(select 1 from piece_prototypes_mr
                where ptype=vptype and undead)
      and not exists (select 1 from crimes_against_nature
                      where (ptype,allegiance,tag) =
                        (vptype,vallegiance,vtag)) then
    insert into crimes_against_nature
      (ptype,allegiance,tag) values
      (vptype,vallegiance,vtag);
  end if;
end;
$$;


--
-- TOC entry 379 (class 1255 OID 260261)
-- Dependencies: 6 1268 1110
-- Name: makenrandoms(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION makenrandoms(n integer, maxi integer) RETURNS SETOF random_entry
    LANGUAGE plpgsql
    AS $$
begin
  return query
    select generate_series(0, n - 1),
      (random() * maxi + 0.5)::int as num;
end;
$$;


--
-- TOC entry 295 (class 1255 OID 259910)
-- Dependencies: 6
-- Name: max(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION max(integer, integer) RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $_$
  select max(n) from (select $1 as n union select $2 as n) as a;
$_$;


--
-- TOC entry 294 (class 1255 OID 259909)
-- Dependencies: 6
-- Name: min(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION min(integer, integer) RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $_$
  select min(n) from (select $1 as n union select $2 as n) as a;
$_$;


--
-- TOC entry 480 (class 1255 OID 261379)
-- Dependencies: 6 1268
-- Name: new_game_widget_state_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION new_game_widget_state_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for new_game_widget_state';
  if not  not exists(select 1 from new_game_widget_state
  where line >= 8) then
    raise exception
      'value violates database constraint "new_game_widget_state_line_valid"';
  end if;

--  raise notice 'complete constraint op for new_game_widget_state';
  return OLD;
end;
$$;


--
-- TOC entry 60 (class 1255 OID 258275)
-- Dependencies: 6 1268
-- Name: new_module(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION new_module(mname text, mparent text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into modules (module_name, module_parent_name)
    values(mname, mparent);
end;
$$;


--
-- TOC entry 96 (class 1255 OID 258945)
-- Dependencies: 6
-- Name: next_turn_phase(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION next_turn_phase(text) RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $_$
  select case
    when $1='choose' then 'cast'
    when $1='cast' then 'autonomous'
    when $1='autonomous' then 'move'
    when $1='move' then 'choose'
  end as result
$_$;


--
-- TOC entry 130 (class 1255 OID 258850)
-- Dependencies: 6
-- Name: next_wizard(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION next_wizard(text) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  select new_wizard_name from next_wizard
    where wizard_name = $1;
$_$;


--
-- TOC entry 135 (class 1255 OID 258841)
-- Dependencies: 6 1268
-- Name: no_deletes_inserts_except_new_game(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION no_deletes_inserts_except_new_game(relvar_name text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform create_delete_transition_tuple_constraint(
    relvar_name,
    relvar_name || '_no_delete',
    'exists(select 1 from creating_new_game_table
      where creating_new_game = true)');
  perform create_insert_transition_tuple_constraint(
    relvar_name,
    relvar_name || '_no_insert',
    'exists(select 1 from creating_new_game_table
      where creating_new_game = true)');

end;
$$;


--
-- TOC entry 67 (class 1255 OID 258280)
-- Dependencies: 6 1268
-- Name: notify_on_changed(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION notify_on_changed(table_name text) RETURNS void
    LANGUAGE plpgsql
    AS $_$
begin
  execute 'create function ' || table_name
    || E'_changed() returns trigger as $a$\n'
    || E'begin\n'
    || 'notify ' || table_name || E';\n'
    || E'return null;\n'
    || E'end;\n'
    || '$a$ language plpgsql volatile;';
  --perform add_to_package('utils', 'notify_table_' ||
--    table_name || '_changed', 'trigger_function');
  insert into system_implementation_objects
    (object_name, object_type) values
     (table_name || '_changed', 'operator');

  execute 'create trigger ' || table_name || '_changed'
    || ' after insert or update or delete on '
    || table_name || ' execute procedure '
    || table_name || '_changed();';
  insert into system_implementation_objects
    (object_name, object_type) values
     (table_name || '_changed', 'trigger');

end;
$_$;


--
-- TOC entry 364 (class 1255 OID 260161)
-- Dependencies: 1268 942 6
-- Name: piece_next_subphase(text, boolean, text, piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION piece_next_subphase(current_subphase text, skip_attack boolean, just_done text, pk piece_key) RETURNS text
    LANGUAGE plpgsql
    AS $$
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
           natural inner join pieces_on_top
           --want to keep rows where the attack piece is range 1 from
           --the piece in question, so the board range source x,y is the
           --x,y of the piece in question, and the target x,y is the
           --x,y positions of the enemy attackable pieces
           inner join board_ranges b on (b.x,b.y,tx,ty)=(r.x,r.y,ap.x,ap.y)
           where allegiance <> pk.allegiance
             and range = 1) then
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
$$;


--
-- TOC entry 406 (class 1255 OID 260508)
-- Dependencies: 6 1268
-- Name: pieces_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pieces_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify pieces;
return null;
end;
$$;


--
-- TOC entry 475 (class 1255 OID 261367)
-- Dependencies: 6 1268
-- Name: pieces_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pieces_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for pieces';
  if not  not exists(select 1 from pieces
    inner join wizards
    on (allegiance = wizard_name)
    where expired = true) then
    raise exception
      'value violates database constraint "dead_wizard_army_empty"';
  end if;
  if not  not exists(select 1 from wizard_spell_choices_mr
    natural inner join wizards
    where expired = true) then
    raise exception
      'value violates database constraint "dead_wizard_no_spell"';
  end if;
  if not  not exists(select 1 from pieces
  cross join board_size
  where x >= width or y >= height) then
    raise exception
      'value violates database constraint "piece_coordinates_valid"';
  end if;
  if not not exists
(select allegiance from pieces
  except
select allegiance from allegiances) then
    raise exception
      'value violates database constraint "pieces_allegiance_fkey"';
  end if;
  if not not exists
(select ptype from pieces
  except
select ptype from piece_prototypes) then
    raise exception
      'value violates database constraint "pieces_ptype_fkey"';
  end if;
  if not  ((not exists(select 1 from remaining_walk_table)) or
   exists(select 1 from creating_new_game_table
      where creating_new_game = true) or
   (select remaining_walk_hack
     from remaining_walk_hack_table) or
   (exists(select 1 from selected_piece)
      and (select move_phase = 'motion' from selected_piece)
      and exists (select 1 from creature_pieces
                  natural inner join selected_piece)
      and (select not flying from creature_pieces
           natural inner join selected_piece)))  then
    raise exception
      'value violates database constraint "remaining_walk_only_motion"';
  end if;

--  raise notice 'complete constraint op for pieces';
  return OLD;
end;
$$;


--
-- TOC entry 162 (class 1255 OID 260524)
-- Dependencies: 1268 6
-- Name: pieces_to_move_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pieces_to_move_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify pieces_to_move;
return null;
end;
$$;


--
-- TOC entry 501 (class 1255 OID 261419)
-- Dependencies: 6 1268
-- Name: pieces_to_move_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pieces_to_move_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for pieces_to_move';
  if not ((select turn_phase = 'move' from turn_phase_table) or
not exists (select 1 from pieces_to_move)) then
    raise exception
      'value violates database constraint "pieces_to_move_empty"';
  end if;

--  raise notice 'complete constraint op for pieces_to_move';
  return OLD;
end;
$$;


--
-- TOC entry 75 (class 1255 OID 258308)
-- Dependencies: 6 1268
-- Name: protect_readonly_relvars(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION protect_readonly_relvars() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r record;
begin
  for r in select relvar_name, type
           from base_relvar_metadata
           where type='readonly' loop
    perform create_update_transition_tuple_constraint(
      r.relvar_name, r.relvar_name || '_u_readonly', 'false');
    perform create_delete_transition_tuple_constraint(
      r.relvar_name, r.relvar_name || '_d_readonly', 'false');
    perform create_insert_transition_tuple_constraint(
      r.relvar_name, r.relvar_name || '_i_readonly', 'false');
    -- get module
    perform set_module_for_preceding_objects(
    (select module_name from module_objects
          where object_type = 'base_relvar'
            and object_name = r.relvar_name));
  end loop;
end;
$$;


--
-- TOC entry 35 (class 1255 OID 258167)
-- Dependencies: 1268 6
-- Name: regenerate_constraint_triggers(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION regenerate_constraint_triggers() RETURNS void
    LANGUAGE plpgsql
    AS $_$
declare
  r record;
  s record;
  f text;
  table_trigger_function text;
  table_trigger text;
begin
  --clean up old triggers and trigger functions
  --store names in table so they can be dropped
  for r in select * from dbcon_triggers loop
    execute 'drop trigger ' || r.trigger_name || ' on '
      || r.relvar_name || ';';
    delete from system_implementation_objects
      where object_name=r.trigger_name
        and object_type='trigger';
  end loop;
  delete from dbcon_triggers;

  for r in select * from dbcon_trigger_ops loop
    execute 'drop function ' || r.operator_name || '();';
    delete from system_implementation_objects
      where object_name=r.operator_name
        and object_type='operator';
  end loop;
  delete from dbcon_trigger_ops;

/*
if constraints are handled by pg constraints, then we
just ignore them here - don't need any operators or triggers
for them.
*/
  for r in select distinct(relvar_name) from non_accelerated_constraints loop
    -- for this table create a trigger function which calls all the relevant
    -- constraint check operators
    table_trigger_function := r.relvar_name || '_constraint_trigger_operator';
    table_trigger := r.relvar_name || '_constraint_trigger';
    --it's a bit tricky following this
    -- start the function definition
    f := $f$create function $f$ || table_trigger_function ||
$f$() returns trigger as $a$
begin
--    raise notice 'in constraint op for $f$ || r.relvar_name || $f$';
$f$;
    -- loop through the constraints
    for s in select distinct constraint_name, expression
             from non_accelerated_constraints
             where relvar_name = r.relvar_name loop
      -- and add a line for each one which checks if
      --it is true, otherwise raises
      f := f  ||
$f$  if not $f$ || s.expression  || $f$ then
    raise exception
      'value violates database constraint "$f$ || s.constraint_name || $f$"';
  end if;
$f$;
    end loop;
    f := f ||
$f$
--  raise notice 'complete constraint op for $f$ || r.relvar_name || $f$';
  return OLD;
end;
$a$ language plpgsql;$f$;
    --create the function
    execute f;
    insert into system_implementation_objects(object_name, object_type)
      values (table_trigger_function, 'operator');

    --now create the trigger to call the function
    execute
$f$create trigger $f$ || table_trigger ||
$f$ after insert or update or delete on $f$ || r.relvar_name ||
$f$ for each statement execute procedure $f$ || table_trigger_function ||
$f$();$f$;
    insert into system_implementation_objects(object_name, object_type)
      values (table_trigger, 'trigger');
    insert into dbcon_triggers
      (trigger_name, relvar_name)
      values(table_trigger, r.relvar_name);
    insert into dbcon_trigger_ops
      (operator_name)
      values(table_trigger_function);
  end loop;
end;
$_$;


--
-- TOC entry 497 (class 1255 OID 261411)
-- Dependencies: 6 1268
-- Name: remaining_walk_hack_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION remaining_walk_hack_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for remaining_walk_hack_table';
  if not (select count(*) from remaining_walk_hack_table) <= 1 then
    raise exception
      'value violates database constraint "remaining_walk_hack_table_01_tuple"';
  end if;
  if not  ((not exists(select 1 from remaining_walk_table)) or
   exists(select 1 from creating_new_game_table
      where creating_new_game = true) or
   (select remaining_walk_hack
     from remaining_walk_hack_table) or
   (exists(select 1 from selected_piece)
      and (select move_phase = 'motion' from selected_piece)
      and exists (select 1 from creature_pieces
                  natural inner join selected_piece)
      and (select not flying from creature_pieces
           natural inner join selected_piece)))  then
    raise exception
      'value violates database constraint "remaining_walk_only_motion"';
  end if;

--  raise notice 'complete constraint op for remaining_walk_hack_table';
  return OLD;
end;
$$;


--
-- TOC entry 408 (class 1255 OID 260512)
-- Dependencies: 6 1268
-- Name: remaining_walk_table_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION remaining_walk_table_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify remaining_walk_table;
return null;
end;
$$;


--
-- TOC entry 476 (class 1255 OID 261371)
-- Dependencies: 6 1268
-- Name: remaining_walk_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION remaining_walk_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for remaining_walk_table';
  if not  ((not exists(select 1 from remaining_walk_table)) or
   exists(select 1 from creating_new_game_table
      where creating_new_game = true) or
   (select remaining_walk_hack
     from remaining_walk_hack_table) or
   (exists(select 1 from selected_piece)
      and (select move_phase = 'motion' from selected_piece)
      and exists (select 1 from creature_pieces
                  natural inner join selected_piece)
      and (select not flying from creature_pieces
           natural inner join selected_piece)))  then
    raise exception
      'value violates database constraint "remaining_walk_only_motion"';
  end if;
  if not (select count(*) from remaining_walk_table) <= 1 then
    raise exception
      'value violates database constraint "remaining_walk_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for remaining_walk_table';
  return OLD;
end;
$$;


--
-- TOC entry 418 (class 1255 OID 260762)
-- Dependencies: 1268 6
-- Name: safe_move_cursor(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION safe_move_cursor(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  update cursor_position
    set x = min(max(x + px, 0), (select width from board_size) - 1),
        y = min(max(y + py, 0), (select height from board_size) - 1);
end;
$$;


--
-- TOC entry 355 (class 1255 OID 260152)
-- Dependencies: 6 942 1268
-- Name: select_piece(piece_key); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION select_piece(pk piece_key) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  nextp text;
  p pos;
begin
  nextp:= piece_next_subphase('start', false, 'none', pk);
  if nextp = 'end' then
    --nothing to do
    delete from pieces_to_move
      where (ptype, allegiance, tag)::piece_key = pk;
    if not exists(select 1 from pieces_to_move) then
      perform action_next_phase();
    end if;
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
$$;


--
-- TOC entry 404 (class 1255 OID 260504)
-- Dependencies: 6 1268
-- Name: selected_piece_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION selected_piece_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify selected_piece;
return null;
end;
$$;


--
-- TOC entry 474 (class 1255 OID 261365)
-- Dependencies: 6 1268
-- Name: selected_piece_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION selected_piece_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for selected_piece';
  if not  ((not exists(select 1 from remaining_walk_table)) or
   exists(select 1 from creating_new_game_table
      where creating_new_game = true) or
   (select remaining_walk_hack
     from remaining_walk_hack_table) or
   (exists(select 1 from selected_piece)
      and (select move_phase = 'motion' from selected_piece)
      and exists (select 1 from creature_pieces
                  natural inner join selected_piece)
      and (select not flying from creature_pieces
           natural inner join selected_piece)))  then
    raise exception
      'value violates database constraint "remaining_walk_only_motion"';
  end if;
  if not (select count(*) from selected_piece) <= 1 then
    raise exception
      'value violates database constraint "selected_piece_01_tuple"';
  end if;

--  raise notice 'complete constraint op for selected_piece';
  return OLD;
end;
$$;


--
-- TOC entry 367 (class 1255 OID 260164)
-- Dependencies: 1268 6
-- Name: selected_piece_move_to(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION selected_piece_move_to(px integer, py integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
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
  if exists(select 1 from creature_pieces
            natural inner join selected_piece
            where not flying) and
     (select move_phase from selected_piece)='motion' then
    update remaining_walk_table
    set remaining_walk = remaining_walk - 1;
    perform check_engaged();
  end if;

end;
$$;


--
-- TOC entry 64 (class 1255 OID 258277)
-- Dependencies: 6 1268
-- Name: set_all_attributes_to_not_null(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION set_all_attributes_to_not_null() RETURNS void
    LANGUAGE plpgsql
    AS $_$
begin
  update pg_attribute set attnotnull = true
    where attrelid in
      (select oid from pg_class where relnamespace =
        (select oid from pg_namespace
           where nspname = 'public')
           and relkind = 'r'
           --don't touch 'multirelations'
           and not exists(select 1 from regexp_matches(relname, '_mr$')))
    and attnum >= 1;
end;
$_$;


--
-- TOC entry 59 (class 1255 OID 258274)
-- Dependencies: 6 1268
-- Name: set_module_for_preceding_objects(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION set_module_for_preceding_objects(vmodule_name text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into all_module_objects(module_name, object_name, object_type)
    select vmodule_name as module_name, object_name, object_type from
      (select object_name, object_type from all_database_objects
        except select object_name, object_type
        from all_module_objects) as a;
end;
$$;


--
-- TOC entry 76 (class 1255 OID 258309)
-- Dependencies: 6 1268
-- Name: set_notifies_on_all_data_tables(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION set_notifies_on_all_data_tables() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r record;
begin
  for r in select relvar_name from base_relvar_metadata where type='data'
  except
  select relvar_name from triggers where trigger_name like '%_changed' loop
    perform notify_on_changed(r.relvar_name);
  end loop;
end;
$$;


--
-- TOC entry 34 (class 1255 OID 258143)
-- Dependencies: 1268 6
-- Name: set_pg_check(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION set_pg_check(vconstraint_name text, vrelvar_name text, vexpression text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_constraint_name(vconstraint_name);
  execute 'alter table ' || vrelvar_name || 'add constraint '
    || vconstraint_name || ' check(' || vexpression || ');';
  insert into con_pg(constraint_name)
    values(vconstraint_name);
end;
$$;


--
-- TOC entry 20 (class 1255 OID 258144)
-- Dependencies: 1268 6
-- Name: set_pg_fk(text, text, text[], text, text[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION set_pg_fk(vconstraint_name text, vrelvar_name text, vattributes text[], vtarget_relvar_name text, vtarget_attributes text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_constraint_name(vconstraint_name);
  execute 'alter table ' || vrelvar_name || ' add constraint '
     || vconstraint_name || ' foreign key(' ||
    array_to_string(vattributes, ',') || ') references ' ||
    vtarget_relvar_name || '(' ||
    array_to_string(vtarget_attributes, ',') ||
--just uses cascade for now, revisit this decision at some point.
    ') on update cascade on delete cascade;';

  insert into con_pg(constraint_name)
    values(vconstraint_name);
end;
$$;


--
-- TOC entry 33 (class 1255 OID 258142)
-- Dependencies: 1268 6
-- Name: set_pg_unique(text, text, text[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION set_pg_unique(vconstraint_name text, vrelvar_name text, vattributes text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_constraint_name(vconstraint_name);
  execute 'alter table ' || vrelvar_name ||
    ' add constraint ' || vconstraint_name || ' unique(' ||
   array_to_string(vattributes, ', ') || ');';
  insert into con_pg(constraint_name)
    values(vconstraint_name);
end;
$$;


--
-- TOC entry 70 (class 1255 OID 258302)
-- Dependencies: 6 1268
-- Name: set_relvar_type(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION set_relvar_type(vname text, vtype text) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into base_relvar_metadata (relvar_name, type)
    values (vname, vtype);
end;
$$;


--
-- TOC entry 256 (class 1255 OID 260111)
-- Dependencies: 1268 6
-- Name: skip_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION skip_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform add_history_spell_skipped();
  perform spend_current_wizard_spell();
end;
$$;


--
-- TOC entry 31 (class 1255 OID 258125)
-- Dependencies: 6 1268
-- Name: sort_out_constraint_name(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION sort_out_constraint_name(cn text, suffix text) RETURNS text
    LANGUAGE plpgsql
    AS $$
begin
  --truncate the name if too long
  --make sure it's unique
  --preserve the suffix
  if length(cn) > 54 then
    --wtf kind of syntax is this?!
    --don't know if it's clear evidence that the sql designers were
    --heavy drug users, or if I think it's great, or both
    return substring(cn from 0 for (54 - length(suffix))) || suffix;
  else
    return cn;
  end if;
end;
$$;


--
-- TOC entry 492 (class 1255 OID 261403)
-- Dependencies: 6 1268
-- Name: spell_book_show_all_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spell_book_show_all_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for spell_book_show_all_table';
  if not (select count(*) from spell_book_show_all_table) <= 1 then
    raise exception
      'value violates database constraint "spell_book_show_all_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for spell_book_show_all_table';
  return OLD;
end;
$$;


--
-- TOC entry 411 (class 1255 OID 260518)
-- Dependencies: 6 1268
-- Name: spell_books_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spell_books_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify spell_books;
return null;
end;
$$;


--
-- TOC entry 498 (class 1255 OID 261413)
-- Dependencies: 1268 6
-- Name: spell_books_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spell_books_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for spell_books';
  if not  not exists(select 1 from spell_books
  natural inner join wizards where expired = true) then
    raise exception
      'value violates database constraint "no_spells_for_stiffs"';
  end if;
  if not not exists
(select spell_name from spell_books
  except
select spell_name from spells) then
    raise exception
      'value violates database constraint "spell_books_spell_name_fkey"';
  end if;
  if not ((select spell_choice_hack from spell_choice_hack_table) or
not exists(select wizard_name, spell_name from wizard_spell_choices
  except
select wizard_name, spell_name from spell_books)) then
    raise exception
      'value violates database constraint "wizard_spell_choices_wizard_name_spell_name_fkey"';
  end if;

--  raise notice 'complete constraint op for spell_books';
  return OLD;
end;
$$;


--
-- TOC entry 339 (class 1255 OID 260120)
-- Dependencies: 6
-- Name: spell_cast_chance(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spell_cast_chance(text) RETURNS integer
    LANGUAGE sql STABLE
    AS $_$
  select chance from spell_cast_chance where spell_name = $1;
$_$;


--
-- TOC entry 487 (class 1255 OID 261393)
-- Dependencies: 6 1268
-- Name: spell_choice_hack_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spell_choice_hack_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for spell_choice_hack_table';
  if not (select count(*) from spell_choice_hack_table) <= 1 then
    raise exception
      'value violates database constraint "spell_choice_hack_table_01_tuple"';
  end if;
  if not ((select spell_choice_hack from spell_choice_hack_table) or
not exists(select wizard_name, spell_name from wizard_spell_choices
  except
select wizard_name, spell_name from spell_books)) then
    raise exception
      'value violates database constraint "wizard_spell_choices_wizard_name_spell_name_fkey"';
  end if;

--  raise notice 'complete constraint op for spell_choice_hack_table';
  return OLD;
end;
$$;


--
-- TOC entry 424 (class 1255 OID 261158)
-- Dependencies: 6 1268
-- Name: spell_colour(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spell_colour(vspell text, vcount integer) RETURNS text
    LANGUAGE plpgsql STABLE
    AS $$
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
$$;


--
-- TOC entry 401 (class 1255 OID 260498)
-- Dependencies: 6 1268
-- Name: spell_parts_to_cast_table_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spell_parts_to_cast_table_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify spell_parts_to_cast_table;
return null;
end;
$$;


--
-- TOC entry 471 (class 1255 OID 261359)
-- Dependencies: 1268 6
-- Name: spell_parts_to_cast_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spell_parts_to_cast_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for spell_parts_to_cast_table';
  if not 
  ((select turn_phase = 'cast' from turn_phase_table)
  or not exists(select 1 from spell_parts_to_cast_table))
 then
    raise exception
      'value violates database constraint "parts_to_cast_only"';
  end if;
  if not (select count(*) from spell_parts_to_cast_table) <= 1 then
    raise exception
      'value violates database constraint "spell_parts_to_cast_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for spell_parts_to_cast_table';
  return OLD;
end;
$$;


--
-- TOC entry 338 (class 1255 OID 260114)
-- Dependencies: 6 1268
-- Name: spend_current_wizard_spell(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spend_current_wizard_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  --remove current wizard's spell from spell book
  --make sure we only remove one shot of the spell
  --don't remove disbelieve
  update spell_choice_hack_table
    set spell_choice_hack = true;

  delete from spell_parts_to_cast_table;
  delete from cast_success_checked_table;

  delete from spell_books where id =
    (select id from spell_books
       natural inner join wizard_spell_choices
       where wizard_name = get_current_wizard()
         and spell_name != 'disbelieve'
         limit 1);
  -- and wipe it from the wizard_spell_choices_table
  delete from wizard_spell_choices_mr
    where wizard_name = get_current_wizard();

  update spell_choice_hack_table
    set spell_choice_hack = false;

  --auto move to next wizard
  perform action_next_phase();
end;
$$;


--
-- TOC entry 265 (class 1255 OID 269060)
-- Dependencies: 1268 6
-- Name: t1(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION t1() RETURNS void
    LANGUAGE plpgsql STABLE
    AS $$
declare
  r record;
  t int;
begin
  for r in select adnum from pg_attrdef loop
    t := r;
  end loop;
end;
$$;


--
-- TOC entry 261 (class 1255 OID 269192)
-- Dependencies: 6 1268
-- Name: t10(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION t10() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  t pg_attrdef;
  u fake_renamed3_pg_attrdef;
  r record;
begin
  u := (1,2,'adbinval');
  r := u;
  t := r;
  raise notice 'u: %',r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$;


--
-- TOC entry 260 (class 1255 OID 269061)
-- Dependencies: 1268 6
-- Name: t2(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION t2() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r record;
  t pg_attrdef;
begin
  r.adrelid := 1;
  --r.adnum := 2;
  --r.adbin = 'adbinval';
  --r.adsrc = 'adsrcval';
  --raise notice 'r: %',t;
  --t := r;
  --raise notice 't: %',t;
end;
$$;


--
-- TOC entry 262 (class 1255 OID 269063)
-- Dependencies: 1268 6
-- Name: t3(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION t3() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r record;
  t pg_attrdef;
begin
  r := row(1,2,'adbinval','adsrcval');
  raise notice 'r: %',r;
  t := r;
  raise notice 't: %',t;
end;
$$;


--
-- TOC entry 263 (class 1255 OID 269066)
-- Dependencies: 6 1268
-- Name: t4(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION t4() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r record;
  t pg_attrdef;
begin
  r := row('adbinval',1,2,'adsrcval');
  raise notice 'r: %',t;
  t := r;
  raise notice 't: %',t;
end;
$$;


--
-- TOC entry 264 (class 1255 OID 269067)
-- Dependencies: 6 1268
-- Name: t5(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION t5() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  r record;
  t pg_attrdef;
  b bool;
begin
  r := row(1,2,'adbinval','adsrcval', 3,null,true,'is this ignored?');
  raise notice 'r: %',t;
  t := r;
  raise notice 't: %',t;
  --todo: investigate equalities in same way as assignments
  b := row(1,2,'adbinval','adsrcval') = row(1,2,'adbinval','a1dsrcval');
  raise notice 'b: %',b;
end;
$$;


--
-- TOC entry 266 (class 1255 OID 269083)
-- Dependencies: 1268 6
-- Name: t6(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION t6() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  t pg_attrdef;
  u fake_pg_attrdef;
  r record;
begin
  u := (1,2,'adbinval','adsrcval');
  t := u;
  raise notice 'u: %',u;
  raise notice 't: %',t;
  r := u;
  t := r;
  raise notice 'u: %',r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$;


--
-- TOC entry 267 (class 1255 OID 269096)
-- Dependencies: 6 1268
-- Name: t7(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION t7() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  t pg_attrdef;
  u fake_renamed_pg_attrdef;
  r record;
begin
  u := (1,2,'adbinval','adsrcval');
  t := u;
  raise notice 'u: %',u;
  raise notice 't: %',t;
  r := u;
  t := r;
  raise notice 'u: %',r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$;


--
-- TOC entry 268 (class 1255 OID 269112)
-- Dependencies: 6 1268
-- Name: t8(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION t8() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  t pg_attrdef;
  u fake_renamed2_pg_attrdef;
  r record;
begin
  u := (2,'adbinval','adsrcval',1);
  --t := u;
  raise notice 'u: %',u;
  raise notice 't: %',t;
  r := u;
  t := r;
  raise notice 'u: %',r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$;


--
-- TOC entry 259 (class 1255 OID 269143)
-- Dependencies: 6 1268
-- Name: t9(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION t9() RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  t pg_attrdef;
  u fake_renamed3_pg_attrdef;
  r record;
begin
  u := (1,2,'adbinval','adsrcval','test');
  r := u;
  t := r;
  raise notice 'u: %',r;
  raise notice 'r: %',r;
  raise notice 't: %',t;
end;
$$;


--
-- TOC entry 227 (class 1255 OID 269059)
-- Dependencies: 1268 6
-- Name: test(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION test() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  for i in 0 .. 10 loop
    raise notice '%', i;
  end loop;
end;
$$;


--
-- TOC entry 402 (class 1255 OID 260500)
-- Dependencies: 6 1268
-- Name: test_action_overrides_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION test_action_overrides_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify test_action_overrides;
return null;
end;
$$;


--
-- TOC entry 163 (class 1255 OID 260526)
-- Dependencies: 6 1268
-- Name: turn_number_table_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION turn_number_table_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify turn_number_table;
return null;
end;
$$;


--
-- TOC entry 502 (class 1255 OID 261421)
-- Dependencies: 6 1268
-- Name: turn_number_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION turn_number_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for turn_number_table';
  if not (select count(*) from turn_number_table) <= 1 then
    raise exception
      'value violates database constraint "turn_number_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for turn_number_table';
  return OLD;
end;
$$;


--
-- TOC entry 165 (class 1255 OID 260530)
-- Dependencies: 1268 6
-- Name: turn_phase_table_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION turn_phase_table_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify turn_phase_table;
return null;
end;
$$;


--
-- TOC entry 488 (class 1255 OID 261395)
-- Dependencies: 6 1268
-- Name: turn_phase_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION turn_phase_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for turn_phase_table';
  if not ((get_turn_phase() = 'cast') or
  not exists(select 1 from cast_alignment_table)) then
    raise exception
      'value violates database constraint "cast_alignment_empty"';
  end if;
  if not 
  ((select turn_phase = 'cast' from turn_phase_table)
  or not exists(select 1 from cast_success_checked_table))
 then
    raise exception
      'value violates database constraint "cast_checked_cast_only"';
  end if;
  if not 
((select in_next_phase_hack from in_next_phase_hack_table) or
(((select turn_phase='choose' from turn_phase_table) and
 (select max(place) from wizard_spell_choices
   natural inner join live_wizards) <=
 (select place from live_wizards
   inner join current_wizard_table
     on wizard_name = current_wizard))
or
((select turn_phase='cast' from turn_phase_table) and
 (select min(place) from wizard_spell_choices
    natural inner join live_wizards) >=
  (select place from live_wizards
    inner join current_wizard_table
      on wizard_name = current_wizard))
or not exists(select 1 from wizard_spell_choices)
)) then
    raise exception
      'value violates database constraint "chosen_spell_phase_valid"';
  end if;
  if not 
  ((select turn_phase = 'cast' from turn_phase_table)
  or not exists(select 1 from spell_parts_to_cast_table))
 then
    raise exception
      'value violates database constraint "parts_to_cast_only"';
  end if;
  if not ((select turn_phase = 'move' from turn_phase_table) or
not exists (select 1 from pieces_to_move)) then
    raise exception
      'value violates database constraint "pieces_to_move_empty"';
  end if;
  if not (select count(*) from turn_phase_table) <= 1 then
    raise exception
      'value violates database constraint "turn_phase_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for turn_phase_table';
  return OLD;
end;
$$;


--
-- TOC entry 351 (class 1255 OID 260132)
-- Dependencies: 1268 6
-- Name: update_alignment_from_cast(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION update_alignment_from_cast() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  update cast_alignment_table
    set cast_alignment = cast_alignment +
      (select alignment from spells
        natural inner join current_wizard_spell);
  perform adjust_world_alignment();
end;
$$;


--
-- TOC entry 195 (class 1255 OID 260806)
-- Dependencies: 1268 6
-- Name: update_board_sprites_cache(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION update_board_sprites_cache() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  if get_running_effects() then
    return;
  end if;
  --raise notice 'update bpc';
  delete from board_sprites1_cache;
  insert into board_sprites1_cache
    select * from board_sprites1_view;
end;
$$;


--
-- TOC entry 194 (class 1255 OID 260794)
-- Dependencies: 1268 6
-- Name: update_missing_startticks(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION update_missing_startticks() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  insert into piece_starting_ticks (ptype,allegiance,tag,start_tick)
    select ptype,allegiance,tag, random()*2500 from pieces
      where (ptype,allegiance,tag) not in
        (select ptype,allegiance,tag
        from piece_starting_ticks);
end;
$$;


--
-- TOC entry 164 (class 1255 OID 260528)
-- Dependencies: 1268 6
-- Name: wizard_spell_choices_mr_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION wizard_spell_choices_mr_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify wizard_spell_choices_mr;
return null;
end;
$$;


--
-- TOC entry 503 (class 1255 OID 261423)
-- Dependencies: 6 1268
-- Name: wizard_spell_choices_mr_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION wizard_spell_choices_mr_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for wizard_spell_choices_mr';
  if not 
((select in_next_phase_hack from in_next_phase_hack_table) or
(((select turn_phase='choose' from turn_phase_table) and
 (select max(place) from wizard_spell_choices
   natural inner join live_wizards) <=
 (select place from live_wizards
   inner join current_wizard_table
     on wizard_name = current_wizard))
or
((select turn_phase='cast' from turn_phase_table) and
 (select min(place) from wizard_spell_choices
    natural inner join live_wizards) >=
  (select place from live_wizards
    inner join current_wizard_table
      on wizard_name = current_wizard))
or not exists(select 1 from wizard_spell_choices)
)) then
    raise exception
      'value violates database constraint "chosen_spell_phase_valid"';
  end if;
  if not ((select spell_choice_hack from spell_choice_hack_table) or
not exists(select wizard_name, spell_name from wizard_spell_choices
  except
select wizard_name, spell_name from spell_books)) then
    raise exception
      'value violates database constraint "wizard_spell_choices_wizard_name_spell_name_fkey"';
  end if;

--  raise notice 'complete constraint op for wizard_spell_choices_mr';
  return OLD;
end;
$$;


--
-- TOC entry 504 (class 1255 OID 261425)
-- Dependencies: 6 1268
-- Name: wizard_starting_positions_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION wizard_starting_positions_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for wizard_starting_positions';
  if not not exists(select 1 from wizard_starting_positions
    where place >= wizard_count) then
    raise exception
      'value violates database constraint "wizard_starting_positions_place_valid"';
  end if;

--  raise notice 'complete constraint op for wizard_starting_positions';
  return OLD;
end;
$$;


--
-- TOC entry 168 (class 1255 OID 260536)
-- Dependencies: 1268 6
-- Name: wizards_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION wizards_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify wizards;
return null;
end;
$$;


--
-- TOC entry 491 (class 1255 OID 261401)
-- Dependencies: 6 1268
-- Name: wizards_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION wizards_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for wizards';
  if not 
((select in_next_phase_hack from in_next_phase_hack_table) or
(((select turn_phase='choose' from turn_phase_table) and
 (select max(place) from wizard_spell_choices
   natural inner join live_wizards) <=
 (select place from live_wizards
   inner join current_wizard_table
     on wizard_name = current_wizard))
or
((select turn_phase='cast' from turn_phase_table) and
 (select min(place) from wizard_spell_choices
    natural inner join live_wizards) >=
  (select place from live_wizards
    inner join current_wizard_table
      on wizard_name = current_wizard))
or not exists(select 1 from wizard_spell_choices)
)) then
    raise exception
      'value violates database constraint "chosen_spell_phase_valid"';
  end if;
  if not (select not expired from current_wizard_table
     inner join wizards on current_wizard = wizard_name) then
    raise exception
      'value violates database constraint "current_wizard_must_be_alive"';
  end if;
  if not  not exists(select 1 from pieces
    inner join wizards
    on (allegiance = wizard_name)
    where expired = true) then
    raise exception
      'value violates database constraint "dead_wizard_army_empty"';
  end if;
  if not  not exists(select 1 from wizard_spell_choices_mr
    natural inner join wizards
    where expired = true) then
    raise exception
      'value violates database constraint "dead_wizard_no_spell"';
  end if;
  if not  not exists(select 1 from spell_books
  natural inner join wizards where expired = true) then
    raise exception
      'value violates database constraint "no_spells_for_stiffs"';
  end if;

--  raise notice 'complete constraint op for wizards';
  return OLD;
end;
$$;


--
-- TOC entry 407 (class 1255 OID 260510)
-- Dependencies: 6 1268
-- Name: world_alignment_table_changed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION world_alignment_table_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify world_alignment_table;
return null;
end;
$$;


--
-- TOC entry 463 (class 1255 OID 261369)
-- Dependencies: 6 1268
-- Name: world_alignment_table_constraint_trigger_operator(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION world_alignment_table_constraint_trigger_operator() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
--    raise notice 'in constraint op for world_alignment_table';
  if not (select count(*) from world_alignment_table) <= 1 then
    raise exception
      'value violates database constraint "world_alignment_table_01_tuple"';
  end if;

--  raise notice 'complete constraint op for world_alignment_table';
  return OLD;
end;
$$;


SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 2648 (class 1259 OID 261334)
-- Dependencies: 6
-- Name: action_client_new_game_argument; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE action_client_new_game_argument (
    place integer NOT NULL,
    wizard_name text NOT NULL,
    sprite text NOT NULL,
    colour text NOT NULL,
    computer_controlled boolean NOT NULL
);


--
-- TOC entry 2589 (class 1259 OID 260278)
-- Dependencies: 6 983 1112
-- Name: action_history_mr; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE action_history_mr (
    id integer NOT NULL,
    history_name history_name_enum NOT NULL,
    ptype text,
    allegiance text,
    tag integer,
    spell_name text,
    turn_number integer,
    turn_phase turn_phase_enum,
    num_wizards integer,
    x integer,
    y integer,
    tx integer,
    ty integer
);


--
-- TOC entry 2604 (class 1259 OID 260570)
-- Dependencies: 6
-- Name: wizard_display_info; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE wizard_display_info (
    wizard_name text NOT NULL,
    default_sprite text NOT NULL,
    colour text NOT NULL
);


--
-- TOC entry 2606 (class 1259 OID 260625)
-- Dependencies: 2837 6
-- Name: allegiance_colours; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW allegiance_colours AS
    SELECT wizard_display_info.wizard_name AS allegiance, wizard_display_info.colour FROM wizard_display_info UNION SELECT 'dead' AS allegiance, 'grey' AS colour;


--
-- TOC entry 2607 (class 1259 OID 260629)
-- Dependencies: 2838 6 1112 983
-- Name: action_history_colour_mr; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW action_history_colour_mr AS
    SELECT a.id, a.history_name, a.ptype, a.allegiance, a.tag, a.spell_name, a.turn_number, a.turn_phase, a.num_wizards, a.x, a.y, a.tx, a.ty, allegiance_colours.colour FROM (action_history_mr a NATURAL JOIN allegiance_colours);


--
-- TOC entry 2588 (class 1259 OID 260276)
-- Dependencies: 2589 6
-- Name: action_history_mr_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE action_history_mr_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- TOC entry 3410 (class 0 OID 0)
-- Dependencies: 2588
-- Name: action_history_mr_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE action_history_mr_id_seq OWNED BY action_history_mr.id;


--
-- TOC entry 3411 (class 0 OID 0)
-- Dependencies: 2588
-- Name: action_history_mr_id_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('action_history_mr_id_seq', 11, true);


--
-- TOC entry 2646 (class 1259 OID 261325)
-- Dependencies: 2859 6
-- Name: action_instructions; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW action_instructions AS
    ((((((((((SELECT 'cast_target_spell'::text AS action, (('Cast spell: Select a square to cast '::text || get_current_wizard_spell()) || ' on'::text) AS help UNION SELECT 'select_piece_at_position' AS action, 'Select: choose a piece to move by selecting its square' AS help) UNION SELECT 'walk' AS action, 'Walk: select a square to move piece to' AS help) UNION SELECT 'fly' AS action, 'Fly: select a square to move piece to' AS help) UNION SELECT 'attack' AS action, 'Attack: select a square to attack that piece' AS help) UNION SELECT 'ranged_attack' AS action, 'Ranged attack: select a square to attack that piece' AS help) UNION SELECT 'next_phase' AS action, 'Next phase: press space to finish this wizard''s turn' AS help) UNION SELECT 'set_imaginary' AS action, 'Press y to cast an imaginary monster' AS help) UNION SELECT 'set_real' AS action, 'Press n to cast a real monster' AS help) UNION SELECT 'cast_activate_spell' AS action, ('Cast: Press enter to cast '::text || get_current_wizard_spell()) AS help) UNION SELECT 'cancel' AS action, 'Cancel: press End to cancel move/attack/ranged attack' AS help) UNION SELECT 'choose_disbelieve_spell' AS action, 'Press a key from the spell book to choose that spell to cast' AS help;


--
-- TOC entry 2591 (class 1259 OID 260380)
-- Dependencies: 6
-- Name: action_new_game_argument; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE action_new_game_argument (
    place integer NOT NULL,
    wizard_name text NOT NULL,
    computer_controlled boolean NOT NULL
);


--
-- TOC entry 2493 (class 1259 OID 258349)
-- Dependencies: 6 901 899
-- Name: spells_mr; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE spells_mr (
    spell_name text NOT NULL,
    base_chance integer NOT NULL,
    alignment integer NOT NULL,
    spell_category spell_category NOT NULL,
    description text NOT NULL,
    activate boolean,
    target boolean,
    range integer,
    num integer,
    ptype text,
    valid_square_category spell_square_category
);


--
-- TOC entry 2499 (class 1259 OID 258378)
-- Dependencies: 2772 6
-- Name: activate_spells; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW activate_spells AS
    SELECT spells_mr.spell_name FROM spells_mr WHERE ((spells_mr.activate IS NOT NULL) AND spells_mr.activate);


--
-- TOC entry 2550 (class 1259 OID 259933)
-- Dependencies: 2796 6
-- Name: board_ranges; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW board_ranges AS
    SELECT x.x, y.y, range.range, tx.tx, ty.ty FROM ((((generate_series(0, 14) x(x) CROSS JOIN generate_series(0, 9) y(y)) CROSS JOIN generate_series(1, 20) range(range)) CROSS JOIN generate_series(0, 14) tx(tx)) CROSS JOIN generate_series(0, 9) ty(ty)) WHERE (((x.x <> tx.tx) OR (y.y <> ty.ty)) AND ((distance(x.x, y.y, tx.tx, ty.ty) - (0.5)::double precision) <= (range.range)::double precision));


--
-- TOC entry 2577 (class 1259 OID 260134)
-- Dependencies: 6
-- Name: cast_magic_wood_squares; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE cast_magic_wood_squares (
    x integer NOT NULL,
    y integer NOT NULL
);


--
-- TOC entry 2578 (class 1259 OID 260139)
-- Dependencies: 2823 6
-- Name: adjacent_to_new_tree_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW adjacent_to_new_tree_squares AS
    SELECT board_ranges.tx AS x, board_ranges.ty AS y FROM (board_ranges NATURAL JOIN cast_magic_wood_squares) WHERE (board_ranges.range = 1);


--
-- TOC entry 2509 (class 1259 OID 258516)
-- Dependencies: 6
-- Name: pieces; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE pieces (
    ptype text NOT NULL,
    allegiance text NOT NULL,
    tag integer NOT NULL,
    x integer NOT NULL,
    y integer NOT NULL
);


--
-- TOC entry 2554 (class 1259 OID 259949)
-- Dependencies: 2800 6
-- Name: adjacent_to_tree_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW adjacent_to_tree_squares AS
    SELECT board_ranges.tx AS x, board_ranges.ty AS y FROM (board_ranges NATURAL JOIN pieces) WHERE ((pieces.ptype = ANY (ARRAY['magic_tree'::text, 'shadow_tree'::text])) AND (board_ranges.range = 1));


--
-- TOC entry 2513 (class 1259 OID 258653)
-- Dependencies: 6
-- Name: crimes_against_nature; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE crimes_against_nature (
    ptype text NOT NULL,
    allegiance text NOT NULL,
    tag integer NOT NULL
);


--
-- TOC entry 2512 (class 1259 OID 258615)
-- Dependencies: 6
-- Name: imaginary_pieces; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE imaginary_pieces (
    ptype text NOT NULL,
    allegiance text NOT NULL,
    tag integer NOT NULL
);


--
-- TOC entry 2486 (class 1259 OID 258312)
-- Dependencies: 882 6
-- Name: piece_prototypes_mr; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE piece_prototypes_mr (
    ptype text NOT NULL,
    flying boolean,
    speed integer,
    agility integer,
    undead boolean,
    ridable boolean,
    ranged_weapon_type ranged_weapon_type,
    range integer,
    ranged_attack_strength integer,
    attack_strength integer,
    physical_defense integer,
    magic_defense integer
);


--
-- TOC entry 2504 (class 1259 OID 258435)
-- Dependencies: 3056 3057 3058 3059 3060 3061 3062 3063 6
-- Name: wizards; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE wizards (
    wizard_name text NOT NULL,
    shadow_form boolean DEFAULT false NOT NULL,
    magic_sword boolean DEFAULT false NOT NULL,
    magic_knife boolean DEFAULT false NOT NULL,
    magic_shield boolean DEFAULT false NOT NULL,
    magic_wings boolean DEFAULT false NOT NULL,
    magic_armour boolean DEFAULT false NOT NULL,
    magic_bow boolean DEFAULT false NOT NULL,
    computer_controlled boolean NOT NULL,
    original_place integer NOT NULL,
    expired boolean DEFAULT false NOT NULL
);


--
-- TOC entry 2514 (class 1259 OID 258693)
-- Dependencies: 2777 6
-- Name: wizard_upgrade_stats; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW wizard_upgrade_stats AS
    SELECT pp.ptype, p.allegiance, p.tag, p.x, p.y, false AS imaginary, wizards.magic_wings AS flying, CASE WHEN wizards.magic_wings THEN 6 WHEN wizards.shadow_form THEN 3 ELSE pp.speed END AS speed, CASE WHEN wizards.shadow_form THEN (pp.agility + 2) ELSE pp.agility END AS agility, pp.undead, pp.ridable, CASE WHEN wizards.magic_bow THEN 'projectile'::text ELSE NULL::text END AS ranged_weapon_type, CASE WHEN wizards.magic_bow THEN 6 ELSE NULL::integer END AS range, CASE WHEN wizards.magic_bow THEN 6 ELSE NULL::integer END AS ranged_attack_strength, CASE WHEN wizards.magic_sword THEN (pp.attack_strength + 4) WHEN wizards.magic_knife THEN (pp.attack_strength + 2) ELSE pp.attack_strength END AS attack_strength, CASE WHEN (wizards.magic_armour AND wizards.shadow_form) THEN (pp.physical_defense + 6) WHEN (wizards.magic_shield AND wizards.shadow_form) THEN (pp.physical_defense + 4) WHEN wizards.magic_armour THEN (pp.physical_defense + 4) WHEN wizards.magic_shield THEN (pp.physical_defense + 2) WHEN wizards.shadow_form THEN (pp.physical_defense + 2) ELSE pp.physical_defense END AS physical_defense, pp.magic_defense FROM ((pieces p JOIN wizards ON ((p.allegiance = wizards.wizard_name))) JOIN piece_prototypes_mr pp ON ((pp.ptype = 'wizard'::text))) WHERE (p.ptype = 'wizard'::text);


--
-- TOC entry 2515 (class 1259 OID 258698)
-- Dependencies: 2778 6
-- Name: pieces_mr; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW pieces_mr AS
    SELECT pieces.ptype, pieces.allegiance, pieces.tag, pieces.x, pieces.y, COALESCE(a.imaginary, CASE WHEN (NOT (piece_prototypes_mr.ridable IS NULL)) THEN false ELSE NULL::boolean END) AS imaginary, piece_prototypes_mr.flying, piece_prototypes_mr.speed, piece_prototypes_mr.agility, COALESCE(b.raised, piece_prototypes_mr.undead) AS undead, piece_prototypes_mr.ridable, piece_prototypes_mr.ranged_weapon_type, piece_prototypes_mr.range, piece_prototypes_mr.ranged_attack_strength, piece_prototypes_mr.attack_strength, piece_prototypes_mr.physical_defense, piece_prototypes_mr.magic_defense FROM (((pieces NATURAL JOIN piece_prototypes_mr) NATURAL LEFT JOIN (SELECT imaginary_pieces.ptype, imaginary_pieces.allegiance, imaginary_pieces.tag, true AS imaginary FROM imaginary_pieces) a) NATURAL LEFT JOIN (SELECT crimes_against_nature.ptype, crimes_against_nature.allegiance, crimes_against_nature.tag, true AS raised FROM crimes_against_nature) b) WHERE (pieces.ptype <> 'wizard'::text) UNION SELECT wizard_upgrade_stats.ptype, wizard_upgrade_stats.allegiance, wizard_upgrade_stats.tag, wizard_upgrade_stats.x, wizard_upgrade_stats.y, wizard_upgrade_stats.imaginary, wizard_upgrade_stats.flying, wizard_upgrade_stats.speed, wizard_upgrade_stats.agility, wizard_upgrade_stats.undead, wizard_upgrade_stats.ridable, wizard_upgrade_stats.ranged_weapon_type, wizard_upgrade_stats.range, wizard_upgrade_stats.ranged_attack_strength, wizard_upgrade_stats.attack_strength, wizard_upgrade_stats.physical_defense, wizard_upgrade_stats.magic_defense FROM wizard_upgrade_stats;


--
-- TOC entry 2521 (class 1259 OID 258723)
-- Dependencies: 2784 6
-- Name: attackable_pieces; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW attackable_pieces AS
    SELECT pieces_mr.ptype, pieces_mr.allegiance, pieces_mr.tag, pieces_mr.x, pieces_mr.y, pieces_mr.physical_defense FROM pieces_mr WHERE (pieces_mr.physical_defense IS NOT NULL);


--
-- TOC entry 2489 (class 1259 OID 258329)
-- Dependencies: 2763 6
-- Name: monster_prototypes; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW monster_prototypes AS
    SELECT piece_prototypes_mr.ptype, piece_prototypes_mr.flying, piece_prototypes_mr.speed, piece_prototypes_mr.agility, piece_prototypes_mr.undead, piece_prototypes_mr.ridable FROM piece_prototypes_mr WHERE ((piece_prototypes_mr.undead IS NOT NULL) AND (piece_prototypes_mr.ridable IS NOT NULL));


--
-- TOC entry 2545 (class 1259 OID 259912)
-- Dependencies: 2791 6
-- Name: pieces_with_priorities; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW pieces_with_priorities AS
    SELECT pieces.ptype, pieces.allegiance, pieces.tag, pieces.x, pieces.y, CASE WHEN (pieces.allegiance = 'dead'::text) THEN 3 WHEN (pieces.ptype = 'wizard'::text) THEN 2 WHEN (pieces.ptype IN (SELECT monster_prototypes.ptype FROM monster_prototypes)) THEN 1 ELSE 0 END AS sp FROM pieces;


--
-- TOC entry 2546 (class 1259 OID 259916)
-- Dependencies: 2792 6
-- Name: pieces_on_top; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW pieces_on_top AS
    SELECT pwp.x, pwp.y, pwp.ptype, pwp.allegiance, pwp.tag, pwp.sp FROM (SELECT row_number() OVER (PARTITION BY ROW(pieces_with_priorities.x, pieces_with_priorities.y) ORDER BY pieces_with_priorities.sp) AS rn, pieces_with_priorities.x, pieces_with_priorities.y, pieces_with_priorities.ptype, pieces_with_priorities.allegiance, pieces_with_priorities.tag, pieces_with_priorities.sp FROM pieces_with_priorities) pwp WHERE (pwp.rn = 1);


--
-- TOC entry 2556 (class 1259 OID 259957)
-- Dependencies: 2802 6
-- Name: attackable_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW attackable_squares AS
    SELECT attackable_pieces.x, attackable_pieces.y FROM (attackable_pieces NATURAL JOIN pieces_on_top);


--
-- TOC entry 2517 (class 1259 OID 258707)
-- Dependencies: 2780 6
-- Name: monster_pieces; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW monster_pieces AS
    SELECT pieces_mr.ptype, pieces_mr.allegiance, pieces_mr.tag, pieces_mr.x, pieces_mr.y, pieces_mr.flying, pieces_mr.speed, pieces_mr.agility, pieces_mr.undead, pieces_mr.ridable, pieces_mr.imaginary FROM pieces_mr WHERE (((((pieces_mr.flying IS NOT NULL) AND (pieces_mr.speed IS NOT NULL)) AND (pieces_mr.agility IS NOT NULL)) AND (pieces_mr.undead IS NOT NULL)) AND (pieces_mr.ridable IS NOT NULL));


--
-- TOC entry 2518 (class 1259 OID 258711)
-- Dependencies: 2781 6
-- Name: dead_monster_pieces; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW dead_monster_pieces AS
    SELECT monster_pieces.ptype, monster_pieces.allegiance, monster_pieces.tag, monster_pieces.x, monster_pieces.y, monster_pieces.flying, monster_pieces.speed, monster_pieces.agility, monster_pieces.undead, monster_pieces.ridable, monster_pieces.imaginary FROM monster_pieces WHERE (monster_pieces.allegiance = 'dead'::text);


--
-- TOC entry 2552 (class 1259 OID 259941)
-- Dependencies: 2798 6
-- Name: corpse_only_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW corpse_only_squares AS
    SELECT pieces_on_top.x, pieces_on_top.y FROM (pieces_on_top NATURAL JOIN dead_monster_pieces);


--
-- TOC entry 2516 (class 1259 OID 258703)
-- Dependencies: 2779 6
-- Name: creature_pieces; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW creature_pieces AS
    SELECT pieces_mr.ptype, pieces_mr.allegiance, pieces_mr.tag, pieces_mr.x, pieces_mr.y, pieces_mr.flying, pieces_mr.speed, pieces_mr.agility FROM pieces_mr WHERE (((pieces_mr.flying IS NOT NULL) AND (pieces_mr.speed IS NOT NULL)) AND (pieces_mr.agility IS NOT NULL));


--
-- TOC entry 2557 (class 1259 OID 259961)
-- Dependencies: 2803 6
-- Name: creature_on_top_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW creature_on_top_squares AS
    SELECT creature_pieces.x, creature_pieces.y FROM (creature_pieces NATURAL JOIN pieces_on_top);


--
-- TOC entry 2527 (class 1259 OID 258851)
-- Dependencies: 6
-- Name: current_wizard_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE current_wizard_table (
    current_wizard text NOT NULL
);


--
-- TOC entry 2497 (class 1259 OID 258370)
-- Dependencies: 2770 6 899
-- Name: spell_ranges; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spell_ranges AS
    SELECT spells_mr.spell_name, spells_mr.base_chance, spells_mr.alignment, spells_mr.spell_category, spells_mr.description, spells_mr.range FROM spells_mr WHERE (spells_mr.range IS NOT NULL);


--
-- TOC entry 2531 (class 1259 OID 259001)
-- Dependencies: 6
-- Name: wizard_spell_choices_mr; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE wizard_spell_choices_mr (
    wizard_name text NOT NULL,
    spell_name text NOT NULL,
    imaginary boolean
);


--
-- TOC entry 2532 (class 1259 OID 259045)
-- Dependencies: 2788 6
-- Name: wizard_spell_choices; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW wizard_spell_choices AS
    SELECT wizard_spell_choices_mr.wizard_name, wizard_spell_choices_mr.spell_name FROM wizard_spell_choices_mr;


--
-- TOC entry 2561 (class 1259 OID 259977)
-- Dependencies: 2807 6
-- Name: current_wizard_spell_range_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_wizard_spell_range_squares AS
    SELECT board_ranges.tx AS x, board_ranges.ty AS y FROM board_ranges WHERE ((board_ranges.x, board_ranges.y, board_ranges.range) = (SELECT pieces.x, pieces.y, spell_ranges.range FROM (((pieces JOIN current_wizard_table ON ((pieces.allegiance = current_wizard_table.current_wizard))) JOIN wizard_spell_choices ON ((wizard_spell_choices.wizard_name = current_wizard_table.current_wizard))) NATURAL JOIN spell_ranges) WHERE (pieces.ptype = 'wizard'::text)));


--
-- TOC entry 2551 (class 1259 OID 259937)
-- Dependencies: 2797 6
-- Name: empty_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW empty_squares AS
    SELECT x.x, y.y FROM (generate_series(0, 14) x(x) CROSS JOIN generate_series(0, 9) y(y)) EXCEPT SELECT pieces.x, pieces.y FROM pieces;


--
-- TOC entry 2555 (class 1259 OID 259953)
-- Dependencies: 2801 6
-- Name: empty_and_not_adjacent_to_tree_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW empty_and_not_adjacent_to_tree_squares AS
    SELECT empty_squares.x, empty_squares.y FROM empty_squares EXCEPT SELECT adjacent_to_tree_squares.x, adjacent_to_tree_squares.y FROM adjacent_to_tree_squares;


--
-- TOC entry 2553 (class 1259 OID 259945)
-- Dependencies: 2799 6
-- Name: empty_or_corpse_only_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW empty_or_corpse_only_squares AS
    SELECT empty_squares.x, empty_squares.y FROM empty_squares UNION SELECT corpse_only_squares.x, corpse_only_squares.y FROM corpse_only_squares;


--
-- TOC entry 2558 (class 1259 OID 259965)
-- Dependencies: 2804 6
-- Name: monster_on_top_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW monster_on_top_squares AS
    SELECT monster_pieces.x, monster_pieces.y FROM (monster_pieces NATURAL JOIN pieces_on_top);


--
-- TOC entry 2496 (class 1259 OID 258366)
-- Dependencies: 2769 899 6 901
-- Name: spell_valid_square_types; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spell_valid_square_types AS
    SELECT spells_mr.spell_name, spells_mr.base_chance, spells_mr.alignment, spells_mr.spell_category, spells_mr.description, spells_mr.valid_square_category FROM spells_mr WHERE (spells_mr.valid_square_category IS NOT NULL);


--
-- TOC entry 2559 (class 1259 OID 259969)
-- Dependencies: 2805 6
-- Name: spell_valid_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spell_valid_squares AS
    (((((SELECT 'empty' AS valid_square_category, empty_squares.x, empty_squares.y FROM empty_squares UNION SELECT 'empty_or_corpse_only' AS valid_square_category, empty_or_corpse_only_squares.x, empty_or_corpse_only_squares.y FROM empty_or_corpse_only_squares) UNION SELECT 'attackable' AS valid_square_category, attackable_squares.x, attackable_squares.y FROM attackable_squares) UNION SELECT 'creature_on_top' AS valid_square_category, creature_on_top_squares.x, creature_on_top_squares.y FROM creature_on_top_squares) UNION SELECT 'monster_on_top' AS valid_square_category, monster_on_top_squares.x, monster_on_top_squares.y FROM monster_on_top_squares) UNION SELECT 'corpse_only' AS valid_square_category, corpse_only_squares.x, corpse_only_squares.y FROM corpse_only_squares) UNION SELECT 'empty_and_not_adjacent_to_tree' AS valid_square_category, empty_and_not_adjacent_to_tree_squares.x, empty_and_not_adjacent_to_tree_squares.y FROM empty_and_not_adjacent_to_tree_squares;


--
-- TOC entry 2560 (class 1259 OID 259973)
-- Dependencies: 2806 6
-- Name: current_wizard_spell_type_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_wizard_spell_type_squares AS
    SELECT spell_valid_squares.x, spell_valid_squares.y FROM (((wizard_spell_choices JOIN current_wizard_table ON ((wizard_spell_choices.wizard_name = current_wizard_table.current_wizard))) NATURAL JOIN spell_valid_square_types) NATURAL JOIN spell_valid_squares);


--
-- TOC entry 2562 (class 1259 OID 259982)
-- Dependencies: 2808 6
-- Name: current_wizard_spell_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_wizard_spell_squares AS
    (SELECT current_wizard_spell_type_squares.x, current_wizard_spell_type_squares.y FROM current_wizard_spell_type_squares INTERSECT SELECT current_wizard_spell_range_squares.x, current_wizard_spell_range_squares.y FROM current_wizard_spell_range_squares) EXCEPT SELECT pieces.x, pieces.y FROM (pieces JOIN current_wizard_table ON ((pieces.allegiance = current_wizard_table.current_wizard))) WHERE (pieces.ptype = 'wizard'::text);


--
-- TOC entry 2492 (class 1259 OID 258341)
-- Dependencies: 2766 6
-- Name: enterable_piece_types; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW enterable_piece_types AS
    (SELECT 'magic_tree'::text AS ptype UNION SELECT 'magic_castle' AS ptype) UNION SELECT 'dark_citadel' AS ptype;


--
-- TOC entry 2543 (class 1259 OID 259778)
-- Dependencies: 6
-- Name: game_completed_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE game_completed_table (
    game_completed boolean NOT NULL
);


--
-- TOC entry 2548 (class 1259 OID 259924)
-- Dependencies: 2794 6
-- Name: moving_pieces; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW moving_pieces AS
    SELECT pieces_mr.ptype, pieces_mr.allegiance, pieces_mr.tag, pieces_mr.x, pieces_mr.y FROM pieces_mr WHERE (((pieces_mr.speed IS NOT NULL) OR (pieces_mr.attack_strength IS NOT NULL)) OR (pieces_mr.ranged_attack_strength IS NOT NULL));


--
-- TOC entry 2539 (class 1259 OID 259466)
-- Dependencies: 6
-- Name: pieces_to_move; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE pieces_to_move (
    ptype text NOT NULL,
    allegiance text NOT NULL,
    tag integer NOT NULL
);


--
-- TOC entry 2520 (class 1259 OID 258719)
-- Dependencies: 2783 6
-- Name: ranged_weapon_pieces; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW ranged_weapon_pieces AS
    SELECT pieces_mr.ptype, pieces_mr.allegiance, pieces_mr.tag, pieces_mr.x, pieces_mr.y, pieces_mr.ranged_weapon_type, pieces_mr.range, pieces_mr.ranged_attack_strength FROM pieces_mr WHERE (((pieces_mr.ranged_weapon_type IS NOT NULL) AND (pieces_mr.range IS NOT NULL)) AND (pieces_mr.ranged_attack_strength IS NOT NULL));


--
-- TOC entry 2491 (class 1259 OID 258337)
-- Dependencies: 2765 6
-- Name: ridable_prototypes; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW ridable_prototypes AS
    SELECT piece_prototypes_mr.ptype FROM piece_prototypes_mr WHERE piece_prototypes_mr.ridable;


--
-- TOC entry 2549 (class 1259 OID 259928)
-- Dependencies: 2795 6
-- Name: selectable_pieces_with_priorities; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selectable_pieces_with_priorities AS
    SELECT moving_pieces.ptype, moving_pieces.allegiance, moving_pieces.tag, moving_pieces.x, moving_pieces.y, CASE WHEN (moving_pieces.ptype = 'wizard'::text) THEN 0 ELSE 1 END AS sp FROM moving_pieces WHERE (NOT ((moving_pieces.x, moving_pieces.y) IN (SELECT pieces.x, pieces.y FROM pieces WHERE (pieces.ptype = 'gooey_blob'::text))));


--
-- TOC entry 2540 (class 1259 OID 259536)
-- Dependencies: 6 1010
-- Name: selected_piece; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE selected_piece (
    ptype text NOT NULL,
    allegiance text NOT NULL,
    tag integer NOT NULL,
    move_phase move_phase NOT NULL,
    engaged boolean NOT NULL
);


--
-- TOC entry 2573 (class 1259 OID 260031)
-- Dependencies: 2819 6
-- Name: selectable_pieces; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selectable_pieces AS
    SELECT s.rn, s.ptype, s.allegiance, s.tag, s.x, s.y, s.sp FROM (SELECT row_number() OVER (PARTITION BY ROW(selectable_pieces_with_priorities.x, selectable_pieces_with_priorities.y) ORDER BY selectable_pieces_with_priorities.sp ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS rn, selectable_pieces_with_priorities.ptype, selectable_pieces_with_priorities.allegiance, selectable_pieces_with_priorities.tag, selectable_pieces_with_priorities.x, selectable_pieces_with_priorities.y, selectable_pieces_with_priorities.sp FROM (selectable_pieces_with_priorities NATURAL JOIN pieces_to_move) WHERE (NOT (EXISTS (SELECT 1 FROM selected_piece)))) s WHERE (s.rn = 1);


--
-- TOC entry 2568 (class 1259 OID 260009)
-- Dependencies: 2814 6
-- Name: selected_piece_attackable_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piece_attackable_squares AS
    SELECT t.x, t.y FROM ((pieces_on_top t NATURAL JOIN pieces_mr p) CROSS JOIN selected_piece s) WHERE (((((p.physical_defense IS NOT NULL) AND (p.allegiance <> s.allegiance)) AND (p.allegiance <> 'dead'::text)) AND (NOT ((p.ptype = 'magic_tree'::text) AND (s.ptype = 'wizard'::text)))) AND (((NOT COALESCE(p.undead, false)) OR COALESCE((SELECT COALESCE(pieces_mr.undead, false) AS "coalesce" FROM (pieces_mr NATURAL JOIN selected_piece)), false)) OR COALESCE(((s.ptype = 'wizard'::text) AND is_equipped(s.allegiance)), false)));


--
-- TOC entry 2565 (class 1259 OID 259996)
-- Dependencies: 2811 6
-- Name: squares_within_selected_piece_flight_range; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW squares_within_selected_piece_flight_range AS
    SELECT board_ranges.tx AS x, board_ranges.ty AS y FROM ((board_ranges NATURAL JOIN selected_piece) NATURAL JOIN creature_pieces) WHERE (creature_pieces.flying AND (board_ranges.range <= creature_pieces.speed));


--
-- TOC entry 2570 (class 1259 OID 260019)
-- Dependencies: 2816 6
-- Name: selected_piece_fly_attack_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piece_fly_attack_squares AS
    SELECT selected_piece_attackable_squares.x, selected_piece_attackable_squares.y FROM (selected_piece_attackable_squares NATURAL JOIN squares_within_selected_piece_flight_range) WHERE (((SELECT selected_piece.move_phase FROM selected_piece))::text = 'motion'::text);


--
-- TOC entry 2563 (class 1259 OID 259986)
-- Dependencies: 2809 6
-- Name: selected_piece_move_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piece_move_squares AS
    SELECT empty_or_corpse_only_squares.x, empty_or_corpse_only_squares.y FROM empty_or_corpse_only_squares UNION SELECT pieces.x, pieces.y FROM (pieces NATURAL JOIN (SELECT enterable_piece_types.ptype FROM enterable_piece_types WHERE (SELECT (selected_piece.ptype = 'wizard'::text) FROM selected_piece) UNION SELECT ridable_prototypes.ptype FROM ridable_prototypes WHERE (SELECT (selected_piece.ptype = 'wizard'::text) FROM selected_piece)) a) WHERE (pieces.allegiance = (SELECT selected_piece.allegiance FROM selected_piece));


--
-- TOC entry 2566 (class 1259 OID 260000)
-- Dependencies: 2812 6
-- Name: selected_piece_fly_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piece_fly_squares AS
    SELECT selected_piece_move_squares.x, selected_piece_move_squares.y FROM selected_piece_move_squares INTERSECT SELECT squares_within_selected_piece_flight_range.x, squares_within_selected_piece_flight_range.y FROM squares_within_selected_piece_flight_range WHERE (((SELECT selected_piece.move_phase FROM selected_piece))::text = 'motion'::text);


--
-- TOC entry 2571 (class 1259 OID 260023)
-- Dependencies: 2817 6
-- Name: selected_piece_in_range_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piece_in_range_squares AS
    SELECT b.tx AS x, b.ty AS y FROM ((board_ranges b NATURAL JOIN ranged_weapon_pieces s) NATURAL JOIN selected_piece) WHERE (b.range <= s.range);


--
-- TOC entry 2572 (class 1259 OID 260027)
-- Dependencies: 2818 6
-- Name: selected_piece_ranged_attackable_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piece_ranged_attackable_squares AS
    SELECT selected_piece_attackable_squares.x, selected_piece_attackable_squares.y FROM (selected_piece_attackable_squares NATURAL JOIN selected_piece_in_range_squares);


--
-- TOC entry 2567 (class 1259 OID 260004)
-- Dependencies: 2813 6 1010
-- Name: selected_piecexy; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piecexy AS
    SELECT selected_piece.ptype, selected_piece.allegiance, selected_piece.tag, selected_piece.move_phase, selected_piece.engaged, pieces.x, pieces.y FROM (selected_piece NATURAL JOIN pieces);


--
-- TOC entry 2569 (class 1259 OID 260014)
-- Dependencies: 2815 6
-- Name: selected_piece_walk_attack_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piece_walk_attack_squares AS
    SELECT selected_piece_attackable_squares.x, selected_piece_attackable_squares.y FROM selected_piece_attackable_squares INTERSECT SELECT r.tx AS x, r.ty AS y FROM (board_ranges r NATURAL JOIN selected_piecexy) WHERE ((r.range = 1) AND ((selected_piecexy.move_phase)::text = ANY (ARRAY['motion'::text, 'attack'::text])));


--
-- TOC entry 2564 (class 1259 OID 259991)
-- Dependencies: 2810 6
-- Name: selected_piece_walk_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piece_walk_squares AS
    SELECT selected_piece_move_squares.x, selected_piece_move_squares.y FROM selected_piece_move_squares INTERSECT SELECT board_ranges.tx AS x, board_ranges.ty AS y FROM ((board_ranges NATURAL JOIN selected_piece) NATURAL JOIN pieces) WHERE (((board_ranges.range = 1) AND (NOT (SELECT (creature_pieces.flying OR selected_piece.engaged) FROM (creature_pieces NATURAL JOIN selected_piece)))) AND (get_remaining_walk() > 0));


--
-- TOC entry 2574 (class 1259 OID 260035)
-- Dependencies: 2820 6
-- Name: valid_target_actions; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW valid_target_actions AS
    SELECT s.x, s.y, s.action FROM (SELECT current_wizard_spell_squares.x, current_wizard_spell_squares.y, 'cast_target_spell'::text AS action FROM current_wizard_spell_squares WHERE ((get_turn_phase())::text = 'cast'::text) UNION SELECT s1.x, s1.y, s1.action FROM (((((SELECT selectable_pieces.x, selectable_pieces.y, 'select_piece_at_position'::text AS action FROM selectable_pieces UNION SELECT selected_piece_walk_squares.x, selected_piece_walk_squares.y, 'walk'::text AS action FROM selected_piece_walk_squares) UNION SELECT selected_piece_fly_squares.x, selected_piece_fly_squares.y, 'fly'::text AS action FROM selected_piece_fly_squares) UNION SELECT selected_piece_walk_attack_squares.x, selected_piece_walk_attack_squares.y, 'attack'::text AS action FROM selected_piece_walk_attack_squares) UNION SELECT selected_piece_fly_attack_squares.x, selected_piece_fly_attack_squares.y, 'attack'::text AS action FROM selected_piece_fly_attack_squares) UNION SELECT selected_piece_ranged_attackable_squares.x, selected_piece_ranged_attackable_squares.y, 'ranged_attack'::text AS action FROM selected_piece_ranged_attackable_squares) s1 WHERE ((get_turn_phase())::text = 'move'::text)) s WHERE (NOT (EXISTS (SELECT 1 FROM game_completed_table)));


--
-- TOC entry 2596 (class 1259 OID 260475)
-- Dependencies: 2832 6
-- Name: ai_filtered_target_spells; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW ai_filtered_target_spells AS
    SELECT valid_target_actions.x, valid_target_actions.y, valid_target_actions.action FROM valid_target_actions WHERE ((valid_target_actions.action = 'cast_target_spell'::text) AND (NOT ((valid_target_actions.x, valid_target_actions.y) IN (SELECT pieces_on_top.x, pieces_on_top.y FROM pieces_on_top WHERE (pieces_on_top.allegiance = ANY (ARRAY[get_current_wizard(), 'dead'::text]))))));


--
-- TOC entry 2597 (class 1259 OID 260481)
-- Dependencies: 2833 6
-- Name: ai_selected_piece_actions; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW ai_selected_piece_actions AS
    SELECT a.x, a.y, a.action FROM (valid_target_actions a LEFT JOIN pieces_on_top p USING (x, y)) WHERE ((a.action = ANY (ARRAY['walk'::text, 'fly'::text])) OR ((a.action = ANY (ARRAY['attack'::text, 'ranged_attack'::text])) AND (p.allegiance <> ALL (ARRAY[get_current_wizard(), 'dead'::text]))));


--
-- TOC entry 2593 (class 1259 OID 260460)
-- Dependencies: 2829 6
-- Name: current_wizard_square; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_wizard_square AS
    SELECT pieces.x, pieces.y FROM (pieces JOIN current_wizard_table ON ((pieces.allegiance = current_wizard_table.current_wizard))) WHERE (pieces.ptype = 'wizard'::text);


--
-- TOC entry 2507 (class 1259 OID 258458)
-- Dependencies: 6
-- Name: spell_books; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE spell_books (
    id integer NOT NULL,
    wizard_name text NOT NULL,
    spell_name text NOT NULL
);


--
-- TOC entry 2592 (class 1259 OID 260456)
-- Dependencies: 2828 6
-- Name: current_wizard_target_spells; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_wizard_target_spells AS
    SELECT spell_books.spell_name, spell_ranges.range FROM ((spell_books JOIN current_wizard_table ON ((current_wizard_table.current_wizard = spell_books.wizard_name))) NATURAL JOIN spell_ranges);


--
-- TOC entry 2594 (class 1259 OID 260464)
-- Dependencies: 2830 6
-- Name: castable_target_spells; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW castable_target_spells AS
    SELECT cwts.spell_name, svs.x, svs.y FROM (((current_wizard_target_spells cwts NATURAL JOIN spell_valid_squares svs) NATURAL JOIN spell_valid_square_types svst) JOIN board_ranges br ON (((((br.x, br.y) = (SELECT current_wizard_square.x, current_wizard_square.y FROM current_wizard_square)) AND (br.range = cwts.range)) AND ((br.tx = svs.x) AND (br.ty = svs.y)))));


--
-- TOC entry 2595 (class 1259 OID 260469)
-- Dependencies: 2831 6
-- Name: ai_useful_spells; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW ai_useful_spells AS
    SELECT spell_books.spell_name FROM ((spell_books JOIN current_wizard_table ON ((spell_books.wizard_name = current_wizard_table.current_wizard))) NATURAL JOIN activate_spells) UNION SELECT castable_target_spells.spell_name FROM castable_target_spells WHERE (NOT ((castable_target_spells.x, castable_target_spells.y) IN (SELECT corpse_only_squares.x, corpse_only_squares.y FROM corpse_only_squares UNION SELECT pieces_on_top.x, pieces_on_top.y FROM (pieces_on_top JOIN current_wizard_table ON ((current_wizard_table.current_wizard = pieces_on_top.allegiance))))));


--
-- TOC entry 2456 (class 1259 OID 258040)
-- Dependencies: 2741 6
-- Name: base_relvars; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW base_relvars AS
    SELECT pg_class.relname AS relvar_name FROM pg_class WHERE ((pg_class.relnamespace = (SELECT pg_namespace.oid FROM pg_namespace WHERE (pg_namespace.nspname = 'public'::name))) AND (pg_class.relkind = 'r'::"char"));


--
-- TOC entry 2457 (class 1259 OID 258044)
-- Dependencies: 2742 6
-- Name: base_relvar_attributes; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW base_relvar_attributes AS
    SELECT pg_attribute.attname AS attribute_name, pg_type.typname AS type_name, pg_class.relname AS relvar_name FROM (((pg_attribute JOIN pg_class ON ((pg_attribute.attrelid = pg_class.oid))) JOIN pg_type ON ((pg_attribute.atttypid = pg_type.oid))) JOIN base_relvars ON ((pg_class.relname = base_relvars.relvar_name))) WHERE (pg_attribute.attnum >= 1);


--
-- TOC entry 2466 (class 1259 OID 258085)
-- Dependencies: 6
-- Name: database_constraints; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE database_constraints (
    constraint_name text NOT NULL,
    expression text NOT NULL
);


--
-- TOC entry 2461 (class 1259 OID 258063)
-- Dependencies: 2746 6
-- Name: operators; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW operators AS
    SELECT pg_proc.proname AS operator_name FROM pg_proc WHERE (pg_proc.pronamespace = (SELECT pg_namespace.oid FROM pg_namespace WHERE (pg_namespace.nspname = 'public'::name)));


--
-- TOC entry 2458 (class 1259 OID 258049)
-- Dependencies: 2743 6
-- Name: scalars; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW scalars AS
    SELECT DISTINCT base_relvar_attributes.type_name AS scalar_name FROM base_relvar_attributes;


--
-- TOC entry 2463 (class 1259 OID 258071)
-- Dependencies: 2748 6
-- Name: triggers; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW triggers AS
    SELECT pg_class.relname AS relvar_name, pg_trigger.tgname AS trigger_name, pg_proc.proname AS operator_name FROM (((pg_trigger JOIN pg_class ON ((pg_trigger.tgrelid = pg_class.oid))) JOIN pg_proc ON ((pg_trigger.tgfoid = pg_proc.oid))) JOIN base_relvars ON ((pg_class.relname = base_relvars.relvar_name))) WHERE (NOT pg_trigger.tgisconstraint);


--
-- TOC entry 2464 (class 1259 OID 258076)
-- Dependencies: 2749 6
-- Name: views; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW views AS
    SELECT pg_views.viewname AS view_name, pg_views.definition FROM pg_views WHERE (pg_views.schemaname = 'public'::name);


--
-- TOC entry 2467 (class 1259 OID 258091)
-- Dependencies: 2751 6
-- Name: all_database_objects; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW all_database_objects AS
    ((((SELECT 'scalar' AS object_type, scalars.scalar_name AS object_name FROM scalars UNION SELECT 'base_relvar' AS object_type, base_relvars.relvar_name AS object_name FROM base_relvars) UNION SELECT 'operator' AS object_type, operators.operator_name AS object_name FROM operators) UNION SELECT 'view' AS object_type, views.view_name AS object_name FROM views) UNION SELECT 'trigger' AS object_type, triggers.trigger_name AS object_name FROM triggers) UNION SELECT 'database_constraint' AS object_type, database_constraints.constraint_name AS object_name FROM database_constraints;


--
-- TOC entry 2481 (class 1259 OID 258251)
-- Dependencies: 6
-- Name: all_module_objects; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE all_module_objects (
    object_name text NOT NULL,
    object_type text NOT NULL,
    module_name text NOT NULL
);


--
-- TOC entry 2508 (class 1259 OID 258512)
-- Dependencies: 2776 6
-- Name: allegiances; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW allegiances AS
    SELECT wizards.wizard_name AS allegiance FROM wizards WHERE (wizards.expired = false) UNION SELECT 'dead' AS allegiance;


--
-- TOC entry 2519 (class 1259 OID 258715)
-- Dependencies: 2782 6
-- Name: attacking_pieces; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW attacking_pieces AS
    SELECT pieces_mr.ptype, pieces_mr.allegiance, pieces_mr.tag, pieces_mr.x, pieces_mr.y, pieces_mr.attack_strength FROM pieces_mr WHERE ((pieces_mr.attack_strength IS NOT NULL) AND (pieces_mr.allegiance <> 'dead'::text));


--
-- TOC entry 2650 (class 1259 OID 261512)
-- Dependencies: 6
-- Name: b; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE b (
    adrelid oid,
    adnum smallint,
    adbin text,
    adsrc text
);


--
-- TOC entry 2460 (class 1259 OID 258058)
-- Dependencies: 2745 6
-- Name: base_relvar_key_attributes; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW base_relvar_key_attributes AS
    SELECT a.constraint_name, c.attribute_name FROM (((SELECT pg_constraint.conname AS constraint_name, pg_constraint.conrelid, pg_constraint.conkey[generate_series.generate_series] AS attnum FROM (pg_constraint CROSS JOIN generate_series(1, (SELECT max(array_upper(pg_constraint.conkey, 1)) AS max FROM pg_constraint)) generate_series(generate_series)) WHERE (((pg_constraint.contype = ANY (ARRAY['p'::"char", 'u'::"char"])) AND (pg_constraint.connamespace = (SELECT pg_namespace.oid FROM pg_namespace WHERE (pg_namespace.nspname = 'public'::name)))) AND ((generate_series.generate_series >= array_lower(pg_constraint.conkey, 1)) AND (generate_series.generate_series <= array_upper(pg_constraint.conkey, 1))))) a NATURAL JOIN (SELECT pg_class.oid AS conrelid, pg_class.relname AS relvar_name FROM pg_class) b) NATURAL JOIN (SELECT pg_attribute.attrelid AS conrelid, pg_attribute.attname AS attribute_name, pg_attribute.attnum FROM pg_attribute) c);


--
-- TOC entry 2459 (class 1259 OID 258053)
-- Dependencies: 2744 6
-- Name: base_relvar_keys; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW base_relvar_keys AS
    SELECT pg_constraint.conname AS constraint_name, b.relvar_name FROM (pg_constraint NATURAL JOIN (SELECT pg_class.oid AS conrelid, pg_class.relname AS relvar_name FROM pg_class) b) WHERE ((pg_constraint.contype = ANY (ARRAY['p'::"char", 'u'::"char"])) AND (pg_constraint.connamespace = (SELECT pg_namespace.oid FROM pg_namespace WHERE (pg_namespace.nspname = 'public'::name))));


--
-- TOC entry 2484 (class 1259 OID 258281)
-- Dependencies: 3055 6
-- Name: base_relvar_metadata; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE base_relvar_metadata (
    relvar_name text NOT NULL,
    type text NOT NULL,
    CONSTRAINT base_relvar_metadata_type_check CHECK ((type = ANY (ARRAY['readonly'::text, 'data'::text, 'stack'::text])))
);


--
-- TOC entry 2619 (class 1259 OID 260826)
-- Dependencies: 6
-- Name: board_beam_effects; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE board_beam_effects (
    id integer NOT NULL,
    subtype text NOT NULL,
    x1 integer NOT NULL,
    y1 integer NOT NULL,
    x2 integer NOT NULL,
    y2 integer NOT NULL,
    queuepos integer NOT NULL
);


--
-- TOC entry 2618 (class 1259 OID 260824)
-- Dependencies: 6 2619
-- Name: board_beam_effects_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE board_beam_effects_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- TOC entry 3412 (class 0 OID 0)
-- Dependencies: 2618
-- Name: board_beam_effects_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE board_beam_effects_id_seq OWNED BY board_beam_effects.id;


--
-- TOC entry 3413 (class 0 OID 0)
-- Dependencies: 2618
-- Name: board_beam_effects_id_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('board_beam_effects_id_seq', 45, true);


--
-- TOC entry 2611 (class 1259 OID 260775)
-- Dependencies: 2841 6
-- Name: board_highlights; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW board_highlights AS
    SELECT current_wizard_spell_squares.x, current_wizard_spell_squares.y, 'highlight_cast_target_spell' AS sprite FROM current_wizard_spell_squares WHERE ((get_turn_phase())::text = 'choose'::text) UNION SELECT valid_target_actions.x, valid_target_actions.y, ('highlight_'::text || valid_target_actions.action) AS sprite FROM valid_target_actions;


--
-- TOC entry 2502 (class 1259 OID 258390)
-- Dependencies: 6
-- Name: board_size; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE board_size (
    width integer NOT NULL,
    height integer NOT NULL
);


--
-- TOC entry 2621 (class 1259 OID 260838)
-- Dependencies: 6
-- Name: board_sound_effects; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE board_sound_effects (
    id integer NOT NULL,
    subtype text NOT NULL,
    sound_name text NOT NULL,
    queuepos integer NOT NULL
);


--
-- TOC entry 2620 (class 1259 OID 260836)
-- Dependencies: 6 2621
-- Name: board_sound_effects_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE board_sound_effects_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- TOC entry 3414 (class 0 OID 0)
-- Dependencies: 2620
-- Name: board_sound_effects_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE board_sound_effects_id_seq OWNED BY board_sound_effects.id;


--
-- TOC entry 3415 (class 0 OID 0)
-- Dependencies: 2620
-- Name: board_sound_effects_id_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('board_sound_effects_id_seq', 2697, true);


--
-- TOC entry 2614 (class 1259 OID 260800)
-- Dependencies: 6
-- Name: board_sprites1_cache; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE board_sprites1_cache (
    x integer NOT NULL,
    y integer NOT NULL,
    ptype text NOT NULL,
    allegiance text NOT NULL,
    tag integer NOT NULL,
    sprite text NOT NULL,
    colour text NOT NULL,
    sp integer NOT NULL,
    start_tick integer NOT NULL,
    animation_speed integer NOT NULL,
    selected boolean NOT NULL
);


--
-- TOC entry 2608 (class 1259 OID 260633)
-- Dependencies: 6
-- Name: cursor_position; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE cursor_position (
    x integer NOT NULL,
    y integer NOT NULL
);


--
-- TOC entry 2603 (class 1259 OID 260561)
-- Dependencies: 6
-- Name: sprites; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE sprites (
    sprite text NOT NULL,
    animation_speed integer NOT NULL
);


--
-- TOC entry 2615 (class 1259 OID 260807)
-- Dependencies: 2843 6
-- Name: board_sprites; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW board_sprites AS
    SELECT board_sprites1_cache.x, board_sprites1_cache.y, board_sprites1_cache.ptype, board_sprites1_cache.allegiance, board_sprites1_cache.tag, board_sprites1_cache.sprite, board_sprites1_cache.colour, board_sprites1_cache.sp, board_sprites1_cache.start_tick, board_sprites1_cache.animation_speed, board_sprites1_cache.selected FROM board_sprites1_cache UNION SELECT cursor_position.x, cursor_position.y, '' AS ptype, '' AS allegiance, (-1) AS tag, 'cursor' AS sprite, 'white' AS colour, 6 AS sp, 0 AS start_tick, sprites.animation_speed, false AS selected FROM (cursor_position JOIN sprites ON ((sprites.sprite = 'cursor'::text)));


--
-- TOC entry 2494 (class 1259 OID 258358)
-- Dependencies: 2767 6 899
-- Name: spells; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spells AS
    SELECT spells_mr.spell_name, spells_mr.base_chance, spells_mr.alignment, spells_mr.spell_category, spells_mr.description FROM spells_mr;


--
-- TOC entry 2609 (class 1259 OID 260766)
-- Dependencies: 2839 6
-- Name: wizard_sprites; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW wizard_sprites AS
    SELECT w.wizard_name, w.sprite, w.colour FROM (SELECT row_number() OVER (PARTITION BY a.wizard_name ORDER BY a.o DESC) AS rn, a.wizard_name, CASE WHEN wizards.shadow_form THEN (a.sprite || '_shadow'::text) ELSE a.sprite END AS sprite, w.colour FROM (((SELECT (-1) AS o, wizard_display_info.wizard_name, wizard_display_info.default_sprite AS sprite FROM wizard_display_info UNION SELECT action_history_mr.id AS o, action_history_mr.allegiance AS wizard_name, ('wizard_'::text || action_history_mr.spell_name) FROM (action_history_mr NATURAL JOIN spells) WHERE (((action_history_mr.spell_name <> 'shadow_form'::text) AND ((spells.spell_category)::text = 'wizard'::text)) AND ((action_history_mr.history_name)::text = 'spell_succeeded'::text))) a NATURAL JOIN wizard_display_info w) NATURAL JOIN wizards)) w WHERE (w.rn = 1);


--
-- TOC entry 2610 (class 1259 OID 260771)
-- Dependencies: 2840 6
-- Name: piece_sprite; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW piece_sprite AS
    SELECT p.x, p.y, p.ptype, CASE WHEN (p.ptype = 'wizard'::text) THEN w.sprite WHEN (p.allegiance = 'dead'::text) THEN ('dead_'::text || p.ptype) ELSE p.ptype END AS sprite, ac.colour, p.tag, p.allegiance FROM ((pieces p LEFT JOIN wizard_sprites w ON (((p.allegiance = w.wizard_name) AND (p.ptype = 'wizard'::text)))) JOIN allegiance_colours ac USING (allegiance));


--
-- TOC entry 2612 (class 1259 OID 260779)
-- Dependencies: 6
-- Name: piece_starting_ticks; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE piece_starting_ticks (
    ptype text NOT NULL,
    allegiance text NOT NULL,
    tag integer NOT NULL,
    start_tick integer NOT NULL
);


--
-- TOC entry 2613 (class 1259 OID 260795)
-- Dependencies: 2842 6
-- Name: board_sprites1_view; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW board_sprites1_view AS
    SELECT a.x, a.y, a.ptype, a.allegiance, a.tag, a.sprite, a.colour, a.sp, a.start_tick, sprites.animation_speed, a.selected FROM ((SELECT piece_sprite.x, piece_sprite.y, piece_sprite.ptype, piece_sprite.allegiance, piece_sprite.tag, piece_sprite.sprite, piece_sprite.colour, pieces_on_top.sp, piece_starting_ticks.start_tick, CASE WHEN (NOT (selected_piece.move_phase IS NULL)) THEN true ELSE false END AS selected FROM ((((piece_sprite NATURAL JOIN pieces_on_top) NATURAL JOIN piece_starting_ticks) NATURAL JOIN sprites) NATURAL LEFT JOIN selected_piece) UNION SELECT board_highlights.x, board_highlights.y, '', '', (-1), board_highlights.sprite, 'white', 5, 0, false AS bool FROM board_highlights) a NATURAL JOIN sprites) ORDER BY a.sp;


--
-- TOC entry 2617 (class 1259 OID 260814)
-- Dependencies: 6
-- Name: board_square_effects; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE board_square_effects (
    id integer NOT NULL,
    subtype text NOT NULL,
    x1 integer NOT NULL,
    y1 integer NOT NULL,
    queuepos integer NOT NULL
);


--
-- TOC entry 2616 (class 1259 OID 260812)
-- Dependencies: 6 2617
-- Name: board_square_effects_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE board_square_effects_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- TOC entry 3416 (class 0 OID 0)
-- Dependencies: 2616
-- Name: board_square_effects_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE board_square_effects_id_seq OWNED BY board_square_effects.id;


--
-- TOC entry 3417 (class 0 OID 0)
-- Dependencies: 2616
-- Name: board_square_effects_id_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('board_square_effects_id_seq', 81, true);


--
-- TOC entry 2538 (class 1259 OID 259368)
-- Dependencies: 6
-- Name: cast_alignment_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE cast_alignment_table (
    cast_alignment integer NOT NULL
);


--
-- TOC entry 2579 (class 1259 OID 260143)
-- Dependencies: 2824 6
-- Name: cast_magic_wood_available_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW cast_magic_wood_available_squares AS
    SELECT empty_and_not_adjacent_to_tree_squares.x, empty_and_not_adjacent_to_tree_squares.y FROM empty_and_not_adjacent_to_tree_squares EXCEPT SELECT adjacent_to_new_tree_squares.x, adjacent_to_new_tree_squares.y FROM adjacent_to_new_tree_squares;


--
-- TOC entry 2537 (class 1259 OID 259275)
-- Dependencies: 6
-- Name: cast_success_checked_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE cast_success_checked_table (
    cast_success_checked boolean NOT NULL
);


--
-- TOC entry 2455 (class 1259 OID 258033)
-- Dependencies: 3053 6
-- Name: system_implementation_objects; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE system_implementation_objects (
    object_name text NOT NULL,
    object_type text NOT NULL,
    CONSTRAINT system_implementation_objects_object_type_check CHECK ((object_type = ANY (ARRAY['scalar'::text, 'base_relvar'::text, 'operator'::text, 'view'::text, 'trigger'::text, 'database_constraint'::text])))
);


--
-- TOC entry 2482 (class 1259 OID 258266)
-- Dependencies: 2758 6
-- Name: implementation_module_objects; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW implementation_module_objects AS
    SELECT all_module_objects.object_name, all_module_objects.object_type, all_module_objects.module_name FROM (all_module_objects NATURAL JOIN system_implementation_objects);


--
-- TOC entry 2483 (class 1259 OID 258270)
-- Dependencies: 2759 6
-- Name: module_objects; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW module_objects AS
    SELECT all_module_objects.object_name, all_module_objects.object_type, all_module_objects.module_name FROM all_module_objects EXCEPT SELECT implementation_module_objects.object_name, implementation_module_objects.object_type, implementation_module_objects.module_name FROM implementation_module_objects;


--
-- TOC entry 2468 (class 1259 OID 258095)
-- Dependencies: 2752 6
-- Name: public_database_objects; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public_database_objects AS
    SELECT all_database_objects.object_name, all_database_objects.object_type FROM all_database_objects EXCEPT SELECT system_implementation_objects.object_name, system_implementation_objects.object_type FROM system_implementation_objects;


--
-- TOC entry 2485 (class 1259 OID 258303)
-- Dependencies: 2760 6
-- Name: chaos_base_relvars; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW chaos_base_relvars AS
    SELECT public_database_objects.object_name, public_database_objects.object_type FROM public_database_objects WHERE (public_database_objects.object_type = 'base_relvar'::text) EXCEPT SELECT module_objects.object_name, module_objects.object_type FROM module_objects WHERE ((module_objects.module_name = 'catalog'::text) AND (module_objects.object_type = 'base_relvar'::text));


--
-- TOC entry 2472 (class 1259 OID 258126)
-- Dependencies: 2754 6
-- Name: check_pg; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW check_pg AS
    SELECT pg_constraint.conname AS constraint_name, pg_class.relname AS relvar_name, pg_get_constraintdef(pg_constraint.oid) AS expression FROM (pg_constraint JOIN pg_class ON ((pg_constraint.conrelid = pg_class.oid))) WHERE ((pg_constraint.contype = 'c'::"char") AND (pg_constraint.connamespace = (SELECT pg_namespace.oid FROM pg_namespace WHERE (pg_namespace.nspname = 'public'::name))));


--
-- TOC entry 2528 (class 1259 OID 258939)
-- Dependencies: 2787 6
-- Name: current_wizard; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_wizard AS
    SELECT current_wizard_table.current_wizard AS wizard_name FROM current_wizard_table;


--
-- TOC entry 2534 (class 1259 OID 259053)
-- Dependencies: 2790 6
-- Name: current_wizard_spell; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_wizard_spell AS
    SELECT wizard_spell_choices.spell_name FROM (wizard_spell_choices NATURAL JOIN current_wizard);


--
-- TOC entry 2495 (class 1259 OID 258362)
-- Dependencies: 2768 901 899 6
-- Name: monster_spells; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW monster_spells AS
    SELECT s.spell_name, s.base_chance, s.alignment, s.spell_category, s.description, s.activate, s.target, s.range, s.num, s.ptype, s.valid_square_category FROM (spells_mr s JOIN monster_prototypes m ON ((s.ptype = m.ptype)));


--
-- TOC entry 2529 (class 1259 OID 258946)
-- Dependencies: 983 6
-- Name: turn_phase_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE turn_phase_table (
    turn_phase turn_phase_enum NOT NULL
);


--
-- TOC entry 2575 (class 1259 OID 260041)
-- Dependencies: 2821 6
-- Name: valid_activate_actions; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW valid_activate_actions AS
    SELECT a.action FROM ((((((((SELECT 'next_phase'::text AS action UNION SELECT 'set_imaginary'::text AS action FROM monster_spells WHERE ((get_current_wizard_spell() IS NOT NULL) AND (monster_spells.spell_name = get_current_wizard_spell()))) UNION SELECT 'set_real'::text AS action FROM monster_spells WHERE ((get_current_wizard_spell() IS NOT NULL) AND (monster_spells.spell_name = get_current_wizard_spell()))) UNION SELECT 'cast_activate_spell'::text AS action WHERE ((EXISTS (SELECT 1 FROM (current_wizard_spell NATURAL JOIN activate_spells) WHERE ((get_turn_phase())::text = 'cast'::text))) OR (SELECT (current_wizard_spell.spell_name = 'magic_wood'::text) FROM current_wizard_spell WHERE ((get_turn_phase())::text = 'cast'::text)))) UNION SELECT 'unselect_piece'::text AS action FROM selected_piece) UNION SELECT 'cancel'::text AS action FROM selected_piece) UNION SELECT (('choose_'::text || spell_books.spell_name) || '_spell'::text) AS action FROM spell_books WHERE ((spell_books.wizard_name = get_current_wizard()) AND ((get_turn_phase())::text = 'choose'::text))) UNION SELECT 'choose_no_spell'::text AS action FROM turn_phase_table WHERE ((turn_phase_table.turn_phase)::text = 'choose'::text)) UNION SELECT 'ai_continue' FROM (wizards JOIN current_wizard_table ON ((wizards.wizard_name = current_wizard_table.current_wizard))) WHERE wizards.computer_controlled) a WHERE (NOT (EXISTS (SELECT 1 FROM game_completed_table)));


--
-- TOC entry 2644 (class 1259 OID 261297)
-- Dependencies: 2858 6
-- Name: client_valid_activate_actions; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW client_valid_activate_actions AS
    SELECT a.action FROM ((((((((((((((SELECT valid_activate_actions.action FROM valid_activate_actions UNION SELECT 'move_cursor_up') UNION SELECT 'move_cursor_down') UNION SELECT 'move_cursor_left') UNION SELECT 'move_cursor_right') UNION SELECT 'move_cursor_up_left') UNION SELECT 'move_cursor_down_left') UNION SELECT 'move_cursor_up_right') UNION SELECT 'move_cursor_down_right') UNION SELECT 'print_widget_info') UNION SELECT 'refresh_windows') UNION SELECT 'spell_book_show_all_update_on') UNION SELECT 'spell_book_show_all_update_off') UNION SELECT 'client_next_phase') UNION SELECT 'go') a WHERE (NOT (EXISTS (SELECT 1 FROM game_completed_table)));


--
-- TOC entry 2643 (class 1259 OID 261293)
-- Dependencies: 2857 6
-- Name: client_valid_target_actions; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW client_valid_target_actions AS
    SELECT valid_target_actions.x, valid_target_actions.y, valid_target_actions.action FROM valid_target_actions WHERE (NOT (EXISTS (SELECT 1 FROM game_completed_table)));


--
-- TOC entry 2599 (class 1259 OID 260489)
-- Dependencies: 2835 6
-- Name: closest_enemy_to_selected_piece; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW closest_enemy_to_selected_piece AS
    SELECT a.x, a.y FROM ((selected_piece_attackable_squares a CROSS JOIN selected_piece s) JOIN pieces s1 USING (ptype, allegiance, tag)) ORDER BY distance(s1.x, s1.y, a.x, a.y) LIMIT 1;


--
-- TOC entry 2602 (class 1259 OID 260552)
-- Dependencies: 6
-- Name: colours; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE colours (
    name text NOT NULL,
    red integer NOT NULL,
    green integer NOT NULL,
    blue integer NOT NULL
);


--
-- TOC entry 2475 (class 1259 OID 258145)
-- Dependencies: 6
-- Name: con_pg; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE con_pg (
    constraint_name text NOT NULL
);


--
-- TOC entry 2524 (class 1259 OID 258765)
-- Dependencies: 6
-- Name: creating_new_game_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE creating_new_game_table (
    creating_new_game boolean NOT NULL
);


--
-- TOC entry 2488 (class 1259 OID 258325)
-- Dependencies: 2762 6
-- Name: creature_prototypes; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW creature_prototypes AS
    SELECT piece_prototypes_mr.ptype, piece_prototypes_mr.flying, piece_prototypes_mr.speed, piece_prototypes_mr.agility FROM piece_prototypes_mr WHERE (((piece_prototypes_mr.flying IS NOT NULL) AND (piece_prototypes_mr.speed IS NOT NULL)) AND (piece_prototypes_mr.agility IS NOT NULL));


--
-- TOC entry 2625 (class 1259 OID 260940)
-- Dependencies: 6
-- Name: current_effects; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE current_effects (
    ticks integer NOT NULL,
    queuepos integer NOT NULL
);


--
-- TOC entry 2627 (class 1259 OID 261014)
-- Dependencies: 2845 6
-- Name: current_board_beam_effects; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_board_beam_effects AS
    SELECT board_beam_effects.queuepos, board_beam_effects.id, board_beam_effects.subtype, board_beam_effects.x1, board_beam_effects.y1, board_beam_effects.x2, board_beam_effects.y2, current_effects.ticks FROM (board_beam_effects NATURAL JOIN current_effects);


--
-- TOC entry 2626 (class 1259 OID 261010)
-- Dependencies: 2844 6
-- Name: current_board_sound_effects; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_board_sound_effects AS
    SELECT board_sound_effects.queuepos, board_sound_effects.id, board_sound_effects.subtype, board_sound_effects.sound_name, current_effects.ticks FROM (board_sound_effects NATURAL JOIN current_effects);


--
-- TOC entry 2628 (class 1259 OID 261018)
-- Dependencies: 2846 6
-- Name: current_board_square_effects; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_board_square_effects AS
    SELECT board_square_effects.queuepos, board_square_effects.id, board_square_effects.subtype, board_square_effects.x1, board_square_effects.y1, current_effects.ticks FROM (board_square_effects NATURAL JOIN current_effects);


--
-- TOC entry 2636 (class 1259 OID 261147)
-- Dependencies: 2852 6
-- Name: current_wizard_spell_counts; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_wizard_spell_counts AS
    SELECT a.spell_name, 0 AS count FROM (SELECT spells.spell_name FROM spells EXCEPT SELECT spell_books.spell_name FROM (spell_books JOIN current_wizard_table ON ((spell_books.wizard_name = current_wizard_table.current_wizard)))) a UNION SELECT spell_books.spell_name, count(spell_books.spell_name) AS count FROM (spell_books JOIN current_wizard_table ON ((spell_books.wizard_name = current_wizard_table.current_wizard))) GROUP BY spell_books.spell_name;


--
-- TOC entry 2634 (class 1259 OID 261139)
-- Dependencies: 2850 6
-- Name: section_order; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW section_order AS
    (((SELECT 1 AS section_order, 'wizard' AS spell_category UNION SELECT 2 AS section_order, 'attacking' AS spell_category) UNION SELECT 3 AS section_order, 'object' AS spell_category) UNION SELECT 4 AS section_order, 'miscellaneous' AS spell_category) UNION SELECT 5 AS section_order, 'monster' AS spell_category;


--
-- TOC entry 2633 (class 1259 OID 261062)
-- Dependencies: 6
-- Name: spell_book_show_all_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE spell_book_show_all_table (
    spell_book_show_all boolean NOT NULL
);


--
-- TOC entry 2638 (class 1259 OID 261160)
-- Dependencies: 6
-- Name: spell_keys; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE spell_keys (
    spell_name text NOT NULL,
    key text NOT NULL
);


--
-- TOC entry 2632 (class 1259 OID 261041)
-- Dependencies: 6
-- Name: spell_sprites; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE spell_sprites (
    spell_name text NOT NULL,
    sprite text NOT NULL
);


--
-- TOC entry 2635 (class 1259 OID 261143)
-- Dependencies: 2851 6
-- Name: spells_with_order; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spells_with_order AS
    SELECT section_order.spell_category, spells.spell_name, spells.base_chance, spells.alignment, spells.description, section_order.section_order, CASE WHEN (spells.alignment > 0) THEN 0 WHEN (spells.alignment = 0) THEN 1 WHEN (spells.alignment < 0) THEN 2 ELSE NULL::integer END AS alignment_order FROM (spells NATURAL JOIN section_order);


--
-- TOC entry 2639 (class 1259 OID 261178)
-- Dependencies: 2854 6
-- Name: spell_book_table; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spell_book_table AS
    SELECT spells_with_order.spell_category, spells_with_order.spell_name, current_wizard_spell_counts.count, spell_cast_chance(spells_with_order.spell_name) AS chance, spells_with_order.alignment, format_alignment(spells_with_order.alignment) AS alignment_string, spell_keys.key, spell_sprites.sprite, spells_with_order.section_order, spells_with_order.alignment_order, spells_with_order.base_chance, count_icons((current_wizard_spell_counts.count)::integer) AS count_icons, align_icons(spells_with_order.alignment) AS align_icons, spell_colour(spells_with_order.spell_name, (current_wizard_spell_counts.count)::integer) AS colour FROM ((((spells_with_order NATURAL JOIN current_wizard_spell_counts) NATURAL JOIN spell_keys) NATURAL JOIN spell_sprites) CROSS JOIN spell_book_show_all_table) WHERE (NOT ((spell_book_show_all_table.spell_book_show_all = false) AND (current_wizard_spell_counts.count = 0)));


--
-- TOC entry 2640 (class 1259 OID 261183)
-- Dependencies: 2855 901 899 6
-- Name: spell_details; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spell_details AS
    SELECT spell_name, spells_mr.base_chance, spells_mr.alignment, spells_mr.spell_category, spells_mr.description, spells_mr.activate, spells_mr.target, spells_mr.range, spells_mr.num, spells_mr.ptype, spells_mr.valid_square_category, spell_sprites.sprite, balls.count, balls.chance, balls.alignment_string, balls.key, balls.section_order, balls.alignment_order, balls.count_icons, balls.align_icons, balls.colour FROM ((spells_mr FULL JOIN spell_sprites USING (spell_name)) FULL JOIN (SELECT spell_book_table.spell_name, spell_book_table.count, spell_book_table.chance, spell_book_table.alignment_string, spell_book_table.key, spell_book_table.section_order, spell_book_table.alignment_order, spell_book_table.count_icons, spell_book_table.align_icons, spell_book_table.colour FROM spell_book_table) balls USING (spell_name));


--
-- TOC entry 2641 (class 1259 OID 261188)
-- Dependencies: 2856 899 6
-- Name: current_wizard_selected_spell_details; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW current_wizard_selected_spell_details AS
    SELECT spell_details.spell_name, spell_details.spell_category, spell_details.sprite, spell_details.base_chance, spell_details.description, spell_details.num, spell_details.range, spell_details.count, spell_details.chance, spell_details.alignment_string FROM ((spell_details NATURAL JOIN wizard_spell_choices) JOIN current_wizard_table ON ((wizard_spell_choices.wizard_name = current_wizard_table.current_wizard)));


--
-- TOC entry 2505 (class 1259 OID 258452)
-- Dependencies: 2775 6
-- Name: live_wizards; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW live_wizards AS
    SELECT wizards.wizard_name, wizards.shadow_form, wizards.magic_sword, wizards.magic_knife, wizards.magic_shield, wizards.magic_wings, wizards.magic_armour, wizards.magic_bow, wizards.computer_controlled, wizards.original_place, wizards.expired, (row_number() OVER (ORDER BY wizards.original_place) - 1) AS place FROM wizards WHERE (NOT wizards.expired);


--
-- TOC entry 2629 (class 1259 OID 261026)
-- Dependencies: 2847 6
-- Name: piece_details; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW piece_details AS
    SELECT pieces_mr.ptype, pieces_mr.allegiance, pieces_mr.tag, pieces_mr.x, pieces_mr.y, pieces_mr.imaginary, pieces_mr.flying, pieces_mr.speed, pieces_mr.agility, pieces_mr.undead, pieces_mr.ridable, pieces_mr.ranged_weapon_type, pieces_mr.range, pieces_mr.ranged_attack_strength, pieces_mr.attack_strength, pieces_mr.physical_defense, pieces_mr.magic_defense, a.wtype, a.wizard_name, a.shadow_form, a.magic_sword, a.magic_knife, a.magic_shield, a.magic_wings, a.magic_armour, a.magic_bow, a.computer_controlled, a.original_place, a.expired, a.place, pieces_with_priorities.sp, piece_sprite.sprite, piece_sprite.colour FROM (((pieces_mr FULL JOIN (SELECT 'wizard'::text AS wtype, live_wizards.wizard_name, live_wizards.shadow_form, live_wizards.magic_sword, live_wizards.magic_knife, live_wizards.magic_shield, live_wizards.magic_wings, live_wizards.magic_armour, live_wizards.magic_bow, live_wizards.computer_controlled, live_wizards.original_place, live_wizards.expired, live_wizards.place FROM live_wizards) a ON (((pieces_mr.allegiance = a.wizard_name) AND (pieces_mr.ptype = a.wtype)))) NATURAL JOIN pieces_with_priorities) NATURAL JOIN piece_sprite);


--
-- TOC entry 2630 (class 1259 OID 261031)
-- Dependencies: 2848 6
-- Name: cursor_piece_details; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW cursor_piece_details AS
    SELECT piece_details.x, piece_details.y, piece_details.ptype, piece_details.allegiance, piece_details.tag, piece_details.imaginary, piece_details.flying, piece_details.speed, piece_details.agility, piece_details.undead, piece_details.ridable, piece_details.ranged_weapon_type, piece_details.range, piece_details.ranged_attack_strength, piece_details.attack_strength, piece_details.physical_defense, piece_details.magic_defense, piece_details.wtype, piece_details.wizard_name, piece_details.shadow_form, piece_details.magic_sword, piece_details.magic_knife, piece_details.magic_shield, piece_details.magic_wings, piece_details.magic_armour, piece_details.magic_bow, piece_details.computer_controlled, piece_details.original_place, piece_details.expired, piece_details.place, piece_details.sp, piece_details.sprite, piece_details.colour FROM (piece_details NATURAL JOIN cursor_position);


--
-- TOC entry 2470 (class 1259 OID 258113)
-- Dependencies: 6
-- Name: dbcon_ops; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE dbcon_ops (
    constraint_name text NOT NULL,
    operator_name text NOT NULL
);


--
-- TOC entry 2471 (class 1259 OID 258119)
-- Dependencies: 6
-- Name: dbcon_relvars; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE dbcon_relvars (
    constraint_name text NOT NULL,
    relvar_name text NOT NULL
);


--
-- TOC entry 2476 (class 1259 OID 258151)
-- Dependencies: 6
-- Name: dbcon_trigger_ops; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE dbcon_trigger_ops (
    operator_name text NOT NULL
);


--
-- TOC entry 2477 (class 1259 OID 258157)
-- Dependencies: 6
-- Name: dbcon_triggers; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE dbcon_triggers (
    trigger_name text NOT NULL,
    relvar_name text NOT NULL
);


--
-- TOC entry 2584 (class 1259 OID 260181)
-- Dependencies: 6
-- Name: disable_spreading_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE disable_spreading_table (
    disable_spreading boolean NOT NULL
);


--
-- TOC entry 2474 (class 1259 OID 258136)
-- Dependencies: 2756 6
-- Name: fk_pg; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW fk_pg AS
    SELECT a.constraint_name, b.relvar_name, d.attribute_name, c.target_relvar_name, e.target_attribute_name FROM (((((SELECT pg_constraint.conname AS constraint_name, pg_constraint.conrelid, pg_constraint.confrelid, pg_constraint.conkey[generate_series.generate_series] AS attnum, pg_constraint.confkey[generate_series.generate_series] AS fattnum FROM (pg_constraint CROSS JOIN generate_series(1, (SELECT max(array_upper(pg_constraint.conkey, 1)) AS max FROM pg_constraint)) generate_series(generate_series)) WHERE (((pg_constraint.contype = 'f'::"char") AND (pg_constraint.connamespace = (SELECT pg_namespace.oid FROM pg_namespace WHERE (pg_namespace.nspname = 'public'::name)))) AND ((generate_series.generate_series >= array_lower(pg_constraint.conkey, 1)) AND (generate_series.generate_series <= array_upper(pg_constraint.conkey, 1))))) a NATURAL JOIN (SELECT pg_class.oid AS conrelid, pg_class.relname AS relvar_name FROM pg_class) b) NATURAL JOIN (SELECT pg_class.oid AS confrelid, pg_class.relname AS target_relvar_name FROM pg_class) c) NATURAL JOIN (SELECT pg_attribute.attrelid AS conrelid, pg_attribute.attname AS attribute_name, pg_attribute.attnum FROM pg_attribute) d) NATURAL JOIN (SELECT pg_attribute.attrelid AS confrelid, pg_attribute.attname AS target_attribute_name, pg_attribute.attnum AS fattnum FROM pg_attribute) e);


--
-- TOC entry 2623 (class 1259 OID 260858)
-- Dependencies: 6
-- Name: history_no_visuals; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE history_no_visuals (
    history_name text NOT NULL
);


--
-- TOC entry 2622 (class 1259 OID 260849)
-- Dependencies: 6
-- Name: history_sounds; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE history_sounds (
    history_name text NOT NULL,
    sound_name text NOT NULL
);


--
-- TOC entry 2523 (class 1259 OID 258731)
-- Dependencies: 6
-- Name: in_next_phase_hack_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE in_next_phase_hack_table (
    in_next_phase_hack boolean NOT NULL
);


--
-- TOC entry 2605 (class 1259 OID 260597)
-- Dependencies: 6
-- Name: init_wizard_display_info_argument; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE init_wizard_display_info_argument (
    wizard_name text NOT NULL,
    sprite text NOT NULL,
    colour text NOT NULL
);


--
-- TOC entry 2645 (class 1259 OID 261302)
-- Dependencies: 6
-- Name: key_control_settings; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE key_control_settings (
    key_code text NOT NULL,
    action_name text NOT NULL
);


--
-- TOC entry 2473 (class 1259 OID 258131)
-- Dependencies: 2755 6
-- Name: key_pg; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW key_pg AS
    SELECT a.constraint_name, b.relvar_name, c.attribute_name FROM (((SELECT pg_constraint.conname AS constraint_name, pg_constraint.conrelid, pg_constraint.conkey[generate_series.generate_series] AS attnum FROM (pg_constraint CROSS JOIN generate_series(1, (SELECT max(array_upper(pg_constraint.conkey, 1)) AS max FROM pg_constraint)) generate_series(generate_series)) WHERE (((pg_constraint.contype = ANY (ARRAY['p'::"char", 'u'::"char"])) AND (pg_constraint.connamespace = (SELECT pg_namespace.oid FROM pg_namespace WHERE (pg_namespace.nspname = 'public'::name)))) AND ((generate_series.generate_series >= array_lower(pg_constraint.conkey, 1)) AND (generate_series.generate_series <= array_upper(pg_constraint.conkey, 1))))) a NATURAL JOIN (SELECT pg_class.oid AS conrelid, pg_class.relname AS relvar_name FROM pg_class) b) NATURAL JOIN (SELECT pg_attribute.attrelid AS conrelid, pg_attribute.attname AS attribute_name, pg_attribute.attnum FROM pg_attribute) c);


--
-- TOC entry 2624 (class 1259 OID 260867)
-- Dependencies: 6
-- Name: last_history_effect_id_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE last_history_effect_id_table (
    last_history_effect_id integer NOT NULL
);


--
-- TOC entry 2522 (class 1259 OID 258727)
-- Dependencies: 2785 6
-- Name: magic_attackable_pieces; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW magic_attackable_pieces AS
    SELECT pieces_mr.ptype, pieces_mr.allegiance, pieces_mr.tag, pieces_mr.x, pieces_mr.y, pieces_mr.magic_defense FROM pieces_mr WHERE (pieces_mr.magic_defense IS NOT NULL);


--
-- TOC entry 2480 (class 1259 OID 258241)
-- Dependencies: 6
-- Name: modules; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE modules (
    module_name text NOT NULL,
    module_parent_name text NOT NULL,
    module_order integer NOT NULL
);


--
-- TOC entry 2479 (class 1259 OID 258239)
-- Dependencies: 2480 6
-- Name: modules_module_order_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE modules_module_order_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- TOC entry 3418 (class 0 OID 0)
-- Dependencies: 2479
-- Name: modules_module_order_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE modules_module_order_seq OWNED BY modules.module_order;


--
-- TOC entry 3419 (class 0 OID 0)
-- Dependencies: 2479
-- Name: modules_module_order_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('modules_module_order_seq', 27, true);


--
-- TOC entry 2642 (class 1259 OID 261195)
-- Dependencies: 1236 6
-- Name: new_game_widget_state; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE new_game_widget_state (
    line integer NOT NULL,
    wizard_name text NOT NULL,
    sprite text NOT NULL,
    colour text NOT NULL,
    state new_wizard_state NOT NULL
);


--
-- TOC entry 2526 (class 1259 OID 258846)
-- Dependencies: 2786 6
-- Name: next_wizard; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW next_wizard AS
    SELECT b.wizard_name, a.new_wizard_name FROM ((SELECT live_wizards.wizard_name AS new_wizard_name, live_wizards.place FROM live_wizards) a JOIN (SELECT live_wizards.wizard_name, ((live_wizards.place + 1) % (SELECT (max(live_wizards.place) + 1) FROM live_wizards)) AS old_place FROM live_wizards) b ON ((a.place = b.old_place)));


--
-- TOC entry 2478 (class 1259 OID 258163)
-- Dependencies: 2757 6
-- Name: non_accelerated_constraints; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW non_accelerated_constraints AS
    SELECT dbcon_relvars.relvar_name, dbcon_relvars.constraint_name, database_constraints.expression FROM (dbcon_relvars NATURAL JOIN database_constraints) WHERE (NOT (dbcon_relvars.constraint_name IN (SELECT con_pg.constraint_name FROM con_pg)));


--
-- TOC entry 2469 (class 1259 OID 258099)
-- Dependencies: 2753 6
-- Name: object_orders; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW object_orders AS
    ((((SELECT 'scalar'::text AS object_type, 0 AS object_order UNION SELECT 'database_constraint' AS object_type, 1 AS object_order) UNION SELECT 'base_relvar' AS object_type, 2 AS object_order) UNION SELECT 'operator' AS object_type, 3 AS object_order) UNION SELECT 'view' AS object_type, 4 AS object_order) UNION SELECT 'trigger' AS object_type, 5 AS object_order;


--
-- TOC entry 2490 (class 1259 OID 258333)
-- Dependencies: 2764 6
-- Name: object_piece_types; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW object_piece_types AS
    SELECT piece_prototypes_mr.ptype FROM piece_prototypes_mr WHERE (piece_prototypes_mr.speed IS NULL);


--
-- TOC entry 2462 (class 1259 OID 258067)
-- Dependencies: 2747 6
-- Name: operator_source; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW operator_source AS
    SELECT pg_proc.proname AS operator_name, pg_proc.prosrc AS source FROM pg_proc WHERE (pg_proc.pronamespace = (SELECT pg_namespace.oid FROM pg_namespace WHERE (pg_namespace.nspname = 'public'::name)));


--
-- TOC entry 2487 (class 1259 OID 258321)
-- Dependencies: 2761 6
-- Name: piece_prototypes; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW piece_prototypes AS
    SELECT piece_prototypes_mr.ptype FROM piece_prototypes_mr;


--
-- TOC entry 2547 (class 1259 OID 259920)
-- Dependencies: 2793 6
-- Name: pieces_on_top_view; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW pieces_on_top_view AS
    SELECT p.ptype, p.allegiance, p.tag, p.x, p.y, p.imaginary, p.flying, p.speed, p.agility, p.undead, p.ridable, p.ranged_weapon_type, p.range, p.ranged_attack_strength, p.attack_strength, p.physical_defense, p.magic_defense FROM (pieces_mr p JOIN pieces_on_top USING (ptype, allegiance, tag));


--
-- TOC entry 2598 (class 1259 OID 260485)
-- Dependencies: 2834 6
-- Name: prefered_targets; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW prefered_targets AS
    SELECT valid_target_actions.x, valid_target_actions.y, valid_target_actions.action, CASE WHEN (pieces_mr.ptype = 'wizard'::text) THEN (-500) ELSE (20 - pieces_mr.physical_defense) END AS preference FROM (valid_target_actions NATURAL JOIN pieces_mr) WHERE (valid_target_actions.action = ANY (ARRAY['attack'::text, 'ranged_attack'::text]));


--
-- TOC entry 2647 (class 1259 OID 261330)
-- Dependencies: 2860 6
-- Name: prompt; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW prompt AS
    SELECT action_instructions.action, action_instructions.help FROM (action_instructions NATURAL JOIN (SELECT client_valid_target_actions.action FROM client_valid_target_actions UNION SELECT client_valid_activate_actions.action FROM client_valid_activate_actions) a);


--
-- TOC entry 2542 (class 1259 OID 259664)
-- Dependencies: 6
-- Name: remaining_walk_hack_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE remaining_walk_hack_table (
    remaining_walk_hack boolean NOT NULL
);


--
-- TOC entry 2541 (class 1259 OID 259606)
-- Dependencies: 6
-- Name: remaining_walk_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE remaining_walk_table (
    remaining_walk integer NOT NULL
);


--
-- TOC entry 2600 (class 1259 OID 260493)
-- Dependencies: 2836 6
-- Name: select_best_move; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW select_best_move AS
    SELECT a.action, a.x, a.y FROM (ai_selected_piece_actions a CROSS JOIN closest_enemy_to_selected_piece e) WHERE (a.action = ANY (ARRAY['walk'::text, 'fly'::text])) ORDER BY distance(a.x, a.y, e.x, e.y) LIMIT 1;


--
-- TOC entry 2581 (class 1259 OID 260165)
-- Dependencies: 2825 6
-- Name: selected_piece_adjacent_attacking_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piece_adjacent_attacking_squares AS
    SELECT pieces_on_top.x, pieces_on_top.y FROM (pieces_on_top NATURAL JOIN pieces_mr) WHERE (((pieces_mr.attack_strength IS NOT NULL) AND (pieces_on_top.allegiance <> (SELECT selected_piece.allegiance FROM selected_piece))) AND (pieces_on_top.allegiance <> 'dead'::text)) INTERSECT SELECT r.tx AS x, r.ty AS y FROM (board_ranges r NATURAL JOIN selected_piecexy) WHERE (r.range = 1);


--
-- TOC entry 2631 (class 1259 OID 261036)
-- Dependencies: 2849 6 1010
-- Name: selected_piece_details; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW selected_piece_details AS
    SELECT piece_details.ptype, piece_details.allegiance, piece_details.tag, piece_details.x, piece_details.y, piece_details.imaginary, piece_details.flying, piece_details.speed, piece_details.agility, piece_details.undead, piece_details.ridable, piece_details.ranged_weapon_type, piece_details.range, piece_details.ranged_attack_strength, piece_details.attack_strength, piece_details.physical_defense, piece_details.magic_defense, piece_details.wtype, piece_details.wizard_name, piece_details.shadow_form, piece_details.magic_sword, piece_details.magic_knife, piece_details.magic_shield, piece_details.magic_wings, piece_details.magic_armour, piece_details.magic_bow, piece_details.computer_controlled, piece_details.original_place, piece_details.expired, piece_details.place, piece_details.sp, piece_details.sprite, piece_details.colour, selected_piece.move_phase, selected_piece.engaged, remaining_walk_table.remaining_walk FROM ((piece_details NATURAL JOIN selected_piece) NATURAL FULL JOIN remaining_walk_table);


--
-- TOC entry 2506 (class 1259 OID 258456)
-- Dependencies: 6 2507
-- Name: spell_books_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE spell_books_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- TOC entry 3420 (class 0 OID 0)
-- Dependencies: 2506
-- Name: spell_books_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE spell_books_id_seq OWNED BY spell_books.id;


--
-- TOC entry 3421 (class 0 OID 0)
-- Dependencies: 2506
-- Name: spell_books_id_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('spell_books_id_seq', 17149, true);


--
-- TOC entry 2576 (class 1259 OID 260115)
-- Dependencies: 2822 6
-- Name: spell_cast_chance; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spell_cast_chance AS
    SELECT a.spell_name, a.base_chance AS chance FROM ((SELECT spells.spell_name, sign((spells.alignment)::double precision) AS salign, spells.base_chance, 'neutral' AS alignment FROM spells UNION SELECT spells.spell_name, sign((spells.alignment)::double precision) AS salign, limit_chance((spells.base_chance + ((@ get_world_alignment()) * 10))) AS limit_chance, 'same' AS alignment FROM spells) UNION SELECT spells.spell_name, sign((spells.alignment)::double precision) AS salign, limit_chance((spells.base_chance - 10)) AS limit_chance, 'opposite' AS alignment FROM spells) a WHERE (((((a.salign = (0)::double precision) AND (a.alignment = 'neutral'::text)) OR ((sign((get_world_alignment())::double precision) = (0)::double precision) AND (a.alignment = 'neutral'::text))) OR ((sign((get_world_alignment())::double precision) = (1)::double precision) AND (((a.salign = (1)::double precision) AND (a.alignment = 'same'::text)) OR ((a.salign = ((-1))::double precision) AND (a.alignment = 'opposite'::text))))) OR ((sign((get_world_alignment())::double precision) = ((-1))::double precision) AND (((a.salign = ((-1))::double precision) AND (a.alignment = 'same'::text)) OR ((a.salign = (1)::double precision) AND (a.alignment = 'opposite'::text)))));


--
-- TOC entry 2535 (class 1259 OID 259058)
-- Dependencies: 6
-- Name: spell_choice_hack_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE spell_choice_hack_table (
    spell_choice_hack boolean NOT NULL
);


--
-- TOC entry 2637 (class 1259 OID 261154)
-- Dependencies: 2853 6
-- Name: spell_colours; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spell_colours AS
    SELECT spell_cast_chance.spell_name, chance_colour(spell_cast_chance.chance) AS colour FROM spell_cast_chance;


--
-- TOC entry 2586 (class 1259 OID 260248)
-- Dependencies: 6
-- Name: spell_indexes_no_dis_turm; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE spell_indexes_no_dis_turm (
    row_number integer NOT NULL,
    spell_name text NOT NULL
);


--
-- TOC entry 2585 (class 1259 OID 260246)
-- Dependencies: 2586 6
-- Name: spell_indexes_no_dis_turm_row_number_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE spell_indexes_no_dis_turm_row_number_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- TOC entry 3422 (class 0 OID 0)
-- Dependencies: 2585
-- Name: spell_indexes_no_dis_turm_row_number_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE spell_indexes_no_dis_turm_row_number_seq OWNED BY spell_indexes_no_dis_turm.row_number;


--
-- TOC entry 3423 (class 0 OID 0)
-- Dependencies: 2585
-- Name: spell_indexes_no_dis_turm_row_number_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('spell_indexes_no_dis_turm_row_number_seq', 53, true);


--
-- TOC entry 2536 (class 1259 OID 259186)
-- Dependencies: 6
-- Name: spell_parts_to_cast_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE spell_parts_to_cast_table (
    spell_parts_to_cast integer NOT NULL
);


--
-- TOC entry 2501 (class 1259 OID 258386)
-- Dependencies: 2774 6 899
-- Name: spells_with_num_shots; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spells_with_num_shots AS
    SELECT spells_mr.spell_name, spells_mr.base_chance, spells_mr.alignment, spells_mr.spell_category, spells_mr.description, spells_mr.num FROM spells_mr WHERE (spells_mr.num IS NOT NULL);


--
-- TOC entry 2583 (class 1259 OID 260177)
-- Dependencies: 2827 6
-- Name: spreadable_squares; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW spreadable_squares AS
    SELECT x.x, y.y FROM (generate_series(0, 14) x(x) CROSS JOIN generate_series(0, 9) y(y)) EXCEPT SELECT pieces.x, pieces.y FROM (pieces NATURAL JOIN object_piece_types);


--
-- TOC entry 2498 (class 1259 OID 258374)
-- Dependencies: 2771 899 6
-- Name: summon_spells; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW summon_spells AS
    SELECT spells_mr.spell_name, spells_mr.base_chance, spells_mr.alignment, spells_mr.spell_category, spells_mr.description, spells_mr.ptype FROM spells_mr WHERE (spells_mr.ptype IS NOT NULL);


--
-- TOC entry 2649 (class 1259 OID 261508)
-- Dependencies: 2861 6
-- Name: t1; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW t1 AS
    SELECT ARRAY[1, 2, 3] AS "array", '2' INTERSECT SELECT ARRAY[1, 2] AS "array", '2';


--
-- TOC entry 2500 (class 1259 OID 258382)
-- Dependencies: 2773 6
-- Name: target_spells; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW target_spells AS
    SELECT spells_mr.spell_name FROM spells_mr WHERE ((spells_mr.target IS NOT NULL) AND spells_mr.target);


--
-- TOC entry 2544 (class 1259 OID 259898)
-- Dependencies: 6 1021
-- Name: test_action_overrides; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE test_action_overrides (
    override random_test NOT NULL,
    setting boolean NOT NULL
);


--
-- TOC entry 2525 (class 1259 OID 258801)
-- Dependencies: 6
-- Name: turn_number_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE turn_number_table (
    turn_number integer NOT NULL
);


--
-- TOC entry 2465 (class 1259 OID 258080)
-- Dependencies: 2750 6
-- Name: view_attributes; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW view_attributes AS
    SELECT pg_attribute.attname AS attribute_name, pg_type.typname AS type_name, pg_class.relname AS relvar_name FROM (((pg_attribute JOIN pg_class ON ((pg_attribute.attrelid = pg_class.oid))) JOIN pg_type ON ((pg_attribute.atttypid = pg_type.oid))) JOIN views ON ((pg_class.relname = views.view_name))) WHERE (pg_attribute.attnum >= 1);


--
-- TOC entry 2601 (class 1259 OID 260540)
-- Dependencies: 1141 6
-- Name: windows; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE windows (
    window_name text NOT NULL,
    px integer NOT NULL,
    py integer NOT NULL,
    sx integer NOT NULL,
    sy integer NOT NULL,
    state window_state NOT NULL
);


--
-- TOC entry 2533 (class 1259 OID 259049)
-- Dependencies: 2789 6
-- Name: wizard_spell_choices_imaginary; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW wizard_spell_choices_imaginary AS
    SELECT wizard_spell_choices_mr.wizard_name, wizard_spell_choices_mr.imaginary FROM wizard_spell_choices_mr WHERE (wizard_spell_choices_mr.imaginary IS NOT NULL);


--
-- TOC entry 2590 (class 1259 OID 260312)
-- Dependencies: 6
-- Name: wizard_starting_positions; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE wizard_starting_positions (
    wizard_count integer NOT NULL,
    place integer NOT NULL,
    x integer NOT NULL,
    y integer NOT NULL
);


--
-- TOC entry 2582 (class 1259 OID 260171)
-- Dependencies: 2826 6
-- Name: wizards_in_trees; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW wizards_in_trees AS
    SELECT pieces.ptype, pieces.allegiance, pieces.tag FROM pieces WHERE ((pieces.ptype = 'wizard'::text) AND ((pieces.x, pieces.y) IN (SELECT pieces.x, pieces.y FROM pieces WHERE (pieces.ptype = 'magic_tree'::text))));


--
-- TOC entry 2503 (class 1259 OID 258412)
-- Dependencies: 6
-- Name: world_alignment_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE world_alignment_table (
    world_alignment integer NOT NULL
);


--
-- TOC entry 3066 (class 2604 OID 260281)
-- Dependencies: 2588 2589 2589
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE action_history_mr ALTER COLUMN id SET DEFAULT nextval('action_history_mr_id_seq'::regclass);


--
-- TOC entry 3068 (class 2604 OID 260829)
-- Dependencies: 2619 2618 2619
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE board_beam_effects ALTER COLUMN id SET DEFAULT nextval('board_beam_effects_id_seq'::regclass);


--
-- TOC entry 3069 (class 2604 OID 260841)
-- Dependencies: 2621 2620 2621
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE board_sound_effects ALTER COLUMN id SET DEFAULT nextval('board_sound_effects_id_seq'::regclass);


--
-- TOC entry 3067 (class 2604 OID 260817)
-- Dependencies: 2616 2617 2617
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE board_square_effects ALTER COLUMN id SET DEFAULT nextval('board_square_effects_id_seq'::regclass);


--
-- TOC entry 3054 (class 2604 OID 258244)
-- Dependencies: 2479 2480 2480
-- Name: module_order; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE modules ALTER COLUMN module_order SET DEFAULT nextval('modules_module_order_seq'::regclass);


--
-- TOC entry 3064 (class 2604 OID 258461)
-- Dependencies: 2506 2507 2507
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE spell_books ALTER COLUMN id SET DEFAULT nextval('spell_books_id_seq'::regclass);


--
-- TOC entry 3065 (class 2604 OID 260251)
-- Dependencies: 2585 2586 2586
-- Name: row_number; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE spell_indexes_no_dis_turm ALTER COLUMN row_number SET DEFAULT nextval('spell_indexes_no_dis_turm_row_number_seq'::regclass);


--
-- TOC entry 3403 (class 0 OID 261334)
-- Dependencies: 2648
-- Data for Name: action_client_new_game_argument; Type: TABLE DATA; Schema: public; Owner: -
--

COPY action_client_new_game_argument (place, wizard_name, sprite, colour, computer_controlled) FROM stdin;
0	Buddha	wizard0	blue	f
1	Kong Fuzi	wizard1	purple	f
2	Laozi	wizard2	cyan	f
3	Moshe	wizard3	yellow	f
4	Muhammad	wizard4	green	f
5	Shiva	wizard5	red	f
6	Yeshua	wizard6	white	f
7	Zarathushthra	wizard7	orange	f
\.


--
-- TOC entry 3380 (class 0 OID 260278)
-- Dependencies: 2589
-- Data for Name: action_history_mr; Type: TABLE DATA; Schema: public; Owner: -
--

COPY action_history_mr (id, history_name, ptype, allegiance, tag, spell_name, turn_number, turn_phase, num_wizards, x, y, tx, ty) FROM stdin;
2	new_game	\N	\N	\N	\N	\N	\N	8	\N	\N	\N	\N
3	wizard_up	\N	Kong Fuzi	\N	\N	\N	choose	\N	3	0	\N	\N
4	wizard_up	\N	Buddha	\N	\N	\N	cast	\N	0	0	\N	\N
5	wizard_up	\N	Kong Fuzi	\N	\N	\N	cast	\N	3	0	\N	\N
6	wizard_up	\N	Buddha	\N	\N	\N	move	\N	0	0	\N	\N
7	wizard_up	\N	Kong Fuzi	\N	\N	\N	move	\N	3	0	\N	\N
8	attack	green_dragon	Kong Fuzi	0	\N	\N	\N	\N	1	0	0	0
9	chinned	wizard	Buddha	0	\N	\N	\N	\N	0	0	\N	\N
10	game_won	\N	Kong Fuzi	\N	\N	\N	\N	\N	3	0	\N	\N
11	game_drawn	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N
\.


--
-- TOC entry 3382 (class 0 OID 260380)
-- Dependencies: 2591
-- Data for Name: action_new_game_argument; Type: TABLE DATA; Schema: public; Owner: -
--

COPY action_new_game_argument (place, wizard_name, computer_controlled) FROM stdin;
0	Buddha	f
1	Kong Fuzi	f
2	Laozi	f
3	Moshe	f
4	Muhammad	f
5	Shiva	f
6	Yeshua	f
7	Zarathushthra	f
\.


--
-- TOC entry 3350 (class 0 OID 258251)
-- Dependencies: 2481
-- Data for Name: all_module_objects; Type: TABLE DATA; Schema: public; Owner: -
--

COPY all_module_objects (object_name, object_type, module_name) FROM stdin;
dbcon_triggers_constraint_trigger	trigger	catalog
base_relvar_keys	view	catalog
dbcon_trigger_ops	base_relvar	catalog
con_pg	base_relvar	catalog
check_con_dbcon_relvars_constraint_name_relvar_name_key	operator	catalog
base_relvar_key_attributes	view	catalog
regenerate_constraint_triggers	operator	catalog
con_pg_constraint_trigger_operator	operator	catalog
all_module_objects_module_name_fkey	database_constraint	catalog
create_delete_transition_tuple_constraint	operator	catalog
con_pg_constraint_name_fkey	database_constraint	catalog
check_code_slow_si_objects_constraints	operator	catalog
set_module_for_preceding_objects	operator	catalog
public_database_objects	view	catalog
int4	scalar	catalog
check_con_dbcon_trigger_ops_operator_name_key	operator	catalog
add_key	operator	catalog
all_module_objects_object_name_object_type_key	database_constraint	catalog
check_con_database_constraints_constraint_name_key	operator	catalog
system_implementation_objects	base_relvar	catalog
dbcon_ops_constraint_name_fkey	database_constraint	catalog
con_pg_constraint_name_fkey1	database_constraint	catalog
attrs_are_key	operator	catalog
dbcon_trigger_ops_operator_name_fkey	database_constraint	catalog
check_con_con_pg_constraint_name_fkey	operator	catalog
operator_source	view	catalog
dbcon_trigger_ops_operator_name_key	database_constraint	catalog
all_database_objects	view	catalog
dbcon_trigger_ops_constraint_trigger_operator	operator	catalog
dbcon_triggers_trigger_name_key	database_constraint	catalog
dbcon_ops	base_relvar	catalog
view_attributes	view	catalog
check_con_con_pg_constraint_name_fkey1	operator	catalog
object_orders	view	catalog
check_con_all_module_objects_object_name_object_type_key	operator	catalog
check_con_dbcon_ops_constraint_name_fkey	operator	catalog
check_con_dbcon_relvars_constraint_name_fkey	operator	catalog
check_con_dbcon_relvars_relvar_name_fkey	operator	catalog
con_pg_constraint_name_key	database_constraint	catalog
views	view	catalog
create_update_transition_tuple_constraint	operator	catalog
set_pg_unique	operator	catalog
con_pg_constraint_trigger	trigger	catalog
non_accelerated_constraints	view	catalog
check_con_dbcon_ops_constraint_name_key	operator	catalog
add_constraint	operator	catalog
check_con_dbcon_ops_operator_name_key	operator	catalog
base_relvar_attributes	view	catalog
check_con_dbcon_triggers_trigger_name_key	operator	catalog
create_insert_transition_tuple_constraint	operator	catalog
modules_module_name_key	database_constraint	catalog
dbcon_relvars	base_relvar	catalog
dbcon_triggers	base_relvar	catalog
dbcon_triggers_trigger_name_relvar_name_fkey	database_constraint	catalog
check_constraint_name	operator	catalog
dbcon_relvars_constraint_name_relvar_name_key	database_constraint	catalog
module_objects	view	catalog
check_con_con_pg_constraint_name_key	operator	catalog
key_pg	view	catalog
dbcon_trigger_ops_constraint_trigger	trigger	catalog
triggers	view	catalog
database_constraints_constraint_name_key	database_constraint	catalog
dbcon_ops_operator_name_key	database_constraint	catalog
create_x_transition_tuple_constraint	operator	catalog
dbcon_relvars_constraint_trigger_operator	operator	catalog
implementation_module_objects	view	catalog
dbcon_relvars_relvar_name_fkey	database_constraint	catalog
add_foreign_key	operator	catalog
text	scalar	catalog
dbcon_relvars_constraint_name_fkey	database_constraint	catalog
constrain_to_zero_or_one_tuple	operator	catalog
dbcon_triggers_constraint_trigger_operator	operator	catalog
check_pg	view	catalog
base_relvars	view	catalog
add_constraint_internal	operator	catalog
check_con_all_module_objects_module_name_fkey	operator	catalog
modules	base_relvar	catalog
scalars	view	catalog
operators	view	catalog
dbcon_ops_constraint_name_key	database_constraint	catalog
check_con_dbcon_triggers_trigger_name_relvar_name_fkey	operator	catalog
set_pg_fk	operator	catalog
new_module	operator	catalog
system_implementation_objects_object_name_object__key	database_constraint	catalog
all_module_objects	base_relvar	catalog
dbcon_relvars_constraint_trigger	trigger	catalog
check_con_dbcon_trigger_ops_operator_name_fkey	operator	catalog
check_con_modules_module_name_key	operator	catalog
sort_out_constraint_name	operator	catalog
set_pg_check	operator	catalog
check_con_system_implementation_objects_object_name_object__key	operator	catalog
fk_pg	view	catalog
database_constraints	base_relvar	catalog
check_code_module_membership	operator	catalog
set_all_attributes_to_not_null	operator	utils
notify_on_changed	operator	utils
create_var	operator	utils
check_code_no_nullable_table_columns	operator	utils
check_code_some_tags	operator	metadata
check_con_base_relvar_metadata_relvar_name_key	operator	metadata
base_relvar_metadata_relvar_name_key	database_constraint	metadata
chaos_base_relvars	view	metadata
protect_readonly_relvars	operator	metadata
set_relvar_type	operator	metadata
base_relvar_metadata_relvar_name_fkey	database_constraint	metadata
base_relvar_metadata	base_relvar	metadata
set_notifies_on_all_data_tables	operator	metadata
base_relvar_metadata_constraint_trigger	trigger	metadata
base_relvar_metadata_constraint_trigger_operator	operator	metadata
check_con_base_relvar_metadata_relvar_name_fkey	operator	metadata
enterable_piece_types	view	piece_prototypes
piece_prototypes	view	piece_prototypes
bool	scalar	piece_prototypes
ridable_prototypes	view	piece_prototypes
check_con_piece_prototypes_mr_ptype_key	operator	piece_prototypes
monster_prototypes	view	piece_prototypes
object_piece_types	view	piece_prototypes
ranged_weapon_type	scalar	piece_prototypes
piece_prototypes_mr_ptype_key	database_constraint	piece_prototypes
creature_prototypes	view	piece_prototypes
piece_prototypes_mr	base_relvar	piece_prototypes
summon_spells	view	spells
spell_category	scalar	spells
spell_ranges	view	spells
spell_valid_square_types	view	spells
spells_mr_spell_name_key	database_constraint	spells
target_spells	view	spells
spell_square_category	scalar	spells
activate_spells	view	spells
spells	view	spells
spells_mr	base_relvar	spells
monster_spells	view	spells
check_con_spells_mr_spell_name_key	operator	spells
spells_with_num_shots	view	spells
board_size_constraint_trigger	trigger	global_data
board_size_01_tuple	database_constraint	global_data
check_con_world_alignment_table_01_tuple	operator	global_data
board_size_width_height_key	database_constraint	global_data
get_world_alignment	operator	global_data
world_alignment_table_world_alignment_key	database_constraint	global_data
check_con_board_size_01_tuple	operator	global_data
board_size	base_relvar	global_data
check_con_world_alignment_table_world_alignment_key	operator	global_data
init_board_size	operator	global_data
init_world_alignment	operator	global_data
world_alignment_table_constraint_trigger_operator	operator	global_data
world_alignment_table_constraint_trigger	trigger	global_data
world_alignment_table	base_relvar	global_data
board_size_constraint_trigger_operator	operator	global_data
world_alignment_table_01_tuple	database_constraint	global_data
check_con_board_size_width_height_key	operator	global_data
spell_books_spell_name_fkey	database_constraint	wizards
check_con_wizards_wizard_name_key	operator	wizards
spell_books_id_key	database_constraint	wizards
wizards_constraint_trigger	trigger	wizards
check_con_spell_books_id_key	operator	wizards
check_con_spell_books_spell_name_fkey	operator	wizards
spell_books	base_relvar	wizards
live_wizards	view	wizards
check_con_no_spells_for_stiffs	operator	wizards
spell_books_constraint_trigger_operator	operator	wizards
wizards	base_relvar	wizards
wizards_wizard_name_key	database_constraint	wizards
check_con_spell_books_wizard_name_fkey	operator	wizards
spell_books_wizard_name_fkey	database_constraint	wizards
wizards_constraint_trigger_operator	operator	wizards
spell_books_constraint_trigger	trigger	wizards
no_spells_for_stiffs	database_constraint	wizards
crimes_against_nature_constraint_trigger	trigger	pieces
imaginary_pieces_ptype_allegiance_tag_fkey	database_constraint	pieces
crimes_against_nature	base_relvar	pieces
piece_coordinates_valid	database_constraint	pieces
imaginary_pieces_constraint_trigger	trigger	pieces
dead_monster_pieces	view	pieces
check_con_crimes_against_nature_ptype_allegiance_tag_key	operator	pieces
check_con_pieces_allegiance_fkey	operator	pieces
pieces	base_relvar	pieces
imaginary_pieces	base_relvar	pieces
check_con_piece_coordinates_valid	operator	pieces
check_con_pieces_ptype_allegiance_tag_key	operator	pieces
pieces_constraint_trigger_operator	operator	pieces
check_con_imaginary_pieces_ptype_allegiance_tag_fkey	operator	pieces
pieces_mr	view	pieces
check_con_imaginary_pieces_ptype_fkey	operator	pieces
pieces_ptype_fkey	database_constraint	pieces
wizard_upgrade_stats	view	pieces
crimes_against_nature_constraint_trigger_operator	operator	pieces
check_con_crimes_against_nature_ptype_allegiance_tag_fkey	operator	pieces
pieces_constraint_trigger	trigger	pieces
magic_attackable_pieces	view	pieces
check_con_dead_wizard_army_empty	operator	pieces
check_con_crimes_against_nature_ptype_fkey	operator	pieces
pieces_ptype_allegiance_tag_key	database_constraint	pieces
ranged_weapon_pieces	view	pieces
monster_pieces	view	pieces
dead_wizard_army_empty	database_constraint	pieces
attacking_pieces	view	pieces
imaginary_pieces_ptype_allegiance_tag_key	database_constraint	pieces
creature_pieces	view	pieces
pieces_allegiance_fkey	database_constraint	pieces
allegiances	view	pieces
imaginary_pieces_ptype_fkey	database_constraint	pieces
crimes_against_nature_ptype_allegiance_tag_key	database_constraint	pieces
imaginary_pieces_constraint_trigger_operator	operator	pieces
crimes_against_nature_ptype_fkey	database_constraint	pieces
check_con_imaginary_pieces_ptype_allegiance_tag_key	operator	pieces
attackable_pieces	view	pieces
check_con_pieces_ptype_fkey	operator	pieces
crimes_against_nature_ptype_allegiance_tag_fkey	database_constraint	pieces
in_next_phase_hack_table	base_relvar	squares_valid
check_next_wizard_change_valid	operator	squares_valid
spell_parts_to_cast_table	base_relvar	squares_valid
board_ranges	view	squares_valid
check_con_spell_parts_to_cast_table_01_tuple	operator	squares_valid
check_con_pieces_to_move_ptype_allegiance_tag_key	operator	squares_valid
max	operator	squares_valid
remaining_walk_table_remaining_walk_key	database_constraint	squares_valid
check_con_spell_choice_hack_table_spell_choice_hack_key	operator	squares_valid
wizard_spell_choices_mr	base_relvar	squares_valid
check_turn_number_change_valid	operator	squares_valid
current_wizard_table_current_wizard_fkey	database_constraint	squares_valid
get_cast_alignment	operator	squares_valid
spell_choice_hack_table_spell_choice_hack_key	database_constraint	squares_valid
check_con_remaining_walk_table_01_tuple	operator	squares_valid
selectable_pieces_with_priorities	view	squares_valid
turn_number_table_turn_number_key	database_constraint	squares_valid
check_con_turn_phase_table_turn_phase_key	operator	squares_valid
get_current_wizard_spell	operator	squares_valid
check_con_wizard_spell_choices_wizard_name_spell_name_fkey	operator	squares_valid
update_spell_choice_restricted_transition_trigger	trigger	squares_valid
cast_success_checked_table_cast_success_checked_key	database_constraint	squares_valid
pieces_to_move_constraint_trigger	trigger	squares_valid
selected_piece_ptype_allegiance_tag_key	database_constraint	squares_valid
pieces_on_top_view	view	squares_valid
check_con_remaining_walk_only_motion	operator	squares_valid
random_test	scalar	squares_valid
wizard_spell_choices_wizard_name_spell_name_fkey	database_constraint	squares_valid
next_turn_phase	operator	squares_valid
get_spell_choice_hack	operator	squares_valid
remaining_walk_hack_table_constraint_trigger	trigger	squares_valid
pieces_to_move_constraint_trigger_operator	operator	squares_valid
selected_piece_01_tuple	database_constraint	squares_valid
turn_phase_table_constraint_trigger_operator	operator	squares_valid
check_con_game_completed_table_01_tuple	operator	squares_valid
cast_alignment_table_constraint_trigger_operator	operator	squares_valid
pieces_to_move	base_relvar	squares_valid
check_con_cast_alignment_empty	operator	squares_valid
cast_alignment_table_constraint_trigger	trigger	squares_valid
check_con_spell_parts_to_cast_table_spell_parts_to_cast_key	operator	squares_valid
spell_parts_to_cast_table_01_tuple	database_constraint	squares_valid
turn_phase_table_turn_phase_key	database_constraint	squares_valid
remaining_walk_table	base_relvar	squares_valid
pieces_on_top	view	squares_valid
check_con_pieces_to_move_allegiance_fkey	operator	squares_valid
in_next_phase_hack_table_in_next_phase_hack_key	database_constraint	squares_valid
test_action_overrides	base_relvar	squares_valid
wizard_spell_choices_mr_constraint_trigger	trigger	squares_valid
check_current_wizard_table_no_delete	operator	squares_valid
remaining_walk_table_01_tuple	database_constraint	squares_valid
spell_choice_hack_table_01_tuple	database_constraint	squares_valid
turn_number_change_valid_transition_trigger	trigger	squares_valid
check_turn_number_table_no_delete	operator	squares_valid
check_turn_phase_table_no_delete	operator	squares_valid
get_remaining_walk	operator	squares_valid
check_con_parts_to_cast_only	operator	squares_valid
check_con_wizard_spell_choices_mr_wizard_name_key	operator	squares_valid
get_creating_new_game	operator	squares_valid
turn_number_table_01_tuple	database_constraint	squares_valid
current_wizard_table_no_insert_transition_trigger	trigger	squares_valid
remaining_walk_table_constraint_trigger	trigger	squares_valid
turn_number_table_no_delete_transition_trigger	trigger	squares_valid
check_con_in_next_phase_hack_table_in_next_phase_hack_key	operator	squares_valid
creating_new_game_table_01_tuple	database_constraint	squares_valid
check_con_selected_piece_ptype_allegiance_tag_fkey	operator	squares_valid
test_action_overrides_override_key	database_constraint	squares_valid
get_remaining_walk_hack	operator	squares_valid
check_con_creating_new_game_table_creating_new_game_key	operator	squares_valid
spell_parts_to_cast_table_spell_parts_to_cast_key	database_constraint	squares_valid
selected_piece_ptype_allegiance_tag_fkey	database_constraint	squares_valid
check_con_dead_wizard_no_spell	operator	squares_valid
delete_spell_choice_restricted_transition_trigger	trigger	squares_valid
check_con_pieces_to_move_empty	operator	squares_valid
check_con_test_action_overrides_override_key	operator	squares_valid
cast_alignment_empty	database_constraint	squares_valid
no_deletes_inserts_except_new_game	operator	squares_valid
check_turn_phase_table_no_insert	operator	squares_valid
get_spell_parts_to_cast	operator	squares_valid
check_con_turn_number_table_turn_number_key	operator	squares_valid
remaining_walk_hack_table	base_relvar	squares_valid
turn_phase_table_no_delete_transition_trigger	trigger	squares_valid
moving_pieces	view	squares_valid
pieces_with_priorities	view	squares_valid
parts_to_cast_only	database_constraint	squares_valid
get_turn_number	operator	squares_valid
current_wizard_table_01_tuple	database_constraint	squares_valid
check_con_turn_number_table_01_tuple	operator	squares_valid
selected_piece_allegiance_fkey	database_constraint	squares_valid
check_con_selected_piece_ptype_allegiance_tag_key	operator	squares_valid
cast_success_checked_table_constraint_trigger_operator	operator	squares_valid
turn_number_table_no_insert_transition_trigger	trigger	squares_valid
check_delete_spell_choice_restricted	operator	squares_valid
check_con_current_wizard_table_current_wizard_key	operator	squares_valid
check_con_in_next_phase_hack_table_01_tuple	operator	squares_valid
game_completed_table_game_completed_key	database_constraint	squares_valid
remaining_walk_hack_table_remaining_walk_hack_key	database_constraint	squares_valid
wizard_spell_choices	view	squares_valid
spell_choice_hack_table	base_relvar	squares_valid
check_turn_phase_change_valid	operator	squares_valid
check_con_cast_success_checked_table_cast_success_checked_key	operator	squares_valid
check_random_success	operator	squares_valid
check_con_cast_alignment_table_01_tuple	operator	squares_valid
spell_parts_to_cast_table_constraint_trigger	trigger	squares_valid
cast_checked_cast_only	database_constraint	squares_valid
check_con_cast_alignment_table_cast_alignment_key	operator	squares_valid
check_con_remaining_walk_table_remaining_walk_key	operator	squares_valid
creating_new_game_table	base_relvar	squares_valid
pieces_to_move_allegiance_fkey	database_constraint	squares_valid
pieces_to_move_ptype_allegiance_tag_fkey	database_constraint	squares_valid
dead_wizard_no_spell	database_constraint	squares_valid
in_next_phase_hack_table_constraint_trigger_operator	operator	squares_valid
get_turn_phase	operator	squares_valid
limit_chance	operator	squares_valid
check_con_spell_choice_hack_table_01_tuple	operator	squares_valid
check_con_creating_new_game_table_01_tuple	operator	squares_valid
pieces_to_move_ptype_allegiance_tag_key	database_constraint	squares_valid
next_wizard_change_valid_transition_trigger	trigger	squares_valid
current_wizard_table_constraint_trigger_operator	operator	squares_valid
selected_piece_constraint_trigger_operator	operator	squares_valid
check_con_pieces_to_move_ptype_allegiance_tag_fkey	operator	squares_valid
turn_phase_table_no_insert_transition_trigger	trigger	squares_valid
turn_phase_enum	scalar	squares_valid
distance	operator	squares_valid
game_completed_table_constraint_trigger_operator	operator	squares_valid
check_insert_spell_choice_restricted	operator	squares_valid
cast_success_checked_table	base_relvar	squares_valid
init_turn_stuff	operator	squares_valid
check_update_spell_choice_restricted	operator	squares_valid
wizard_spell_choices_mr_wizard_name_key	database_constraint	squares_valid
check_con_selected_piece_allegiance_fkey	operator	squares_valid
turn_phase_table_constraint_trigger	trigger	squares_valid
adjust_world_alignment	operator	squares_valid
check_current_wizard_table_no_insert	operator	squares_valid
cast_alignment_table_cast_alignment_key	database_constraint	squares_valid
get_in_next_phase_hack	operator	squares_valid
get_current_wizard	operator	squares_valid
check_con_selected_piece_01_tuple	operator	squares_valid
game_completed_table_01_tuple	database_constraint	squares_valid
current_wizard_table_no_delete_transition_trigger	trigger	squares_valid
get_game_completed	operator	squares_valid
selected_piece	base_relvar	squares_valid
in_next_phase_hack_table_01_tuple	database_constraint	squares_valid
turn_phase_table_01_tuple	database_constraint	squares_valid
remaining_walk_hack_table_01_tuple	database_constraint	squares_valid
turn_number_table_constraint_trigger	trigger	squares_valid
wizard_spell_choices_mr_constraint_trigger_operator	operator	squares_valid
min	operator	squares_valid
remaining_walk_hack_table_constraint_trigger_operator	operator	squares_valid
current_wizard_spell	view	squares_valid
turn_number_table_constraint_trigger_operator	operator	squares_valid
cast_alignment_table	base_relvar	squares_valid
check_con_game_completed_table_game_completed_key	operator	squares_valid
selected_piece_constraint_trigger	trigger	squares_valid
current_wizard_table_constraint_trigger	trigger	squares_valid
check_con_remaining_walk_hack_table_01_tuple	operator	squares_valid
current_wizard_table	base_relvar	squares_valid
insert_spell_choice_restricted_transition_trigger	trigger	squares_valid
turn_number_table	base_relvar	squares_valid
game_completed	operator	squares_valid
check_con_turn_phase_table_01_tuple	operator	squares_valid
creating_new_game_table_constraint_trigger	trigger	squares_valid
action_rig_action_success	operator	squares_valid
check_con_current_wizard_must_be_alive	operator	squares_valid
spell_choice_hack_table_constraint_trigger	trigger	squares_valid
spell_choice_hack_table_constraint_trigger_operator	operator	squares_valid
check_con_remaining_walk_hack_table_remaining_walk_hack_key	operator	squares_valid
check_turn_number_table_no_insert	operator	squares_valid
creating_new_game_table_constraint_trigger_operator	operator	squares_valid
check_con_current_wizard_table_01_tuple	operator	squares_valid
check_con_cast_checked_cast_only	operator	squares_valid
next_wizard	view	squares_valid
game_completed_wizards	database_constraint	squares_valid
cast_success_checked_table_constraint_trigger	trigger	squares_valid
pieces_to_move_empty	database_constraint	squares_valid
next_wizard	operator	squares_valid
get_cast_success_checked	operator	squares_valid
check_con_current_wizard_table_current_wizard_fkey	operator	squares_valid
wizard_spell_choices_imaginary	view	squares_valid
check_con_chosen_spell_phase_valid	operator	squares_valid
turn_phase_change_valid_transition_trigger	trigger	squares_valid
check_con_cast_success_checked_table_01_tuple	operator	squares_valid
game_completed_table_constraint_trigger	trigger	squares_valid
move_phase	scalar	squares_valid
check_con_game_completed_wizards	operator	squares_valid
current_wizard_must_be_alive	database_constraint	squares_valid
in_next_phase_hack_table_constraint_trigger	trigger	squares_valid
cast_alignment_table_01_tuple	database_constraint	squares_valid
spell_parts_to_cast_table_constraint_trigger_operator	operator	squares_valid
remaining_walk_only_motion	database_constraint	squares_valid
cast_success_checked_table_01_tuple	database_constraint	squares_valid
current_wizard	view	squares_valid
get_current_turn_pos	operator	squares_valid
remaining_walk_table_constraint_trigger_operator	operator	squares_valid
game_completed_table	base_relvar	squares_valid
turn_phase_table	base_relvar	squares_valid
current_wizard_table_current_wizard_key	database_constraint	squares_valid
chosen_spell_phase_valid	database_constraint	squares_valid
creating_new_game_table_creating_new_game_key	database_constraint	squares_valid
action_choose_magic_wood_spell	operator	actions
selected_piece_move_squares	view	actions
action_set_imaginary	operator	actions
action_choose_goblin_spell	operator	actions
cast_subversion	operator	actions
check_con_disable_spreading_table_01_tuple	operator	actions
action_unselect_piece	operator	actions
action_select_piece_at_position	operator	actions
is_first_wizard	operator	actions
selected_piece_fly_squares	view	actions
action_choose_dark_power_spell	operator	actions
selected_piecexy	view	actions
cast_ballistic_spell	operator	actions
valid_activate_actions	view	actions
action_choose_orc_spell	operator	actions
action_fly	operator	actions
action_choose_no_spell	operator	actions
current_wizard_spell_type_squares	view	actions
corpse_only_squares	view	actions
action_choose_gooey_blob_spell	operator	actions
action_choose_magic_bow_spell	operator	actions
action_choose_magic_bolt_spell	operator	actions
action_choose_law_spell	operator	actions
cast_disbelieve	operator	actions
action_choose_lightning_spell	operator	actions
cast_decree_spell	operator	actions
action_choose_shadow_wood_spell	operator	actions
action_choose_spell	operator	actions
action_choose_magic_armour_spell	operator	actions
makenrandoms	operator	actions
action_choose_manticore_spell	operator	actions
action_choose_spectre_spell	operator	actions
disable_spreading_table_constraint_trigger	trigger	actions
creature_on_top_squares	view	actions
action_choose_large_law_spell	operator	actions
disintegrate_wizards_army	operator	actions
do_next_move_subphase	operator	actions
spell_valid_squares	view	actions
do_autonomous_phase	operator	actions
current_wizard_spell_range_squares	view	actions
action_choose_large_chaos_spell	operator	actions
action_choose_gryphon_spell	operator	actions
action_cast_wizard_spell	operator	actions
disable_spreading_table	base_relvar	actions
action_choose_giant_rat_spell	operator	actions
spell_cast_chance	operator	actions
wizards_in_trees	view	actions
cast_monster_spell	operator	actions
make_piece_undead	operator	actions
action_cast_activate_spell	operator	actions
select_piece	operator	actions
selected_piece_walk_squares	view	actions
get_disable_spreading	operator	actions
empty_and_not_adjacent_to_tree_squares	view	actions
action_choose_skeleton_spell	operator	actions
check_con_spell_indexes_no_dis_turm_row_number_key	operator	actions
action_choose_king_cobra_spell	operator	actions
spell_indexes_no_dis_turm	base_relvar	actions
spell_cast_chance	view	actions
attackable_squares	view	actions
action_choose_harpy_spell	operator	actions
action_choose_shadow_form_spell	operator	actions
create_corpse	operator	actions
selected_piece_ranged_attackable_squares	view	actions
is_equipped	operator	actions
action_choose_dark_citadel_spell	operator	actions
add_chinned_history	operator	actions
action_choose_red_dragon_spell	operator	actions
action_choose_magic_sword_spell	operator	actions
action_cast_target_spell	operator	actions
cast_turmoil	operator	actions
action_choose_horse_spell	operator	actions
disable_spreading_table_01_tuple	database_constraint	actions
get_square_range	operator	actions
create_monster	operator	actions
create_object	operator	actions
action_choose_magic_fire_spell	operator	actions
piece_next_subphase	operator	actions
action_choose_wall_spell	operator	actions
array_contains	operator	actions
action_choose_vampire_spell	operator	actions
update_alignment_from_cast	operator	actions
cast_object_spell	operator	actions
squares_within_selected_piece_flight_range	view	actions
spreadable_squares	view	actions
adjacent_to_new_tree_squares	view	actions
current_wizard_spell_squares	view	actions
check_con_disable_spreading_table_disable_spreading_key	operator	actions
action_choose_golden_dragon_spell	operator	actions
spend_current_wizard_spell	operator	actions
action_choose_faun_spell	operator	actions
empty_or_corpse_only_squares	view	actions
create_piece_internal	operator	actions
selected_piece_fly_attack_squares	view	actions
cast_magic_wood_available_squares	view	actions
action_choose_turmoil_spell	operator	actions
is_last_wizard	operator	actions
action_choose_gorilla_spell	operator	actions
skip_spell	operator	actions
cast_magic_wood_squares	base_relvar	actions
kill_monster	operator	actions
cast_raise_dead	operator	actions
cast_lawchaos	operator	actions
monster_on_top_squares	view	actions
selectable_pieces	view	actions
action_choose_vengeance_spell	operator	actions
disintegrate	operator	actions
action_choose_raise_dead_spell	operator	actions
check_can_run_action	operator	actions
do_spreading	operator	actions
selected_piece_adjacent_attacking_squares	view	actions
action_choose_chaos_spell	operator	actions
cast_magic_wood	operator	actions
empty_squares	view	actions
spell_indexes_no_dis_turm_row_number_key	database_constraint	actions
action_ranged_attack	operator	actions
action_choose_ghost_spell	operator	actions
get_next_tag	operator	actions
action_choose_zombie_spell	operator	actions
check_spell_success	operator	actions
add_history_shrugged_off	operator	actions
action_choose_green_dragon_spell	operator	actions
action_cancel	operator	actions
action_choose_magic_knife_spell	operator	actions
disable_spreading_table_constraint_trigger_operator	operator	actions
action_choose_disbelieve_spell	operator	actions
action_choose_hydra_spell	operator	actions
check_engaged	operator	actions
action_choose_eagle_spell	operator	actions
selected_piece_move_to	operator	actions
kill_piece	operator	actions
kill_wizard	operator	actions
action_choose_subversion_spell	operator	actions
action_next_phase	operator	actions
action_choose_lion_spell	operator	actions
selected_piece_attackable_squares	view	actions
action_choose_justice_spell	operator	actions
action_walk	operator	actions
action_choose_magic_wings_spell	operator	actions
action_choose_decree_spell	operator	actions
adjacent_to_tree_squares	view	actions
kill_top_piece_at	operator	actions
action_choose_elf_spell	operator	actions
action_choose_magic_shield_spell	operator	actions
action_cast_failed	operator	actions
disable_spreading_table_disable_spreading_key	database_constraint	actions
valid_target_actions	view	actions
current_wizard_replicant	operator	actions
action_choose_giant_spell	operator	actions
action_choose_ogre_spell	operator	actions
action_choose_unicorn_spell	operator	actions
action_set_real	operator	actions
action_choose_wraith_spell	operator	actions
action_attack	operator	actions
selected_piece_walk_attack_squares	view	actions
selected_piece_in_range_squares	view	actions
action_choose_pegasus_spell	operator	actions
action_choose_magic_castle_spell	operator	actions
add_history_spell_succeeded	operator	action_history
action_history_mr	base_relvar	action_history
add_history_receive_spell	operator	action_history
add_history_recede	operator	action_history
action_history_mr_id_key	database_constraint	action_history
add_history_walked	operator	action_history
add_history_ranged_attack	operator	action_history
check_con_action_history_mr_id_key	operator	action_history
add_history_game_drawn	operator	action_history
add_history_set_real	operator	action_history
add_history_new_game	operator	action_history
add_history_spell_skipped	operator	action_history
add_history_game_won	operator	action_history
get_current_wizard_pos	operator	action_history
add_history_attack	operator	action_history
add_history_wizard_up	operator	action_history
add_history_disappear	operator	action_history
add_history_new_turn	operator	action_history
add_history_chinned	operator	action_history
add_history_attempt_activate_spell	operator	action_history
add_history_choose_spell	operator	action_history
history_name_enum	scalar	action_history
add_history_attempt_target_spell	operator	action_history
add_history_spell_failed	operator	action_history
add_history_spread	operator	action_history
add_history_fly	operator	action_history
add_history_set_imaginary	operator	action_history
wizard_starting_positions_wizard_count_place_key	database_constraint	new_game
check_con_action_new_game_argument_place_valid	operator	new_game
action_new_game_argument_constraint_trigger	trigger	new_game
check_con_action_new_game_argument_wizard_name_key	operator	new_game
check_con_wizard_starting_positions_wizard_count_x_y_key	operator	new_game
check_con_action_new_game_argument_place_key	operator	new_game
wizard_starting_positions_place_valid	database_constraint	new_game
action_new_game_argument	base_relvar	new_game
action_new_game_argument_wizard_name_key	database_constraint	new_game
check_con_wizard_starting_positions_wizard_count_place_key	operator	new_game
wizard_starting_positions_constraint_trigger	trigger	new_game
wizard_starting_positions_constraint_trigger_operator	operator	new_game
action_new_game_argument_constraint_trigger_operator	operator	new_game
wizard_starting_positions	base_relvar	new_game
wizard_starting_positions_wizard_count_x_y_key	database_constraint	new_game
action_new_game	operator	new_game
action_new_game_argument_place_key	database_constraint	new_game
action_new_game_argument_place_valid	database_constraint	new_game
check_con_wizard_starting_positions_place_valid	operator	new_game
action_setup_test_board	operator	new_game
turn_phase_table_changed	operator	window_management
cast_success_checked_table_changed	operator	window_management
prefered_targets	view	window_management
turn_phase_table_changed	trigger	window_management
cast_success_checked_table_changed	trigger	window_management
pieces_changed	operator	window_management
action_ai_continue	operator	window_management
pieces_changed	trigger	window_management
world_alignment_table_changed	trigger	window_management
wizard_spell_choices_mr_changed	operator	window_management
world_alignment_table_changed	operator	window_management
ai_useful_spells	view	window_management
windows	base_relvar	window_management
board_size_changed	operator	window_management
wizard_spell_choices_mr_changed	trigger	window_management
remaining_walk_table_changed	operator	window_management
wizards_changed	operator	window_management
spell_books_changed	trigger	window_management
closest_enemy_to_selected_piece	view	window_management
remaining_walk_table_changed	trigger	window_management
spell_books_changed	operator	window_management
wizards_changed	trigger	window_management
castable_target_spells	view	window_management
action_history_mr_changed	operator	window_management
game_completed_table_changed	operator	window_management
current_wizard_table_changed	operator	window_management
action_reset_windows	operator	window_management
turn_number_table_changed	trigger	window_management
action_history_mr_changed	trigger	window_management
turn_number_table_changed	operator	window_management
game_completed_table_changed	trigger	window_management
current_wizard_table_changed	trigger	window_management
windows_window_name_key	database_constraint	window_management
current_wizard_target_spells	view	window_management
test_action_overrides_changed	trigger	window_management
test_action_overrides_changed	operator	window_management
pieces_to_move_changed	trigger	window_management
check_con_windows_window_name_key	operator	window_management
selected_piece_changed	operator	window_management
pieces_to_move_changed	operator	window_management
ai_cast_spell	operator	window_management
ai_filtered_target_spells	view	window_management
selected_piece_changed	trigger	window_management
select_best_move	view	window_management
ai_choose_spell	operator	window_management
disable_spreading_table_changed	trigger	window_management
disable_spreading_table_changed	operator	window_management
ai_move_pieces	operator	window_management
current_wizard_square	view	window_management
spell_parts_to_cast_table_changed	trigger	window_management
ai_selected_piece_actions	view	window_management
crimes_against_nature_changed	operator	window_management
spell_parts_to_cast_table_changed	operator	window_management
window_state	scalar	window_management
crimes_against_nature_changed	trigger	window_management
imaginary_pieces_changed	trigger	window_management
board_size_changed	trigger	window_management
ai_move_selected_piece	operator	window_management
action_hide_window	operator	window_management
imaginary_pieces_changed	operator	window_management
sprites	base_relvar	sprites
sprites_sprite_key	database_constraint	sprites
colours	base_relvar	sprites
check_con_sprites_sprite_key	operator	sprites
action_refresh_widgets	operator	sprites
check_con_colours_name_key	operator	sprites
colours_name_key	database_constraint	sprites
check_con_init_wizard_display_info_argument_sprite_key	operator	wizard_display_info
wizard_display_info_default_sprite_fkey	database_constraint	wizard_display_info
wizard_display_info_wizard_name_key	database_constraint	wizard_display_info
init_wizard_display_info_argument	base_relvar	wizard_display_info
wizard_display_info_wizard_name_fkey	database_constraint	wizard_display_info
check_con_init_wizard_display_info_argument_wizard_name_key	operator	wizard_display_info
init_wizard_display_info_argument_colour_key	database_constraint	wizard_display_info
wizard_display_info_colour_key	database_constraint	wizard_display_info
check_con_init_wizard_display_info_argument_sprite_fkey	operator	wizard_display_info
check_con_init_wizard_display_info_argument_wizard_name_fkey	operator	wizard_display_info
init_wizard_display_info_argument_sprite_fkey	database_constraint	wizard_display_info
init_wizard_display_info_argument_sprite_key	database_constraint	wizard_display_info
init_wizard_display_info	operator	wizard_display_info
init_wizard_display_info_argument_wizard_name_fkey	database_constraint	wizard_display_info
check_con_wizard_display_info_default_sprite_key	operator	wizard_display_info
init_wizard_display_info_argument_wizard_name_key	database_constraint	wizard_display_info
check_con_wizard_display_info_colour_key	operator	wizard_display_info
wizard_display_info_default_sprite_key	database_constraint	wizard_display_info
check_con_wizard_display_info_wizard_name_key	operator	wizard_display_info
check_con_wizard_display_info_wizard_name_fkey	operator	wizard_display_info
check_con_wizard_display_info_default_sprite_fkey	operator	wizard_display_info
check_con_init_wizard_display_info_argument_colour_key	operator	wizard_display_info
wizard_display_info	base_relvar	wizard_display_info
last_history_effect_id_table	base_relvar	board_widget
check_con_cursor_position_01_tuple	operator	board_widget
board_sprites1_view	view	board_widget
action_reset_current_effects	operator	board_widget
current_effects_01_tuple	database_constraint	board_widget
update_board_sprites_cache	operator	board_widget
action_move_cursor_to_current_wizard	operator	board_widget
last_history_effect_id_table_last_history_effect__key	database_constraint	board_widget
board_sound_effects_id_key	database_constraint	board_widget
piece_sprite	view	board_widget
check_con_piece_starting_ticks_ptype_allegiance_tag_fkey	operator	board_widget
history_no_visuals_history_name_key	database_constraint	board_widget
action_history_colour_mr	view	board_widget
current_board_sound_effects	view	board_widget
board_square_effects	base_relvar	board_widget
init_cursor_position	operator	board_widget
board_square_effects_id_key	database_constraint	board_widget
action_update_effects_ticks	operator	board_widget
history_no_visuals	base_relvar	board_widget
cursor_position	base_relvar	board_widget
check_con_board_square_effects_id_key	operator	board_widget
check_for_effects	operator	board_widget
check_con_board_beam_effects_id_key	operator	board_widget
check_con_history_sounds_history_name_sound_name_key	operator	board_widget
current_effects_constraint_trigger_operator	operator	board_widget
piece_details	view	board_widget
action_client_ai_continue_if	operator	board_widget
check_con_cursor_position_coordinates_valid	operator	board_widget
check_con_current_effects_01_tuple	operator	board_widget
cursor_position_constraint_trigger	trigger	board_widget
cursor_position_01_tuple	database_constraint	board_widget
board_sprites	view	board_widget
board_sprites1_cache	base_relvar	board_widget
board_sound_effects	base_relvar	board_widget
get_running_effects	operator	board_widget
cursor_piece_details	view	board_widget
update_missing_startticks	operator	board_widget
last_history_effect_id_table_01_tuple	database_constraint	board_widget
get_last_history_effect_id	operator	board_widget
check_con_board_sound_effects_id_key	operator	board_widget
cursor_position_constraint_trigger_operator	operator	board_widget
cursor_position_coordinates_valid	database_constraint	board_widget
allegiance_colours	view	board_widget
wizard_sprites	view	board_widget
check_con_last_history_effect_id_table_01_tuple	operator	board_widget
action_move_cursor	operator	board_widget
current_board_beam_effects	view	board_widget
piece_starting_ticks_ptype_allegiance_tag_fkey	database_constraint	board_widget
piece_starting_ticks_ptype_allegiance_tag_key	database_constraint	board_widget
check_con_history_no_visuals_history_name_key	operator	board_widget
current_board_square_effects	view	board_widget
current_effects_constraint_trigger	trigger	board_widget
board_highlights	view	board_widget
board_beam_effects_id_key	database_constraint	board_widget
check_con_piece_starting_ticks_ptype_allegiance_tag_key	operator	board_widget
check_con_last_history_effect_id_table_last_history_effect__key	operator	board_widget
safe_move_cursor	operator	board_widget
action_client_ai_continue	operator	board_widget
piece_starting_ticks	base_relvar	board_widget
last_history_effect_id_table_constraint_trigger_operator	operator	board_widget
history_sounds	base_relvar	board_widget
selected_piece_details	view	board_widget
history_sounds_history_name_sound_name_key	database_constraint	board_widget
board_beam_effects	base_relvar	board_widget
current_effects	base_relvar	board_widget
last_history_effect_id_table_constraint_trigger	trigger	board_widget
format_alignment	operator	spell_book_widget
count_icons	operator	spell_book_widget
spell_book_show_all_table_01_tuple	database_constraint	spell_book_widget
check_con_spell_sprites_spell_name_key	operator	spell_book_widget
spell_sprites_spell_name_fkey	database_constraint	spell_book_widget
check_con_spell_book_show_all_table_spell_book_show_all_key	operator	spell_book_widget
check_con_spell_keys_spell_name_key	operator	spell_book_widget
spell_details	view	spell_book_widget
align_icons	operator	spell_book_widget
action_spell_book_show_all_update	operator	spell_book_widget
section_order	view	spell_book_widget
spell_book_table	view	spell_book_widget
spell_book_show_all_table_constraint_trigger	trigger	spell_book_widget
check_con_spell_sprites_sprite_fkey	operator	spell_book_widget
spell_keys	base_relvar	spell_book_widget
current_wizard_selected_spell_details	view	spell_book_widget
spells_with_order	view	spell_book_widget
current_wizard_spell_counts	view	spell_book_widget
check_con_spell_keys_spell_name_fkey	operator	spell_book_widget
spell_keys_key_key	database_constraint	spell_book_widget
chance_colour	operator	spell_book_widget
spell_keys_spell_name_key	database_constraint	spell_book_widget
spell_book_show_all_table_spell_book_show_all_key	database_constraint	spell_book_widget
spell_book_show_all_table_constraint_trigger_operator	operator	spell_book_widget
check_con_spell_keys_key_key	operator	spell_book_widget
spell_sprites_sprite_fkey	database_constraint	spell_book_widget
check_con_spell_sprites_spell_name_fkey	operator	spell_book_widget
spell_colour	operator	spell_book_widget
spell_sprites_spell_name_key	database_constraint	spell_book_widget
spell_sprites	base_relvar	spell_book_widget
spell_book_show_all_table	base_relvar	spell_book_widget
check_con_spell_book_show_all_table_01_tuple	operator	spell_book_widget
spell_colours	view	spell_book_widget
get_spell_book_show_all	operator	spell_book_widget
spell_keys_spell_name_fkey	database_constraint	spell_book_widget
action_reset_new_game_widget_state	operator	new_game_widget
extract_wizard_state	operator	new_game_widget
check_con_new_game_widget_state_colour_key	operator	new_game_widget
check_con_new_game_widget_state_sprite_fkey	operator	new_game_widget
new_game_widget_state_constraint_trigger_operator	operator	new_game_widget
new_game_widget_state	base_relvar	new_game_widget
check_con_new_game_widget_state_line_valid	operator	new_game_widget
check_con_new_game_widget_state_wizard_name_key	operator	new_game_widget
new_game_widget_state_line_valid	database_constraint	new_game_widget
new_game_widget_state_colour_key	database_constraint	new_game_widget
action_client_new_game_using_new_game_widget_state	operator	new_game_widget
new_game_widget_state_line_key	database_constraint	new_game_widget
new_game_widget_state_constraint_trigger	trigger	new_game_widget
new_game_widget_state_sprite_key	database_constraint	new_game_widget
check_con_new_game_widget_state_line_key	operator	new_game_widget
new_wizard_state	scalar	new_game_widget
check_con_new_game_widget_state_sprite_key	operator	new_game_widget
new_game_widget_state_wizard_name_key	database_constraint	new_game_widget
new_game_widget_state_sprite_fkey	database_constraint	new_game_widget
action_move_cursor_down	operator	client_new_game
client_valid_activate_actions	view	client_new_game
action_move_cursor_down_left	operator	client_new_game
action_client_new_game_argument_colour_key	database_constraint	client_new_game
action_move_cursor_left	operator	client_new_game
action_client_new_game	operator	client_new_game
prompt	view	client_new_game
action_client_next_phase	operator	client_new_game
action_spell_book_show_all_update_on	operator	client_new_game
check_con_action_client_new_game_argument_wizard_name_key	operator	client_new_game
action_move_cursor_down_right	operator	client_new_game
action_move_cursor_up_right	operator	client_new_game
action_client_new_game_argument_sprite_key	database_constraint	client_new_game
action_client_new_game_argument_constraint_trigger	trigger	client_new_game
action_client_new_game_argument_constraint_trigger_operator	operator	client_new_game
check_con_action_client_new_game_place_valid	operator	client_new_game
create_client_action_wrapper	operator	client_new_game
action_move_cursor_up_left	operator	client_new_game
action_spell_book_show_all_update_off	operator	client_new_game
action_client_new_game_argument_place_key	database_constraint	client_new_game
action_client_new_game_place_valid	database_constraint	client_new_game
action_key_pressed	operator	client_new_game
check_con_action_client_new_game_argument_sprite_fkey	operator	client_new_game
action_client_new_game_argument	base_relvar	client_new_game
check_con_action_client_new_game_argument_sprite_key	operator	client_new_game
action_move_cursor_up	operator	client_new_game
action_go	operator	client_new_game
check_con_action_client_new_game_argument_place_key	operator	client_new_game
key_control_settings_key_code_action_name_key	database_constraint	client_new_game
key_control_settings	base_relvar	client_new_game
action_instructions	view	client_new_game
client_valid_target_actions	view	client_new_game
action_client_new_game_argument_wizard_name_key	database_constraint	client_new_game
action_client_new_game_argument_sprite_fkey	database_constraint	client_new_game
check_con_action_client_new_game_argument_colour_key	operator	client_new_game
action_move_cursor_right	operator	client_new_game
check_con_key_control_settings_key_code_action_name_key	operator	client_new_game
base_relvar_metadata_d_readonly_transition_trigger	trigger	metadata
check_base_relvar_metadata_u_readonly	operator	metadata
check_base_relvar_metadata_d_readonly	operator	metadata
base_relvar_metadata_i_readonly_transition_trigger	trigger	metadata
check_base_relvar_metadata_i_readonly	operator	metadata
base_relvar_metadata_u_readonly_transition_trigger	trigger	metadata
check_piece_prototypes_mr_u_readonly	operator	piece_prototypes
piece_prototypes_mr_d_readonly_transition_trigger	trigger	piece_prototypes
piece_prototypes_mr_i_readonly_transition_trigger	trigger	piece_prototypes
check_piece_prototypes_mr_d_readonly	operator	piece_prototypes
piece_prototypes_mr_u_readonly_transition_trigger	trigger	piece_prototypes
check_piece_prototypes_mr_i_readonly	operator	piece_prototypes
check_spells_mr_d_readonly	operator	spells
check_spells_mr_u_readonly	operator	spells
check_spells_mr_i_readonly	operator	spells
spells_mr_d_readonly_transition_trigger	trigger	spells
spells_mr_u_readonly_transition_trigger	trigger	spells
spells_mr_i_readonly_transition_trigger	trigger	spells
spell_indexes_no_dis_turm_u_readonly_transition_trigger	trigger	actions
check_spell_indexes_no_dis_turm_i_readonly	operator	actions
check_spell_indexes_no_dis_turm_d_readonly	operator	actions
spell_indexes_no_dis_turm_d_readonly_transition_trigger	trigger	actions
spell_indexes_no_dis_turm_i_readonly_transition_trigger	trigger	actions
check_spell_indexes_no_dis_turm_u_readonly	operator	actions
wizard_starting_positions_d_readonly_transition_trigger	trigger	new_game
wizard_starting_positions_u_readonly_transition_trigger	trigger	new_game
check_wizard_starting_positions_u_readonly	operator	new_game
check_wizard_starting_positions_i_readonly	operator	new_game
wizard_starting_positions_i_readonly_transition_trigger	trigger	new_game
check_wizard_starting_positions_d_readonly	operator	new_game
colours_u_readonly_transition_trigger	trigger	sprites
colours_d_readonly_transition_trigger	trigger	sprites
check_colours_i_readonly	operator	sprites
check_colours_u_readonly	operator	sprites
colours_i_readonly_transition_trigger	trigger	sprites
check_colours_d_readonly	operator	sprites
sprites_i_readonly_transition_trigger	trigger	sprites
check_sprites_u_readonly	operator	sprites
check_sprites_d_readonly	operator	sprites
sprites_u_readonly_transition_trigger	trigger	sprites
sprites_d_readonly_transition_trigger	trigger	sprites
check_sprites_i_readonly	operator	sprites
check_history_sounds_u_readonly	operator	board_widget
history_sounds_u_readonly_transition_trigger	trigger	board_widget
history_sounds_i_readonly_transition_trigger	trigger	board_widget
check_history_sounds_d_readonly	operator	board_widget
history_sounds_d_readonly_transition_trigger	trigger	board_widget
check_history_sounds_i_readonly	operator	board_widget
check_history_no_visuals_u_readonly	operator	board_widget
history_no_visuals_u_readonly_transition_trigger	trigger	board_widget
check_history_no_visuals_i_readonly	operator	board_widget
history_no_visuals_d_readonly_transition_trigger	trigger	board_widget
check_history_no_visuals_d_readonly	operator	board_widget
history_no_visuals_i_readonly_transition_trigger	trigger	board_widget
spell_sprites_u_readonly_transition_trigger	trigger	spell_book_widget
check_spell_sprites_u_readonly	operator	spell_book_widget
spell_sprites_i_readonly_transition_trigger	trigger	spell_book_widget
check_spell_sprites_i_readonly	operator	spell_book_widget
spell_sprites_d_readonly_transition_trigger	trigger	spell_book_widget
check_spell_sprites_d_readonly	operator	spell_book_widget
check_spell_keys_i_readonly	operator	spell_book_widget
check_spell_keys_d_readonly	operator	spell_book_widget
spell_keys_d_readonly_transition_trigger	trigger	spell_book_widget
spell_keys_u_readonly_transition_trigger	trigger	spell_book_widget
check_spell_keys_u_readonly	operator	spell_book_widget
spell_keys_i_readonly_transition_trigger	trigger	spell_book_widget
check_key_control_settings_u_readonly	operator	client_new_game
key_control_settings_d_readonly_transition_trigger	trigger	client_new_game
key_control_settings_u_readonly_transition_trigger	trigger	client_new_game
check_key_control_settings_i_readonly	operator	client_new_game
key_control_settings_i_readonly_transition_trigger	trigger	client_new_game
check_key_control_settings_d_readonly	operator	client_new_game
\.


--
-- TOC entry 3404 (class 0 OID 261512)
-- Dependencies: 2650
-- Data for Name: b; Type: TABLE DATA; Schema: public; Owner: -
--

COPY b (adrelid, adnum, adbin, adsrc) FROM stdin;
258241	3	{FUNCEXPR :funcid 480 :funcresulttype 23 :funcretset false :funcformat 2 :args ({FUNCEXPR :funcid 1574 :funcresulttype 20 :funcretset false :funcformat 0 :args ({CONST :consttype 2205 :consttypmod -1 :constlen 4 :constbyval true :constisnull false :location -1 :constvalue 4 [ -65 -16 3 0 ]}) :location -1}) :location -1}	nextval('modules_module_order_seq'::regclass)
258435	2	{CONST :consttype 16 :consttypmod -1 :constlen 1 :constbyval true :constisnull false :location 73 :constvalue 1 [ 0 0 0 0 ]}	false
258435	3	{CONST :consttype 16 :consttypmod -1 :constlen 1 :constbyval true :constisnull false :location 110 :constvalue 1 [ 0 0 0 0 ]}	false
258435	4	{CONST :consttype 16 :consttypmod -1 :constlen 1 :constbyval true :constisnull false :location 147 :constvalue 1 [ 0 0 0 0 ]}	false
258435	5	{CONST :consttype 16 :consttypmod -1 :constlen 1 :constbyval true :constisnull false :location 185 :constvalue 1 [ 0 0 0 0 ]}	false
258435	6	{CONST :consttype 16 :consttypmod -1 :constlen 1 :constbyval true :constisnull false :location 222 :constvalue 1 [ 0 0 0 0 ]}	false
258435	7	{CONST :consttype 16 :consttypmod -1 :constlen 1 :constbyval true :constisnull false :location 260 :constvalue 1 [ 0 0 0 0 ]}	false
258435	8	{CONST :consttype 16 :consttypmod -1 :constlen 1 :constbyval true :constisnull false :location 295 :constvalue 1 [ 0 0 0 0 ]}	false
258435	11	{CONST :consttype 16 :consttypmod -1 :constlen 1 :constbyval true :constisnull false :location 382 :constvalue 1 [ 0 0 0 0 ]}	false
258458	1	{FUNCEXPR :funcid 480 :funcresulttype 23 :funcretset false :funcformat 2 :args ({FUNCEXPR :funcid 1574 :funcresulttype 20 :funcretset false :funcformat 0 :args ({CONST :consttype 2205 :consttypmod -1 :constlen 4 :constbyval true :constisnull false :location -1 :constvalue 4 [ -104 -15 3 0 ]}) :location -1}) :location -1}	nextval('spell_books_id_seq'::regclass)
260248	1	{FUNCEXPR :funcid 480 :funcresulttype 23 :funcretset false :funcformat 2 :args ({FUNCEXPR :funcid 1574 :funcresulttype 20 :funcretset false :funcformat 0 :args ({CONST :consttype 2205 :consttypmod -1 :constlen 4 :constbyval true :constisnull false :location -1 :constvalue 4 [ -106 -8 3 0 ]}) :location -1}) :location -1}	nextval('spell_indexes_no_dis_turm_row_number_seq'::regclass)
260278	1	{FUNCEXPR :funcid 480 :funcresulttype 23 :funcretset false :funcformat 2 :args ({FUNCEXPR :funcid 1574 :funcresulttype 20 :funcretset false :funcformat 0 :args ({CONST :consttype 2205 :consttypmod -1 :constlen 4 :constbyval true :constisnull false :location -1 :constvalue 4 [ -76 -8 3 0 ]}) :location -1}) :location -1}	nextval('action_history_mr_id_seq'::regclass)
260814	1	{FUNCEXPR :funcid 480 :funcresulttype 23 :funcretset false :funcformat 2 :args ({FUNCEXPR :funcid 1574 :funcresulttype 20 :funcretset false :funcformat 0 :args ({CONST :consttype 2205 :consttypmod -1 :constlen 4 :constbyval true :constisnull false :location -1 :constvalue 4 [ -52 -6 3 0 ]}) :location -1}) :location -1}	nextval('board_square_effects_id_seq'::regclass)
260826	1	{FUNCEXPR :funcid 480 :funcresulttype 23 :funcretset false :funcformat 2 :args ({FUNCEXPR :funcid 1574 :funcresulttype 20 :funcretset false :funcformat 0 :args ({CONST :consttype 2205 :consttypmod -1 :constlen 4 :constbyval true :constisnull false :location -1 :constvalue 4 [ -40 -6 3 0 ]}) :location -1}) :location -1}	nextval('board_beam_effects_id_seq'::regclass)
260838	1	{FUNCEXPR :funcid 480 :funcresulttype 23 :funcretset false :funcformat 2 :args ({FUNCEXPR :funcid 1574 :funcresulttype 20 :funcretset false :funcformat 0 :args ({CONST :consttype 2205 :consttypmod -1 :constlen 4 :constbyval true :constisnull false :location -1 :constvalue 4 [ -28 -6 3 0 ]}) :location -1}) :location -1}	nextval('board_sound_effects_id_seq'::regclass)
\.


--
-- TOC entry 3351 (class 0 OID 258281)
-- Dependencies: 2484
-- Data for Name: base_relvar_metadata; Type: TABLE DATA; Schema: public; Owner: -
--

COPY base_relvar_metadata (relvar_name, type) FROM stdin;
base_relvar_metadata	readonly
piece_prototypes_mr	readonly
spells_mr	readonly
board_size	data
world_alignment_table	data
wizards	data
spell_books	data
pieces	data
imaginary_pieces	data
crimes_against_nature	data
in_next_phase_hack_table	stack
creating_new_game_table	stack
turn_number_table	data
current_wizard_table	data
turn_phase_table	data
spell_choice_hack_table	stack
wizard_spell_choices_mr	data
spell_parts_to_cast_table	data
cast_success_checked_table	data
cast_alignment_table	stack
pieces_to_move	data
selected_piece	data
remaining_walk_table	data
remaining_walk_hack_table	stack
game_completed_table	data
test_action_overrides	data
cast_magic_wood_squares	stack
disable_spreading_table	data
spell_indexes_no_dis_turm	readonly
action_history_mr	data
wizard_starting_positions	readonly
action_new_game_argument	stack
windows	data
colours	readonly
sprites	readonly
wizard_display_info	data
init_wizard_display_info_argument	stack
cursor_position	data
piece_starting_ticks	data
board_sprites1_cache	data
board_square_effects	data
board_beam_effects	data
board_sound_effects	data
history_sounds	readonly
history_no_visuals	readonly
last_history_effect_id_table	data
current_effects	data
spell_sprites	readonly
spell_book_show_all_table	data
spell_keys	readonly
new_game_widget_state	data
key_control_settings	readonly
action_client_new_game_argument	stack
\.


--
-- TOC entry 3392 (class 0 OID 260826)
-- Dependencies: 2619
-- Data for Name: board_beam_effects; Type: TABLE DATA; Schema: public; Owner: -
--

COPY board_beam_effects (id, subtype, x1, y1, x2, y2, queuepos) FROM stdin;
\.


--
-- TOC entry 3354 (class 0 OID 258390)
-- Dependencies: 2502
-- Data for Name: board_size; Type: TABLE DATA; Schema: public; Owner: -
--

COPY board_size (width, height) FROM stdin;
15	10
\.


--
-- TOC entry 3393 (class 0 OID 260838)
-- Dependencies: 2621
-- Data for Name: board_sound_effects; Type: TABLE DATA; Schema: public; Owner: -
--

COPY board_sound_effects (id, subtype, sound_name, queuepos) FROM stdin;
\.


--
-- TOC entry 3390 (class 0 OID 260800)
-- Dependencies: 2614
-- Data for Name: board_sprites1_cache; Type: TABLE DATA; Schema: public; Owner: -
--

COPY board_sprites1_cache (x, y, ptype, allegiance, tag, sprite, colour, sp, start_tick, animation_speed, selected) FROM stdin;
1	0	green_dragon	Kong Fuzi	0	green_dragon	purple	1	2420	32	t
0	0	wizard	Buddha	0	wizard0	blue	2	1679	250	f
3	0	wizard	Kong Fuzi	0	wizard1	purple	2	1183	250	f
0	2			-1	highlight_fly	white	5	0	250	f
0	3			-1	highlight_fly	white	5	0	250	f
1	1			-1	highlight_fly	white	5	0	250	f
1	2			-1	highlight_fly	white	5	0	250	f
1	3			-1	highlight_fly	white	5	0	250	f
2	0			-1	highlight_fly	white	5	0	250	f
2	1			-1	highlight_fly	white	5	0	250	f
2	2			-1	highlight_fly	white	5	0	250	f
2	3			-1	highlight_fly	white	5	0	250	f
3	1			-1	highlight_fly	white	5	0	250	f
3	2			-1	highlight_fly	white	5	0	250	f
4	0			-1	highlight_fly	white	5	0	250	f
0	0			-1	highlight_attack	white	5	0	250	f
4	1			-1	highlight_fly	white	5	0	250	f
0	0			-1	highlight_ranged_attack	white	5	0	250	f
0	1			-1	highlight_fly	white	5	0	250	f
\.


--
-- TOC entry 3391 (class 0 OID 260814)
-- Dependencies: 2617
-- Data for Name: board_square_effects; Type: TABLE DATA; Schema: public; Owner: -
--

COPY board_square_effects (id, subtype, x1, y1, queuepos) FROM stdin;
\.


--
-- TOC entry 3370 (class 0 OID 259368)
-- Dependencies: 2538
-- Data for Name: cast_alignment_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY cast_alignment_table (cast_alignment) FROM stdin;
\.


--
-- TOC entry 3377 (class 0 OID 260134)
-- Dependencies: 2577
-- Data for Name: cast_magic_wood_squares; Type: TABLE DATA; Schema: public; Owner: -
--

COPY cast_magic_wood_squares (x, y) FROM stdin;
\.


--
-- TOC entry 3369 (class 0 OID 259275)
-- Dependencies: 2537
-- Data for Name: cast_success_checked_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY cast_success_checked_table (cast_success_checked) FROM stdin;
\.


--
-- TOC entry 3384 (class 0 OID 260552)
-- Dependencies: 2602
-- Data for Name: colours; Type: TABLE DATA; Schema: public; Owner: -
--

COPY colours (name, red, green, blue) FROM stdin;
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


--
-- TOC entry 3346 (class 0 OID 258145)
-- Dependencies: 2475
-- Data for Name: con_pg; Type: TABLE DATA; Schema: public; Owner: -
--

COPY con_pg (constraint_name) FROM stdin;
system_implementation_objects_object_name_object__key
database_constraints_constraint_name_key
dbcon_ops_constraint_name_key
dbcon_ops_operator_name_key
dbcon_ops_constraint_name_fkey
dbcon_relvars_constraint_name_relvar_name_key
dbcon_relvars_constraint_name_fkey
con_pg_constraint_name_key
con_pg_constraint_name_fkey
dbcon_trigger_ops_operator_name_key
dbcon_triggers_trigger_name_key
modules_module_name_key
all_module_objects_object_name_object_type_key
all_module_objects_module_name_fkey
base_relvar_metadata_relvar_name_key
piece_prototypes_mr_ptype_key
spells_mr_spell_name_key
board_size_width_height_key
world_alignment_table_world_alignment_key
wizards_wizard_name_key
spell_books_id_key
spell_books_wizard_name_fkey
pieces_ptype_allegiance_tag_key
imaginary_pieces_ptype_allegiance_tag_key
imaginary_pieces_ptype_allegiance_tag_fkey
crimes_against_nature_ptype_allegiance_tag_key
crimes_against_nature_ptype_allegiance_tag_fkey
in_next_phase_hack_table_in_next_phase_hack_key
creating_new_game_table_creating_new_game_key
turn_number_table_turn_number_key
current_wizard_table_current_wizard_key
current_wizard_table_current_wizard_fkey
turn_phase_table_turn_phase_key
wizard_spell_choices_mr_wizard_name_key
spell_choice_hack_table_spell_choice_hack_key
spell_parts_to_cast_table_spell_parts_to_cast_key
cast_success_checked_table_cast_success_checked_key
cast_alignment_table_cast_alignment_key
pieces_to_move_ptype_allegiance_tag_key
pieces_to_move_ptype_allegiance_tag_fkey
pieces_to_move_allegiance_fkey
selected_piece_ptype_allegiance_tag_key
selected_piece_ptype_allegiance_tag_fkey
selected_piece_allegiance_fkey
remaining_walk_table_remaining_walk_key
remaining_walk_hack_table_remaining_walk_hack_key
game_completed_table_game_completed_key
test_action_overrides_override_key
disable_spreading_table_disable_spreading_key
spell_indexes_no_dis_turm_row_number_key
action_history_mr_id_key
wizard_starting_positions_wizard_count_place_key
wizard_starting_positions_wizard_count_x_y_key
action_new_game_argument_place_key
action_new_game_argument_wizard_name_key
windows_window_name_key
colours_name_key
sprites_sprite_key
wizard_display_info_wizard_name_key
wizard_display_info_default_sprite_key
wizard_display_info_colour_key
wizard_display_info_wizard_name_fkey
wizard_display_info_default_sprite_fkey
init_wizard_display_info_argument_wizard_name_key
init_wizard_display_info_argument_sprite_key
init_wizard_display_info_argument_colour_key
init_wizard_display_info_argument_wizard_name_fkey
init_wizard_display_info_argument_sprite_fkey
piece_starting_ticks_ptype_allegiance_tag_key
piece_starting_ticks_ptype_allegiance_tag_fkey
board_square_effects_id_key
board_beam_effects_id_key
board_sound_effects_id_key
history_sounds_history_name_sound_name_key
history_no_visuals_history_name_key
last_history_effect_id_table_last_history_effect__key
spell_sprites_spell_name_key
spell_sprites_sprite_fkey
spell_sprites_spell_name_fkey
spell_book_show_all_table_spell_book_show_all_key
spell_keys_spell_name_key
spell_keys_key_key
spell_keys_spell_name_fkey
new_game_widget_state_line_key
new_game_widget_state_wizard_name_key
new_game_widget_state_sprite_key
new_game_widget_state_colour_key
new_game_widget_state_sprite_fkey
key_control_settings_key_code_action_name_key
action_client_new_game_argument_place_key
action_client_new_game_argument_wizard_name_key
action_client_new_game_argument_sprite_key
action_client_new_game_argument_colour_key
action_client_new_game_argument_sprite_fkey
\.


--
-- TOC entry 3362 (class 0 OID 258765)
-- Dependencies: 2524
-- Data for Name: creating_new_game_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY creating_new_game_table (creating_new_game) FROM stdin;
f
\.


--
-- TOC entry 3360 (class 0 OID 258653)
-- Dependencies: 2513
-- Data for Name: crimes_against_nature; Type: TABLE DATA; Schema: public; Owner: -
--

COPY crimes_against_nature (ptype, allegiance, tag) FROM stdin;
\.


--
-- TOC entry 3397 (class 0 OID 260940)
-- Dependencies: 2625
-- Data for Name: current_effects; Type: TABLE DATA; Schema: public; Owner: -
--

COPY current_effects (ticks, queuepos) FROM stdin;
\.


--
-- TOC entry 3364 (class 0 OID 258851)
-- Dependencies: 2527
-- Data for Name: current_wizard_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY current_wizard_table (current_wizard) FROM stdin;
\.


--
-- TOC entry 3388 (class 0 OID 260633)
-- Dependencies: 2608
-- Data for Name: cursor_position; Type: TABLE DATA; Schema: public; Owner: -
--

COPY cursor_position (x, y) FROM stdin;
0	0
\.


--
-- TOC entry 3343 (class 0 OID 258085)
-- Dependencies: 2466
-- Data for Name: database_constraints; Type: TABLE DATA; Schema: public; Owner: -
--

COPY database_constraints (constraint_name, expression) FROM stdin;
system_implementation_objects_object_name_object__key	(select count(*) from system_implementation_objects ) =\n(select count(distinct object_name, object_type)\nfrom system_implementation_objects)
database_constraints_constraint_name_key	(select count(*) from database_constraints ) =\n(select count(distinct constraint_name)\nfrom database_constraints)
dbcon_ops_constraint_name_key	(select count(*) from dbcon_ops ) =\n(select count(distinct constraint_name)\nfrom dbcon_ops)
dbcon_ops_operator_name_key	(select count(*) from dbcon_ops ) =\n(select count(distinct operator_name)\nfrom dbcon_ops)
dbcon_ops_constraint_name_fkey	not exists\n(select constraint_name from dbcon_ops\n  except\nselect constraint_name from database_constraints)
dbcon_relvars_constraint_name_relvar_name_key	(select count(*) from dbcon_relvars ) =\n(select count(distinct constraint_name, relvar_name)\nfrom dbcon_relvars)
dbcon_relvars_constraint_name_fkey	not exists\n(select constraint_name from dbcon_relvars\n  except\nselect constraint_name from database_constraints)
dbcon_relvars_relvar_name_fkey	not exists\n(select relvar_name from dbcon_relvars\n  except\nselect relvar_name from base_relvars)
con_pg_constraint_name_key	(select count(*) from con_pg ) =\n(select count(distinct constraint_name)\nfrom con_pg)
con_pg_constraint_name_fkey	not exists\n(select constraint_name from con_pg\n  except\nselect constraint_name from database_constraints)
con_pg_constraint_name_fkey1	not exists\n(select constraint_name from con_pg\n  except\nselect constraint_name from (select constraint_name from check_pg union\n   select constraint_name from key_pg union\n   select constraint_name from fk_pg) as x)
dbcon_trigger_ops_operator_name_key	(select count(*) from dbcon_trigger_ops ) =\n(select count(distinct operator_name)\nfrom dbcon_trigger_ops)
dbcon_trigger_ops_operator_name_fkey	not exists\n(select operator_name from dbcon_trigger_ops\n  except\nselect operator_name from operators)
dbcon_triggers_trigger_name_key	(select count(*) from dbcon_triggers ) =\n(select count(distinct trigger_name)\nfrom dbcon_triggers)
dbcon_triggers_trigger_name_relvar_name_fkey	not exists\n(select trigger_name, relvar_name from dbcon_triggers\n  except\nselect trigger_name, relvar_name from triggers)
modules_module_name_key	(select count(*) from modules ) =\n(select count(distinct module_name)\nfrom modules)
all_module_objects_object_name_object_type_key	(select count(*) from all_module_objects ) =\n(select count(distinct object_name, object_type)\nfrom all_module_objects)
all_module_objects_module_name_fkey	not exists\n(select module_name from all_module_objects\n  except\nselect module_name from modules)
base_relvar_metadata_relvar_name_key	(select count(*) from base_relvar_metadata ) =\n(select count(distinct relvar_name)\nfrom base_relvar_metadata)
base_relvar_metadata_relvar_name_fkey	not exists\n(select relvar_name from base_relvar_metadata\n  except\nselect relvar_name from base_relvars)
piece_prototypes_mr_ptype_key	(select count(*) from piece_prototypes_mr ) =\n(select count(distinct ptype)\nfrom piece_prototypes_mr)
spells_mr_spell_name_key	(select count(*) from spells_mr ) =\n(select count(distinct spell_name)\nfrom spells_mr)
board_size_width_height_key	(select count(*) from board_size ) =\n(select count(distinct width, height)\nfrom board_size)
board_size_01_tuple	(select count(*) from board_size) <= 1
world_alignment_table_world_alignment_key	(select count(*) from world_alignment_table ) =\n(select count(distinct world_alignment)\nfrom world_alignment_table)
world_alignment_table_01_tuple	(select count(*) from world_alignment_table) <= 1
wizards_wizard_name_key	(select count(*) from wizards ) =\n(select count(distinct wizard_name)\nfrom wizards)
spell_books_id_key	(select count(*) from spell_books ) =\n(select count(distinct id)\nfrom spell_books)
spell_books_wizard_name_fkey	not exists\n(select wizard_name from spell_books\n  except\nselect wizard_name from wizards)
no_spells_for_stiffs	 not exists(select 1 from spell_books\n  natural inner join wizards where expired = true)
spell_books_spell_name_fkey	not exists\n(select spell_name from spell_books\n  except\nselect spell_name from spells)
pieces_ptype_allegiance_tag_key	(select count(*) from pieces ) =\n(select count(distinct ptype, allegiance, tag)\nfrom pieces)
pieces_ptype_fkey	not exists\n(select ptype from pieces\n  except\nselect ptype from piece_prototypes)
piece_coordinates_valid	 not exists(select 1 from pieces\n  cross join board_size\n  where x >= width or y >= height)
pieces_allegiance_fkey	not exists\n(select allegiance from pieces\n  except\nselect allegiance from allegiances)
dead_wizard_army_empty	 not exists(select 1 from pieces\n    inner join wizards\n    on (allegiance = wizard_name)\n    where expired = true)
imaginary_pieces_ptype_allegiance_tag_key	(select count(*) from imaginary_pieces ) =\n(select count(distinct ptype, allegiance, tag)\nfrom imaginary_pieces)
imaginary_pieces_ptype_allegiance_tag_fkey	not exists\n(select ptype, allegiance, tag from imaginary_pieces\n  except\nselect ptype, allegiance, tag from pieces)
imaginary_pieces_ptype_fkey	not exists\n(select ptype from imaginary_pieces\n  except\nselect ptype from monster_prototypes)
crimes_against_nature_ptype_allegiance_tag_key	(select count(*) from crimes_against_nature ) =\n(select count(distinct ptype, allegiance, tag)\nfrom crimes_against_nature)
crimes_against_nature_ptype_allegiance_tag_fkey	not exists\n(select ptype, allegiance, tag from crimes_against_nature\n  except\nselect ptype, allegiance, tag from pieces)
crimes_against_nature_ptype_fkey	not exists\n(select ptype from crimes_against_nature\n  except\nselect ptype from monster_prototypes)
in_next_phase_hack_table_in_next_phase_hack_key	(select count(*) from in_next_phase_hack_table ) =\n(select count(distinct in_next_phase_hack)\nfrom in_next_phase_hack_table)
in_next_phase_hack_table_01_tuple	(select count(*) from in_next_phase_hack_table) <= 1
creating_new_game_table_creating_new_game_key	(select count(*) from creating_new_game_table ) =\n(select count(distinct creating_new_game)\nfrom creating_new_game_table)
creating_new_game_table_01_tuple	(select count(*) from creating_new_game_table) <= 1
turn_number_table_turn_number_key	(select count(*) from turn_number_table ) =\n(select count(distinct turn_number)\nfrom turn_number_table)
turn_number_table_01_tuple	(select count(*) from turn_number_table) <= 1
current_wizard_table_current_wizard_key	(select count(*) from current_wizard_table ) =\n(select count(distinct current_wizard)\nfrom current_wizard_table)
current_wizard_table_01_tuple	(select count(*) from current_wizard_table) <= 1
current_wizard_table_current_wizard_fkey	not exists\n(select current_wizard from current_wizard_table\n  except\nselect wizard_name from wizards)
current_wizard_must_be_alive	(select not expired from current_wizard_table\n     inner join wizards on current_wizard = wizard_name)
turn_phase_table_turn_phase_key	(select count(*) from turn_phase_table ) =\n(select count(distinct turn_phase)\nfrom turn_phase_table)
turn_phase_table_01_tuple	(select count(*) from turn_phase_table) <= 1
wizard_spell_choices_mr_wizard_name_key	(select count(*) from wizard_spell_choices_mr ) =\n(select count(distinct wizard_name)\nfrom wizard_spell_choices_mr)
dead_wizard_no_spell	 not exists(select 1 from wizard_spell_choices_mr\n    natural inner join wizards\n    where expired = true)
spell_choice_hack_table_spell_choice_hack_key	(select count(*) from spell_choice_hack_table ) =\n(select count(distinct spell_choice_hack)\nfrom spell_choice_hack_table)
spell_choice_hack_table_01_tuple	(select count(*) from spell_choice_hack_table) <= 1
wizard_spell_choices_wizard_name_spell_name_fkey	((select spell_choice_hack from spell_choice_hack_table) or\nnot exists(select wizard_name, spell_name from wizard_spell_choices\n  except\nselect wizard_name, spell_name from spell_books))
chosen_spell_phase_valid	\n((select in_next_phase_hack from in_next_phase_hack_table) or\n(((select turn_phase='choose' from turn_phase_table) and\n (select max(place) from wizard_spell_choices\n   natural inner join live_wizards) <=\n (select place from live_wizards\n   inner join current_wizard_table\n     on wizard_name = current_wizard))\nor\n((select turn_phase='cast' from turn_phase_table) and\n (select min(place) from wizard_spell_choices\n    natural inner join live_wizards) >=\n  (select place from live_wizards\n    inner join current_wizard_table\n      on wizard_name = current_wizard))\nor not exists(select 1 from wizard_spell_choices)\n))
spell_parts_to_cast_table_spell_parts_to_cast_key	(select count(*) from spell_parts_to_cast_table ) =\n(select count(distinct spell_parts_to_cast)\nfrom spell_parts_to_cast_table)
spell_parts_to_cast_table_01_tuple	(select count(*) from spell_parts_to_cast_table) <= 1
parts_to_cast_only	\n  ((select turn_phase = 'cast' from turn_phase_table)\n  or not exists(select 1 from spell_parts_to_cast_table))\n
cast_success_checked_table_cast_success_checked_key	(select count(*) from cast_success_checked_table ) =\n(select count(distinct cast_success_checked)\nfrom cast_success_checked_table)
cast_success_checked_table_01_tuple	(select count(*) from cast_success_checked_table) <= 1
cast_checked_cast_only	\n  ((select turn_phase = 'cast' from turn_phase_table)\n  or not exists(select 1 from cast_success_checked_table))\n
cast_alignment_table_cast_alignment_key	(select count(*) from cast_alignment_table ) =\n(select count(distinct cast_alignment)\nfrom cast_alignment_table)
cast_alignment_table_01_tuple	(select count(*) from cast_alignment_table) <= 1
cast_alignment_empty	((get_turn_phase() = 'cast') or\n  not exists(select 1 from cast_alignment_table))
pieces_to_move_ptype_allegiance_tag_key	(select count(*) from pieces_to_move ) =\n(select count(distinct ptype, allegiance, tag)\nfrom pieces_to_move)
pieces_to_move_ptype_allegiance_tag_fkey	not exists\n(select ptype, allegiance, tag from pieces_to_move\n  except\nselect ptype, allegiance, tag from pieces)
pieces_to_move_allegiance_fkey	not exists\n(select allegiance from pieces_to_move\n  except\nselect current_wizard from current_wizard_table)
pieces_to_move_empty	((select turn_phase = 'move' from turn_phase_table) or\nnot exists (select 1 from pieces_to_move))
selected_piece_ptype_allegiance_tag_key	(select count(*) from selected_piece ) =\n(select count(distinct ptype, allegiance, tag)\nfrom selected_piece)
selected_piece_ptype_allegiance_tag_fkey	not exists\n(select ptype, allegiance, tag from selected_piece\n  except\nselect ptype, allegiance, tag from pieces)
selected_piece_allegiance_fkey	not exists\n(select allegiance from selected_piece\n  except\nselect current_wizard from current_wizard_table)
selected_piece_01_tuple	(select count(*) from selected_piece) <= 1
remaining_walk_table_remaining_walk_key	(select count(*) from remaining_walk_table ) =\n(select count(distinct remaining_walk)\nfrom remaining_walk_table)
remaining_walk_table_01_tuple	(select count(*) from remaining_walk_table) <= 1
remaining_walk_hack_table_remaining_walk_hack_key	(select count(*) from remaining_walk_hack_table ) =\n(select count(distinct remaining_walk_hack)\nfrom remaining_walk_hack_table)
remaining_walk_hack_table_01_tuple	(select count(*) from remaining_walk_hack_table) <= 1
remaining_walk_only_motion	 ((not exists(select 1 from remaining_walk_table)) or\n   exists(select 1 from creating_new_game_table\n      where creating_new_game = true) or\n   (select remaining_walk_hack\n     from remaining_walk_hack_table) or\n   (exists(select 1 from selected_piece)\n      and (select move_phase = 'motion' from selected_piece)\n      and exists (select 1 from creature_pieces\n                  natural inner join selected_piece)\n      and (select not flying from creature_pieces\n           natural inner join selected_piece))) 
game_completed_table_game_completed_key	(select count(*) from game_completed_table ) =\n(select count(distinct game_completed)\nfrom game_completed_table)
game_completed_table_01_tuple	(select count(*) from game_completed_table) <= 1
game_completed_wizards	(not exists(select 1 from game_completed_table)\n           or (select count(1) <= 1 from live_wizards))
test_action_overrides_override_key	(select count(*) from test_action_overrides ) =\n(select count(distinct override)\nfrom test_action_overrides)
disable_spreading_table_disable_spreading_key	(select count(*) from disable_spreading_table ) =\n(select count(distinct disable_spreading)\nfrom disable_spreading_table)
disable_spreading_table_01_tuple	(select count(*) from disable_spreading_table) <= 1
spell_indexes_no_dis_turm_row_number_key	(select count(*) from spell_indexes_no_dis_turm ) =\n(select count(distinct row_number)\nfrom spell_indexes_no_dis_turm)
action_history_mr_id_key	(select count(*) from action_history_mr ) =\n(select count(distinct id)\nfrom action_history_mr)
wizard_starting_positions_wizard_count_place_key	(select count(*) from wizard_starting_positions ) =\n(select count(distinct wizard_count, place)\nfrom wizard_starting_positions)
wizard_starting_positions_wizard_count_x_y_key	(select count(*) from wizard_starting_positions ) =\n(select count(distinct wizard_count, x, y)\nfrom wizard_starting_positions)
wizard_starting_positions_place_valid	not exists(select 1 from wizard_starting_positions\n    where place >= wizard_count)
action_new_game_argument_place_key	(select count(*) from action_new_game_argument ) =\n(select count(distinct place)\nfrom action_new_game_argument)
action_new_game_argument_wizard_name_key	(select count(*) from action_new_game_argument ) =\n(select count(distinct wizard_name)\nfrom action_new_game_argument)
action_new_game_argument_place_valid	(select count(*) from action_new_game_argument\n    where place >= (select count(*) from action_new_game_argument)) = 0
windows_window_name_key	(select count(*) from windows ) =\n(select count(distinct window_name)\nfrom windows)
colours_name_key	(select count(*) from colours ) =\n(select count(distinct name)\nfrom colours)
sprites_sprite_key	(select count(*) from sprites ) =\n(select count(distinct sprite)\nfrom sprites)
wizard_display_info_wizard_name_key	(select count(*) from wizard_display_info ) =\n(select count(distinct wizard_name)\nfrom wizard_display_info)
wizard_display_info_default_sprite_key	(select count(*) from wizard_display_info ) =\n(select count(distinct default_sprite)\nfrom wizard_display_info)
wizard_display_info_colour_key	(select count(*) from wizard_display_info ) =\n(select count(distinct colour)\nfrom wizard_display_info)
wizard_display_info_wizard_name_fkey	not exists\n(select wizard_name from wizard_display_info\n  except\nselect wizard_name from wizards)
wizard_display_info_default_sprite_fkey	not exists\n(select default_sprite from wizard_display_info\n  except\nselect sprite from sprites)
init_wizard_display_info_argument_wizard_name_key	(select count(*) from init_wizard_display_info_argument ) =\n(select count(distinct wizard_name)\nfrom init_wizard_display_info_argument)
init_wizard_display_info_argument_sprite_key	(select count(*) from init_wizard_display_info_argument ) =\n(select count(distinct sprite)\nfrom init_wizard_display_info_argument)
init_wizard_display_info_argument_colour_key	(select count(*) from init_wizard_display_info_argument ) =\n(select count(distinct colour)\nfrom init_wizard_display_info_argument)
init_wizard_display_info_argument_wizard_name_fkey	not exists\n(select wizard_name from init_wizard_display_info_argument\n  except\nselect wizard_name from wizards)
init_wizard_display_info_argument_sprite_fkey	not exists\n(select sprite from init_wizard_display_info_argument\n  except\nselect sprite from sprites)
cursor_position_coordinates_valid	 not exists (select 1 from cursor_position\n  cross join board_size\n  where x >= width or y >= height)
cursor_position_01_tuple	(select count(*) from cursor_position) <= 1
piece_starting_ticks_ptype_allegiance_tag_key	(select count(*) from piece_starting_ticks ) =\n(select count(distinct ptype, allegiance, tag)\nfrom piece_starting_ticks)
piece_starting_ticks_ptype_allegiance_tag_fkey	not exists\n(select ptype, allegiance, tag from piece_starting_ticks\n  except\nselect ptype, allegiance, tag from pieces)
board_square_effects_id_key	(select count(*) from board_square_effects ) =\n(select count(distinct id)\nfrom board_square_effects)
board_beam_effects_id_key	(select count(*) from board_beam_effects ) =\n(select count(distinct id)\nfrom board_beam_effects)
board_sound_effects_id_key	(select count(*) from board_sound_effects ) =\n(select count(distinct id)\nfrom board_sound_effects)
history_sounds_history_name_sound_name_key	(select count(*) from history_sounds ) =\n(select count(distinct history_name, sound_name)\nfrom history_sounds)
history_no_visuals_history_name_key	(select count(*) from history_no_visuals ) =\n(select count(distinct history_name)\nfrom history_no_visuals)
last_history_effect_id_table_last_history_effect__key	(select count(*) from last_history_effect_id_table ) =\n(select count(distinct last_history_effect_id)\nfrom last_history_effect_id_table)
last_history_effect_id_table_01_tuple	(select count(*) from last_history_effect_id_table) <= 1
current_effects_01_tuple	(select count(*) from current_effects) <= 1
spell_sprites_spell_name_key	(select count(*) from spell_sprites ) =\n(select count(distinct spell_name)\nfrom spell_sprites)
spell_sprites_sprite_fkey	not exists\n(select sprite from spell_sprites\n  except\nselect sprite from sprites)
spell_sprites_spell_name_fkey	not exists\n(select spell_name from spell_sprites\n  except\nselect spell_name from spells_mr)
spell_book_show_all_table_spell_book_show_all_key	(select count(*) from spell_book_show_all_table ) =\n(select count(distinct spell_book_show_all)\nfrom spell_book_show_all_table)
spell_book_show_all_table_01_tuple	(select count(*) from spell_book_show_all_table) <= 1
spell_keys_spell_name_key	(select count(*) from spell_keys ) =\n(select count(distinct spell_name)\nfrom spell_keys)
spell_keys_key_key	(select count(*) from spell_keys ) =\n(select count(distinct key)\nfrom spell_keys)
spell_keys_spell_name_fkey	not exists\n(select spell_name from spell_keys\n  except\nselect spell_name from spells_mr)
new_game_widget_state_line_key	(select count(*) from new_game_widget_state ) =\n(select count(distinct line)\nfrom new_game_widget_state)
new_game_widget_state_wizard_name_key	(select count(*) from new_game_widget_state ) =\n(select count(distinct wizard_name)\nfrom new_game_widget_state)
new_game_widget_state_sprite_key	(select count(*) from new_game_widget_state ) =\n(select count(distinct sprite)\nfrom new_game_widget_state)
new_game_widget_state_colour_key	(select count(*) from new_game_widget_state ) =\n(select count(distinct colour)\nfrom new_game_widget_state)
new_game_widget_state_sprite_fkey	not exists\n(select sprite from new_game_widget_state\n  except\nselect sprite from sprites)
new_game_widget_state_line_valid	 not exists(select 1 from new_game_widget_state\n  where line >= 8)
key_control_settings_key_code_action_name_key	(select count(*) from key_control_settings ) =\n(select count(distinct key_code, action_name)\nfrom key_control_settings)
action_client_new_game_argument_place_key	(select count(*) from action_client_new_game_argument ) =\n(select count(distinct place)\nfrom action_client_new_game_argument)
action_client_new_game_argument_wizard_name_key	(select count(*) from action_client_new_game_argument ) =\n(select count(distinct wizard_name)\nfrom action_client_new_game_argument)
action_client_new_game_argument_sprite_key	(select count(*) from action_client_new_game_argument ) =\n(select count(distinct sprite)\nfrom action_client_new_game_argument)
action_client_new_game_argument_colour_key	(select count(*) from action_client_new_game_argument ) =\n(select count(distinct colour)\nfrom action_client_new_game_argument)
action_client_new_game_argument_sprite_fkey	not exists\n(select sprite from action_client_new_game_argument\n  except\nselect sprite from sprites)
action_client_new_game_place_valid	(select count(*) from action_client_new_game_argument\n  where place >=\n  (select count(*) from action_client_new_game_argument)) = 0
\.


--
-- TOC entry 3344 (class 0 OID 258113)
-- Dependencies: 2470
-- Data for Name: dbcon_ops; Type: TABLE DATA; Schema: public; Owner: -
--

COPY dbcon_ops (constraint_name, operator_name) FROM stdin;
system_implementation_objects_object_name_object__key	check_con_system_implementation_objects_object_name_object__key
database_constraints_constraint_name_key	check_con_database_constraints_constraint_name_key
dbcon_ops_constraint_name_key	check_con_dbcon_ops_constraint_name_key
dbcon_ops_operator_name_key	check_con_dbcon_ops_operator_name_key
dbcon_ops_constraint_name_fkey	check_con_dbcon_ops_constraint_name_fkey
dbcon_relvars_constraint_name_relvar_name_key	check_con_dbcon_relvars_constraint_name_relvar_name_key
dbcon_relvars_constraint_name_fkey	check_con_dbcon_relvars_constraint_name_fkey
dbcon_relvars_relvar_name_fkey	check_con_dbcon_relvars_relvar_name_fkey
con_pg_constraint_name_key	check_con_con_pg_constraint_name_key
con_pg_constraint_name_fkey	check_con_con_pg_constraint_name_fkey
con_pg_constraint_name_fkey1	check_con_con_pg_constraint_name_fkey1
dbcon_trigger_ops_operator_name_key	check_con_dbcon_trigger_ops_operator_name_key
dbcon_trigger_ops_operator_name_fkey	check_con_dbcon_trigger_ops_operator_name_fkey
dbcon_triggers_trigger_name_key	check_con_dbcon_triggers_trigger_name_key
dbcon_triggers_trigger_name_relvar_name_fkey	check_con_dbcon_triggers_trigger_name_relvar_name_fkey
modules_module_name_key	check_con_modules_module_name_key
all_module_objects_object_name_object_type_key	check_con_all_module_objects_object_name_object_type_key
all_module_objects_module_name_fkey	check_con_all_module_objects_module_name_fkey
base_relvar_metadata_relvar_name_key	check_con_base_relvar_metadata_relvar_name_key
base_relvar_metadata_relvar_name_fkey	check_con_base_relvar_metadata_relvar_name_fkey
piece_prototypes_mr_ptype_key	check_con_piece_prototypes_mr_ptype_key
spells_mr_spell_name_key	check_con_spells_mr_spell_name_key
board_size_width_height_key	check_con_board_size_width_height_key
board_size_01_tuple	check_con_board_size_01_tuple
world_alignment_table_world_alignment_key	check_con_world_alignment_table_world_alignment_key
world_alignment_table_01_tuple	check_con_world_alignment_table_01_tuple
wizards_wizard_name_key	check_con_wizards_wizard_name_key
spell_books_id_key	check_con_spell_books_id_key
spell_books_wizard_name_fkey	check_con_spell_books_wizard_name_fkey
no_spells_for_stiffs	check_con_no_spells_for_stiffs
spell_books_spell_name_fkey	check_con_spell_books_spell_name_fkey
pieces_ptype_allegiance_tag_key	check_con_pieces_ptype_allegiance_tag_key
pieces_ptype_fkey	check_con_pieces_ptype_fkey
piece_coordinates_valid	check_con_piece_coordinates_valid
pieces_allegiance_fkey	check_con_pieces_allegiance_fkey
dead_wizard_army_empty	check_con_dead_wizard_army_empty
imaginary_pieces_ptype_allegiance_tag_key	check_con_imaginary_pieces_ptype_allegiance_tag_key
imaginary_pieces_ptype_allegiance_tag_fkey	check_con_imaginary_pieces_ptype_allegiance_tag_fkey
imaginary_pieces_ptype_fkey	check_con_imaginary_pieces_ptype_fkey
crimes_against_nature_ptype_allegiance_tag_key	check_con_crimes_against_nature_ptype_allegiance_tag_key
crimes_against_nature_ptype_allegiance_tag_fkey	check_con_crimes_against_nature_ptype_allegiance_tag_fkey
crimes_against_nature_ptype_fkey	check_con_crimes_against_nature_ptype_fkey
in_next_phase_hack_table_in_next_phase_hack_key	check_con_in_next_phase_hack_table_in_next_phase_hack_key
in_next_phase_hack_table_01_tuple	check_con_in_next_phase_hack_table_01_tuple
creating_new_game_table_creating_new_game_key	check_con_creating_new_game_table_creating_new_game_key
creating_new_game_table_01_tuple	check_con_creating_new_game_table_01_tuple
turn_number_table_turn_number_key	check_con_turn_number_table_turn_number_key
turn_number_table_01_tuple	check_con_turn_number_table_01_tuple
current_wizard_table_current_wizard_key	check_con_current_wizard_table_current_wizard_key
current_wizard_table_01_tuple	check_con_current_wizard_table_01_tuple
current_wizard_table_current_wizard_fkey	check_con_current_wizard_table_current_wizard_fkey
current_wizard_must_be_alive	check_con_current_wizard_must_be_alive
turn_phase_table_turn_phase_key	check_con_turn_phase_table_turn_phase_key
turn_phase_table_01_tuple	check_con_turn_phase_table_01_tuple
wizard_spell_choices_mr_wizard_name_key	check_con_wizard_spell_choices_mr_wizard_name_key
dead_wizard_no_spell	check_con_dead_wizard_no_spell
spell_choice_hack_table_spell_choice_hack_key	check_con_spell_choice_hack_table_spell_choice_hack_key
spell_choice_hack_table_01_tuple	check_con_spell_choice_hack_table_01_tuple
wizard_spell_choices_wizard_name_spell_name_fkey	check_con_wizard_spell_choices_wizard_name_spell_name_fkey
chosen_spell_phase_valid	check_con_chosen_spell_phase_valid
spell_parts_to_cast_table_spell_parts_to_cast_key	check_con_spell_parts_to_cast_table_spell_parts_to_cast_key
spell_parts_to_cast_table_01_tuple	check_con_spell_parts_to_cast_table_01_tuple
parts_to_cast_only	check_con_parts_to_cast_only
cast_success_checked_table_cast_success_checked_key	check_con_cast_success_checked_table_cast_success_checked_key
cast_success_checked_table_01_tuple	check_con_cast_success_checked_table_01_tuple
cast_checked_cast_only	check_con_cast_checked_cast_only
cast_alignment_table_cast_alignment_key	check_con_cast_alignment_table_cast_alignment_key
cast_alignment_table_01_tuple	check_con_cast_alignment_table_01_tuple
cast_alignment_empty	check_con_cast_alignment_empty
pieces_to_move_ptype_allegiance_tag_key	check_con_pieces_to_move_ptype_allegiance_tag_key
pieces_to_move_ptype_allegiance_tag_fkey	check_con_pieces_to_move_ptype_allegiance_tag_fkey
pieces_to_move_allegiance_fkey	check_con_pieces_to_move_allegiance_fkey
pieces_to_move_empty	check_con_pieces_to_move_empty
selected_piece_ptype_allegiance_tag_key	check_con_selected_piece_ptype_allegiance_tag_key
selected_piece_ptype_allegiance_tag_fkey	check_con_selected_piece_ptype_allegiance_tag_fkey
selected_piece_allegiance_fkey	check_con_selected_piece_allegiance_fkey
selected_piece_01_tuple	check_con_selected_piece_01_tuple
remaining_walk_table_remaining_walk_key	check_con_remaining_walk_table_remaining_walk_key
remaining_walk_table_01_tuple	check_con_remaining_walk_table_01_tuple
remaining_walk_hack_table_remaining_walk_hack_key	check_con_remaining_walk_hack_table_remaining_walk_hack_key
remaining_walk_hack_table_01_tuple	check_con_remaining_walk_hack_table_01_tuple
remaining_walk_only_motion	check_con_remaining_walk_only_motion
game_completed_table_game_completed_key	check_con_game_completed_table_game_completed_key
game_completed_table_01_tuple	check_con_game_completed_table_01_tuple
game_completed_wizards	check_con_game_completed_wizards
test_action_overrides_override_key	check_con_test_action_overrides_override_key
disable_spreading_table_disable_spreading_key	check_con_disable_spreading_table_disable_spreading_key
disable_spreading_table_01_tuple	check_con_disable_spreading_table_01_tuple
spell_indexes_no_dis_turm_row_number_key	check_con_spell_indexes_no_dis_turm_row_number_key
action_history_mr_id_key	check_con_action_history_mr_id_key
wizard_starting_positions_wizard_count_place_key	check_con_wizard_starting_positions_wizard_count_place_key
wizard_starting_positions_wizard_count_x_y_key	check_con_wizard_starting_positions_wizard_count_x_y_key
wizard_starting_positions_place_valid	check_con_wizard_starting_positions_place_valid
action_new_game_argument_place_key	check_con_action_new_game_argument_place_key
action_new_game_argument_wizard_name_key	check_con_action_new_game_argument_wizard_name_key
action_new_game_argument_place_valid	check_con_action_new_game_argument_place_valid
windows_window_name_key	check_con_windows_window_name_key
colours_name_key	check_con_colours_name_key
sprites_sprite_key	check_con_sprites_sprite_key
wizard_display_info_wizard_name_key	check_con_wizard_display_info_wizard_name_key
wizard_display_info_default_sprite_key	check_con_wizard_display_info_default_sprite_key
wizard_display_info_colour_key	check_con_wizard_display_info_colour_key
wizard_display_info_wizard_name_fkey	check_con_wizard_display_info_wizard_name_fkey
wizard_display_info_default_sprite_fkey	check_con_wizard_display_info_default_sprite_fkey
init_wizard_display_info_argument_wizard_name_key	check_con_init_wizard_display_info_argument_wizard_name_key
init_wizard_display_info_argument_sprite_key	check_con_init_wizard_display_info_argument_sprite_key
init_wizard_display_info_argument_colour_key	check_con_init_wizard_display_info_argument_colour_key
init_wizard_display_info_argument_wizard_name_fkey	check_con_init_wizard_display_info_argument_wizard_name_fkey
init_wizard_display_info_argument_sprite_fkey	check_con_init_wizard_display_info_argument_sprite_fkey
cursor_position_coordinates_valid	check_con_cursor_position_coordinates_valid
cursor_position_01_tuple	check_con_cursor_position_01_tuple
piece_starting_ticks_ptype_allegiance_tag_key	check_con_piece_starting_ticks_ptype_allegiance_tag_key
piece_starting_ticks_ptype_allegiance_tag_fkey	check_con_piece_starting_ticks_ptype_allegiance_tag_fkey
board_square_effects_id_key	check_con_board_square_effects_id_key
board_beam_effects_id_key	check_con_board_beam_effects_id_key
board_sound_effects_id_key	check_con_board_sound_effects_id_key
history_sounds_history_name_sound_name_key	check_con_history_sounds_history_name_sound_name_key
history_no_visuals_history_name_key	check_con_history_no_visuals_history_name_key
last_history_effect_id_table_last_history_effect__key	check_con_last_history_effect_id_table_last_history_effect__key
last_history_effect_id_table_01_tuple	check_con_last_history_effect_id_table_01_tuple
current_effects_01_tuple	check_con_current_effects_01_tuple
spell_sprites_spell_name_key	check_con_spell_sprites_spell_name_key
spell_sprites_sprite_fkey	check_con_spell_sprites_sprite_fkey
spell_sprites_spell_name_fkey	check_con_spell_sprites_spell_name_fkey
spell_book_show_all_table_spell_book_show_all_key	check_con_spell_book_show_all_table_spell_book_show_all_key
spell_book_show_all_table_01_tuple	check_con_spell_book_show_all_table_01_tuple
spell_keys_spell_name_key	check_con_spell_keys_spell_name_key
spell_keys_key_key	check_con_spell_keys_key_key
spell_keys_spell_name_fkey	check_con_spell_keys_spell_name_fkey
new_game_widget_state_line_key	check_con_new_game_widget_state_line_key
new_game_widget_state_wizard_name_key	check_con_new_game_widget_state_wizard_name_key
new_game_widget_state_sprite_key	check_con_new_game_widget_state_sprite_key
new_game_widget_state_colour_key	check_con_new_game_widget_state_colour_key
new_game_widget_state_sprite_fkey	check_con_new_game_widget_state_sprite_fkey
new_game_widget_state_line_valid	check_con_new_game_widget_state_line_valid
key_control_settings_key_code_action_name_key	check_con_key_control_settings_key_code_action_name_key
action_client_new_game_argument_place_key	check_con_action_client_new_game_argument_place_key
action_client_new_game_argument_wizard_name_key	check_con_action_client_new_game_argument_wizard_name_key
action_client_new_game_argument_sprite_key	check_con_action_client_new_game_argument_sprite_key
action_client_new_game_argument_colour_key	check_con_action_client_new_game_argument_colour_key
action_client_new_game_argument_sprite_fkey	check_con_action_client_new_game_argument_sprite_fkey
action_client_new_game_place_valid	check_con_action_client_new_game_place_valid
\.


--
-- TOC entry 3345 (class 0 OID 258119)
-- Dependencies: 2471
-- Data for Name: dbcon_relvars; Type: TABLE DATA; Schema: public; Owner: -
--

COPY dbcon_relvars (constraint_name, relvar_name) FROM stdin;
dbcon_relvars_relvar_name_fkey	dbcon_relvars
con_pg_constraint_name_fkey1	con_pg
dbcon_trigger_ops_operator_name_fkey	dbcon_trigger_ops
dbcon_triggers_trigger_name_relvar_name_fkey	dbcon_triggers
base_relvar_metadata_relvar_name_fkey	base_relvar_metadata
board_size_01_tuple	board_size
world_alignment_table_01_tuple	world_alignment_table
no_spells_for_stiffs	spell_books
no_spells_for_stiffs	wizards
spell_books_spell_name_fkey	spell_books
pieces_ptype_fkey	pieces
piece_coordinates_valid	pieces
piece_coordinates_valid	board_size
pieces_allegiance_fkey	pieces
dead_wizard_army_empty	wizards
dead_wizard_army_empty	pieces
imaginary_pieces_ptype_fkey	imaginary_pieces
crimes_against_nature_ptype_fkey	crimes_against_nature
in_next_phase_hack_table_01_tuple	in_next_phase_hack_table
creating_new_game_table_01_tuple	creating_new_game_table
turn_number_table_01_tuple	turn_number_table
current_wizard_table_01_tuple	current_wizard_table
current_wizard_must_be_alive	wizards
current_wizard_must_be_alive	current_wizard_table
turn_phase_table_01_tuple	turn_phase_table
dead_wizard_no_spell	wizards
dead_wizard_no_spell	pieces
spell_choice_hack_table_01_tuple	spell_choice_hack_table
wizard_spell_choices_wizard_name_spell_name_fkey	spell_choice_hack_table
wizard_spell_choices_wizard_name_spell_name_fkey	wizard_spell_choices_mr
wizard_spell_choices_wizard_name_spell_name_fkey	spell_books
chosen_spell_phase_valid	turn_phase_table
chosen_spell_phase_valid	current_wizard_table
chosen_spell_phase_valid	wizard_spell_choices_mr
chosen_spell_phase_valid	wizards
chosen_spell_phase_valid	in_next_phase_hack_table
spell_parts_to_cast_table_01_tuple	spell_parts_to_cast_table
parts_to_cast_only	turn_phase_table
parts_to_cast_only	spell_parts_to_cast_table
cast_success_checked_table_01_tuple	cast_success_checked_table
cast_checked_cast_only	cast_success_checked_table
cast_checked_cast_only	turn_phase_table
cast_alignment_table_01_tuple	cast_alignment_table
cast_alignment_empty	turn_phase_table
cast_alignment_empty	cast_alignment_table
pieces_to_move_empty	pieces_to_move
pieces_to_move_empty	turn_phase_table
selected_piece_01_tuple	selected_piece
remaining_walk_table_01_tuple	remaining_walk_table
remaining_walk_hack_table_01_tuple	remaining_walk_hack_table
remaining_walk_only_motion	selected_piece
remaining_walk_only_motion	pieces
remaining_walk_only_motion	remaining_walk_table
remaining_walk_only_motion	remaining_walk_hack_table
remaining_walk_only_motion	creating_new_game_table
game_completed_table_01_tuple	game_completed_table
game_completed_wizards	game_completed_table
disable_spreading_table_01_tuple	disable_spreading_table
wizard_starting_positions_place_valid	wizard_starting_positions
action_new_game_argument_place_valid	action_new_game_argument
cursor_position_coordinates_valid	cursor_position
cursor_position_coordinates_valid	board_size
cursor_position_01_tuple	cursor_position
last_history_effect_id_table_01_tuple	last_history_effect_id_table
current_effects_01_tuple	current_effects
spell_book_show_all_table_01_tuple	spell_book_show_all_table
new_game_widget_state_line_valid	new_game_widget_state
action_client_new_game_place_valid	action_client_new_game_argument
\.


--
-- TOC entry 3347 (class 0 OID 258151)
-- Dependencies: 2476
-- Data for Name: dbcon_trigger_ops; Type: TABLE DATA; Schema: public; Owner: -
--

COPY dbcon_trigger_ops (operator_name) FROM stdin;
spell_parts_to_cast_table_constraint_trigger_operator
last_history_effect_id_table_constraint_trigger_operator
in_next_phase_hack_table_constraint_trigger_operator
selected_piece_constraint_trigger_operator
pieces_constraint_trigger_operator
world_alignment_table_constraint_trigger_operator
remaining_walk_table_constraint_trigger_operator
crimes_against_nature_constraint_trigger_operator
disable_spreading_table_constraint_trigger_operator
current_effects_constraint_trigger_operator
new_game_widget_state_constraint_trigger_operator
dbcon_relvars_constraint_trigger_operator
con_pg_constraint_trigger_operator
dbcon_triggers_constraint_trigger_operator
creating_new_game_table_constraint_trigger_operator
cursor_position_constraint_trigger_operator
dbcon_trigger_ops_constraint_trigger_operator
spell_choice_hack_table_constraint_trigger_operator
turn_phase_table_constraint_trigger_operator
game_completed_table_constraint_trigger_operator
current_wizard_table_constraint_trigger_operator
wizards_constraint_trigger_operator
spell_book_show_all_table_constraint_trigger_operator
board_size_constraint_trigger_operator
cast_alignment_table_constraint_trigger_operator
action_new_game_argument_constraint_trigger_operator
remaining_walk_hack_table_constraint_trigger_operator
spell_books_constraint_trigger_operator
action_client_new_game_argument_constraint_trigger_operator
cast_success_checked_table_constraint_trigger_operator
pieces_to_move_constraint_trigger_operator
turn_number_table_constraint_trigger_operator
wizard_spell_choices_mr_constraint_trigger_operator
wizard_starting_positions_constraint_trigger_operator
imaginary_pieces_constraint_trigger_operator
base_relvar_metadata_constraint_trigger_operator
\.


--
-- TOC entry 3348 (class 0 OID 258157)
-- Dependencies: 2477
-- Data for Name: dbcon_triggers; Type: TABLE DATA; Schema: public; Owner: -
--

COPY dbcon_triggers (trigger_name, relvar_name) FROM stdin;
spell_parts_to_cast_table_constraint_trigger	spell_parts_to_cast_table
last_history_effect_id_table_constraint_trigger	last_history_effect_id_table
in_next_phase_hack_table_constraint_trigger	in_next_phase_hack_table
selected_piece_constraint_trigger	selected_piece
pieces_constraint_trigger	pieces
world_alignment_table_constraint_trigger	world_alignment_table
remaining_walk_table_constraint_trigger	remaining_walk_table
crimes_against_nature_constraint_trigger	crimes_against_nature
disable_spreading_table_constraint_trigger	disable_spreading_table
current_effects_constraint_trigger	current_effects
new_game_widget_state_constraint_trigger	new_game_widget_state
dbcon_relvars_constraint_trigger	dbcon_relvars
con_pg_constraint_trigger	con_pg
dbcon_triggers_constraint_trigger	dbcon_triggers
creating_new_game_table_constraint_trigger	creating_new_game_table
cursor_position_constraint_trigger	cursor_position
dbcon_trigger_ops_constraint_trigger	dbcon_trigger_ops
spell_choice_hack_table_constraint_trigger	spell_choice_hack_table
turn_phase_table_constraint_trigger	turn_phase_table
game_completed_table_constraint_trigger	game_completed_table
current_wizard_table_constraint_trigger	current_wizard_table
wizards_constraint_trigger	wizards
spell_book_show_all_table_constraint_trigger	spell_book_show_all_table
board_size_constraint_trigger	board_size
cast_alignment_table_constraint_trigger	cast_alignment_table
action_new_game_argument_constraint_trigger	action_new_game_argument
remaining_walk_hack_table_constraint_trigger	remaining_walk_hack_table
spell_books_constraint_trigger	spell_books
action_client_new_game_argument_constraint_trigger	action_client_new_game_argument
cast_success_checked_table_constraint_trigger	cast_success_checked_table
pieces_to_move_constraint_trigger	pieces_to_move
turn_number_table_constraint_trigger	turn_number_table
wizard_spell_choices_mr_constraint_trigger	wizard_spell_choices_mr
wizard_starting_positions_constraint_trigger	wizard_starting_positions
imaginary_pieces_constraint_trigger	imaginary_pieces
base_relvar_metadata_constraint_trigger	base_relvar_metadata
\.


--
-- TOC entry 3378 (class 0 OID 260181)
-- Dependencies: 2584
-- Data for Name: disable_spreading_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY disable_spreading_table (disable_spreading) FROM stdin;
f
\.


--
-- TOC entry 3375 (class 0 OID 259778)
-- Dependencies: 2543
-- Data for Name: game_completed_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY game_completed_table (game_completed) FROM stdin;
t
\.


--
-- TOC entry 3395 (class 0 OID 260858)
-- Dependencies: 2623
-- Data for Name: history_no_visuals; Type: TABLE DATA; Schema: public; Owner: -
--

COPY history_no_visuals (history_name) FROM stdin;
wizard_up
new_turn
new_game
game_won
game_drawn
choose_spell
set_imaginary
set_real
\.


--
-- TOC entry 3394 (class 0 OID 260849)
-- Dependencies: 2622
-- Data for Name: history_sounds; Type: TABLE DATA; Schema: public; Owner: -
--

COPY history_sounds (history_name, sound_name) FROM stdin;
walked	walk
fly	fly
attack	attack
ranged_attack	shoot
game_drawn	draw
game_won	win
spell_failed	fail
spell_succeeded	success
shrugged_off	shrugged_off
wizard_up	wizard_up
new_game	new_game
chinned	kill
attempt_target_spell	cast
\.


--
-- TOC entry 3359 (class 0 OID 258615)
-- Dependencies: 2512
-- Data for Name: imaginary_pieces; Type: TABLE DATA; Schema: public; Owner: -
--

COPY imaginary_pieces (ptype, allegiance, tag) FROM stdin;
\.


--
-- TOC entry 3361 (class 0 OID 258731)
-- Dependencies: 2523
-- Data for Name: in_next_phase_hack_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY in_next_phase_hack_table (in_next_phase_hack) FROM stdin;
f
\.


--
-- TOC entry 3387 (class 0 OID 260597)
-- Dependencies: 2605
-- Data for Name: init_wizard_display_info_argument; Type: TABLE DATA; Schema: public; Owner: -
--

COPY init_wizard_display_info_argument (wizard_name, sprite, colour) FROM stdin;
Buddha	wizard0	blue
Kong Fuzi	wizard1	purple
Laozi	wizard2	cyan
Moshe	wizard3	yellow
Muhammad	wizard4	green
Shiva	wizard5	red
Yeshua	wizard6	white
Zarathushthra	wizard7	orange
\.


--
-- TOC entry 3402 (class 0 OID 261302)
-- Dependencies: 2645
-- Data for Name: key_control_settings; Type: TABLE DATA; Schema: public; Owner: -
--

COPY key_control_settings (key_code, action_name) FROM stdin;
Up	move_cursor_up
KP_Up	move_cursor_up
Left	move_cursor_left
KP_Left	move_cursor_left
Right	move_cursor_right
KP_Right	move_cursor_right
Down	move_cursor_down
KP_Down	move_cursor_down
KP_Home	move_cursor_up_left
KP_Page_Up	move_cursor_up_right
KP_Page_Down	move_cursor_down_right
KP_End	move_cursor_down_left
End	cancel
F11	print_widget_info
F12	refresh_widgets
0	choose_no_spell
Insert	spell_book_show_all_update_on
Delete	spell_book_show_all_update_off
space	client_next_phase
KP_Begin	go
Return	go
KP_5	go
y	set_imaginary
Y	set_imaginary
n	set_real
N	set_real
1	choose_magic_knife_spell
2	choose_magic_shield_spell
3	choose_magic_armour_spell
4	choose_magic_bow_spell
5	choose_magic_sword_spell
6	choose_shadow_form_spell
7	choose_magic_wings_spell
A	choose_decree_spell
B	choose_justice_spell
C	choose_lightning_spell
D	choose_magic_bolt_spell
E	choose_vengeance_spell
F	choose_dark_power_spell
G	choose_magic_wood_spell
H	choose_magic_castle_spell
I	choose_wall_spell
J	choose_gooey_blob_spell
K	choose_magic_fire_spell
L	choose_dark_citadel_spell
M	choose_shadow_wood_spell
O	choose_law_spell
P	choose_large_law_spell
Q	choose_disbelieve_spell
R	choose_subversion_spell
S	choose_turmoil_spell
T	choose_chaos_spell
U	choose_large_chaos_spell
V	choose_raise_dead_spell
a	choose_horse_spell
b	choose_king_cobra_spell
c	choose_eagle_spell
d	choose_elf_spell
e	choose_unicorn_spell
f	choose_gryphon_spell
g	choose_lion_spell
h	choose_pegasus_spell
i	choose_giant_spell
j	choose_golden_dragon_spell
k	choose_giant_rat_spell
l	choose_gorilla_spell
m	choose_goblin_spell
o	choose_orc_spell
p	choose_zombie_spell
q	choose_faun_spell
r	choose_ogre_spell
s	choose_skeleton_spell
t	choose_harpy_spell
u	choose_spectre_spell
v	choose_ghost_spell
w	choose_hydra_spell
x	choose_manticore_spell
z	choose_wraith_spell
W	choose_vampire_spell
X	choose_green_dragon_spell
Z	choose_red_dragon_spell
\.


--
-- TOC entry 3396 (class 0 OID 260867)
-- Dependencies: 2624
-- Data for Name: last_history_effect_id_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY last_history_effect_id_table (last_history_effect_id) FROM stdin;
9
\.


--
-- TOC entry 3349 (class 0 OID 258241)
-- Dependencies: 2480
-- Data for Name: modules; Type: TABLE DATA; Schema: public; Owner: -
--

COPY modules (module_name, module_parent_name, module_order) FROM stdin;
root	root	1
system	root	2
catalog	system	3
utils	system	4
chaos	root	5
server	chaos	6
metadata	server	7
piece_prototypes	server	8
spells	server	9
global_data	server	10
wizards	server	11
pieces	server	12
turn_sequence	server	13
actions	server	14
squares_valid	actions	15
action_history	server	16
new_game	server	17
client	chaos	18
window_management	client	19
sprites	client	20
wizard_display_info	client	21
board_widget	client	22
spell_book_widget	client	23
new_game_widget	client	24
client_actions	client	25
key_controls	client	26
client_new_game	client	27
\.


--
-- TOC entry 3401 (class 0 OID 261195)
-- Dependencies: 2642
-- Data for Name: new_game_widget_state; Type: TABLE DATA; Schema: public; Owner: -
--

COPY new_game_widget_state (line, wizard_name, sprite, colour, state) FROM stdin;
0	Buddha	wizard0	blue	human
1	Kong Fuzi	wizard1	purple	human
2	Laozi	wizard2	cyan	human
3	Moshe	wizard3	yellow	human
4	Muhammad	wizard4	green	human
5	Shiva	wizard5	red	human
6	Yeshua	wizard6	white	human
7	Zarathushthra	wizard7	orange	human
\.


--
-- TOC entry 3352 (class 0 OID 258312)
-- Dependencies: 2486
-- Data for Name: piece_prototypes_mr; Type: TABLE DATA; Schema: public; Owner: -
--

COPY piece_prototypes_mr (ptype, flying, speed, agility, undead, ridable, ranged_weapon_type, range, ranged_attack_strength, attack_strength, physical_defense, magic_defense) FROM stdin;
bat	t	5	4	f	f	\N	\N	\N	1	1	9
bear	f	2	2	f	f	\N	\N	\N	6	7	6
centaur	f	4	5	f	t	projectile	4	2	1	3	5
crocodile	f	1	2	f	f	\N	\N	\N	5	6	2
dark_citadel	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N
dire_wolf	f	3	2	f	f	\N	\N	\N	3	2	7
eagle	t	6	2	f	f	\N	\N	\N	3	3	8
elf	f	1	7	f	f	projectile	6	2	1	2	5
faun	f	1	8	f	f	\N	\N	\N	3	2	7
ghost	t	2	6	t	f	\N	\N	\N	1	3	9
giant	f	2	5	f	f	\N	\N	\N	9	7	6
giant_rat	f	3	2	f	f	\N	\N	\N	1	1	8
goblin	f	1	4	f	f	\N	\N	\N	2	4	4
golden_dragon	t	3	5	f	f	fire	4	5	9	9	5
gooey_blob	\N	\N	\N	\N	\N	\N	\N	\N	\N	1	\N
gorilla	f	1	2	f	f	\N	\N	\N	6	5	4
green_dragon	t	3	4	f	f	fire	6	4	5	8	4
gryphon	t	5	6	f	t	\N	\N	\N	3	5	5
harpy	t	5	5	f	f	\N	\N	\N	4	2	8
horse	f	4	1	f	t	\N	\N	\N	1	3	8
hydra	f	1	6	f	f	\N	\N	\N	7	8	4
king_cobra	f	1	1	f	f	\N	\N	\N	4	1	6
lion	f	4	3	f	f	\N	\N	\N	6	4	8
magic_castle	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N
magic_fire	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N
magic_tree	\N	\N	\N	\N	\N	\N	\N	\N	\N	5	\N
manticore	t	5	8	f	t	projectile	3	1	3	6	6
ogre	f	1	6	f	f	\N	\N	\N	4	7	3
orc	f	1	4	f	f	\N	\N	\N	2	1	4
pegasus	t	5	7	f	t	\N	\N	\N	2	4	6
red_dragon	t	3	5	f	f	fire	5	3	7	9	4
shadow_tree	\N	\N	\N	\N	\N	\N	\N	\N	2	4	\N
skeleton	f	1	4	t	f	\N	\N	\N	3	2	3
spectre	f	1	4	t	f	\N	\N	\N	4	2	6
unicorn	f	4	7	f	t	\N	\N	\N	5	4	9
vampire	t	4	5	t	f	\N	\N	\N	6	8	6
wall	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N
wizard	f	1	3	\N	\N	\N	\N	\N	3	3	5
wraith	f	2	5	t	f	\N	\N	\N	5	5	4
zombie	f	1	3	t	f	\N	\N	\N	1	1	2
\.


--
-- TOC entry 3389 (class 0 OID 260779)
-- Dependencies: 2612
-- Data for Name: piece_starting_ticks; Type: TABLE DATA; Schema: public; Owner: -
--

COPY piece_starting_ticks (ptype, allegiance, tag, start_tick) FROM stdin;
\.


--
-- TOC entry 3358 (class 0 OID 258516)
-- Dependencies: 2509
-- Data for Name: pieces; Type: TABLE DATA; Schema: public; Owner: -
--

COPY pieces (ptype, allegiance, tag, x, y) FROM stdin;
\.


--
-- TOC entry 3371 (class 0 OID 259466)
-- Dependencies: 2539
-- Data for Name: pieces_to_move; Type: TABLE DATA; Schema: public; Owner: -
--

COPY pieces_to_move (ptype, allegiance, tag) FROM stdin;
\.


--
-- TOC entry 3374 (class 0 OID 259664)
-- Dependencies: 2542
-- Data for Name: remaining_walk_hack_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY remaining_walk_hack_table (remaining_walk_hack) FROM stdin;
f
\.


--
-- TOC entry 3373 (class 0 OID 259606)
-- Dependencies: 2541
-- Data for Name: remaining_walk_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY remaining_walk_table (remaining_walk) FROM stdin;
\.


--
-- TOC entry 3372 (class 0 OID 259536)
-- Dependencies: 2540
-- Data for Name: selected_piece; Type: TABLE DATA; Schema: public; Owner: -
--

COPY selected_piece (ptype, allegiance, tag, move_phase, engaged) FROM stdin;
\.


--
-- TOC entry 3399 (class 0 OID 261062)
-- Dependencies: 2633
-- Data for Name: spell_book_show_all_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY spell_book_show_all_table (spell_book_show_all) FROM stdin;
f
\.


--
-- TOC entry 3357 (class 0 OID 258458)
-- Dependencies: 2507
-- Data for Name: spell_books; Type: TABLE DATA; Schema: public; Owner: -
--

COPY spell_books (id, wizard_name, spell_name) FROM stdin;
\.


--
-- TOC entry 3367 (class 0 OID 259058)
-- Dependencies: 2535
-- Data for Name: spell_choice_hack_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY spell_choice_hack_table (spell_choice_hack) FROM stdin;
f
\.


--
-- TOC entry 3379 (class 0 OID 260248)
-- Dependencies: 2586
-- Data for Name: spell_indexes_no_dis_turm; Type: TABLE DATA; Schema: public; Owner: -
--

COPY spell_indexes_no_dis_turm (row_number, spell_name) FROM stdin;
1	dark_citadel
2	dark_power
3	decree
4	eagle
5	elf
6	chaos
7	faun
8	ghost
9	giant
10	giant_rat
11	goblin
12	golden_dragon
13	law
14	gooey_blob
15	gorilla
16	green_dragon
17	gryphon
18	harpy
19	horse
20	hydra
21	justice
22	king_cobra
23	large_chaos
24	large_law
25	lightning
26	lion
27	magic_armour
28	magic_bolt
29	magic_bow
30	magic_castle
31	magic_fire
32	magic_knife
33	magic_shield
34	magic_sword
35	magic_wings
36	magic_wood
37	manticore
38	ogre
39	orc
40	pegasus
41	raise_dead
42	red_dragon
43	shadow_form
44	shadow_wood
45	skeleton
46	spectre
47	subversion
48	unicorn
49	vampire
50	vengeance
51	wall
52	wraith
53	zombie
\.


--
-- TOC entry 3400 (class 0 OID 261160)
-- Dependencies: 2638
-- Data for Name: spell_keys; Type: TABLE DATA; Schema: public; Owner: -
--

COPY spell_keys (spell_name, key) FROM stdin;
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


--
-- TOC entry 3368 (class 0 OID 259186)
-- Dependencies: 2536
-- Data for Name: spell_parts_to_cast_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY spell_parts_to_cast_table (spell_parts_to_cast) FROM stdin;
\.


--
-- TOC entry 3398 (class 0 OID 261041)
-- Dependencies: 2632
-- Data for Name: spell_sprites; Type: TABLE DATA; Schema: public; Owner: -
--

COPY spell_sprites (spell_name, sprite) FROM stdin;
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


--
-- TOC entry 3353 (class 0 OID 258349)
-- Dependencies: 2493
-- Data for Name: spells_mr; Type: TABLE DATA; Schema: public; Owner: -
--

COPY spells_mr (spell_name, base_chance, alignment, spell_category, description, activate, target, range, num, ptype, valid_square_category) FROM stdin;
dark_citadel	50	-1	object	Gives wizard building to hide in.	\N	\N	8	1	dark_citadel	empty
dark_power	50	-2	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 3 attacks on enemy creatures	\N	t	20	3	\N	creature_on_top
decree	90	1	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 1 attack on an enemy creature.	\N	t	20	1	\N	creature_on_top
disbelieve	100	0	miscellaneous	Allows illusion creatures to be destroyed. This spell has 100% casting chance, and is always available.	\N	t	20	1	\N	monster_on_top
eagle	70	1	monster	monster	\N	\N	1	1	eagle	empty_or_corpse_only
elf	70	2	monster	monster	\N	\N	1	1	elf	empty_or_corpse_only
chaos	80	-2	miscellaneous	Makes the world more chaos.	t	\N	\N	\N	\N	\N
faun	80	-1	monster	monster	\N	\N	1	1	faun	empty_or_corpse_only
ghost	50	-1	monster	monster	\N	\N	1	1	ghost	empty_or_corpse_only
giant	40	1	monster	monster	\N	\N	1	1	giant	empty_or_corpse_only
giant_rat	100	0	monster	monster	\N	\N	1	1	giant_rat	empty_or_corpse_only
goblin	100	-1	monster	monster	\N	\N	1	1	goblin	empty_or_corpse_only
golden_dragon	10	2	monster	monster	\N	\N	1	1	golden_dragon	empty_or_corpse_only
law	80	2	miscellaneous	Makes the world more law.	t	\N	\N	\N	\N	\N
gooey_blob	100	-1	object	Attacks enemy units it covers and randomly spreads across the map. Any unit covered up by a gooey blob will be able to carry on once it is uncovered (except wizards who are killed by gooey blobs).	\N	\N	6	1	gooey_blob	empty_or_corpse_only
gorilla	70	0	monster	monster	\N	\N	1	1	gorilla	empty_or_corpse_only
green_dragon	10	-1	monster	monster	\N	\N	1	1	green_dragon	empty_or_corpse_only
gryphon	60	1	monster	monster	\N	\N	1	1	gryphon	empty_or_corpse_only
harpy	60	-1	monster	monster	\N	\N	1	1	harpy	empty_or_corpse_only
horse	90	1	monster	monster	\N	\N	1	1	horse	empty_or_corpse_only
hydra	50	-1	monster	monster	\N	\N	1	1	hydra	empty_or_corpse_only
justice	50	2	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 3 attacks.	\N	t	20	3	\N	creature_on_top
king_cobra	90	1	monster	monster	\N	\N	1	1	king_cobra	empty_or_corpse_only
large_chaos	60	-4	miscellaneous	Makes the world more chaos.	t	\N	\N	\N	\N	\N
large_law	60	4	miscellaneous	Makes the world more law.	t	\N	\N	\N	\N	\N
lightning	100	0	attacking	Attacks creature it is cast at (more powerful than magic bolt)	\N	t	4	1	\N	attackable
lion	60	1	monster	monster	\N	\N	1	1	lion	empty_or_corpse_only
magic_armour	50	1	wizard	Gives wizard increased protection from attack.	t	\N	\N	\N	\N	\N
magic_bolt	100	0	attacking	Attacks creature it is cast at.	\N	t	6	1	\N	attackable
magic_bow	50	1	wizard	Gives wizard ranged weapon including undead creatures.	t	\N	\N	\N	\N	\N
magic_castle	50	1	object	Gives wizard building to hide in.	\N	\N	8	1	magic_castle	empty
magic_fire	80	-1	object	Attacks and kills enemy units it covers and randomly spreads across the map.	\N	\N	6	1	magic_fire	empty
magic_knife	70	1	wizard	Gives wizard increase attack power including undead creatures.	t	\N	\N	\N	\N	\N
magic_shield	70	1	wizard	Gives wizard increased protection from attack.	t	\N	\N	\N	\N	\N
magic_sword	40	1	wizard	Gives wizard increase attack power including undead creatures.	t	\N	\N	\N	\N	\N
magic_wings	60	0	wizard	Gives wizard ability to fly.	t	\N	\N	\N	\N	\N
magic_wood	80	1	object	Summons up to eight magic trees near your wizard. If you put your wizard in a magic tree and leave him there, he gets a new spell after a few turns.	\N	\N	8	8	magic_tree	empty_and_not_adjacent_to_tree
manticore	50	-1	monster	monster	\N	\N	1	1	manticore	empty_or_corpse_only
ogre	70	-1	monster	monster	\N	\N	1	1	ogre	empty_or_corpse_only
orc	100	-1	monster	monster	\N	\N	1	1	orc	empty_or_corpse_only
pegasus	60	2	monster	monster	\N	\N	1	1	pegasus	empty_or_corpse_only
raise_dead	60	-1	miscellaneous	Allows reanimation of dead bodies left on screen. Any creatures raised from the dead become undead creatures, able to attack other undeads.	\N	t	4	1	\N	corpse_only
red_dragon	10	-2	monster	monster	\N	\N	1	1	red_dragon	empty_or_corpse_only
shadow_form	80	0	wizard	Gives wizard increased protection and allows movement of 3 spaces per turn. Disappears if wizard attacks anything.	t	\N	\N	\N	\N	\N
shadow_wood	50	-1	object	Allows you to place up to eight shadow trees near your wizard. No two trees can be adjacent, and line of sight is needed in placing. Shadow trees can attack anything in contact with them (except undead).	\N	\N	8	8	shadow_tree	empty_and_not_adjacent_to_tree
skeleton	70	-1	monster	monster	\N	\N	1	1	skeleton	empty_or_corpse_only
spectre	60	-1	monster	monster	\N	\N	1	1	spectre	empty_or_corpse_only
subversion	100	0	miscellaneous	Realigns enemy creature to your side.	\N	t	7	1	\N	monster_on_top
turmoil	100	-2	miscellaneous	Randomly moves all objects onscreen to a different location. Only available from a magic tree.	t	\N	\N	\N	\N	\N
unicorn	70	2	monster	monster	\N	\N	1	1	unicorn	empty_or_corpse_only
vampire	20	-2	monster	monster	\N	\N	1	1	vampire	empty_or_corpse_only
vengeance	90	-1	attacking	When cast on a wizard it kills all that wizards creations if successful. Allows 1 attack on an enemy creature.	\N	t	20	1	\N	creature_on_top
wall	80	0	object	Allows four wall blocks to be built near the wizard, which blocks creatures paths, but can be flown over.	\N	\N	8	4	wall	empty
wraith	50	-1	monster	monster	\N	\N	1	1	wraith	empty_or_corpse_only
zombie	90	-1	monster	monster	\N	\N	1	1	zombie	empty_or_corpse_only
\.


--
-- TOC entry 3385 (class 0 OID 260561)
-- Dependencies: 2603
-- Data for Name: sprites; Type: TABLE DATA; Schema: public; Owner: -
--

COPY sprites (sprite, animation_speed) FROM stdin;
bat	8
dead_bat	250
bear	23
dead_bear	250
centaur	23
dead_centaur	250
crocodile	34
dead_crocodile	250
dark_citadel	50
dire_wolf	12
dead_dire_wolf	250
eagle	14
dead_eagle	250
elf	26
dead_elf	250
faun	20
dead_faun	250
ghost	15
giant	23
dead_giant	250
giant_rat	13
dead_giant_rat	250
goblin	12
dead_goblin	250
golden_dragon	27
dead_golden_dragon	250
gooey_blob	40
gorilla	18
dead_gorilla	250
green_dragon	32
dead_green_dragon	250
gryphon	10
dead_gryphon	250
harpy	13
dead_harpy	250
horse	21
dead_horse	250
hydra	36
dead_hydra	250
king_cobra	30
dead_king_cobra	250
lion	38
dead_lion	250
magic_castle	50
magic_fire	12
magic_tree	250
manticore	13
dead_manticore	250
ogre	23
dead_ogre	250
orc	21
dead_orc	250
pegasus	16
dead_pegasus	250
red_dragon	34
dead_red_dragon	250
shadow_tree	30
skeleton	17
spectre	15
unicorn	16
dead_unicorn	250
vampire	40
wall	30
wizard0	250
wizard1	250
wizard2	250
wizard3	250
wizard4	250
wizard5	250
wizard6	250
wizard7	250
wizard_magic_armour	250
wizard_magic_bow	50
wizard_magic_knife	50
wizard_magic_shield	250
wizard_magic_sword	50
wizard_magic_wings	50
wizard0_shadow	20
wizard1_shadow	20
wizard2_shadow	20
wizard3_shadow	20
wizard4_shadow	20
wizard5_shadow	20
wizard6_shadow	20
wizard7_shadow	20
wizard_magic_armour_shadow	20
wizard_magic_bow_shadow	20
wizard_magic_knife_shadow	20
wizard_magic_shield_shadow	20
wizard_magic_sword_shadow	20
wizard_magic_wings_shadow	20
wraith	10
zombie	25
magic_bolt	250
lightning	250
law	250
large_law	250
chaos	250
large_chaos	250
vengeance	250
subversion	250
turmoil	250
disbelieve	250
justice	250
dark_power	250
decree	250
raise_dead	250
cursor	250
highlight_cast_target_spell	250
highlight_select_piece_at_position	250
highlight_walk	250
highlight_fly	250
highlight_attack	250
highlight_ranged_attack	250
effect_attack	250
\.


--
-- TOC entry 3342 (class 0 OID 258033)
-- Dependencies: 2455
-- Data for Name: system_implementation_objects; Type: TABLE DATA; Schema: public; Owner: -
--

COPY system_implementation_objects (object_name, object_type) FROM stdin;
system_implementation_objects	base_relvar
all_database_objects	view
attrs_are_key	operator
dbcon_ops	base_relvar
dbcon_relvars	base_relvar
check_pg	view
key_pg	view
fk_pg	view
set_pg_unique	operator
set_pg_check	operator
set_pg_fk	operator
con_pg	base_relvar
dbcon_trigger_ops	base_relvar
dbcon_triggers	base_relvar
non_accelerated_constraints	view
regenerate_constraint_triggers	operator
check_con_system_implementation_objects_object_name_object__key	operator
system_implementation_objects_object_name_object__key	database_constraint
check_con_database_constraints_constraint_name_key	operator
check_con_dbcon_ops_constraint_name_key	operator
dbcon_ops_constraint_name_key	database_constraint
check_con_dbcon_ops_operator_name_key	operator
dbcon_ops_operator_name_key	database_constraint
check_con_dbcon_ops_constraint_name_fkey	operator
dbcon_ops_constraint_name_fkey	database_constraint
check_con_dbcon_relvars_constraint_name_relvar_name_key	operator
dbcon_relvars_constraint_name_relvar_name_key	database_constraint
check_con_dbcon_relvars_constraint_name_fkey	operator
dbcon_relvars_constraint_name_fkey	database_constraint
check_con_dbcon_relvars_relvar_name_fkey	operator
dbcon_relvars_relvar_name_fkey	database_constraint
check_con_con_pg_constraint_name_key	operator
con_pg_constraint_name_key	database_constraint
check_con_con_pg_constraint_name_fkey	operator
con_pg_constraint_name_fkey	database_constraint
check_con_con_pg_constraint_name_fkey1	operator
con_pg_constraint_name_fkey1	database_constraint
check_con_dbcon_trigger_ops_operator_name_key	operator
dbcon_trigger_ops_operator_name_key	database_constraint
check_con_dbcon_trigger_ops_operator_name_fkey	operator
dbcon_trigger_ops_operator_name_fkey	database_constraint
check_con_dbcon_triggers_trigger_name_key	operator
dbcon_triggers_trigger_name_key	database_constraint
check_con_dbcon_triggers_trigger_name_relvar_name_fkey	operator
dbcon_triggers_trigger_name_relvar_name_fkey	database_constraint
create_x_transition_tuple_constraint	operator
check_con_modules_module_name_key	operator
all_module_objects	base_relvar
check_con_all_module_objects_object_name_object_type_key	operator
all_module_objects_object_name_object_type_key	database_constraint
check_con_all_module_objects_module_name_fkey	operator
all_module_objects_module_name_fkey	database_constraint
implementation_module_objects	view
check_con_base_relvar_metadata_relvar_name_key	operator
check_con_base_relvar_metadata_relvar_name_fkey	operator
check_con_piece_prototypes_mr_ptype_key	operator
check_con_spells_mr_spell_name_key	operator
check_con_board_size_width_height_key	operator
check_con_board_size_01_tuple	operator
check_con_world_alignment_table_world_alignment_key	operator
check_con_world_alignment_table_01_tuple	operator
check_con_wizards_wizard_name_key	operator
check_con_spell_books_id_key	operator
check_con_spell_books_wizard_name_fkey	operator
check_con_no_spells_for_stiffs	operator
check_con_spell_books_spell_name_fkey	operator
check_con_pieces_ptype_allegiance_tag_key	operator
check_con_pieces_ptype_fkey	operator
check_con_piece_coordinates_valid	operator
check_con_pieces_allegiance_fkey	operator
check_con_dead_wizard_army_empty	operator
check_con_imaginary_pieces_ptype_allegiance_tag_key	operator
check_con_imaginary_pieces_ptype_allegiance_tag_fkey	operator
check_con_imaginary_pieces_ptype_fkey	operator
check_con_crimes_against_nature_ptype_allegiance_tag_key	operator
check_con_crimes_against_nature_ptype_allegiance_tag_fkey	operator
check_con_crimes_against_nature_ptype_fkey	operator
check_con_in_next_phase_hack_table_in_next_phase_hack_key	operator
check_con_in_next_phase_hack_table_01_tuple	operator
check_con_creating_new_game_table_creating_new_game_key	operator
check_con_creating_new_game_table_01_tuple	operator
check_con_turn_number_table_turn_number_key	operator
check_con_turn_number_table_01_tuple	operator
check_turn_number_change_valid	operator
turn_number_change_valid_transition_trigger	trigger
check_turn_number_table_no_delete	operator
turn_number_table_no_delete_transition_trigger	trigger
check_turn_number_table_no_insert	operator
turn_number_table_no_insert_transition_trigger	trigger
check_con_current_wizard_table_current_wizard_key	operator
check_con_current_wizard_table_01_tuple	operator
check_con_current_wizard_table_current_wizard_fkey	operator
check_next_wizard_change_valid	operator
next_wizard_change_valid_transition_trigger	trigger
check_current_wizard_table_no_delete	operator
current_wizard_table_no_delete_transition_trigger	trigger
check_current_wizard_table_no_insert	operator
current_wizard_table_no_insert_transition_trigger	trigger
check_con_current_wizard_must_be_alive	operator
check_con_turn_phase_table_turn_phase_key	operator
check_con_turn_phase_table_01_tuple	operator
check_turn_phase_change_valid	operator
turn_phase_change_valid_transition_trigger	trigger
check_turn_phase_table_no_delete	operator
turn_phase_table_no_delete_transition_trigger	trigger
check_turn_phase_table_no_insert	operator
turn_phase_table_no_insert_transition_trigger	trigger
check_con_wizard_spell_choices_mr_wizard_name_key	operator
check_con_dead_wizard_no_spell	operator
check_con_spell_choice_hack_table_spell_choice_hack_key	operator
check_con_spell_choice_hack_table_01_tuple	operator
check_con_wizard_spell_choices_wizard_name_spell_name_fkey	operator
check_con_chosen_spell_phase_valid	operator
check_update_spell_choice_restricted	operator
update_spell_choice_restricted_transition_trigger	trigger
check_insert_spell_choice_restricted	operator
insert_spell_choice_restricted_transition_trigger	trigger
check_delete_spell_choice_restricted	operator
delete_spell_choice_restricted_transition_trigger	trigger
check_con_spell_parts_to_cast_table_spell_parts_to_cast_key	operator
check_con_spell_parts_to_cast_table_01_tuple	operator
check_con_parts_to_cast_only	operator
check_con_cast_success_checked_table_cast_success_checked_key	operator
check_con_cast_success_checked_table_01_tuple	operator
check_con_cast_checked_cast_only	operator
check_con_cast_alignment_table_cast_alignment_key	operator
check_con_cast_alignment_table_01_tuple	operator
check_con_cast_alignment_empty	operator
check_con_pieces_to_move_ptype_allegiance_tag_key	operator
check_con_pieces_to_move_ptype_allegiance_tag_fkey	operator
check_con_pieces_to_move_allegiance_fkey	operator
check_con_pieces_to_move_empty	operator
check_con_selected_piece_ptype_allegiance_tag_key	operator
check_con_selected_piece_ptype_allegiance_tag_fkey	operator
check_con_selected_piece_allegiance_fkey	operator
check_con_selected_piece_01_tuple	operator
check_con_remaining_walk_table_remaining_walk_key	operator
check_con_remaining_walk_table_01_tuple	operator
check_con_remaining_walk_hack_table_remaining_walk_hack_key	operator
check_con_remaining_walk_hack_table_01_tuple	operator
check_con_remaining_walk_only_motion	operator
check_con_game_completed_table_game_completed_key	operator
check_con_game_completed_table_01_tuple	operator
check_con_game_completed_wizards	operator
check_con_test_action_overrides_override_key	operator
check_con_disable_spreading_table_disable_spreading_key	operator
check_con_disable_spreading_table_01_tuple	operator
check_con_spell_indexes_no_dis_turm_row_number_key	operator
check_con_action_history_mr_id_key	operator
check_con_wizard_starting_positions_wizard_count_place_key	operator
check_con_wizard_starting_positions_wizard_count_x_y_key	operator
check_con_wizard_starting_positions_place_valid	operator
check_con_action_new_game_argument_place_key	operator
check_con_action_new_game_argument_wizard_name_key	operator
check_con_action_new_game_argument_place_valid	operator
spell_parts_to_cast_table_changed	operator
spell_parts_to_cast_table_changed	trigger
test_action_overrides_changed	operator
test_action_overrides_changed	trigger
imaginary_pieces_changed	operator
imaginary_pieces_changed	trigger
selected_piece_changed	operator
selected_piece_changed	trigger
board_size_changed	operator
board_size_changed	trigger
pieces_changed	operator
pieces_changed	trigger
world_alignment_table_changed	operator
world_alignment_table_changed	trigger
remaining_walk_table_changed	operator
remaining_walk_table_changed	trigger
crimes_against_nature_changed	operator
crimes_against_nature_changed	trigger
disable_spreading_table_changed	operator
disable_spreading_table_changed	trigger
spell_books_changed	operator
spell_books_changed	trigger
action_history_mr_changed	operator
action_history_mr_changed	trigger
cast_success_checked_table_changed	operator
cast_success_checked_table_changed	trigger
pieces_to_move_changed	operator
pieces_to_move_changed	trigger
turn_number_table_changed	operator
turn_number_table_changed	trigger
wizard_spell_choices_mr_changed	operator
wizard_spell_choices_mr_changed	trigger
turn_phase_table_changed	operator
turn_phase_table_changed	trigger
game_completed_table_changed	operator
game_completed_table_changed	trigger
current_wizard_table_changed	operator
current_wizard_table_changed	trigger
wizards_changed	operator
wizards_changed	trigger
check_con_windows_window_name_key	operator
check_con_colours_name_key	operator
check_con_sprites_sprite_key	operator
check_con_wizard_display_info_wizard_name_key	operator
check_con_wizard_display_info_default_sprite_key	operator
check_con_wizard_display_info_colour_key	operator
check_con_wizard_display_info_wizard_name_fkey	operator
check_con_wizard_display_info_default_sprite_fkey	operator
check_con_init_wizard_display_info_argument_wizard_name_key	operator
check_con_init_wizard_display_info_argument_sprite_key	operator
check_con_init_wizard_display_info_argument_colour_key	operator
check_con_init_wizard_display_info_argument_wizard_name_fkey	operator
check_con_init_wizard_display_info_argument_sprite_fkey	operator
check_con_cursor_position_coordinates_valid	operator
check_con_cursor_position_01_tuple	operator
check_con_piece_starting_ticks_ptype_allegiance_tag_key	operator
check_con_piece_starting_ticks_ptype_allegiance_tag_fkey	operator
check_con_board_square_effects_id_key	operator
check_con_board_beam_effects_id_key	operator
check_con_board_sound_effects_id_key	operator
check_con_history_sounds_history_name_sound_name_key	operator
check_con_history_no_visuals_history_name_key	operator
check_con_last_history_effect_id_table_last_history_effect__key	operator
check_con_last_history_effect_id_table_01_tuple	operator
check_con_current_effects_01_tuple	operator
check_con_spell_sprites_spell_name_key	operator
check_con_spell_sprites_sprite_fkey	operator
check_con_spell_sprites_spell_name_fkey	operator
check_con_spell_book_show_all_table_spell_book_show_all_key	operator
check_con_spell_book_show_all_table_01_tuple	operator
check_con_spell_keys_spell_name_key	operator
check_con_spell_keys_key_key	operator
check_con_spell_keys_spell_name_fkey	operator
check_con_new_game_widget_state_line_key	operator
check_con_new_game_widget_state_wizard_name_key	operator
check_con_new_game_widget_state_sprite_key	operator
check_con_new_game_widget_state_colour_key	operator
check_con_new_game_widget_state_sprite_fkey	operator
check_con_new_game_widget_state_line_valid	operator
check_con_key_control_settings_key_code_action_name_key	operator
check_con_action_client_new_game_argument_place_key	operator
check_con_action_client_new_game_argument_wizard_name_key	operator
check_con_action_client_new_game_argument_sprite_key	operator
check_con_action_client_new_game_argument_colour_key	operator
check_con_action_client_new_game_argument_sprite_fkey	operator
check_con_action_client_new_game_place_valid	operator
spell_parts_to_cast_table_constraint_trigger_operator	operator
spell_parts_to_cast_table_constraint_trigger	trigger
last_history_effect_id_table_constraint_trigger_operator	operator
last_history_effect_id_table_constraint_trigger	trigger
in_next_phase_hack_table_constraint_trigger_operator	operator
in_next_phase_hack_table_constraint_trigger	trigger
selected_piece_constraint_trigger_operator	operator
selected_piece_constraint_trigger	trigger
pieces_constraint_trigger_operator	operator
pieces_constraint_trigger	trigger
world_alignment_table_constraint_trigger_operator	operator
world_alignment_table_constraint_trigger	trigger
remaining_walk_table_constraint_trigger_operator	operator
remaining_walk_table_constraint_trigger	trigger
crimes_against_nature_constraint_trigger_operator	operator
crimes_against_nature_constraint_trigger	trigger
disable_spreading_table_constraint_trigger_operator	operator
disable_spreading_table_constraint_trigger	trigger
current_effects_constraint_trigger_operator	operator
current_effects_constraint_trigger	trigger
new_game_widget_state_constraint_trigger_operator	operator
new_game_widget_state_constraint_trigger	trigger
dbcon_relvars_constraint_trigger_operator	operator
dbcon_relvars_constraint_trigger	trigger
con_pg_constraint_trigger_operator	operator
con_pg_constraint_trigger	trigger
dbcon_triggers_constraint_trigger_operator	operator
dbcon_triggers_constraint_trigger	trigger
creating_new_game_table_constraint_trigger_operator	operator
creating_new_game_table_constraint_trigger	trigger
cursor_position_constraint_trigger_operator	operator
cursor_position_constraint_trigger	trigger
dbcon_trigger_ops_constraint_trigger_operator	operator
dbcon_trigger_ops_constraint_trigger	trigger
spell_choice_hack_table_constraint_trigger_operator	operator
spell_choice_hack_table_constraint_trigger	trigger
turn_phase_table_constraint_trigger_operator	operator
turn_phase_table_constraint_trigger	trigger
game_completed_table_constraint_trigger_operator	operator
game_completed_table_constraint_trigger	trigger
current_wizard_table_constraint_trigger_operator	operator
current_wizard_table_constraint_trigger	trigger
wizards_constraint_trigger_operator	operator
wizards_constraint_trigger	trigger
spell_book_show_all_table_constraint_trigger_operator	operator
spell_book_show_all_table_constraint_trigger	trigger
board_size_constraint_trigger_operator	operator
board_size_constraint_trigger	trigger
cast_alignment_table_constraint_trigger_operator	operator
cast_alignment_table_constraint_trigger	trigger
action_new_game_argument_constraint_trigger_operator	operator
action_new_game_argument_constraint_trigger	trigger
remaining_walk_hack_table_constraint_trigger_operator	operator
remaining_walk_hack_table_constraint_trigger	trigger
spell_books_constraint_trigger_operator	operator
spell_books_constraint_trigger	trigger
action_client_new_game_argument_constraint_trigger_operator	operator
action_client_new_game_argument_constraint_trigger	trigger
cast_success_checked_table_constraint_trigger_operator	operator
cast_success_checked_table_constraint_trigger	trigger
pieces_to_move_constraint_trigger_operator	operator
pieces_to_move_constraint_trigger	trigger
turn_number_table_constraint_trigger_operator	operator
turn_number_table_constraint_trigger	trigger
wizard_spell_choices_mr_constraint_trigger_operator	operator
wizard_spell_choices_mr_constraint_trigger	trigger
wizard_starting_positions_constraint_trigger_operator	operator
wizard_starting_positions_constraint_trigger	trigger
imaginary_pieces_constraint_trigger_operator	operator
imaginary_pieces_constraint_trigger	trigger
base_relvar_metadata_constraint_trigger_operator	operator
base_relvar_metadata_constraint_trigger	trigger
check_base_relvar_metadata_u_readonly	operator
base_relvar_metadata_u_readonly_transition_trigger	trigger
check_base_relvar_metadata_d_readonly	operator
base_relvar_metadata_d_readonly_transition_trigger	trigger
check_base_relvar_metadata_i_readonly	operator
base_relvar_metadata_i_readonly_transition_trigger	trigger
check_piece_prototypes_mr_u_readonly	operator
piece_prototypes_mr_u_readonly_transition_trigger	trigger
check_piece_prototypes_mr_d_readonly	operator
piece_prototypes_mr_d_readonly_transition_trigger	trigger
check_piece_prototypes_mr_i_readonly	operator
piece_prototypes_mr_i_readonly_transition_trigger	trigger
check_spells_mr_u_readonly	operator
spells_mr_u_readonly_transition_trigger	trigger
check_spells_mr_d_readonly	operator
spells_mr_d_readonly_transition_trigger	trigger
check_spells_mr_i_readonly	operator
spells_mr_i_readonly_transition_trigger	trigger
check_spell_indexes_no_dis_turm_u_readonly	operator
spell_indexes_no_dis_turm_u_readonly_transition_trigger	trigger
check_spell_indexes_no_dis_turm_d_readonly	operator
spell_indexes_no_dis_turm_d_readonly_transition_trigger	trigger
check_spell_indexes_no_dis_turm_i_readonly	operator
spell_indexes_no_dis_turm_i_readonly_transition_trigger	trigger
check_wizard_starting_positions_u_readonly	operator
wizard_starting_positions_u_readonly_transition_trigger	trigger
check_wizard_starting_positions_d_readonly	operator
wizard_starting_positions_d_readonly_transition_trigger	trigger
check_wizard_starting_positions_i_readonly	operator
wizard_starting_positions_i_readonly_transition_trigger	trigger
check_colours_u_readonly	operator
colours_u_readonly_transition_trigger	trigger
check_colours_d_readonly	operator
colours_d_readonly_transition_trigger	trigger
check_colours_i_readonly	operator
colours_i_readonly_transition_trigger	trigger
check_sprites_u_readonly	operator
sprites_u_readonly_transition_trigger	trigger
check_sprites_d_readonly	operator
sprites_d_readonly_transition_trigger	trigger
check_sprites_i_readonly	operator
sprites_i_readonly_transition_trigger	trigger
check_history_sounds_u_readonly	operator
history_sounds_u_readonly_transition_trigger	trigger
check_history_sounds_d_readonly	operator
history_sounds_d_readonly_transition_trigger	trigger
check_history_sounds_i_readonly	operator
history_sounds_i_readonly_transition_trigger	trigger
check_history_no_visuals_u_readonly	operator
history_no_visuals_u_readonly_transition_trigger	trigger
check_history_no_visuals_d_readonly	operator
history_no_visuals_d_readonly_transition_trigger	trigger
check_history_no_visuals_i_readonly	operator
history_no_visuals_i_readonly_transition_trigger	trigger
check_spell_sprites_u_readonly	operator
spell_sprites_u_readonly_transition_trigger	trigger
check_spell_sprites_d_readonly	operator
spell_sprites_d_readonly_transition_trigger	trigger
check_spell_sprites_i_readonly	operator
spell_sprites_i_readonly_transition_trigger	trigger
check_spell_keys_u_readonly	operator
spell_keys_u_readonly_transition_trigger	trigger
check_spell_keys_d_readonly	operator
spell_keys_d_readonly_transition_trigger	trigger
check_spell_keys_i_readonly	operator
spell_keys_i_readonly_transition_trigger	trigger
check_key_control_settings_u_readonly	operator
key_control_settings_u_readonly_transition_trigger	trigger
check_key_control_settings_d_readonly	operator
key_control_settings_d_readonly_transition_trigger	trigger
check_key_control_settings_i_readonly	operator
key_control_settings_i_readonly_transition_trigger	trigger
\.


--
-- TOC entry 3376 (class 0 OID 259898)
-- Dependencies: 2544
-- Data for Name: test_action_overrides; Type: TABLE DATA; Schema: public; Owner: -
--

COPY test_action_overrides (override, setting) FROM stdin;
disappear	f
\.


--
-- TOC entry 3363 (class 0 OID 258801)
-- Dependencies: 2525
-- Data for Name: turn_number_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY turn_number_table (turn_number) FROM stdin;
0
\.


--
-- TOC entry 3365 (class 0 OID 258946)
-- Dependencies: 2529
-- Data for Name: turn_phase_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY turn_phase_table (turn_phase) FROM stdin;
move
\.


--
-- TOC entry 3383 (class 0 OID 260540)
-- Dependencies: 2601
-- Data for Name: windows; Type: TABLE DATA; Schema: public; Owner: -
--

COPY windows (window_name, px, py, sx, sy, state) FROM stdin;
info	0	371	579	213	normal
spell_book	587	28	268	556	normal
new_game	514	27	500	500	hidden
board	99	28	480	320	normal
action_history	843	28	429	556	normal
\.


--
-- TOC entry 3386 (class 0 OID 260570)
-- Dependencies: 2604
-- Data for Name: wizard_display_info; Type: TABLE DATA; Schema: public; Owner: -
--

COPY wizard_display_info (wizard_name, default_sprite, colour) FROM stdin;
Buddha	wizard0	blue
Kong Fuzi	wizard1	purple
Laozi	wizard2	cyan
Moshe	wizard3	yellow
Muhammad	wizard4	green
Shiva	wizard5	red
Yeshua	wizard6	white
Zarathushthra	wizard7	orange
\.


--
-- TOC entry 3366 (class 0 OID 259001)
-- Dependencies: 2531
-- Data for Name: wizard_spell_choices_mr; Type: TABLE DATA; Schema: public; Owner: -
--

COPY wizard_spell_choices_mr (wizard_name, spell_name, imaginary) FROM stdin;
\.


--
-- TOC entry 3381 (class 0 OID 260312)
-- Dependencies: 2590
-- Data for Name: wizard_starting_positions; Type: TABLE DATA; Schema: public; Owner: -
--

COPY wizard_starting_positions (wizard_count, place, x, y) FROM stdin;
2	0	1	4
2	1	13	4
3	0	7	1
3	1	1	8
3	2	13	8
4	0	1	1
4	1	13	1
4	2	1	8
4	3	13	8
5	0	7	0
5	1	0	3
5	2	14	3
5	3	3	9
5	4	11	9
6	0	7	0
6	1	0	1
6	2	14	1
6	3	0	8
6	4	14	8
6	5	7	9
7	0	7	0
7	1	1	1
7	2	13	1
7	3	0	6
7	4	14	6
7	5	4	9
7	6	10	9
8	0	0	0
8	1	7	0
8	2	14	0
8	3	0	4
8	4	14	4
8	5	0	9
8	6	7	9
8	7	14	9
\.


--
-- TOC entry 3356 (class 0 OID 258435)
-- Dependencies: 2504
-- Data for Name: wizards; Type: TABLE DATA; Schema: public; Owner: -
--

COPY wizards (wizard_name, shadow_form, magic_sword, magic_knife, magic_shield, magic_wings, magic_armour, magic_bow, computer_controlled, original_place, expired) FROM stdin;
Laozi	f	f	f	f	f	f	f	f	2	t
Moshe	f	f	f	f	f	f	f	f	3	t
Muhammad	f	f	f	f	f	f	f	f	4	t
Shiva	f	f	f	f	f	f	f	f	5	t
Yeshua	f	f	f	f	f	f	f	f	6	t
Zarathushthra	f	f	f	f	f	f	f	f	7	t
Buddha	f	f	f	f	f	f	f	f	0	t
Kong Fuzi	f	f	f	f	f	f	f	f	1	t
\.


--
-- TOC entry 3355 (class 0 OID 258412)
-- Dependencies: 2503
-- Data for Name: world_alignment_table; Type: TABLE DATA; Schema: public; Owner: -
--

COPY world_alignment_table (world_alignment) FROM stdin;
0
\.


--
-- TOC entry 3209 (class 2606 OID 261351)
-- Dependencies: 2648 2648
-- Name: action_client_new_game_argument_colour_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY action_client_new_game_argument
    ADD CONSTRAINT action_client_new_game_argument_colour_key UNIQUE (colour);


--
-- TOC entry 3211 (class 2606 OID 261342)
-- Dependencies: 2648 2648
-- Name: action_client_new_game_argument_place_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY action_client_new_game_argument
    ADD CONSTRAINT action_client_new_game_argument_place_key UNIQUE (place);


--
-- TOC entry 3213 (class 2606 OID 261348)
-- Dependencies: 2648 2648
-- Name: action_client_new_game_argument_sprite_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY action_client_new_game_argument
    ADD CONSTRAINT action_client_new_game_argument_sprite_key UNIQUE (sprite);


--
-- TOC entry 3215 (class 2606 OID 261345)
-- Dependencies: 2648 2648
-- Name: action_client_new_game_argument_wizard_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY action_client_new_game_argument
    ADD CONSTRAINT action_client_new_game_argument_wizard_name_key UNIQUE (wizard_name);


--
-- TOC entry 3149 (class 2606 OID 260287)
-- Dependencies: 2589 2589
-- Name: action_history_mr_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY action_history_mr
    ADD CONSTRAINT action_history_mr_id_key UNIQUE (id);


--
-- TOC entry 3155 (class 2606 OID 260388)
-- Dependencies: 2591 2591
-- Name: action_new_game_argument_place_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY action_new_game_argument
    ADD CONSTRAINT action_new_game_argument_place_key UNIQUE (place);


--
-- TOC entry 3157 (class 2606 OID 260391)
-- Dependencies: 2591 2591
-- Name: action_new_game_argument_wizard_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY action_new_game_argument
    ADD CONSTRAINT action_new_game_argument_wizard_name_key UNIQUE (wizard_name);


--
-- TOC entry 3089 (class 2606 OID 258259)
-- Dependencies: 2481 2481 2481
-- Name: all_module_objects_object_name_object_type_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY all_module_objects
    ADD CONSTRAINT all_module_objects_object_name_object_type_key UNIQUE (object_name, object_type);


--
-- TOC entry 3091 (class 2606 OID 258290)
-- Dependencies: 2484 2484
-- Name: base_relvar_metadata_relvar_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY base_relvar_metadata
    ADD CONSTRAINT base_relvar_metadata_relvar_name_key UNIQUE (relvar_name);


--
-- TOC entry 3181 (class 2606 OID 260835)
-- Dependencies: 2619 2619
-- Name: board_beam_effects_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY board_beam_effects
    ADD CONSTRAINT board_beam_effects_id_key UNIQUE (id);


--
-- TOC entry 3097 (class 2606 OID 258395)
-- Dependencies: 2502 2502 2502
-- Name: board_size_width_height_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY board_size
    ADD CONSTRAINT board_size_width_height_key UNIQUE (width, height);


--
-- TOC entry 3183 (class 2606 OID 260847)
-- Dependencies: 2621 2621
-- Name: board_sound_effects_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY board_sound_effects
    ADD CONSTRAINT board_sound_effects_id_key UNIQUE (id);


--
-- TOC entry 3179 (class 2606 OID 260823)
-- Dependencies: 2617 2617
-- Name: board_square_effects_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY board_square_effects
    ADD CONSTRAINT board_square_effects_id_key UNIQUE (id);


--
-- TOC entry 3129 (class 2606 OID 259373)
-- Dependencies: 2538 2538
-- Name: cast_alignment_table_cast_alignment_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY cast_alignment_table
    ADD CONSTRAINT cast_alignment_table_cast_alignment_key UNIQUE (cast_alignment);


--
-- TOC entry 3143 (class 2606 OID 260138)
-- Dependencies: 2577 2577 2577
-- Name: cast_magic_wood_squares_x_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY cast_magic_wood_squares
    ADD CONSTRAINT cast_magic_wood_squares_x_key UNIQUE (x, y);


--
-- TOC entry 3127 (class 2606 OID 259280)
-- Dependencies: 2537 2537
-- Name: cast_success_checked_table_cast_success_checked_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY cast_success_checked_table
    ADD CONSTRAINT cast_success_checked_table_cast_success_checked_key UNIQUE (cast_success_checked);


--
-- TOC entry 3161 (class 2606 OID 260560)
-- Dependencies: 2602 2602
-- Name: colours_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY colours
    ADD CONSTRAINT colours_name_key UNIQUE (name);


--
-- TOC entry 3081 (class 2606 OID 258201)
-- Dependencies: 2475 2475
-- Name: con_pg_constraint_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY con_pg
    ADD CONSTRAINT con_pg_constraint_name_key UNIQUE (constraint_name);


--
-- TOC entry 3113 (class 2606 OID 258770)
-- Dependencies: 2524 2524
-- Name: creating_new_game_table_creating_new_game_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY creating_new_game_table
    ADD CONSTRAINT creating_new_game_table_creating_new_game_key UNIQUE (creating_new_game);


--
-- TOC entry 3109 (class 2606 OID 258661)
-- Dependencies: 2513 2513 2513 2513
-- Name: crimes_against_nature_ptype_allegiance_tag_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY crimes_against_nature
    ADD CONSTRAINT crimes_against_nature_ptype_allegiance_tag_key UNIQUE (ptype, allegiance, tag);


--
-- TOC entry 3117 (class 2606 OID 258859)
-- Dependencies: 2527 2527
-- Name: current_wizard_table_current_wizard_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY current_wizard_table
    ADD CONSTRAINT current_wizard_table_current_wizard_key UNIQUE (current_wizard);


--
-- TOC entry 3073 (class 2606 OID 258174)
-- Dependencies: 2466 2466
-- Name: database_constraints_constraint_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY database_constraints
    ADD CONSTRAINT database_constraints_constraint_name_key UNIQUE (constraint_name);


--
-- TOC entry 3075 (class 2606 OID 258177)
-- Dependencies: 2470 2470
-- Name: dbcon_ops_constraint_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY dbcon_ops
    ADD CONSTRAINT dbcon_ops_constraint_name_key UNIQUE (constraint_name);


--
-- TOC entry 3077 (class 2606 OID 258180)
-- Dependencies: 2470 2470
-- Name: dbcon_ops_operator_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY dbcon_ops
    ADD CONSTRAINT dbcon_ops_operator_name_key UNIQUE (operator_name);


--
-- TOC entry 3079 (class 2606 OID 258189)
-- Dependencies: 2471 2471 2471
-- Name: dbcon_relvars_constraint_name_relvar_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY dbcon_relvars
    ADD CONSTRAINT dbcon_relvars_constraint_name_relvar_name_key UNIQUE (constraint_name, relvar_name);


--
-- TOC entry 3083 (class 2606 OID 258215)
-- Dependencies: 2476 2476
-- Name: dbcon_trigger_ops_operator_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY dbcon_trigger_ops
    ADD CONSTRAINT dbcon_trigger_ops_operator_name_key UNIQUE (operator_name);


--
-- TOC entry 3085 (class 2606 OID 258225)
-- Dependencies: 2477 2477
-- Name: dbcon_triggers_trigger_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY dbcon_triggers
    ADD CONSTRAINT dbcon_triggers_trigger_name_key UNIQUE (trigger_name);


--
-- TOC entry 3145 (class 2606 OID 260186)
-- Dependencies: 2584 2584
-- Name: disable_spreading_table_disable_spreading_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY disable_spreading_table
    ADD CONSTRAINT disable_spreading_table_disable_spreading_key UNIQUE (disable_spreading);


--
-- TOC entry 3139 (class 2606 OID 259783)
-- Dependencies: 2543 2543
-- Name: game_completed_table_game_completed_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY game_completed_table
    ADD CONSTRAINT game_completed_table_game_completed_key UNIQUE (game_completed);


--
-- TOC entry 3187 (class 2606 OID 260866)
-- Dependencies: 2623 2623
-- Name: history_no_visuals_history_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY history_no_visuals
    ADD CONSTRAINT history_no_visuals_history_name_key UNIQUE (history_name);


--
-- TOC entry 3185 (class 2606 OID 260857)
-- Dependencies: 2622 2622 2622
-- Name: history_sounds_history_name_sound_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY history_sounds
    ADD CONSTRAINT history_sounds_history_name_sound_name_key UNIQUE (history_name, sound_name);


--
-- TOC entry 3107 (class 2606 OID 258623)
-- Dependencies: 2512 2512 2512 2512
-- Name: imaginary_pieces_ptype_allegiance_tag_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY imaginary_pieces
    ADD CONSTRAINT imaginary_pieces_ptype_allegiance_tag_key UNIQUE (ptype, allegiance, tag);


--
-- TOC entry 3111 (class 2606 OID 258736)
-- Dependencies: 2523 2523
-- Name: in_next_phase_hack_table_in_next_phase_hack_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY in_next_phase_hack_table
    ADD CONSTRAINT in_next_phase_hack_table_in_next_phase_hack_key UNIQUE (in_next_phase_hack);


--
-- TOC entry 3171 (class 2606 OID 260611)
-- Dependencies: 2605 2605
-- Name: init_wizard_display_info_argument_colour_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY init_wizard_display_info_argument
    ADD CONSTRAINT init_wizard_display_info_argument_colour_key UNIQUE (colour);


--
-- TOC entry 3173 (class 2606 OID 260608)
-- Dependencies: 2605 2605
-- Name: init_wizard_display_info_argument_sprite_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY init_wizard_display_info_argument
    ADD CONSTRAINT init_wizard_display_info_argument_sprite_key UNIQUE (sprite);


--
-- TOC entry 3175 (class 2606 OID 260605)
-- Dependencies: 2605 2605
-- Name: init_wizard_display_info_argument_wizard_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY init_wizard_display_info_argument
    ADD CONSTRAINT init_wizard_display_info_argument_wizard_name_key UNIQUE (wizard_name);


--
-- TOC entry 3207 (class 2606 OID 261310)
-- Dependencies: 2645 2645 2645
-- Name: key_control_settings_key_code_action_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY key_control_settings
    ADD CONSTRAINT key_control_settings_key_code_action_name_key UNIQUE (key_code, action_name);


--
-- TOC entry 3189 (class 2606 OID 260872)
-- Dependencies: 2624 2624
-- Name: last_history_effect_id_table_last_history_effect__key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY last_history_effect_id_table
    ADD CONSTRAINT last_history_effect_id_table_last_history_effect__key UNIQUE (last_history_effect_id);


--
-- TOC entry 3087 (class 2606 OID 258250)
-- Dependencies: 2480 2480
-- Name: modules_module_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY modules
    ADD CONSTRAINT modules_module_name_key UNIQUE (module_name);


--
-- TOC entry 3199 (class 2606 OID 261212)
-- Dependencies: 2642 2642
-- Name: new_game_widget_state_colour_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY new_game_widget_state
    ADD CONSTRAINT new_game_widget_state_colour_key UNIQUE (colour);


--
-- TOC entry 3201 (class 2606 OID 261203)
-- Dependencies: 2642 2642
-- Name: new_game_widget_state_line_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY new_game_widget_state
    ADD CONSTRAINT new_game_widget_state_line_key UNIQUE (line);


--
-- TOC entry 3203 (class 2606 OID 261209)
-- Dependencies: 2642 2642
-- Name: new_game_widget_state_sprite_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY new_game_widget_state
    ADD CONSTRAINT new_game_widget_state_sprite_key UNIQUE (sprite);


--
-- TOC entry 3205 (class 2606 OID 261206)
-- Dependencies: 2642 2642
-- Name: new_game_widget_state_wizard_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY new_game_widget_state
    ADD CONSTRAINT new_game_widget_state_wizard_name_key UNIQUE (wizard_name);


--
-- TOC entry 3093 (class 2606 OID 258320)
-- Dependencies: 2486 2486
-- Name: piece_prototypes_mr_ptype_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY piece_prototypes_mr
    ADD CONSTRAINT piece_prototypes_mr_ptype_key UNIQUE (ptype);


--
-- TOC entry 3177 (class 2606 OID 260787)
-- Dependencies: 2612 2612 2612 2612
-- Name: piece_starting_ticks_ptype_allegiance_tag_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY piece_starting_ticks
    ADD CONSTRAINT piece_starting_ticks_ptype_allegiance_tag_key UNIQUE (ptype, allegiance, tag);


--
-- TOC entry 3105 (class 2606 OID 258524)
-- Dependencies: 2509 2509 2509 2509
-- Name: pieces_ptype_allegiance_tag_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY pieces
    ADD CONSTRAINT pieces_ptype_allegiance_tag_key UNIQUE (ptype, allegiance, tag);


--
-- TOC entry 3131 (class 2606 OID 259474)
-- Dependencies: 2539 2539 2539 2539
-- Name: pieces_to_move_ptype_allegiance_tag_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY pieces_to_move
    ADD CONSTRAINT pieces_to_move_ptype_allegiance_tag_key UNIQUE (ptype, allegiance, tag);


--
-- TOC entry 3137 (class 2606 OID 259669)
-- Dependencies: 2542 2542
-- Name: remaining_walk_hack_table_remaining_walk_hack_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY remaining_walk_hack_table
    ADD CONSTRAINT remaining_walk_hack_table_remaining_walk_hack_key UNIQUE (remaining_walk_hack);


--
-- TOC entry 3135 (class 2606 OID 259611)
-- Dependencies: 2541 2541
-- Name: remaining_walk_table_remaining_walk_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY remaining_walk_table
    ADD CONSTRAINT remaining_walk_table_remaining_walk_key UNIQUE (remaining_walk);


--
-- TOC entry 3133 (class 2606 OID 259544)
-- Dependencies: 2540 2540 2540 2540
-- Name: selected_piece_ptype_allegiance_tag_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY selected_piece
    ADD CONSTRAINT selected_piece_ptype_allegiance_tag_key UNIQUE (ptype, allegiance, tag);


--
-- TOC entry 3193 (class 2606 OID 261067)
-- Dependencies: 2633 2633
-- Name: spell_book_show_all_table_spell_book_show_all_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY spell_book_show_all_table
    ADD CONSTRAINT spell_book_show_all_table_spell_book_show_all_key UNIQUE (spell_book_show_all);


--
-- TOC entry 3103 (class 2606 OID 258467)
-- Dependencies: 2507 2507
-- Name: spell_books_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY spell_books
    ADD CONSTRAINT spell_books_id_key UNIQUE (id);


--
-- TOC entry 3123 (class 2606 OID 259063)
-- Dependencies: 2535 2535
-- Name: spell_choice_hack_table_spell_choice_hack_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY spell_choice_hack_table
    ADD CONSTRAINT spell_choice_hack_table_spell_choice_hack_key UNIQUE (spell_choice_hack);


--
-- TOC entry 3147 (class 2606 OID 260257)
-- Dependencies: 2586 2586
-- Name: spell_indexes_no_dis_turm_row_number_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY spell_indexes_no_dis_turm
    ADD CONSTRAINT spell_indexes_no_dis_turm_row_number_key UNIQUE (row_number);


--
-- TOC entry 3195 (class 2606 OID 261171)
-- Dependencies: 2638 2638
-- Name: spell_keys_key_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY spell_keys
    ADD CONSTRAINT spell_keys_key_key UNIQUE (key);


--
-- TOC entry 3197 (class 2606 OID 261168)
-- Dependencies: 2638 2638
-- Name: spell_keys_spell_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY spell_keys
    ADD CONSTRAINT spell_keys_spell_name_key UNIQUE (spell_name);


--
-- TOC entry 3125 (class 2606 OID 259191)
-- Dependencies: 2536 2536
-- Name: spell_parts_to_cast_table_spell_parts_to_cast_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY spell_parts_to_cast_table
    ADD CONSTRAINT spell_parts_to_cast_table_spell_parts_to_cast_key UNIQUE (spell_parts_to_cast);


--
-- TOC entry 3191 (class 2606 OID 261049)
-- Dependencies: 2632 2632
-- Name: spell_sprites_spell_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY spell_sprites
    ADD CONSTRAINT spell_sprites_spell_name_key UNIQUE (spell_name);


--
-- TOC entry 3095 (class 2606 OID 258357)
-- Dependencies: 2493 2493
-- Name: spells_mr_spell_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY spells_mr
    ADD CONSTRAINT spells_mr_spell_name_key UNIQUE (spell_name);


--
-- TOC entry 3163 (class 2606 OID 260569)
-- Dependencies: 2603 2603
-- Name: sprites_sprite_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY sprites
    ADD CONSTRAINT sprites_sprite_key UNIQUE (sprite);


--
-- TOC entry 3071 (class 2606 OID 258170)
-- Dependencies: 2455 2455 2455
-- Name: system_implementation_objects_object_name_object__key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY system_implementation_objects
    ADD CONSTRAINT system_implementation_objects_object_name_object__key UNIQUE (object_name, object_type);


--
-- TOC entry 3141 (class 2606 OID 259907)
-- Dependencies: 2544 2544
-- Name: test_action_overrides_override_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY test_action_overrides
    ADD CONSTRAINT test_action_overrides_override_key UNIQUE (override);


--
-- TOC entry 3115 (class 2606 OID 258806)
-- Dependencies: 2525 2525
-- Name: turn_number_table_turn_number_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY turn_number_table
    ADD CONSTRAINT turn_number_table_turn_number_key UNIQUE (turn_number);


--
-- TOC entry 3119 (class 2606 OID 258954)
-- Dependencies: 2529 2529
-- Name: turn_phase_table_turn_phase_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY turn_phase_table
    ADD CONSTRAINT turn_phase_table_turn_phase_key UNIQUE (turn_phase);


--
-- TOC entry 3159 (class 2606 OID 260548)
-- Dependencies: 2601 2601
-- Name: windows_window_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY windows
    ADD CONSTRAINT windows_window_name_key UNIQUE (window_name);


--
-- TOC entry 3165 (class 2606 OID 260584)
-- Dependencies: 2604 2604
-- Name: wizard_display_info_colour_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY wizard_display_info
    ADD CONSTRAINT wizard_display_info_colour_key UNIQUE (colour);


--
-- TOC entry 3167 (class 2606 OID 260581)
-- Dependencies: 2604 2604
-- Name: wizard_display_info_default_sprite_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY wizard_display_info
    ADD CONSTRAINT wizard_display_info_default_sprite_key UNIQUE (default_sprite);


--
-- TOC entry 3169 (class 2606 OID 260578)
-- Dependencies: 2604 2604
-- Name: wizard_display_info_wizard_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY wizard_display_info
    ADD CONSTRAINT wizard_display_info_wizard_name_key UNIQUE (wizard_name);


--
-- TOC entry 3121 (class 2606 OID 259009)
-- Dependencies: 2531 2531
-- Name: wizard_spell_choices_mr_wizard_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY wizard_spell_choices_mr
    ADD CONSTRAINT wizard_spell_choices_mr_wizard_name_key UNIQUE (wizard_name);


--
-- TOC entry 3151 (class 2606 OID 260317)
-- Dependencies: 2590 2590 2590
-- Name: wizard_starting_positions_wizard_count_place_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY wizard_starting_positions
    ADD CONSTRAINT wizard_starting_positions_wizard_count_place_key UNIQUE (wizard_count, place);


--
-- TOC entry 3153 (class 2606 OID 260320)
-- Dependencies: 2590 2590 2590 2590
-- Name: wizard_starting_positions_wizard_count_x_y_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY wizard_starting_positions
    ADD CONSTRAINT wizard_starting_positions_wizard_count_x_y_key UNIQUE (wizard_count, x, y);


--
-- TOC entry 3101 (class 2606 OID 258451)
-- Dependencies: 2504 2504
-- Name: wizards_wizard_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY wizards
    ADD CONSTRAINT wizards_wizard_name_key UNIQUE (wizard_name);


--
-- TOC entry 3099 (class 2606 OID 258417)
-- Dependencies: 2503 2503
-- Name: world_alignment_table_world_alignment_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY world_alignment_table
    ADD CONSTRAINT world_alignment_table_world_alignment_key UNIQUE (world_alignment);


--
-- TOC entry 3341 (class 2620 OID 261416)
-- Dependencies: 499 2648
-- Name: action_client_new_game_argument_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER action_client_new_game_argument_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON action_client_new_game_argument
    FOR EACH STATEMENT
    EXECUTE PROCEDURE action_client_new_game_argument_constraint_trigger_operator();


--
-- TOC entry 3309 (class 2620 OID 260521)
-- Dependencies: 412 2589
-- Name: action_history_mr_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER action_history_mr_changed
    AFTER INSERT OR DELETE OR UPDATE ON action_history_mr
    FOR EACH STATEMENT
    EXECUTE PROCEDURE action_history_mr_changed();


--
-- TOC entry 3314 (class 2620 OID 261410)
-- Dependencies: 2591 495
-- Name: action_new_game_argument_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER action_new_game_argument_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON action_new_game_argument
    FOR EACH STATEMENT
    EXECUTE PROCEDURE action_new_game_argument_constraint_trigger_operator();


--
-- TOC entry 3242 (class 2620 OID 261430)
-- Dependencies: 2484 506
-- Name: base_relvar_metadata_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER base_relvar_metadata_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON base_relvar_metadata
    FOR EACH STATEMENT
    EXECUTE PROCEDURE base_relvar_metadata_constraint_trigger_operator();


--
-- TOC entry 3243 (class 2620 OID 261435)
-- Dependencies: 510 2484
-- Name: base_relvar_metadata_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER base_relvar_metadata_d_readonly_transition_trigger
    AFTER DELETE ON base_relvar_metadata
    FOR EACH ROW
    EXECUTE PROCEDURE check_base_relvar_metadata_d_readonly();


--
-- TOC entry 3244 (class 2620 OID 261437)
-- Dependencies: 511 2484
-- Name: base_relvar_metadata_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER base_relvar_metadata_i_readonly_transition_trigger
    AFTER INSERT ON base_relvar_metadata
    FOR EACH ROW
    EXECUTE PROCEDURE check_base_relvar_metadata_i_readonly();


--
-- TOC entry 3245 (class 2620 OID 261433)
-- Dependencies: 2484 509
-- Name: base_relvar_metadata_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER base_relvar_metadata_u_readonly_transition_trigger
    AFTER UPDATE ON base_relvar_metadata
    FOR EACH ROW
    EXECUTE PROCEDURE check_base_relvar_metadata_u_readonly();


--
-- TOC entry 3252 (class 2620 OID 260507)
-- Dependencies: 405 2502
-- Name: board_size_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER board_size_changed
    AFTER INSERT OR DELETE OR UPDATE ON board_size
    FOR EACH STATEMENT
    EXECUTE PROCEDURE board_size_changed();


--
-- TOC entry 3253 (class 2620 OID 261406)
-- Dependencies: 2502 493
-- Name: board_size_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER board_size_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON board_size
    FOR EACH STATEMENT
    EXECUTE PROCEDURE board_size_constraint_trigger_operator();


--
-- TOC entry 3293 (class 2620 OID 261408)
-- Dependencies: 494 2538
-- Name: cast_alignment_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER cast_alignment_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON cast_alignment_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE cast_alignment_table_constraint_trigger_operator();


--
-- TOC entry 3291 (class 2620 OID 260523)
-- Dependencies: 2537 161
-- Name: cast_success_checked_table_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER cast_success_checked_table_changed
    AFTER INSERT OR DELETE OR UPDATE ON cast_success_checked_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE cast_success_checked_table_changed();


--
-- TOC entry 3292 (class 2620 OID 261418)
-- Dependencies: 2537 500
-- Name: cast_success_checked_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER cast_success_checked_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON cast_success_checked_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE cast_success_checked_table_constraint_trigger_operator();


--
-- TOC entry 3315 (class 2620 OID 261465)
-- Dependencies: 525 2602
-- Name: colours_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER colours_d_readonly_transition_trigger
    AFTER DELETE ON colours
    FOR EACH ROW
    EXECUTE PROCEDURE check_colours_d_readonly();


--
-- TOC entry 3316 (class 2620 OID 261467)
-- Dependencies: 526 2602
-- Name: colours_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER colours_i_readonly_transition_trigger
    AFTER INSERT ON colours
    FOR EACH ROW
    EXECUTE PROCEDURE check_colours_i_readonly();


--
-- TOC entry 3317 (class 2620 OID 261463)
-- Dependencies: 2602 524
-- Name: colours_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER colours_u_readonly_transition_trigger
    AFTER UPDATE ON colours
    FOR EACH ROW
    EXECUTE PROCEDURE check_colours_u_readonly();


--
-- TOC entry 3239 (class 2620 OID 261384)
-- Dependencies: 482 2475
-- Name: con_pg_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER con_pg_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON con_pg
    FOR EACH STATEMENT
    EXECUTE PROCEDURE con_pg_constraint_trigger_operator();


--
-- TOC entry 3267 (class 2620 OID 261388)
-- Dependencies: 2524 484
-- Name: creating_new_game_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER creating_new_game_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON creating_new_game_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE creating_new_game_table_constraint_trigger_operator();


--
-- TOC entry 3264 (class 2620 OID 260515)
-- Dependencies: 409 2513
-- Name: crimes_against_nature_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER crimes_against_nature_changed
    AFTER INSERT OR DELETE OR UPDATE ON crimes_against_nature
    FOR EACH STATEMENT
    EXECUTE PROCEDURE crimes_against_nature_changed();


--
-- TOC entry 3265 (class 2620 OID 261374)
-- Dependencies: 477 2513
-- Name: crimes_against_nature_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER crimes_against_nature_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON crimes_against_nature
    FOR EACH STATEMENT
    EXECUTE PROCEDURE crimes_against_nature_constraint_trigger_operator();


--
-- TOC entry 3329 (class 2620 OID 261378)
-- Dependencies: 479 2625
-- Name: current_effects_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER current_effects_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON current_effects
    FOR EACH STATEMENT
    EXECUTE PROCEDURE current_effects_constraint_trigger_operator();


--
-- TOC entry 3273 (class 2620 OID 260535)
-- Dependencies: 167 2527
-- Name: current_wizard_table_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER current_wizard_table_changed
    AFTER INSERT OR DELETE OR UPDATE ON current_wizard_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE current_wizard_table_changed();


--
-- TOC entry 3274 (class 2620 OID 261400)
-- Dependencies: 490 2527
-- Name: current_wizard_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER current_wizard_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON current_wizard_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE current_wizard_table_constraint_trigger_operator();


--
-- TOC entry 3275 (class 2620 OID 258903)
-- Dependencies: 155 2527
-- Name: current_wizard_table_no_delete_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER current_wizard_table_no_delete_transition_trigger
    AFTER DELETE ON current_wizard_table
    FOR EACH ROW
    EXECUTE PROCEDURE check_current_wizard_table_no_delete();


--
-- TOC entry 3276 (class 2620 OID 258905)
-- Dependencies: 2527 62
-- Name: current_wizard_table_no_insert_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER current_wizard_table_no_insert_transition_trigger
    AFTER INSERT ON current_wizard_table
    FOR EACH ROW
    EXECUTE PROCEDURE check_current_wizard_table_no_insert();


--
-- TOC entry 3321 (class 2620 OID 261390)
-- Dependencies: 485 2608
-- Name: cursor_position_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER cursor_position_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON cursor_position
    FOR EACH STATEMENT
    EXECUTE PROCEDURE cursor_position_constraint_trigger_operator();


--
-- TOC entry 3238 (class 2620 OID 261382)
-- Dependencies: 481 2471
-- Name: dbcon_relvars_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER dbcon_relvars_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON dbcon_relvars
    FOR EACH STATEMENT
    EXECUTE PROCEDURE dbcon_relvars_constraint_trigger_operator();


--
-- TOC entry 3240 (class 2620 OID 261392)
-- Dependencies: 486 2476
-- Name: dbcon_trigger_ops_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER dbcon_trigger_ops_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON dbcon_trigger_ops
    FOR EACH STATEMENT
    EXECUTE PROCEDURE dbcon_trigger_ops_constraint_trigger_operator();


--
-- TOC entry 3241 (class 2620 OID 261386)
-- Dependencies: 483 2477
-- Name: dbcon_triggers_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER dbcon_triggers_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON dbcon_triggers
    FOR EACH STATEMENT
    EXECUTE PROCEDURE dbcon_triggers_constraint_trigger_operator();


--
-- TOC entry 3283 (class 2620 OID 259185)
-- Dependencies: 2531 101
-- Name: delete_spell_choice_restricted_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER delete_spell_choice_restricted_transition_trigger
    AFTER DELETE ON wizard_spell_choices_mr
    FOR EACH ROW
    EXECUTE PROCEDURE check_delete_spell_choice_restricted();


--
-- TOC entry 3304 (class 2620 OID 260517)
-- Dependencies: 410 2584
-- Name: disable_spreading_table_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER disable_spreading_table_changed
    AFTER INSERT OR DELETE OR UPDATE ON disable_spreading_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE disable_spreading_table_changed();


--
-- TOC entry 3305 (class 2620 OID 261376)
-- Dependencies: 478 2584
-- Name: disable_spreading_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER disable_spreading_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON disable_spreading_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE disable_spreading_table_constraint_trigger_operator();


--
-- TOC entry 3301 (class 2620 OID 260533)
-- Dependencies: 166 2543
-- Name: game_completed_table_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER game_completed_table_changed
    AFTER INSERT OR DELETE OR UPDATE ON game_completed_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE game_completed_table_changed();


--
-- TOC entry 3302 (class 2620 OID 261398)
-- Dependencies: 489 2543
-- Name: game_completed_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER game_completed_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON game_completed_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE game_completed_table_constraint_trigger_operator();


--
-- TOC entry 3325 (class 2620 OID 261483)
-- Dependencies: 210 2623
-- Name: history_no_visuals_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER history_no_visuals_d_readonly_transition_trigger
    AFTER DELETE ON history_no_visuals
    FOR EACH ROW
    EXECUTE PROCEDURE check_history_no_visuals_d_readonly();


--
-- TOC entry 3326 (class 2620 OID 261485)
-- Dependencies: 2623 211
-- Name: history_no_visuals_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER history_no_visuals_i_readonly_transition_trigger
    AFTER INSERT ON history_no_visuals
    FOR EACH ROW
    EXECUTE PROCEDURE check_history_no_visuals_i_readonly();


--
-- TOC entry 3327 (class 2620 OID 261481)
-- Dependencies: 2623 209
-- Name: history_no_visuals_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER history_no_visuals_u_readonly_transition_trigger
    AFTER UPDATE ON history_no_visuals
    FOR EACH ROW
    EXECUTE PROCEDURE check_history_no_visuals_u_readonly();


--
-- TOC entry 3322 (class 2620 OID 261477)
-- Dependencies: 2622 207
-- Name: history_sounds_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER history_sounds_d_readonly_transition_trigger
    AFTER DELETE ON history_sounds
    FOR EACH ROW
    EXECUTE PROCEDURE check_history_sounds_d_readonly();


--
-- TOC entry 3323 (class 2620 OID 261479)
-- Dependencies: 208 2622
-- Name: history_sounds_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER history_sounds_i_readonly_transition_trigger
    AFTER INSERT ON history_sounds
    FOR EACH ROW
    EXECUTE PROCEDURE check_history_sounds_i_readonly();


--
-- TOC entry 3324 (class 2620 OID 261475)
-- Dependencies: 2622 206
-- Name: history_sounds_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER history_sounds_u_readonly_transition_trigger
    AFTER UPDATE ON history_sounds
    FOR EACH ROW
    EXECUTE PROCEDURE check_history_sounds_u_readonly();


--
-- TOC entry 3262 (class 2620 OID 260503)
-- Dependencies: 403 2512
-- Name: imaginary_pieces_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER imaginary_pieces_changed
    AFTER INSERT OR DELETE OR UPDATE ON imaginary_pieces
    FOR EACH STATEMENT
    EXECUTE PROCEDURE imaginary_pieces_changed();


--
-- TOC entry 3263 (class 2620 OID 261428)
-- Dependencies: 2512 505
-- Name: imaginary_pieces_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER imaginary_pieces_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON imaginary_pieces
    FOR EACH STATEMENT
    EXECUTE PROCEDURE imaginary_pieces_constraint_trigger_operator();


--
-- TOC entry 3266 (class 2620 OID 261364)
-- Dependencies: 2523 473
-- Name: in_next_phase_hack_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER in_next_phase_hack_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON in_next_phase_hack_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE in_next_phase_hack_table_constraint_trigger_operator();


--
-- TOC entry 3284 (class 2620 OID 259183)
-- Dependencies: 100 2531
-- Name: insert_spell_choice_restricted_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER insert_spell_choice_restricted_transition_trigger
    AFTER INSERT ON wizard_spell_choices_mr
    FOR EACH ROW
    EXECUTE PROCEDURE check_insert_spell_choice_restricted();


--
-- TOC entry 3338 (class 2620 OID 261501)
-- Dependencies: 2645 219
-- Name: key_control_settings_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER key_control_settings_d_readonly_transition_trigger
    AFTER DELETE ON key_control_settings
    FOR EACH ROW
    EXECUTE PROCEDURE check_key_control_settings_d_readonly();


--
-- TOC entry 3339 (class 2620 OID 261503)
-- Dependencies: 2645 220
-- Name: key_control_settings_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER key_control_settings_i_readonly_transition_trigger
    AFTER INSERT ON key_control_settings
    FOR EACH ROW
    EXECUTE PROCEDURE check_key_control_settings_i_readonly();


--
-- TOC entry 3340 (class 2620 OID 261499)
-- Dependencies: 218 2645
-- Name: key_control_settings_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER key_control_settings_u_readonly_transition_trigger
    AFTER UPDATE ON key_control_settings
    FOR EACH ROW
    EXECUTE PROCEDURE check_key_control_settings_u_readonly();


--
-- TOC entry 3328 (class 2620 OID 261362)
-- Dependencies: 472 2624
-- Name: last_history_effect_id_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER last_history_effect_id_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON last_history_effect_id_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE last_history_effect_id_table_constraint_trigger_operator();


--
-- TOC entry 3337 (class 2620 OID 261380)
-- Dependencies: 480 2642
-- Name: new_game_widget_state_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER new_game_widget_state_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON new_game_widget_state
    FOR EACH STATEMENT
    EXECUTE PROCEDURE new_game_widget_state_constraint_trigger_operator();


--
-- TOC entry 3277 (class 2620 OID 258901)
-- Dependencies: 2527 154
-- Name: next_wizard_change_valid_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER next_wizard_change_valid_transition_trigger
    AFTER UPDATE ON current_wizard_table
    FOR EACH ROW
    EXECUTE PROCEDURE check_next_wizard_change_valid();


--
-- TOC entry 3246 (class 2620 OID 261441)
-- Dependencies: 513 2486
-- Name: piece_prototypes_mr_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER piece_prototypes_mr_d_readonly_transition_trigger
    AFTER DELETE ON piece_prototypes_mr
    FOR EACH ROW
    EXECUTE PROCEDURE check_piece_prototypes_mr_d_readonly();


--
-- TOC entry 3247 (class 2620 OID 261443)
-- Dependencies: 2486 514
-- Name: piece_prototypes_mr_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER piece_prototypes_mr_i_readonly_transition_trigger
    AFTER INSERT ON piece_prototypes_mr
    FOR EACH ROW
    EXECUTE PROCEDURE check_piece_prototypes_mr_i_readonly();


--
-- TOC entry 3248 (class 2620 OID 261439)
-- Dependencies: 512 2486
-- Name: piece_prototypes_mr_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER piece_prototypes_mr_u_readonly_transition_trigger
    AFTER UPDATE ON piece_prototypes_mr
    FOR EACH ROW
    EXECUTE PROCEDURE check_piece_prototypes_mr_u_readonly();


--
-- TOC entry 3260 (class 2620 OID 260509)
-- Dependencies: 2509 406
-- Name: pieces_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER pieces_changed
    AFTER INSERT OR DELETE OR UPDATE ON pieces
    FOR EACH STATEMENT
    EXECUTE PROCEDURE pieces_changed();


--
-- TOC entry 3261 (class 2620 OID 261368)
-- Dependencies: 2509 475
-- Name: pieces_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER pieces_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON pieces
    FOR EACH STATEMENT
    EXECUTE PROCEDURE pieces_constraint_trigger_operator();


--
-- TOC entry 3294 (class 2620 OID 260525)
-- Dependencies: 2539 162
-- Name: pieces_to_move_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER pieces_to_move_changed
    AFTER INSERT OR DELETE OR UPDATE ON pieces_to_move
    FOR EACH STATEMENT
    EXECUTE PROCEDURE pieces_to_move_changed();


--
-- TOC entry 3295 (class 2620 OID 261420)
-- Dependencies: 2539 501
-- Name: pieces_to_move_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER pieces_to_move_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON pieces_to_move
    FOR EACH STATEMENT
    EXECUTE PROCEDURE pieces_to_move_constraint_trigger_operator();


--
-- TOC entry 3300 (class 2620 OID 261412)
-- Dependencies: 497 2542
-- Name: remaining_walk_hack_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER remaining_walk_hack_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON remaining_walk_hack_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE remaining_walk_hack_table_constraint_trigger_operator();


--
-- TOC entry 3298 (class 2620 OID 260513)
-- Dependencies: 2541 408
-- Name: remaining_walk_table_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER remaining_walk_table_changed
    AFTER INSERT OR DELETE OR UPDATE ON remaining_walk_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE remaining_walk_table_changed();


--
-- TOC entry 3299 (class 2620 OID 261372)
-- Dependencies: 2541 476
-- Name: remaining_walk_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER remaining_walk_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON remaining_walk_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE remaining_walk_table_constraint_trigger_operator();


--
-- TOC entry 3296 (class 2620 OID 260505)
-- Dependencies: 2540 404
-- Name: selected_piece_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER selected_piece_changed
    AFTER INSERT OR DELETE OR UPDATE ON selected_piece
    FOR EACH STATEMENT
    EXECUTE PROCEDURE selected_piece_changed();


--
-- TOC entry 3297 (class 2620 OID 261366)
-- Dependencies: 474 2540
-- Name: selected_piece_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER selected_piece_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON selected_piece
    FOR EACH STATEMENT
    EXECUTE PROCEDURE selected_piece_constraint_trigger_operator();


--
-- TOC entry 3333 (class 2620 OID 261404)
-- Dependencies: 492 2633
-- Name: spell_book_show_all_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_book_show_all_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON spell_book_show_all_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE spell_book_show_all_table_constraint_trigger_operator();


--
-- TOC entry 3258 (class 2620 OID 260519)
-- Dependencies: 411 2507
-- Name: spell_books_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_books_changed
    AFTER INSERT OR DELETE OR UPDATE ON spell_books
    FOR EACH STATEMENT
    EXECUTE PROCEDURE spell_books_changed();


--
-- TOC entry 3259 (class 2620 OID 261414)
-- Dependencies: 498 2507
-- Name: spell_books_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_books_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON spell_books
    FOR EACH STATEMENT
    EXECUTE PROCEDURE spell_books_constraint_trigger_operator();


--
-- TOC entry 3288 (class 2620 OID 261394)
-- Dependencies: 487 2535
-- Name: spell_choice_hack_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_choice_hack_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON spell_choice_hack_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE spell_choice_hack_table_constraint_trigger_operator();


--
-- TOC entry 3306 (class 2620 OID 261453)
-- Dependencies: 2586 519
-- Name: spell_indexes_no_dis_turm_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_indexes_no_dis_turm_d_readonly_transition_trigger
    AFTER DELETE ON spell_indexes_no_dis_turm
    FOR EACH ROW
    EXECUTE PROCEDURE check_spell_indexes_no_dis_turm_d_readonly();


--
-- TOC entry 3307 (class 2620 OID 261455)
-- Dependencies: 2586 520
-- Name: spell_indexes_no_dis_turm_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_indexes_no_dis_turm_i_readonly_transition_trigger
    AFTER INSERT ON spell_indexes_no_dis_turm
    FOR EACH ROW
    EXECUTE PROCEDURE check_spell_indexes_no_dis_turm_i_readonly();


--
-- TOC entry 3308 (class 2620 OID 261451)
-- Dependencies: 2586 518
-- Name: spell_indexes_no_dis_turm_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_indexes_no_dis_turm_u_readonly_transition_trigger
    AFTER UPDATE ON spell_indexes_no_dis_turm
    FOR EACH ROW
    EXECUTE PROCEDURE check_spell_indexes_no_dis_turm_u_readonly();


--
-- TOC entry 3334 (class 2620 OID 261495)
-- Dependencies: 216 2638
-- Name: spell_keys_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_keys_d_readonly_transition_trigger
    AFTER DELETE ON spell_keys
    FOR EACH ROW
    EXECUTE PROCEDURE check_spell_keys_d_readonly();


--
-- TOC entry 3335 (class 2620 OID 261497)
-- Dependencies: 217 2638
-- Name: spell_keys_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_keys_i_readonly_transition_trigger
    AFTER INSERT ON spell_keys
    FOR EACH ROW
    EXECUTE PROCEDURE check_spell_keys_i_readonly();


--
-- TOC entry 3336 (class 2620 OID 261493)
-- Dependencies: 215 2638
-- Name: spell_keys_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_keys_u_readonly_transition_trigger
    AFTER UPDATE ON spell_keys
    FOR EACH ROW
    EXECUTE PROCEDURE check_spell_keys_u_readonly();


--
-- TOC entry 3289 (class 2620 OID 260499)
-- Dependencies: 401 2536
-- Name: spell_parts_to_cast_table_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_parts_to_cast_table_changed
    AFTER INSERT OR DELETE OR UPDATE ON spell_parts_to_cast_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE spell_parts_to_cast_table_changed();


--
-- TOC entry 3290 (class 2620 OID 261360)
-- Dependencies: 2536 471
-- Name: spell_parts_to_cast_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_parts_to_cast_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON spell_parts_to_cast_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE spell_parts_to_cast_table_constraint_trigger_operator();


--
-- TOC entry 3330 (class 2620 OID 261489)
-- Dependencies: 213 2632
-- Name: spell_sprites_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_sprites_d_readonly_transition_trigger
    AFTER DELETE ON spell_sprites
    FOR EACH ROW
    EXECUTE PROCEDURE check_spell_sprites_d_readonly();


--
-- TOC entry 3331 (class 2620 OID 261491)
-- Dependencies: 2632 214
-- Name: spell_sprites_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_sprites_i_readonly_transition_trigger
    AFTER INSERT ON spell_sprites
    FOR EACH ROW
    EXECUTE PROCEDURE check_spell_sprites_i_readonly();


--
-- TOC entry 3332 (class 2620 OID 261487)
-- Dependencies: 212 2632
-- Name: spell_sprites_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spell_sprites_u_readonly_transition_trigger
    AFTER UPDATE ON spell_sprites
    FOR EACH ROW
    EXECUTE PROCEDURE check_spell_sprites_u_readonly();


--
-- TOC entry 3249 (class 2620 OID 261447)
-- Dependencies: 516 2493
-- Name: spells_mr_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spells_mr_d_readonly_transition_trigger
    AFTER DELETE ON spells_mr
    FOR EACH ROW
    EXECUTE PROCEDURE check_spells_mr_d_readonly();


--
-- TOC entry 3250 (class 2620 OID 261449)
-- Dependencies: 517 2493
-- Name: spells_mr_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spells_mr_i_readonly_transition_trigger
    AFTER INSERT ON spells_mr
    FOR EACH ROW
    EXECUTE PROCEDURE check_spells_mr_i_readonly();


--
-- TOC entry 3251 (class 2620 OID 261445)
-- Dependencies: 515 2493
-- Name: spells_mr_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER spells_mr_u_readonly_transition_trigger
    AFTER UPDATE ON spells_mr
    FOR EACH ROW
    EXECUTE PROCEDURE check_spells_mr_u_readonly();


--
-- TOC entry 3318 (class 2620 OID 261471)
-- Dependencies: 496 2603
-- Name: sprites_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER sprites_d_readonly_transition_trigger
    AFTER DELETE ON sprites
    FOR EACH ROW
    EXECUTE PROCEDURE check_sprites_d_readonly();


--
-- TOC entry 3319 (class 2620 OID 261473)
-- Dependencies: 2603 507
-- Name: sprites_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER sprites_i_readonly_transition_trigger
    AFTER INSERT ON sprites
    FOR EACH ROW
    EXECUTE PROCEDURE check_sprites_i_readonly();


--
-- TOC entry 3320 (class 2620 OID 261469)
-- Dependencies: 527 2603
-- Name: sprites_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER sprites_u_readonly_transition_trigger
    AFTER UPDATE ON sprites
    FOR EACH ROW
    EXECUTE PROCEDURE check_sprites_u_readonly();


--
-- TOC entry 3303 (class 2620 OID 260501)
-- Dependencies: 402 2544
-- Name: test_action_overrides_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER test_action_overrides_changed
    AFTER INSERT OR DELETE OR UPDATE ON test_action_overrides
    FOR EACH STATEMENT
    EXECUTE PROCEDURE test_action_overrides_changed();


--
-- TOC entry 3268 (class 2620 OID 258840)
-- Dependencies: 2525 134
-- Name: turn_number_change_valid_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER turn_number_change_valid_transition_trigger
    AFTER UPDATE ON turn_number_table
    FOR EACH ROW
    EXECUTE PROCEDURE check_turn_number_change_valid();


--
-- TOC entry 3269 (class 2620 OID 260527)
-- Dependencies: 2525 163
-- Name: turn_number_table_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER turn_number_table_changed
    AFTER INSERT OR DELETE OR UPDATE ON turn_number_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE turn_number_table_changed();


--
-- TOC entry 3270 (class 2620 OID 261422)
-- Dependencies: 2525 502
-- Name: turn_number_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER turn_number_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON turn_number_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE turn_number_table_constraint_trigger_operator();


--
-- TOC entry 3271 (class 2620 OID 258843)
-- Dependencies: 2525 136
-- Name: turn_number_table_no_delete_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER turn_number_table_no_delete_transition_trigger
    AFTER DELETE ON turn_number_table
    FOR EACH ROW
    EXECUTE PROCEDURE check_turn_number_table_no_delete();


--
-- TOC entry 3272 (class 2620 OID 258845)
-- Dependencies: 2525 129
-- Name: turn_number_table_no_insert_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER turn_number_table_no_insert_transition_trigger
    AFTER INSERT ON turn_number_table
    FOR EACH ROW
    EXECUTE PROCEDURE check_turn_number_table_no_insert();


--
-- TOC entry 3278 (class 2620 OID 258992)
-- Dependencies: 2529 157
-- Name: turn_phase_change_valid_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER turn_phase_change_valid_transition_trigger
    AFTER UPDATE ON turn_phase_table
    FOR EACH ROW
    EXECUTE PROCEDURE check_turn_phase_change_valid();


--
-- TOC entry 3279 (class 2620 OID 260531)
-- Dependencies: 165 2529
-- Name: turn_phase_table_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER turn_phase_table_changed
    AFTER INSERT OR DELETE OR UPDATE ON turn_phase_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE turn_phase_table_changed();


--
-- TOC entry 3280 (class 2620 OID 261396)
-- Dependencies: 488 2529
-- Name: turn_phase_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER turn_phase_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON turn_phase_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE turn_phase_table_constraint_trigger_operator();


--
-- TOC entry 3281 (class 2620 OID 258994)
-- Dependencies: 2529 158
-- Name: turn_phase_table_no_delete_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER turn_phase_table_no_delete_transition_trigger
    AFTER DELETE ON turn_phase_table
    FOR EACH ROW
    EXECUTE PROCEDURE check_turn_phase_table_no_delete();


--
-- TOC entry 3282 (class 2620 OID 258996)
-- Dependencies: 2529 159
-- Name: turn_phase_table_no_insert_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER turn_phase_table_no_insert_transition_trigger
    AFTER INSERT ON turn_phase_table
    FOR EACH ROW
    EXECUTE PROCEDURE check_turn_phase_table_no_insert();


--
-- TOC entry 3285 (class 2620 OID 259181)
-- Dependencies: 2531 99
-- Name: update_spell_choice_restricted_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER update_spell_choice_restricted_transition_trigger
    AFTER UPDATE ON wizard_spell_choices_mr
    FOR EACH ROW
    EXECUTE PROCEDURE check_update_spell_choice_restricted();


--
-- TOC entry 3286 (class 2620 OID 260529)
-- Dependencies: 2531 164
-- Name: wizard_spell_choices_mr_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER wizard_spell_choices_mr_changed
    AFTER INSERT OR DELETE OR UPDATE ON wizard_spell_choices_mr
    FOR EACH STATEMENT
    EXECUTE PROCEDURE wizard_spell_choices_mr_changed();


--
-- TOC entry 3287 (class 2620 OID 261424)
-- Dependencies: 2531 503
-- Name: wizard_spell_choices_mr_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER wizard_spell_choices_mr_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON wizard_spell_choices_mr
    FOR EACH STATEMENT
    EXECUTE PROCEDURE wizard_spell_choices_mr_constraint_trigger_operator();


--
-- TOC entry 3310 (class 2620 OID 261426)
-- Dependencies: 504 2590
-- Name: wizard_starting_positions_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER wizard_starting_positions_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON wizard_starting_positions
    FOR EACH STATEMENT
    EXECUTE PROCEDURE wizard_starting_positions_constraint_trigger_operator();


--
-- TOC entry 3311 (class 2620 OID 261459)
-- Dependencies: 2590 522
-- Name: wizard_starting_positions_d_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER wizard_starting_positions_d_readonly_transition_trigger
    AFTER DELETE ON wizard_starting_positions
    FOR EACH ROW
    EXECUTE PROCEDURE check_wizard_starting_positions_d_readonly();


--
-- TOC entry 3312 (class 2620 OID 261461)
-- Dependencies: 2590 523
-- Name: wizard_starting_positions_i_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER wizard_starting_positions_i_readonly_transition_trigger
    AFTER INSERT ON wizard_starting_positions
    FOR EACH ROW
    EXECUTE PROCEDURE check_wizard_starting_positions_i_readonly();


--
-- TOC entry 3313 (class 2620 OID 261457)
-- Dependencies: 521 2590
-- Name: wizard_starting_positions_u_readonly_transition_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER wizard_starting_positions_u_readonly_transition_trigger
    AFTER UPDATE ON wizard_starting_positions
    FOR EACH ROW
    EXECUTE PROCEDURE check_wizard_starting_positions_u_readonly();


--
-- TOC entry 3256 (class 2620 OID 260537)
-- Dependencies: 2504 168
-- Name: wizards_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER wizards_changed
    AFTER INSERT OR DELETE OR UPDATE ON wizards
    FOR EACH STATEMENT
    EXECUTE PROCEDURE wizards_changed();


--
-- TOC entry 3257 (class 2620 OID 261402)
-- Dependencies: 2504 491
-- Name: wizards_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER wizards_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON wizards
    FOR EACH STATEMENT
    EXECUTE PROCEDURE wizards_constraint_trigger_operator();


--
-- TOC entry 3254 (class 2620 OID 260511)
-- Dependencies: 407 2503
-- Name: world_alignment_table_changed; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER world_alignment_table_changed
    AFTER INSERT OR DELETE OR UPDATE ON world_alignment_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE world_alignment_table_changed();


--
-- TOC entry 3255 (class 2620 OID 261370)
-- Dependencies: 463 2503
-- Name: world_alignment_table_constraint_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER world_alignment_table_constraint_trigger
    AFTER INSERT OR DELETE OR UPDATE ON world_alignment_table
    FOR EACH STATEMENT
    EXECUTE PROCEDURE world_alignment_table_constraint_trigger_operator();


--
-- TOC entry 3237 (class 2606 OID 261353)
-- Dependencies: 2648 2603 3162
-- Name: action_client_new_game_argument_sprite_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY action_client_new_game_argument
    ADD CONSTRAINT action_client_new_game_argument_sprite_fkey FOREIGN KEY (sprite) REFERENCES sprites(sprite) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3219 (class 2606 OID 258261)
-- Dependencies: 2480 2481 3086
-- Name: all_module_objects_module_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY all_module_objects
    ADD CONSTRAINT all_module_objects_module_name_fkey FOREIGN KEY (module_name) REFERENCES modules(module_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3218 (class 2606 OID 258203)
-- Dependencies: 2475 3072 2466
-- Name: con_pg_constraint_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY con_pg
    ADD CONSTRAINT con_pg_constraint_name_fkey FOREIGN KEY (constraint_name) REFERENCES database_constraints(constraint_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3222 (class 2606 OID 258663)
-- Dependencies: 2513 2509 2509 2509 3104 2513 2513
-- Name: crimes_against_nature_ptype_allegiance_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY crimes_against_nature
    ADD CONSTRAINT crimes_against_nature_ptype_allegiance_tag_fkey FOREIGN KEY (ptype, allegiance, tag) REFERENCES pieces(ptype, allegiance, tag) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3223 (class 2606 OID 258895)
-- Dependencies: 2527 2504 3100
-- Name: current_wizard_table_current_wizard_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY current_wizard_table
    ADD CONSTRAINT current_wizard_table_current_wizard_fkey FOREIGN KEY (current_wizard) REFERENCES wizards(wizard_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3216 (class 2606 OID 258182)
-- Dependencies: 3072 2466 2470
-- Name: dbcon_ops_constraint_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY dbcon_ops
    ADD CONSTRAINT dbcon_ops_constraint_name_fkey FOREIGN KEY (constraint_name) REFERENCES database_constraints(constraint_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3217 (class 2606 OID 258191)
-- Dependencies: 2471 3072 2466
-- Name: dbcon_relvars_constraint_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY dbcon_relvars
    ADD CONSTRAINT dbcon_relvars_constraint_name_fkey FOREIGN KEY (constraint_name) REFERENCES database_constraints(constraint_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3221 (class 2606 OID 258625)
-- Dependencies: 2509 2512 2512 2512 2509 3104 2509
-- Name: imaginary_pieces_ptype_allegiance_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY imaginary_pieces
    ADD CONSTRAINT imaginary_pieces_ptype_allegiance_tag_fkey FOREIGN KEY (ptype, allegiance, tag) REFERENCES pieces(ptype, allegiance, tag) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3231 (class 2606 OID 260619)
-- Dependencies: 3162 2605 2603
-- Name: init_wizard_display_info_argument_sprite_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY init_wizard_display_info_argument
    ADD CONSTRAINT init_wizard_display_info_argument_sprite_fkey FOREIGN KEY (sprite) REFERENCES sprites(sprite) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3230 (class 2606 OID 260613)
-- Dependencies: 3100 2504 2605
-- Name: init_wizard_display_info_argument_wizard_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY init_wizard_display_info_argument
    ADD CONSTRAINT init_wizard_display_info_argument_wizard_name_fkey FOREIGN KEY (wizard_name) REFERENCES wizards(wizard_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3236 (class 2606 OID 261214)
-- Dependencies: 2642 2603 3162
-- Name: new_game_widget_state_sprite_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY new_game_widget_state
    ADD CONSTRAINT new_game_widget_state_sprite_fkey FOREIGN KEY (sprite) REFERENCES sprites(sprite) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3232 (class 2606 OID 260789)
-- Dependencies: 2612 3104 2509 2509 2509 2612 2612
-- Name: piece_starting_ticks_ptype_allegiance_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY piece_starting_ticks
    ADD CONSTRAINT piece_starting_ticks_ptype_allegiance_tag_fkey FOREIGN KEY (ptype, allegiance, tag) REFERENCES pieces(ptype, allegiance, tag) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3224 (class 2606 OID 259482)
-- Dependencies: 2539 3116 2527
-- Name: pieces_to_move_allegiance_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY pieces_to_move
    ADD CONSTRAINT pieces_to_move_allegiance_fkey FOREIGN KEY (allegiance) REFERENCES current_wizard_table(current_wizard) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3225 (class 2606 OID 259476)
-- Dependencies: 2539 3104 2509 2509 2509 2539 2539
-- Name: pieces_to_move_ptype_allegiance_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY pieces_to_move
    ADD CONSTRAINT pieces_to_move_ptype_allegiance_tag_fkey FOREIGN KEY (ptype, allegiance, tag) REFERENCES pieces(ptype, allegiance, tag) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3227 (class 2606 OID 259552)
-- Dependencies: 2527 2540 3116
-- Name: selected_piece_allegiance_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY selected_piece
    ADD CONSTRAINT selected_piece_allegiance_fkey FOREIGN KEY (allegiance) REFERENCES current_wizard_table(current_wizard) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3226 (class 2606 OID 259546)
-- Dependencies: 3104 2540 2540 2540 2509 2509 2509
-- Name: selected_piece_ptype_allegiance_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY selected_piece
    ADD CONSTRAINT selected_piece_ptype_allegiance_tag_fkey FOREIGN KEY (ptype, allegiance, tag) REFERENCES pieces(ptype, allegiance, tag) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3220 (class 2606 OID 258469)
-- Dependencies: 2507 2504 3100
-- Name: spell_books_wizard_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY spell_books
    ADD CONSTRAINT spell_books_wizard_name_fkey FOREIGN KEY (wizard_name) REFERENCES wizards(wizard_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3235 (class 2606 OID 261173)
-- Dependencies: 2638 3094 2493
-- Name: spell_keys_spell_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY spell_keys
    ADD CONSTRAINT spell_keys_spell_name_fkey FOREIGN KEY (spell_name) REFERENCES spells_mr(spell_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3234 (class 2606 OID 261057)
-- Dependencies: 3094 2632 2493
-- Name: spell_sprites_spell_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY spell_sprites
    ADD CONSTRAINT spell_sprites_spell_name_fkey FOREIGN KEY (spell_name) REFERENCES spells_mr(spell_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3233 (class 2606 OID 261051)
-- Dependencies: 3162 2632 2603
-- Name: spell_sprites_sprite_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY spell_sprites
    ADD CONSTRAINT spell_sprites_sprite_fkey FOREIGN KEY (sprite) REFERENCES sprites(sprite) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3229 (class 2606 OID 260592)
-- Dependencies: 2604 3162 2603
-- Name: wizard_display_info_default_sprite_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY wizard_display_info
    ADD CONSTRAINT wizard_display_info_default_sprite_fkey FOREIGN KEY (default_sprite) REFERENCES sprites(sprite) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3228 (class 2606 OID 260586)
-- Dependencies: 2604 3100 2504
-- Name: wizard_display_info_wizard_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY wizard_display_info
    ADD CONSTRAINT wizard_display_info_wizard_name_fkey FOREIGN KEY (wizard_name) REFERENCES wizards(wizard_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 3409 (class 0 OID 0)
-- Dependencies: 6
-- Name: public; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


-- Completed on 2009-10-07 16:57:42 BST

--
-- PostgreSQL database dump complete
--

