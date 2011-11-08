
Here are a few Chaos2000 specific extensions, written just to get
chaos working again then will review approach.

> {-# LANGUAGE QuasiQuotes #-}
> module ChaosExtensions
>     (
>      chaosExtensions
>     ,chaosExtensionsExamples
>     ) where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
>
> import Data.List
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Annotation
>
> import Database.HsSqlPpp.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Quote
> import Database.HsSqlPpp.Extensions.TransitionConstraints
> import Database.HsSqlPpp.Extensions.Modules
> import Database.HsSqlPpp.Extensions.CreateAssertion
> import Database.HsSqlPpp.Extensions.CreateVar
> import Database.HsSqlPpp.Extensions.CardinalityRestrict
> import Database.HsSqlPpp.Extensions.SimplifiedCatalog
> import Database.HsSqlPpp.Extensions.Denormalized6nf
> import Database.HsSqlPpp.Extensions.AstUtils
>
>
> -- | run all the extensions needed for chaos
> chaosExtensions :: [Statement] -> [Statement] -- Data a => a -> a
> chaosExtensions =   modules
>                   . createAssertion
>                   . transitionConstraints
>                   . createVar
>                   . cardinalityRestrict
>                   . clientActionWrapper
>                   . noDelIns
>                   . generateSpellChoiceActions
>                   . notNull
>                   . denormalized6nf
>                   . simplifiedCatalog
>                   . makeFKsCascade

> chaosExtensionsExamples :: [ExtensionTest]
> chaosExtensionsExamples = [clientActionWrapperExample
>                           ,noDelInsExample
>                           ,generateSpellChoiceActionsExample
>                           ,notNullExample1
>                           ,notNullExample2
>                           ,makeFKsCascadeExample1
>                           ,makeFKsCascadeExample2]

--------------------------------------------------------------------------------

client action wrapper

a sort of numpty partial application thing, to support key presses: a
key is pressed, and the key_pressed function is called with the
key. This looks up the action to call in a table, but we only store
actions with no arguments in this table, so need to create wrappers
which have any arguments already bound. e.g. there is a move_cursor
function which takes the direction to move in, we create a wrapper for
each direction which takes no arguments which we can put in the action
lookup table. TODO: rewrite this paragraph in english.

> clientActionWrapperExample :: ExtensionTest
> clientActionWrapperExample =
>   ExtensionTest
>     "clientActionWrapper"
>     clientActionWrapper
>     [sqlStmts|
>         select create_client_action_wrapper('move_cursor_down'
>                                            ,$$move_cursor('down')$$);|]
>     [sqlStmts|
>         create function action_move_cursor_down() returns void as $$
>         begin
>           perform action_move_cursor('down');
>         end;
>         $$ language plpgsql volatile;
>      |]
>
> clientActionWrapper :: Data a => a -> a
> clientActionWrapper =
>     transformBi $ \x ->
>       case x of
>         s@[sqlStmt|
>            select create_client_action_wrapper($s(actname)
>                                               ,$s(actcall)); |]
>             -> let actionname = "action_" ++ actname
>                    expr = case parseScalarExpr defaultParseFlags "" Nothing ("action_" ++ actcall) of
>                             Left e -> error $ show e
>                             Right e1 -> e1
>                in replaceSourcePos1 s [sqlStmt|
>                    create function $(actionname)() returns void as $$
>                    begin
>                      perform $(expr);
>                    end;
>                    $$ language plpgsql volatile;
>                    |]
>         x1 -> x1
>

--------------------------------------------------------------------------------

restrict table to have no deletes or inserts (only updates) except
when starting a new game

which is better - doing something like this, or dropping the
constraints temporarily during new_game? trick question: we should use
multiple updates.

> noDelInsExample :: ExtensionTest
> noDelInsExample =
>   ExtensionTest
>     "noDelIns"
>     (transitionConstraints . noDelIns)
>     [sqlStmts|
>         select no_deletes_inserts_except_new_game('relvar');
>      |]
>     (transitionConstraints [sqlStmts|
>         select create_insert_transition_tuple_constraint
>                    ('relvar'
>                    ,'relvar_no_insert'
>                    ,'exists(select 1 from creating_new_game_table
>                             where creating_new_game = true)');
>         select create_delete_transition_tuple_constraint
>                    ('relvar'
>                    ,'relvar_no_delete'
>                    ,'exists(select 1 from creating_new_game_table
>                             where creating_new_game = true)');
>         |])
>
> noDelIns :: Data a => a -> a
> noDelIns =
>     transformBi $ \x ->
>       case x of
>         s@[sqlStmt| select no_deletes_inserts_except_new_game($s(table));|] : tl ->
>           let icn = table ++ "_no_insert"
>               dcn = table ++ "_no_delete"
>               exprt = "exists(select 1 from creating_new_game_table\n\
>                       \       where creating_new_game = true)"
>           in replaceSourcePos s [sqlStmts|
>               select create_insert_transition_tuple_constraint
>                    ($s(table),$s(icn),$s(exprt));
>               select create_delete_transition_tuple_constraint
>                    ($s(table),$s(dcn),$s(exprt));
>               |] ++ tl
>         x1 -> x1

-------------------------------------------------------------------------------

generate_spell_choice_actions

quite dynamic: need to read the data from the spells_mr table to
generate the names of the functions

approach:
first: remove the create function generate_spell_choice_actions, the
select generate_spel..., and the drop function so these aren't used at
all
second: at the point where generate_spell_choice_actions was called,
insert the create functions. To do this need to get the copydata for
the copy statement which set up the data in spells_mr, then read the
first cell from each line in the copy data string.

additional hack: just hardcode the spell names here since they are
unlikely to change for quite a while - fix this after doing the
readonly tables/ compile time constant relations stuff

> generateSpellChoiceActionsExample :: ExtensionTest
> generateSpellChoiceActionsExample =
>   ExtensionTest
>     "spellChoiceActions"
>     (generateSpellChoiceActionsRun ["horse"])
>     [sqlStmts| select generate_spell_choice_actions(); |]
>     [sqlStmts|
>
> CREATE FUNCTION action_choose_horse_spell() RETURNS void
>     LANGUAGE plpgsql
>     AS $$
> begin
>   perform check_can_run_action('choose_horse_spell');
>   perform action_choose_spell('horse');
> end;
> $$;
>
>      |]

> getSpellNames :: [String] --Data a => a -> [String]
> getSpellNames =   ["chaos"
>                   ,"dark_citadel"
>                   ,"dark_power"
>                   ,"decree"
>                   ,"disbelieve"
>                   ,"eagle"
>                   ,"elf"
>                   ,"faun"
>                   ,"ghost"
>                   ,"giant_rat"
>                   ,"giant"
>                   ,"goblin"
>                   ,"golden_dragon"
>                   ,"gooey_blob"
>                   ,"gorilla"
>                   ,"green_dragon"
>                   ,"gryphon"
>                   ,"harpy"
>                   ,"horse"
>                   ,"hydra"
>                   ,"justice"
>                   ,"king_cobra"
>                   ,"large_chaos"
>                   ,"large_law"
>                   ,"law"
>                   ,"lightning"
>                   ,"lion"
>                   ,"magic_armour"
>                   ,"magic_bolt"
>                   ,"magic_bow"
>                   ,"magic_castle"
>                   ,"magic_fire"
>                   ,"magic_knife"
>                   ,"magic_shield"
>                   ,"magic_sword"
>                   ,"magic_wings"
>                   ,"magic_wood"
>                   ,"manticore"
>                   ,"ogre"
>                   ,"orc"
>                   ,"pegasus"
>                   ,"raise_dead"
>                   ,"red_dragon"
>                   ,"shadow_form"
>                   ,"shadow_wood"
>                   ,"skeleton"
>                   ,"spectre"
>                   ,"subversion"
>                   ,"turmoil"
>                   ,"unicorn"
>                   ,"vampire"
>                   ,"vengeance"
>                   ,"wall"
>                   ,"wraith"
>                   ,"zombie"]
>
> generateSpellChoiceActions :: Data a => a -> a
> generateSpellChoiceActions = generateSpellChoiceActionsRun getSpellNames
>
> generateSpellChoiceActionsRun :: Data a => [String] -> a -> a
> generateSpellChoiceActionsRun spells =
>     transformBi $ \x ->
>       case x of
>         s@[sqlStmt| select generate_spell_choice_actions(); |] : tl
>             -> flip map spells (\spell ->
>                let actionname = "choose_" ++ spell ++ "_spell"
>                    wrappername = "action_" ++ actionname
>                in replaceSourcePos1 s [sqlStmt|
>
> create function $(wrappername)() returns void as $$
> begin
>   perform check_can_run_action($s(actionname));
>   perform action_choose_spell($s(spell));
> end;
> $$ language plpgsql volatile;
>
>                    |]) ++ tl
>         x1 -> x1

------------------------------

not null
--------

saves us putting not null all over almost every table definition

The _mr suffix was left over from an attempt to do some sort of poor
man's multirelation system.  The _mr tables can contain nullable as
well as non-nullable columns, so we leave them alone when setting non
nulls and ignore them when checking for nullable attributes.


> notNullExample1 :: ExtensionTest
> notNullExample1 =
>   ExtensionTest
>     "notNull1"
>     notNull
>     [sqlStmts|
>      create table t1 (
>        a text,
>        b int
>      ); |]
>     [sqlStmts|
>      create table t1 (
>        a text not null,
>        b int not null
>      );
>      |]

> notNullExample2 :: ExtensionTest
> notNullExample2 =
>   ExtensionTest
>     "notNull2"
>     notNull
>     [sqlStmts|
>      create table t1_mr (
>        a text,
>        b int
>      ); |]
>     [sqlStmts|
>      create table t1_mr (
>        a text,
>        b int
>      );
>      |]
> notNull :: Data a => a -> a
> notNull =
>     transformBi $ \x ->
>       case x of
>         CreateTable a nm@(Name _ nmcs) atts c | not ("_mr" `isSuffixOf` ncStr (last nmcs)) ->
>           CreateTable a nm (map fixAtt atts) c
>         x1 -> x1
>     where
>       -- this works because there are no null or not null
>       -- constraints in the source sql except in the tables
>       -- that are skipped (*_mr)
>       fixAtt (AttributeDef a n t d c) =
>          AttributeDef a n t d
>            (NotNullConstraint emptyAnnotation "" : c)

-------------------------------------------------------------------------------


> makeFKsCascadeExample1 :: ExtensionTest
> makeFKsCascadeExample1 =
>   ExtensionTest
>     "makeFKsCascade1"
>     makeFKsCascade
>     [sqlStmts|
>      create table t1 (
>        a text primary key
>      );
>      create table t2 (
>        a text primary key references t1
>      );
>     |]
>     [sqlStmts|
>      create table t1 (
>        a text primary key
>      );
>      create table t2 (
>        a text primary key references t1 on delete cascade on update cascade
>      );
>      |]
>
> makeFKsCascadeExample2 :: ExtensionTest
> makeFKsCascadeExample2 =
>   ExtensionTest
>     "makeFKsCascade2"
>     makeFKsCascade
>     [sqlStmts|
>      create table t1 (
>        a text primary key
>      );
>      create table t2 (
>        a text primary key,
>        foreign key (a) references t1
>      );
>     |]
>     [sqlStmts|
>      create table t1 (
>        a text primary key
>      );
>      create table t2 (
>        a text primary key,
>        foreign key (a) references t1 on update cascade on delete cascade
>      );
>      |]
>
> makeFKsCascade :: Data a => a -> a
> makeFKsCascade =
>     transformBi $ \x ->
>       case x of
>         Restrict -> Cascade
>         x1 -> x1

todo - fix these up to new style and get working.

also want to figure out how to support notifies on views and on
arbitrary selects in the client to support the ui code.

> -- | looks for calls to function set_relvar_type and adds triggers to prevent the
> -- referenced table from being updated
> {-addReadonlyTriggers :: Data a => a -> a
> addReadonlyTriggers =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "set_relvar_type" [StringLit _ _ tableName,StringLit _ _ "readonly"]):tl
>             -> (flip map "diu" ( \t ->
>                     (CreateFunction an ("check_" ++ tableName ++ "_" ++ [t] ++ "_readonly") []
>                                     (SimpleTypeName an "trigger") Plpgsql
>                                     "$a$"
>                                     (PlpgsqlFnBody an [] [
>                                       If an
>                                         [(FunCall an "!not" [BooleanLit an False]
>                                          ,[Raise an RException
>                                            "delete on base_relvar_metadata \
>                                            \violates transition constraint \
>                                            \base_relvar_metadata_d_readonly" []])] []
>                                      ,Return an $ Just $ NullLit an])
>                                     Volatile)) ++ tl)
>         x1 -> x1
> -- | add a trigger to each data table to raise a notify signal when changed
> addNotifyTriggers :: Data a => a -> a
> addNotifyTriggers =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "set_relvar_type" [StringLit _ _ tableName,StringLit _ _ "data"]):tl
>           -> (CreateFunction an (tableName ++ "_changed") []
>                                     (SimpleTypeName an "trigger") Plpgsql
>                                     "$a$"
>                                     (PlpgsqlFnBody an [] [
>                                       Notify an tableName
>                                      ,Return an $ Just $ NullLit an
>                                      ]) Volatile : tl)
>         x1 -> x1 -}

