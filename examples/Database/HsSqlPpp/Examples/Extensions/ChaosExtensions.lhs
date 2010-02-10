Copyright 2009 Jake Wheat

Here are a few Chaos2000 specific extensions, written just to get
chaos working again then will review approach.

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Examples.Extensions.ChaosExtensions
>     (
>      chaosExtensions
>     ,chaosExtensionsExamples
>     ) where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
> import Debug.Trace
>
> import Text.Parsec hiding(many, optional, (<|>))
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language
>
> import Control.Applicative
> import Control.Monad.Identity
> import Data.List
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Parsing.Lexer
> import Database.HsSqlPpp.Parser
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote
>
> -- | run all the extensions in this module on the given ast
> chaosExtensions :: Data a => a -> a
> chaosExtensions = clientActionWrapper

> chaosExtensionsExamples :: [ExtensionTest]
> chaosExtensionsExamples = [clientActionWrapperExample]

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
>     [$sqlStmts|
>         select create_client_action_wrapper('move_cursor_down'
>                                            ,$$move_cursor('down')$$);|]
>     [$sqlStmts|
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
>         [$sqlStmt|
>            select create_client_action_wrapper($s(actname)
>                                               ,$s(actcall)); |]
>             -> let actionname = "action_" ++ actname
>                    expr = case parseExpression "" ("action_" ++ actcall) of
>                             Left e -> error $ show e
>                             Right e1 -> e1
>                in [$sqlStmt|
>                    create function $(actionname)() returns void as $$
>                    begin
>                      perform $(expr);
>                    end;
>                    $$ language plpgsql volatile;
>                    |]
>         x1 -> x1
>





> {-createVarSimpleExample :: ExtensionTest
> createVarSimpleExample =
>   ExtensionTest
>
>     -- name of the extension, used when running the tests
>     "CreateVarSimple"
>
>     -- the transformation function itself, implemented below
>     createVarSimple
>
>     -- example of the SQL we want to replace
>     [$sqlStmts| select create_var('varname', 'vartype'); |]
>
>     -- what the example SQL should be transformed into:
>     [$sqlStmts|
>
>       create table varname_table (
>         varname vartype
>       );
>
>      |]

Ast transform function
----------------------

We look for a function call with the function name "create_var" and
two string literal arguments (calls to create_var which don't use
exactly two string literals will be silently ignored by this code).

We want to replace it with a create table statement.

> createVarSimple :: Data a => a -> a
> createVarSimple =
>     transformBi $ \x ->
>       case x of
>         [$sqlStmt| select create_var($s(varname), $s(typename)); |] : tl
>             -> let tablename = varname ++ "_table"
>                in [$sqlStmts|
>
>   create table $(tablename) (
>    $(varname) $(typename)
>   );
>
>                    |] ++ tl
>         x1 -> x1
> -}








-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

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
>         x1 -> x1

-------------------------------------------------------------------------------

> noDelIns :: Data a => a -> a
> noDelIns =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "no_deletes_inserts_except_new_game"
>                           [StringLit _ _ tbl]):tl
>           -> flip map ["_no_delete","_no_insert"] (\n ->
>                CreateFunction an ("check_" ++ tbl ++ n) []
>                (SimpleTypeName an "trigger") Plpgsql "$a$"
>                (PlpgsqlFnBody an [] [
>                                  NullStatement an
>                                 ]) Stable) ++ tl
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
unlikely to change for quite a while

> getSpellNames :: Data a => a -> [String]
> getSpellNames _ = ["chaos"
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
>
> replaceGenerateSpellChoiceActions :: Data a => a -> a
> replaceGenerateSpellChoiceActions d = let sn = getSpellNames d
>                                       in replaceGenerateSpellChoiceActionsInt sn d
>
> replaceGenerateSpellChoiceActionsInt :: Data a => [String] -> a -> a
> replaceGenerateSpellChoiceActionsInt spellNames =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "generate_spell_choice_actions" []):tl
>           -> map (createChoiceFn an) spellNames ++ tl
>         (CreateFunction _ "generate_spell_choice_actions" _ _ _ _ _ _):tl
>           -> tl
>         (DropFunction _ _ [("generate_spell_choice_actions",[])] _):tl
>           -> tl
>         x1 -> x1
>     where
>       createChoiceFn an sn =
>         CreateFunction an ("action_choose_" ++ sn ++ "_spell")
>                        []
>                        (SimpleTypeName [] "void")
>                        Plpgsql
>                        "$$"
>                        (PlpgsqlFnBody an [] [
>                                            NullStatement an
>                                           ]) Volatile -}

CREATE FUNCTION action_choose_horse_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_horse_spell');
  perform action_choose_spell('horse');
end;
$$;

