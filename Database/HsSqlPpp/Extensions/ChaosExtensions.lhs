Copyright 2009 Jake Wheat

Experimental code to use uniplate to implement extensions

> {-# LANGUAGE ViewPatterns #-}

> {- | Experimental code to half implement some simple syntax
>      extensions for plpgsql. Eventually, want to use this to write
>      macro type things for plpgsql, have implementation of all these
>      in pure plpgsql code but it's a mess, and the plpgsql
>      implementation is opaque to the hssqlppp type checker.
>  -}

> module Database.HsSqlPpp.Extensions.ChaosExtensions
>     (
>      extensionize
>     ,addReadonlyTriggers
>     ,rewriteCreateVars
>     ,createClientActionWrapper
>     ,addNotifyTriggers
>     ,addConstraint
>     ,addKey
>     ,addForeignKey
>     ,zeroOneTuple
>     ,noDelIns
>     ,transitionConstraints
>     ,replaceGenerateSpellChoiceActions
>     ) where

> import Data.Generics
> import Data.Generics.Uniplate.Data
> import Debug.Trace

> import Text.Parsec hiding(many, optional, (<|>))
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language

> import Control.Applicative
> import Control.Monad.Identity
> import Data.List

> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Parsing.Lexer
> import Database.HsSqlPpp.Parsing.Parser



> data FunCallView = FUnit
>                  | FunCallView Annotation String [Expression]

> funCallView :: Statement -> FunCallView
> funCallView (SelectStatement an (Select _ _ (SelectList _ [SelExp _ (FunCall _ fnName
>               args)] []) [] Nothing [] Nothing [] Nothing Nothing)) = FunCallView an fnName args
> funCallView _ = FUnit

> -- | run all the extensions in this module on the given ast
> extensionize :: Data a => a -> a
> extensionize = addReadonlyTriggers .
>                rewriteCreateVars .
>                createClientActionWrapper .
>                addNotifyTriggers .
>                addConstraint .
>                addKey .
>                addForeignKey .
>                zeroOneTuple .
>                noDelIns .
>                transitionConstraints .
>                replaceGenerateSpellChoiceActions


================================================================================

My dodgy SQL code uses a function called create_var to create a table
with a single attribute, which can hold 0 or 1 tuples (it adds the
constraints to handle all this), a bit like a global maybe
variable. It also creates a wrapper function to return the value from
the table which is sometimes more convenient to use in expressions
instead of e.g. a join.
e.g.
select create_var('name', 'type');
->
create table name_table (
    name type
);

create function get_name() returns type as $a$
  select * from name_table;
$a$ language sql stable;
[missing constraint stuff...]

When trying to type check these files, my code can't work out that the
create_var invocation changes the catalog in this way, as a hack to
get round this tried to write a little generic function which
transforms the create var invocation to the create table and create
function by transforming the ast tree before it is type checked. With
this in place, the code can type check these tables.

The future plan is to get rid of the create var function definition in
the sql entirely, and just use this macro feature instead. (so we
still call create_var in the same way). The main motivation is to make
type checking the code easier (the other obvious alternative is to
write a partial interpreter for pl/pgsql which sounds like an immense
amount of work in comparison).

> -- | short cut for creating a table with exactly one attribute whose
> -- cardinality is restricted to 0 or 1. Also provides a get_var function shortcut.
> rewriteCreateVars :: Data a => a -> a
> rewriteCreateVars =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "create_var" [StringLit _ _ tableName,StringLit _ _ typeName]):tl
>             -> [(CreateTable an
>                            (tableName ++ "_table")
>                            [AttributeDef an
>                                          tableName
>                                          (SimpleTypeName an typeName)
>                                          Nothing
>                                          []]
>                            [])
>                 ,(CreateFunction an ("get_" ++ tableName) []
>                     (SimpleTypeName an typeName) Sql "$a$"
>                     (SqlFnBody an
>                      [SelectStatement an
>                       (Select an Dupes
>                        (SelectList an
>                         [SelExp an
>                          (Identifier an "*")] [])
>                        [Tref an (tableName ++ "_table") NoAlias]
>                        Nothing [] Nothing [] Nothing Nothing)]) Stable)]
>                ++ createKey an (tableName ++ "_table") [tableName]
>                ++ createConstraint True an [tableName ++ "_table"] (tableName ++ "_table_01_tuple") "true"
>                ++ tl
>         x1 -> x1

================================================================================

> -- | looks for calls to function set_relvar_type and adds triggers to prevent the
> -- referenced table from being updated
> addReadonlyTriggers :: Data a => a -> a
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

================================================================================

> -- | a numpty currying type thing for plpgsql functions
> createClientActionWrapper :: Data a => a -> a
> createClientActionWrapper =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "create_client_action_wrapper" [StringLit _ _ actname,StringLit _ _ actcall]):tl
>           -> (CreateFunction an ("action_" ++ actname) []
>                                     (SimpleTypeName an "void") Plpgsql
>                                     "$a$"
>                                     (PlpgsqlFnBody an [] [
>                                       let (n,as) = parseActionCall actcall
>                                       in Perform an (FunCall an ("action_" ++ n) (map (StringLit an "'") as))
>                                      ]) Volatile : tl)
>         x1 -> x1
>     where
>       parseActionCall :: String -> (String,[String])
>       parseActionCall = parseCcawac

> parseCcawac :: String -> (String,[String])
> parseCcawac s = case runParser ccawac [] "" s of
>                   Left e -> trace ("failed to parse " ++ s) $ error $ show e
>                   Right r -> r

> ccawac :: ParsecT String LexState Identity (String, [String])
> ccawac = (,)
>          <$> identifierString
>          <*> parens (sepBy ccawacarg (symbol ","))

> ccawacarg :: ParsecT String LexState Identity String
> ccawacarg = (symbol "'" *> many (noneOf "'") <* symbol "'")
>             <|> identifierString

> parens :: ParsecT String LexState Identity a -> ParsecT String LexState Identity a
> parens = between (symbol "(") (symbol ")")

> symbol :: String -> ParsecT String LexState Identity String
> symbol = P.symbol lexer

> lexer :: P.GenTokenParser String LexState Identity
> lexer = P.makeTokenParser emptyDef

================================================================================

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

================================================================================

> -- | wrapped for the extended constraint system - allows adding constraints
> -- which refer to multiple rows/ multiple tables.
> addConstraint :: Data a => a -> a
> addConstraint =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "add_constraint"
>                           [StringLit _ _ conName
>                           ,StringLit _ _ expr
>                           ,FunCall _ "!arrayctor" tbls]):tl
>           -> createConstraint True an (getStrings tbls) conName expr ++ tl
>         x1 -> x1

> parseExpressionWrap :: String -> Expression
> parseExpressionWrap s = case parseExpression "" s of
>                           Left e -> trace ("parsing expression: " ++ s) $ error $ show e
>                           Right ast -> ast

> createConstraint :: Bool
>                  -> Annotation
>                  -> [String]
>                  -> String
>                  -> String
>                  -> [Statement]
> createConstraint tr an tbls name expr =
>     CreateFunction an ("check_con_" ++ name) []
>      (SimpleTypeName an "boolean") Plpgsql "$a$"
>      (PlpgsqlFnBody an [] [
>         Return an $ Just $
>         stripAnnotations (parseExpressionWrap expr)
>       ]) Stable :
>     if tr
>       then concatMap (\t -> [DropFunction an IfExists [(t ++ "_constraint_trigger_operator",[])] Restrict
>                ,CreateFunction an (t ++ "_constraint_trigger_operator") []
>                                    (SimpleTypeName an "trigger") Plpgsql "$a$"
>                                    (PlpgsqlFnBody an [] [
>                                                        NullStatement an]) Volatile]
>                                    ) tbls
>       else []

> -- | implement key constraints so that they are integrated with the extended constraint
> -- system, uses pg keys internally.
> addKey :: Data a => a -> a
> addKey =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "add_key"
>                           [StringLit _ _ tableName
>                           ,StringLit _ _ columnName]):tl
>           -> createKey an tableName [columnName] ++ tl
>         (funCallView -> FunCallView an "add_key"
>                           [StringLit _ _ tableName
>                           ,FunCall _ "!arrayctor" cols]):tl
>           -> createKey an tableName (getStrings cols) ++ tl
>         x1 -> x1

> getStrings :: [Expression] -> [String]
> getStrings = map (\(StringLit _ _ c) -> c)

> createKey :: Annotation
>           -> String
>           -> [String]
>           -> [Statement]
> createKey an tableName colNames =
>    let cn = take 49 (tableName ++ "_" ++ intercalate "_" colNames) ++ "_key"
>    in createConstraint False an [tableName] cn "true"

> -- | extended inclusion dependency (I think that is the fashionable term these days).
> -- to allow 'foreign keys' to non key attributes, as well as regular foreign keys
> addForeignKey :: Data a => a -> a
> addForeignKey =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "add_foreign_key"
>                           [StringLit _ _ tableName
>                           ,StringLit _ _ colName
>                           ,StringLit _ _ ttableName]):tl
>           -> createFk an tableName [colName] ttableName [colName] ++ tl
>         (funCallView -> FunCallView an "add_foreign_key"
>                           [StringLit _ _ tableName
>                           ,FunCall _ "!arrayctor" cols
>                           ,StringLit _ _ ttableName]):tl
>           -> createFk an tableName (getStrings cols) ttableName (getStrings cols) ++ tl
>         (funCallView -> FunCallView an "add_foreign_key"
>                           [StringLit _ _ tableName
>                           ,StringLit _ _ colName
>                           ,StringLit _ _ ttableName
>                           ,StringLit _ _ tcolName]):tl
>           -> createFk an tableName [colName] ttableName [tcolName] ++ tl
>         (funCallView -> FunCallView an "add_foreign_key"
>                           [StringLit _ _ tableName
>                           ,FunCall _ "!arrayctor" cols
>                           ,StringLit _ _ ttableName
>                           ,FunCall _ "!arrayctor" tcols]):tl
>           -> createFk an tableName (getStrings cols) ttableName (getStrings tcols) ++ tl
>         x1 -> x1
>     where
>       createFk an tbl atts _ _  =
>           createConstraint False an [tbl] (tbl ++ "_" ++ intercalate "_" atts ++ "_fkey") "true"

> -- | convert calls to zero or one tuple to constraint
> zeroOneTuple :: Data a => a -> a
> zeroOneTuple =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "constrain_to_zero_or_one_tuple"
>                           [StringLit _ _ tableName]):tl
>           -> createConstraint True an [tableName] (tableName ++ "_01_tuple") "true" ++ tl
>         x1 -> x1

================================================================================

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

================================================================================

> transitionConstraints :: Data a => a -> a
> transitionConstraints =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "create_update_transition_tuple_constraint"
>                           [StringLit _ _ _
>                           ,StringLit _ _ cn
>                           ,StringLit _ _ _]):tl
>           -> addTrigger an cn ++ tl
>         (funCallView -> FunCallView an "create_insert_transition_tuple_constraint"
>                           [StringLit _ _ _
>                           ,StringLit _ _ cn
>                           ,StringLit _ _ _]):tl
>           -> addTrigger an cn ++ tl
>         (funCallView -> FunCallView an "create_delete_transition_tuple_constraint"
>                           [StringLit _ _ _
>                           ,StringLit _ _ cn
>                           ,StringLit _ _ _]):tl
>           -> addTrigger an cn ++ tl
>         x1 -> x1
>     where
>       addTrigger an n =
>           [CreateFunction an ("check_" ++ n) []
>                (SimpleTypeName an "trigger") Plpgsql "$a$"
>                (PlpgsqlFnBody an [] [
>                 NullStatement an
>                 ]) Stable]

================================================================================

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


> replaceGenerateSpellChoiceActions :: Data a => a -> a
> replaceGenerateSpellChoiceActions d = let sn = getSpellNames d
>                                       in replaceGenerateSpellChoiceActionsInt sn d

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
>                                           ]) Volatile

CREATE FUNCTION action_choose_horse_spell() RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  perform check_can_run_action('choose_horse_spell');
  perform action_choose_spell('horse');
end;
$$;

