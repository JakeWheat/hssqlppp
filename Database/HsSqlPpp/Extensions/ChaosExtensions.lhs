Copyright 2009 Jake Wheat

Experimental code to use uniplate to implement extensions

> {-# LANGUAGE ViewPatterns #-}
> module Database.HsSqlPpp.Extensions.ChaosExtensions where

> import Data.Generics
> import Data.Generics.PlateData
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
>               args)] []) Nothing Nothing [] Nothing [] Asc Nothing Nothing)) = (FunCallView an fnName args)
> funCallView _ = FUnit


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
>                transitionConstraints


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
>                        (Just (Tref an (tableName ++ "_table") NoAlias))
>                        Nothing [] Nothing [] Asc Nothing Nothing)]) Stable)]
>                ++ createKey an (tableName ++ "_table") [tableName]
>                ++ createConstraint True an [tableName ++ "_table"] (tableName ++ "_table_01_tuple") "true"
>                ++ tl
>         x1 -> x1

================================================================================

> addReadonlyTriggers :: Data a => a -> a
> addReadonlyTriggers =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "set_relvar_type" [StringLit _ _ tableName,StringLit _ _ "readonly"]):tl
>             -> (flip map "diu" ( \t ->
>                     ((CreateFunction an ("check_" ++ tableName ++ "_" ++ (t:[]) ++ "_readonly") []
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
>                                     Volatile))) ++ tl)
>         x1 -> x1

================================================================================

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
>       parseActionCall s = parseCcawac s

> parseCcawac :: String -> (String,[String])
> parseCcawac s = case runParser ccawac [] "" s of
>                   Left e -> trace ("failed to parse " ++ s) $ error $ show e
>                   Right r -> r

> ccawac :: ParsecT String LexState Identity (String, [[Char]])
> ccawac = (,)
>          <$> identifierString
>          <*> parens (sepBy ccawacarg (symbol ","))

> ccawacarg :: ParsecT String LexState Identity [Char]
> ccawacarg = (symbol "'" *> many (noneOf "'") <* symbol "'")
>             <|> identifierString

> parens :: ParsecT String LexState Identity a -> ParsecT String LexState Identity a
> parens = between (symbol "(") (symbol ")")

> symbol :: String -> ParsecT String LexState Identity String
> symbol = P.symbol lexer

> lexer :: P.GenTokenParser String LexState Identity
> lexer = P.makeTokenParser emptyDef

================================================================================

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
> parseExpressionWrap s = case parseExpression s of
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
>         (stripAnnotations $ parseExpressionWrap expr)
>       ]) Stable :
>     if tr
>       then concatMap (\t -> [DropFunction an IfExists [(t ++ "_constraint_trigger_operator",[])] Restrict
>                ,CreateFunction an (t ++ "_constraint_trigger_operator") []
>                                    (SimpleTypeName an "trigger") Plpgsql "$a$"
>                                    (PlpgsqlFnBody an [] [
>                                                        NullStatement an]) Volatile]
>                                    ) tbls
>       else []

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
> getStrings cols = map (\(StringLit _ _ c) -> c) cols

> createKey :: Annotation
>           -> String
>           -> [String]
>           -> [Statement]
> createKey an tableName colNames =
>    createConstraint False an [tableName] (tableName ++ "_" ++ intercalate "_" colNames ++ "_key") "true"

> zeroOneTuple :: Data a => a -> a
> zeroOneTuple =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "constrain_to_zero_or_one_tuple"
>                           [StringLit _ _ tableName]):tl
>           -> createConstraint True an [tableName] (tableName ++ "_01_tuple") "true" ++ tl
>         x1 -> x1

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

================================================================================

> noDelIns :: Data a => a -> a
> noDelIns =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView an "no_deletes_inserts_except_new_game"
>                           [StringLit _ _ tbl]):tl
>           -> (flip map ["_no_delete","_no_insert"] $ \n ->
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
