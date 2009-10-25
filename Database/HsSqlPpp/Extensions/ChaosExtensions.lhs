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

> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Parsing.Lexer



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
>                addNotifyTriggers


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
>             -> ((CreateTable an
>                            (tableName ++ "_table")
>                            [AttributeDef an
>                                          tableName
>                                          (SimpleTypeName an typeName)
>                                          Nothing
>                                          []]
>                            [])
>                 : (CreateFunction an ("get_" ++ tableName) []
>                     (SimpleTypeName an typeName) Sql "$a$"
>                     (SqlFnBody an
>                      [SelectStatement an
>                       (Select an Dupes
>                        (SelectList an
>                         [SelExp an
>                          (Identifier an "*")] [])
>                        (Just (Tref an (tableName ++ "_table") NoAlias))
>                        Nothing [] Nothing [] Asc Nothing Nothing)]) Stable)
>                 : tl)
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
