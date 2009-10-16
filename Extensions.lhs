Copyright 2009 Jake Wheat

Experimental code to use uniplate to implement extensions

> {-# LANGUAGE ViewPatterns #-}
> module Extensions where

> import Data.Generics
> import Data.Generics.PlateData
> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Ast.Annotation



> data FunCallView = FUnit
>                  | FunCallView Annotation String [Expression]

> funCallView :: Statement -> FunCallView
> funCallView (SelectStatement an (Select _ _ (SelectList _ [SelExp _ (FunCall _ fnName
>               args)] []) Nothing Nothing [] Nothing [] Asc Nothing Nothing)) = (FunCallView an fnName args)
> funCallView _ = FUnit


>{-         (SelectStatement an (Select _ _ (SelectList _ [SelExp _ (FunCall _ "create_var"
>           [StringLit _ _ tableName,StringLit _ _ typeName])] [])
>            Nothing Nothing [] Nothing [] Asc Nothing Nothing):tl)-}

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
