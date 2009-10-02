Copyright 2009 Jake Wheat

Experimental code to use uniplate to implement extensions

> module Extensions where

> import Data.Generics
> import Data.Generics.PlateData
> import Database.HsSqlPpp.Ast.Ast


> rewriteCreateVars :: Data a => a -> a
> rewriteCreateVars =
>     transformBi $ \x ->
>       case x of
>         (SelectStatement an (Select _ _ (SelectList _ [SelExp _ (FunCall _ "create_var"
>           [StringLit _ _ tableName,StringLit _ _ typeName])] []) Nothing Nothing [] Nothing [] Asc Nothing Nothing):tl)
>             -> ((CreateTable an
>                            (tableName ++ "_table")
>                            [AttributeDef an
>                                          tableName
>                                          (SimpleTypeName an typeName)
>                                          Nothing
>                                          []]
>                            [])
>                 : (CreateFunction an Sql ("get_" ++ tableName) []
>                     (SimpleTypeName an typeName) "$a$"
>                     (SqlFnBody an
>                      [SelectStatement an
>                       (Select an Dupes
>                        (SelectList an
>                         [SelExp an
>                          (Identifier an "*")] [])
>                        (Just (Tref an (tableName ++ "_table")))
>                        Nothing [] Nothing [] Asc Nothing Nothing)]) Stable)
>                 : tl)
>         x1 -> x1



select create_var(name, type);
->
create table name_table (
    name type
);

create function get_name() returns type as $a$
  select * from name_table;
$a$ language sql stable;
