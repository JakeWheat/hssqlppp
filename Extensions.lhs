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
