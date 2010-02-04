Copyright 2010 Jake Wheat

> {-# LANGUAGE ScopedTypeVariables #-}
> module Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
>     where

> import Data.Generics.Uniplate.Data
> import Data.Maybe

> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Utils.Utils


> data ExtensionTest = ExtensionTest String ([Statement] -> [Statement]) String String

> replaceSqlStrings :: [(String,String)] -> String -> [Statement]
> replaceSqlStrings reps sql =
>   case doit of
>          Left e -> error $ ppExpr e
>          Right s -> s
>   where
>     doit = do
>       ast <- parseSql "" sql
>       Right $ t ast
>     t = transformBi $ \x ->
>           case x of
>             (s::String) | s `elem` fstReps -> fromJust $ lookup s reps
>             _ -> x
>     fstReps = map fst reps

> data FunCallView = FUnit
>                  | FunCallView Annotation String [Expression]

> funCallView :: Statement -> FunCallView
> funCallView (SelectStatement an (Select _ _ (SelectList _ [SelExp _ (FunCall _ fnName
>               args)] []) [] Nothing [] Nothing [] Nothing Nothing)) = FunCallView an fnName args
> funCallView _ = FUnit
