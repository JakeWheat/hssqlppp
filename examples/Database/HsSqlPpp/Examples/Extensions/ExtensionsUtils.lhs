Copyright 2010 Jake Wheat

Some auxiliary code for use in writing extensions.

> module Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
>     where
>
> import Data.Generics.Uniplate.Data
> import Data.Maybe
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation

Used for the examples/tests for each extensions.

> data ExtensionTest = ExtensionTest String
>                                    ([Statement] -> [Statement])
>                                    [Statement]
>                                    [Statement]

Simple ast transforms, just substitutes the strings given in the reps
(replacements) lookup.

> mapStrings :: [(String,String)] -> [Statement] -> [Statement]
> mapStrings reps =
>   transformBi $ \x ->
>       case x of
>             s | s `elem` fstReps -> fromJust $ lookup s reps
>             x1 -> x1
>   where
>     fstReps = map fst reps

View pattern support to help match function calls in an ast. TODO: try
quasipattern instead

> data FunCallView = FUnit
>                  | FunCallView Annotation String [Expression]
>
> funCallView :: Statement -> FunCallView
> funCallView (SelectStatement an
>              (Select _ _
>               (SelectList _ [SelExp _ (FunCall _ fnName args)] [])
>               []
>               Nothing
>               []
>               Nothing
>               []
>               Nothing
>               Nothing)) = FunCallView an fnName args
> funCallView _ = FUnit
