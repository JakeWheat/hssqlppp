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
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Utils.Utils

Used for the examples/tests for each extensions.

> data ExtensionTest = ExtensionTest String
>                                    ([Statement] -> [Statement])
>                                    String
>                                    String

Helper function for working with the templates.

TODO: make this a quasi quote so that any parse errors are caught at
compile time

> readTemplate :: String -> [Statement]
> readTemplate sql =
>     case parseSql "" sql of
>       Right ast -> ast
>       Left e -> error $ ppExpr e

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

View pattern support to help match function calls in an ast.

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

