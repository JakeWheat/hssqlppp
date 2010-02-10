Copyright 2010 Jake Wheat

Some auxiliary code for use in writing extensions.

> module Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
>     where
>
> import Data.Generics.Uniplate.Data
> import Data.Maybe
>
> import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Annotation

Used for the examples/tests for each extensions.

> data ExtensionTest = ExtensionTest String
>                                    ([Statement] -> [Statement])
>                                    [Statement]
>                                    [Statement]
