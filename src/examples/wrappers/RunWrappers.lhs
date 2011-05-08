Code to run the example wrappers.

> import Database.HsSqlPpp.Examples.Wrappers.ThTupleExample as TT
>
> main :: IO();
> main = do
>   TT.runTest >>= putStrLn
>