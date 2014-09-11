
Used to build the website. To compile and run use

make website

> import System.Environment

> import MakeWebsite
> --import MakeAntiNodes

> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     ["sourcelinks"] -> sourceLinks
>     ["makewebsite"] -> makeWebsite
>     x -> error $ "don't understand " ++ show x
