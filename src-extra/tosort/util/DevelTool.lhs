Make the website,

To compile, use something like:

time ghc -threaded -XDeriveDataTypeable -DPOSTGRES -cpp -pgmPcpphs -optP--cpp -idevel:src:examples/chaos:examples/extensions/:examples/util/:tests/ --make devel/DevelTool

cd /home/jake/wd/hssqlppp/trunk && ghc -XTupleSections -XScopedTypeVariables -XDeriveDataTypeable -isrc:src-extra/examples:src-extra/tests:src-extra/util:src-extra/tosort/util src-extra/tosort/util/DevelTool.lhs -rtsopts -threaded


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
