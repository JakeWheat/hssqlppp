
generate the sections of the makefile for the exes
reads the list of exes from the Makefile
creates the complete list of object files for each exe using
haskell-src-exts

exe : obj_files
	$(HC) $(HC_OPTS) -o exe obj_files

> import Data.List
> import Data.Char
> import GetImports
> import Debug.Trace
> import System.FilePath
> import System.Environment
> import System.Exit

expects: outputfilename ([sourcepaths],[exefilenames])
the source files for the exes are found by adding .lhs

> main :: IO ()
> main = do
>   args <- getArgs
>   let (mkfn,srcs,exes) = case args of
>         [] -> error "no output filename given"
>         (mkfn:as) -> let (srcs,exes') = break (=="EXES") as
>                          exes = drop 1 exes'
>                      in case () of
>                           _ | null srcs -> error $ "no source folders given"
>                             | null exes -> error $ "no exes given"
>                             | otherwise -> (mkfn,srcs,exes)
>   ts <- mapM (makeTg srcs) exes
>   writeFile mkfn $ intercalate "\n" ts
>   return ()
>   where
>     makeTg srcs f = trace ("deps for " ++ f) $ do
>       deps <- tcDependencies srcs (f ++ ".lhs")
>       let objs = (f `replaceExtension` "o")
>                  : map ((`replaceExtension` "o") . snd) deps
>           objstr = intercalate " \\\n" objs
>       return $ f ++ " : " ++ objstr
>                 ++ "\n\t$(HC) $(HC_OPTS) -o " ++ f ++ " " ++ objstr
