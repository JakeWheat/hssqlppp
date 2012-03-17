

make file helper

ghc -M doesn't list the .o files needed for an exe
this does that

pass the output filename (this file is overwritten with no backup
  currently!), the list of source folders, 'EXES' then the list of exe names
the sources for the exes are found by appending lhs, TODO: find either
  lhs or hs

example:

GenerateExeRules exe_rules.mk src src-extra/util src-extra/tests/ src-extra/devel-util src-extra/chaos src-extra/chaos/extensions/ src-extra/examples src-extra/h7c src-extra/tosort/util/ src-extra/extensions src-extra/h7c src-extra/chaos/extensions EXES src-extra/tests/Tests src-extra/devel-util/MakeDefaultTemplate1Catalog src-extra/examples/FixSqlServerTpchSyntax src-extra/examples/MakeSelect src-extra/examples/Parse src-extra/examples/Parse2 src-extra/examples/QQ src-extra/tosort/util/DevelTool

the weird command line format is to make it easy to construct the
argument list in a makefile

outputs for each exe:

exe : obj_files
	$(HC) $(HC_LINK_OPTS) -o exe obj_files

ready to include in your makefile

> import Data.List
> import Data.Char
> import GetImports
> import Debug.Trace
> import System.FilePath
> import System.Environment
> import System.Exit

expects: outputfilename HC_VARNAME HC_OPTS_VARNAME ([sourcepaths],[exefilenames])
the source files for the exes are found by adding .lhs

> main :: IO ()
> main = do
>   args <- getArgs
>   let (mkfn,hc, hcOpts,srcs,exes) = case args of
>         (mkfn:hc:hcOpts:as) ->
>            let (srcs,exes') = break (=="EXES") as
>                exes = drop 1 exes'
>            in case () of
>                       _ | null srcs -> error $ "no source folders given"
>                         | null exes -> error $ "no exes given"
>                         | otherwise -> (mkfn,hc,hcOpts,srcs,exes)
>         _ -> error "bad args"
>   ts <- mapM (makeTg srcs hc hcOpts) exes
>   writeFile mkfn $ intercalate "\n" ts
>   return ()
>   where
>     makeTg srcs hc hcOpts f = trace ("deps for " ++ f) $ do
>       deps <- tcDependencies srcs (f ++ ".lhs")
>       let objs = (f `replaceExtension` "o")
>                  : map ((`replaceExtension` "o") . snd) deps
>           objstr = intercalate " \\\n" objs
>       return $ f ++ " : " ++ objstr
>                 ++ "\n\t$(" ++ hc ++ ") $(" ++ hcOpts ++ ") -o " ++ f ++ " " ++ objstr
