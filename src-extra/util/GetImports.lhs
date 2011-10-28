
make file helper
1. ghc -M doesn't list the .o files needed for an exe
2. something is going wrong with the dependencies when building exes

only need to generate a .o list for each executable,
then write out sections as follows:

exe_file_name : all the os for that exe
	$(HC) $(HC_OPTS) -o exe_file_name all_the_os

this will also force the maintenance of correct package lists




> {-# LANGUAGE TupleSections #-}
> module GetImports (immediateFileDependencies,tcDependencies) where

> import System.Environment
> import Language.Haskell.Exts
> import Data.List hiding (find)
> --import Data.Char
> import System.FilePath.Find
> --import Debug.Trace
> import System.FilePath
> import Data.Maybe
> import Debug.Trace
> import Data.IORef

> {-main :: IO ()
> main = do
>   [f] <- getArgs
>   mapM_ (putStrLn . show) =<< immediateFileDependencies ["src","examples"] f
>   putStrLn "\n\n"
>   mapM_ (putStrLn . show) =<< tcDependencies ["src","examples"] f-}


> immediateFileDependencies :: [FilePath] -> FilePath -> IO [(String,FilePath)]
> immediateFileDependencies srcdirs f = do

take the file f, hs or lhs
get the modules
keep only the modules whose files can be found under one of dirs
return those filenames

>   -- get the list of import module names from the target file
>   imps <- imports f
>   -- get a complete list of all the source files in all the source
>   -- folders
>   lss <- localSources srcdirs
>   return $ ifdp lss imps

> ifdp :: [(FilePath,FilePath)] -> [String] -> [(String,FilePath)]
> ifdp lss = mapMaybe (matchFile lss)

get all dependencies for a filename, using transitive closure


> tcDependencies :: [FilePath] -> FilePath -> IO [(String,FilePath)]
> tcDependencies srcdirs f = do
>   -- cache isn't great because
>   -- doesn't cache between calls to tcDependencies
>   cache <- newIORef []
>   lss <- {-trace ("tc for " ++ f) $ -} localSources srcdirs
>   nub `fmap` recDs cache lss f
>   where
>     recDs cache lss fn = do
>         cv <- readIORef cache
>         (flocalimps,flocalmodfiles) <-
>           case lookup fn cv of
>             Just x -> return x
>             Nothing -> do
>               fimps <- {-trace ("is: " ++ fn) $ -}imports fn
>               let flocalimps = ifdp lss fimps
>                   flocalmodfiles = map snd flocalimps
>               writeIORef cache ((fn,(flocalimps,flocalmodfiles)):cv)
>               return (flocalimps,flocalmodfiles)
>         rs <- mapM (recDs cache lss) flocalmodfiles
>         return $ flocalimps ++ concat rs


find the file for the module named m in the file list errors if there
is more than one match, returns the module name plus the filename for
that module

> matchFile :: [(FilePath,FilePath)] -> String -> Maybe (String,FilePath)
> matchFile fs mn =
>   let fnBase = replaceStr "." "/" mn
>       hsMatches = filter ((== (fnBase `addExtension` "hs")) . snd) fs
>       lhsMatches = filter ((== (fnBase `addExtension` "lhs")) . snd) fs
>   in case hsMatches ++ lhsMatches of
>        [] -> Nothing
>        [(b,f)] -> Just (replaceStr "/" "."  $ dropExtension f
>                        ,b </> f)
>        x -> error $ "ambiguous module: " ++ show x

> imports :: FilePath -> IO [String]
> imports f = do
>   hm <- pf f
>   return $ map importName $ moduleImportDecls hm

get all the source files under the list of folders given

returned as pairs of root path, complete path to file under that root

> localSources :: [FilePath] -> IO [(FilePath, FilePath)]
> localSources dirs =
>   concat `fmap` mapM findHs dirs
>   where
>     findHs dir =
>        map ((dir,) . makeRelative dir) `fmap`
>        find (return True) (extension ==? ".lhs" ||? extension ==? ".hs") dir

> importName :: ImportDecl -> String
> importName m = (\(ModuleName n) -> n) $ importModule m

> moduleImportDecls :: Module -> [ImportDecl]
> moduleImportDecls (Module _ _ _ _ _ is _) = is


> pf :: FilePath -> IO Module
> pf f = do
>   x <- parseFileWithMode pm f
>   case x of
>         ParseOk ast -> return ast
>         e -> error $ show e
>   where
>     pm = defaultParseMode {
>            parseFilename = f
>          ,extensions = [PatternGuards,ScopedTypeVariables
>                        ,TupleSections,FlexibleContexts
>                        ,MultiParamTypeClasses]}

> replaceStr :: String -> String -> String -> String
> replaceStr _ _ [] = []
> replaceStr old new str' = loop str'
>   where
>     loop [] = []
>     loop str =
>       let (prefix, rest) = splitAt n str
>       in
>         if old == prefix                -- found an occurrence?
>         then new ++ loop rest           -- yes: replace it
>         else head str : loop (tail str) -- no: keep looking
>     n = length old
