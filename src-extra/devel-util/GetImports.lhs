
use haskell-src-exts to get the modules from a source file
can keep only the modules which correspond to local files
and also does transitive closure of local dependencies

> {-# LANGUAGE TupleSections #-}
> module GetImports (immediateFileDependencies,tcDependencies) where

> import System.Environment
> --import Language.Haskell.Exts
> import Data.List hiding (find)
> import System.FilePath.Find
> import System.FilePath
> import Data.Maybe
> import Debug.Trace
> import Data.IORef
> import Data.Char

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

get all the source files under the list of folders given

returned as pairs of root path, complete path to file under that root

> localSources :: [FilePath] -> IO [(FilePath, FilePath)]
> localSources dirs =
>   concat `fmap` mapM findHs dirs
>   where
>     findHs dir =
>        map ((dir,) . makeRelative dir) `fmap`
>        find (return True) (extension ==? ".lhs" ||? extension ==? ".hs") dir

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

> imports :: FilePath -> IO [String]
> imports fp = do
>   r <- readFile fp
>   return $ mapMaybe (im . words) $ lines r
>   where
>     im (x:_) | x /= "import" = Nothing
>     im (">":x:_) | x /= "import" = Nothing
>     im (">":"import":"qualified":nm:_) = Just nm
>     im (">":"import":nm:_) = Just nm
>     im ("import":"qualified":nm:_) = Just nm
>     im ("import":nm:_) = Just nm
>     im _ = Nothing
