
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

> main :: IO ()
> main = do
>   exes <- getExes
>   --mapM_ putStrLn exes
>   ts <- mapM makeTg exes
>   writeFile "exe_rules.mk" $ intercalate "\n" ts
>   return ()
>   where
>     makeTg f = trace ("deps for " ++ f) $ do
>       deps <- tcDependencies ["src"
>                              ,"src-extra/util"
>                              ,"src-extra/tests/"
>                              ,"src-extra/devel-util"
>                              ,"src-extra/chaos"
>                              ,"src-extra/chaos/extensions"
>                              ,"src-extra/examples"
>                              ,"src-extra/h7c"
>                              ,"src-extra/tosort/util/"] (f ++ ".lhs")
>       let objs = (f `replaceExtension` "o")
>                  : map ((`replaceExtension` "o") . snd) deps
>           objstr = intercalate " \\\n" objs
>       return $ f ++ " : " ++ objstr
>                 ++ "\n\t$(HC) $(HC_OPTS) -o " ++ f ++ " " ++ objstr
>     getExes :: IO [String]
>     getExes = do
>       f <- readFile "Makefile"
>       let ls = lines f
>           st = dropWhile (not . isPrefixOf "EXE_FILES =") ls
>           en = keepExtended st
>           fs = (\(x:xs) -> drop (length "EXE_FILES =") x : xs) en
>           ns = map (trim . rmSl) fs
>       return ns
>     rmSl x = if last x == '\\'
>              then init x
>              else x
>     trim = t . reverse . t . reverse
>     t = dropWhile isSpace
>     keepExtended :: [String] -> [String]
>     keepExtended (x:xs) = if last x /= '\\'
>                              then [x]
>                              else x : keepExtended xs
>     keepExtended _ = error "reading EXE_FILES value"
