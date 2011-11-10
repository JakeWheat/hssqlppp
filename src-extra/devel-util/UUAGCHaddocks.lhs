

Input: a list of ag files (doesn't follow includes)
A list of data type names
A .hs filename

1) lines the ag files
for each data type name:
  2) finds the lines starting with data ... for each data type name
  3) goes backwards to include prefix haddocks and forwards to end of data definition
     - this produces a list of lines for each data type in question
  4) parses this out to a haskell ast including haddock -> converting to record syntax

5) loads the hs file
6) finds the data declarations in the hs (no obvious way to parse then
pretty print with comments using can't use -src-exts)
7) replaces them with the haddock'd defs from above

assume all datatypes have deriving eq,show,typeable,data so don't have
to parse or match this

> module UUAGCHaddocks where

> import System.FilePath
> import System.Directory
> import Debug.Trace
> import Data.List
> import Data.Char
> import Data.Maybe

> postprocessUuagc :: [FilePath] -> [String] -> FilePath -> IO ()
> postprocessUuagc agFiles dataTypeNames hsFile = do
>   agSources <- mapM readFile agFiles
>   hsSource <- readFile hsFile

>   --putStrLn $ intercalate "\n\n" $ map snd $ defs agSources dataTypeNames


>   let tfn = hsFile ++ ".tmp"
>   writeFile tfn $ postProcessUuagc' agSources dataTypeNames hsSource
>   renameFile tfn hsFile


> postProcessUuagc' :: [String] -> [String] -> String -> String
> postProcessUuagc' agSources dataTypeNames hsSource =
>   unlines $ replaceDef (head $ defs agSources dataTypeNames) $ lines hsSource
>   where
>     replaceDef :: (String,String) -> [String] -> [String]
>     replaceDef (n,def) src =
>       let (s,e) = break (isPrefixOf $ "data " ++ n) src
>           e' = dropWhile startsWithSpace $ tail e
>       in s ++ [def] ++ e'

>     startsWithSpace l =
>       case l of
>              c:_ | isSpace c -> True
>              _ -> False


take the source of a uuagc data definition, and convert to haskell
source, preserving comments

make a lot of assumptions:
can find the first line starting with data, replace the first | on
that line with = and it works
each other line which needs fixing follows this pattern:
[WS]+ attname :: [Type or {Type}]
-> this is translated to
[WS+++] attname :: Type
(without the comma for the first one)


> toHaskell :: String -> String
> toHaskell x = unlines $
>               (addCommas $ (map modline $ lines x))
>               ++ ["    } deriving (Eq,Data,Typeable,Show)"]
>   where
>     modline l | "data" `isPrefixOf` l =
>       let i = fromMaybe (error $ "data def line not found in " ++ x)
>               $ findIndex (=='|') l
>           (s,e) = splitAt i l
>       in s ++ ('=' : tail e) ++ " {"
>     modline l | (h:_) <- dropWS l
>               , isAlpha h =
>       filter (\l -> l `notElem` "{}") l
>     modline l = l
>     dropWS = dropWhile isSpace
>     countWS = length . takeWhile isSpace
>     addCommas [] = []
>     addCommas x = let (s,(e:es)) = break ("data" `isPrefixOf`) x
>                   in s ++ [e] ++ addComma True es
>     addComma f (x:xs) | (h:_) <- dropWS x
>                       , isAlpha h =
>                           if f
>                           then x : addComma False xs
>                           else let (s,e) = span isSpace x
>                                in (s ++ "," ++ e) : addComma False xs
>                       | otherwise = x : addComma f xs
>     addComma _ [] = []

> defs :: [String] -> [String] -> [(String,String)]
> defs agSources dataTypeNames =
>   map def dataTypeNames
>   where
>     -- assume the ag files are 'standalone' so concatting them
>     -- in any order is ok
>     ls = concatMap lines agSources
>     -- replace ls with array/vector since working backwards
>     def :: String -> (String,String)
>     def nm = let si = fromMaybe (error $ "definition not found: " ++ nm)
>                       $ findIndex (isPrefixOf ("data " ++ nm)) ls
>                  (prefix,start) = splitAt si ls
>                  -- go backwards until find a line which isn't empty or a comment
>                  prefcom = reverse $ takeWhile spaceOrComment $ reverse prefix
>                  thisdef = case start of
>                              l:tl -> prefcom ++ l : getRest tl
>                              [] -> error $ "definition not found: " ++ nm
>              in (nm,toHaskell $ unlines thisdef)
>     -- keep going till hit a line which doesn't start with ws
>     -- and isn't a comment
>     getRest = takeWhile startsWithSpaceOrComment
>     startsWithSpaceOrComment l =
>       case l of
>              [] -> True
>              c:_ | isSpace c -> True
>              c:d:_ | c `elem` ['{','-']
>                      && d == '-' -> True
>              _ -> False
>     spaceOrComment l =
>       case l of
>              [] -> True
>              xs | all isSpace xs -> True
>              c:d:_ | c `elem` ['{','-']
>                      && d == '-' -> True
>              _ -> False
