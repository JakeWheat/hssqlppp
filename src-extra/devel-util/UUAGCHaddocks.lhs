

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

plan for now is just to do this for the annotation type and the select
ctor of queryexpr. Maybe will try to do it for most ctors in the ast
at some point soon.

> module UUAGCHaddocks where

> import System.FilePath
> import System.Directory
> import Debug.Trace
> import Data.List
> import Data.Char
> import Data.Maybe

> import Text.Parsec hiding(many, optional, (<|>))
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language
> --import Text.Parsec.String
> import Text.Parsec.Pos
>
> import Control.Applicative
> import Control.Monad.Identity

> postprocessUuagc :: [FilePath] -> [(String,[String])] -> FilePath -> IO ()
> postprocessUuagc agFiles dataTypeNames hsFile = do
>   agSources <- mapM readFile agFiles
>   hsSource <- readFile hsFile

>   --putStrLn $ intercalate "\n\n" $ map snd $ defs agSources dataTypeNames


>   let tfn = hsFile ++ ".tmp"
>   writeFile tfn $ postProcessUuagc' agSources dataTypeNames hsSource
>   renameFile tfn hsFile


> postProcessUuagc' :: [String] -> [(String,[String])] -> String -> String
> postProcessUuagc' agSources dataTypeNames hsSource =
>   unlines
>   $ foldr replaceDef (lines hsSource)
>   $ defs agSources dataTypeNames
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


> {-toHaskell :: DefInfo -> String
> toHaskell di =
>   unlines (prefixComments di
>            ++ cts
>            ++ ["    deriving (Eq,Data,Typeable,Show)"])
>   where
>     cts :: [String]
>     cts = case ctors di of
>             [] -> [dataDef di]
>             x:xs -> (if firstCtorSameLine di
>                      then let (h:tl) = fixFirst x
>                           in (dataDef di ++ h) : tl
>                      else dataDef di : fixFirst x)
>                      ++ concatMap (fixCtor . lines) xs
>     fixFirst :: String -> [String]
>     fixFirst = fixCtor . lines . replaceFirstBar
>     fixCtor :: [String] -> [String]
>     fixCtor (h:t) = ([h ++ "{"] ++ fixLine True t ++ ["      }"])
>     fixCtor [] = []
>     replaceFirstBar :: String -> String
>     replaceFirstBar a = let r [] = []
>                             r (x:xs) = case x of
>                                             '|' -> '=':xs
>                                             x' -> x' : r xs
>                         in r a
>     fixLine :: Bool -> [String] -> [String]
>     fixLine _f [] = []
>     fixLine f (x:xs) =
>       case dropWS x of
>         [] -> x : fixLine f xs
>         (h:_) | isAlpha h -> let (ws,meat) = span isSpace x
>                                  comma = if f then "" else ","
>                              in (ws ++ comma ++ doType meat) : fixLine False xs
>         _ -> x : fixLine f xs
>     dropWS = dropWhile isSpace
>     doType l = let (p,o) = break (==':') l
>                    o' = drop 2 $ filter (`notElem` "{}") o
>                in p ++ ":: (" ++ o' ++ ")"-}

>     {-fixFirst :: String -> [String]
>     fixFirst = replaceFirstBar . fixCtor . lines
>     fixCtor :: [String] -> [String]
>     fixCtor = undefined --id
>     replaceFirstBar :: [String] -> [String]
>     replaceFirstBar a = let r [] = []
>                             r (x:xs) = case x of
>                                          '|' -> '=':xs
>                                          x' -> x' : r xs
>                         in lines $ r $ unlines a-}


>   {-Unlines $
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
>     addComma _ [] = [] -}

> {-data DefInfo = DefInfo
>     { -- haddock comments before the first line
>      prefixComments :: [String]
>      -- the line containing data and the data type name
>      -- if the first | is on the same line, this is split
>     ,dataDef :: String
>      -- true if the '| ctorname' is on the same line as the data def
>     ,firstCtorSameLine :: Bool
>     ,ctors :: [String] -- the text for each ctor starting with the line the | is on
>                        -- which is a partial line for the first ctor if firstCtorSameLine
>     } deriving Show-}
> defs :: [String] -> [(String,[String])] -> [(String,String)]
> defs agSources dataTypeNames =
>   map def dataTypeNames
>   where
>     -- assume the ag files are 'standalone' so concatting them
>     -- in any order is ok
>     ls = concatMap lines agSources
>     -- replace ls with array/vector since working backwards
>     def :: (String,[String]) -> (String,String)
>     def (nm,ctrs) =
>              let si = fromMaybe (error $ "definition not found: " ++ nm)
>                       $ findIndex (isPrefixOf ("data " ++ nm)) ls
>                  (prefix,start) = splitAt si ls
>                  -- go backwards until find a line which isn't empty or a comment
>                  prefcom = reverse $ takeWhile spaceOrComment $ reverse prefix
>                  thisdef = case start of
>                              l:tl -> prefcom ++ l : getRest tl
>                              [] -> error $ "definition not found: " ++ nm
>              in (nm
>                 ,toHaskell $ unlines thisdef)
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


> {-toHaskell s = unlines $ toHaskell' True True $ lines s --  either (error . show) id $ runParser pdef True "" s

> toHaskell' _ _ [] = []

> toHaskell' fc fm (x:xs) | blank x = x : toHaskell' fc fm xs
>                         | otherwise = let (fc',fm',x') = modLine fc fm x
>                                       in x' : toHaskell' fc' fm' xs

> data Def = Def String -- name
>                String --haddock
>                [Ctor]

> data Ctor = Ctor String -- name
>                  String -- type
>                  String -- haddock


> modLine fc fm x =
>   let (sp,d) = parseLine x
>       fc' = case d of
>               Data {} -> False
>               DataFirstCtor {} -> False
>               _ -> fc
>       fm' = case d of
>              Data {} -> True
>              DataFirstCtor {} -> True
>              Ctor {} -> False
>              Mem {} -> False
>   in (fc',fm', sp ++ printLine fc fm d)

> printLine _fc _fm (Data s) =
>     "data " ++ s

> printLine _fc _fm (DataFirstCtor s c) =
>     "data " ++ s ++ " = " ++ c ++ "{"

> printLine fc fm (Ctor nm at ty) =
>   (if fc
>    then "="
>    else "|")
>   ++ " " ++ nm ++ "{" ++ at ++ " :: (" ++ ty ++ ")"

> printLine fc fm (Mem nm ty) =
>   (if fm then "," else "")
>      ++ nm ++ " :: (" ++ ty ++ ")"

> parseLine :: String -> (String,LineInfo)
> parseLine s = either (error . show) id $ runParser p () "" s
>   where
>     p = do
>         ws <- many space
>         l <- choice
>              [try $ do
>               string "data"
>               _ <- many space
>               i <- many1 letter
>               choice [do
>                       _ <- many space
>                       char '|'
>                       _ <- many space
>                       ct <- many1 letter
>                       return $ DataFirstCtor i ct
>                      ,return $ Data i]
>              ,
>              ]
>         return (ws,l)


> data LineInfo =
>      Data String
>    | DataFirstCtor String String
>    | Ctor String String String
>    | CtorFirstMem String
>    | Mem String String

> blank x = case dropWhile isSpace x of
>             [] -> True
>             (x:xs) -> x == '-'

> type P = ParsecT String Bool Identity

> --def :: P String
> pdef :: P String
> pdef = do
>   sp <- many1 space
>   string "data"
>   sp' <- many1 space
>   tn <- many1 letter
>   fst <- pctor True
>   cts <- many (pctor False)
>   return $ sp ++ "data" ++ sp'
>          ++ tn ++ fst ++ concat cts

> pctor :: Bool -> P String
> pctor = undefined -}

> {-parseDef :: [String] -> DefInfo
> parseDef ls =
>   let (coms,notcoms) = traceit "coms" $ break (isPrefixOf "data") ls
>       (dd,ctor0') = traceit "head" $ break (=='|') $ head notcoms
>       nextCtor lss = let (a,b) = traceit "ctor" $ break ((=='|') . head . dropWS) lss
>                      in if null b
>                         then [a]
>                         else a : nextCtor b
>       (x:xs) = nextCtor $ tail notcoms
>       cts = (if null ctor0'
>              then x
>              else ctor0' : x) : xs
>   in let x = DefInfo {prefixComments = coms
>              ,dataDef = dd
>              ,firstCtorSameLine = not (null ctor0')
>              ,ctors = map unlines cts}
>      in trace (show x) x
>   where
>     dropWS = dropWhile isSpace-}

> traceit :: Show a => String -> a -> a
> traceit m a = trace ("\n**********\n" ++ m ++ "\n" ++ show a ++ "\n**********\n") a

> traceit1 :: String -> String -> String
> traceit1 m a = trace ("\n**********\n" ++ m ++ "\n" ++ a ++ "\n**********\n") a


> toHaskell :: String -> String
> toHaskell s = traceit1 "trans" $ transformDef (TST True False False False) s

This takes the definition in uuagc format and converts it to haskell
record syntax, preserving comments.

TODO: it ignores when you don't want to recordize all the ctors
doesn't disambiguate the ann record members

> data TST = TST
>     {firstCtor :: Bool
>     ,firstMem :: Bool
>     ,nextMem :: Bool
>     ,readCtorName :: Bool}

> transformDef st [] = "\n    } deriving (Eq,Show,Data,Typeable)\n"

skip comments

> transformDef st s@(x:x1:_) | x == '-', x1 == '-' =
>   let (a,b) = break (=='\n') s
>   in a ++ transformDef st b

transform the first | to =

> transformDef st ('|':xs)
>   | firstCtor st =
>       '=' : transformDef st {firstCtor = False,readCtorName = True} xs
>   | otherwise =
>       "    }\n    | " ++
>       transformDef st {firstCtor = False,readCtorName = True} xs

read ctor name

> transformDef st s@(x:_) | isAlpha x, readCtorName st =
>   let (a,b) = span isAlpha s
>   in a ++ " {" ++ transformDef st {readCtorName = False,firstMem = True} b

add commas

> transformDef st s@(x:xs) | isAlpha x=
>   let (a,b) = break (=='\n') s
>   in case () of
>     _ | firstMem st -> fixType a ++ transformDef st {firstMem = False,nextMem = True} b
>       | nextMem st -> (',' : fixType a) ++ transformDef st b
>       | otherwise -> x : transformDef st xs

> transformDef st (x:xs) = x : transformDef st xs

> fixType :: String -> String
> fixType s =
>     let (pt,rs) = break (==':') s
>         -- don't touch trailing comment
>         (ty,cm) = break (=='-') rs
>         ty' = ":: (" ++ drop 2 (filter (`notElem` "{}") ty) ++ ")"
>     in pt ++ ty' ++ cm
