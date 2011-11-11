

Input: a list of ag files (doesn't follow includes)
A list of data type names
A .hs filename

finds the definition for each of the data types in the ag files
(complete source code with ws and comments, included prefix comment)

converts this uuagc syntax into haskell record syntax preserving
comments

splices it into the hs file replacing the existing def which will not
be in record syntax and will have had any haddock stripped out


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
>     -- find the definition in the hs file, and splice
>     -- in the replacement
>     replaceDef :: (String,String) -> [String] -> [String]
>     replaceDef (n,def) src =
>       let (s,e) = break (isPrefixOf $ "data " ++ n) src
>           e' = dropWhile startsWithSpace $ tail e
>       in s ++ [def] ++ e'

>     startsWithSpace l =
>       case l of
>              c:_ | isSpace c -> True
>              _ -> False

this finds the definition of a datatype in the ag files, including any
preceeding comments

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


This takes the definition in uuagc format and converts it to haskell
record syntax, preserving comments.

TODO: it ignores when you don't want to recordize all the ctors
doesn't disambiguate the ann record members

> toHaskell :: String -> String
> toHaskell s = transformDef (TST True False False False) s

parse state

> data TST = TST
>     {firstCtor :: Bool -- still waiting to see the first ctor?
>     ,firstMem :: Bool -- waiting to see the first member of a ctor
>     ,nextMem :: Bool -- looking for another member?
>     ,readCtorName :: Bool} -- looking for a ctor name

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
