Copyright 2010 Jake Wheat

This file creates the documentation for the project which is uploaded
to the website.

> {-# LANGUAGE QuasiQuotes #-}
>
> module Database.HsSqlPpp.DevelTools.MakeWebsite
>     (makeWebsite) where
>
> import System.Directory
> import Control.Monad
> import System.Cmd
> import System.FilePath.Find
> import System.IO
> import System.FilePath
> import Data.DateTime
> --import Text.RegexPR
> --import Debug.Trace
> --import Control.Applicative
> --import qualified Data.List as L
> --import Data.Char
> --import Text.XML.HaXml hiding (find,x)
> --import Text.PrettyPrint (render)
> --import Text.XML.HaXml.Pretty
>
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.DevelTools.TestFileProcessor
> import Database.HsSqlPpp.DevelTools.PandocUtils
> import Database.HsSqlPpp.DevelTools.DoChaosSql

> makeWebsite :: IO ()
> makeWebsite = do
>   hSetBuffering stdout NoBuffering
>   {-doesDirectoryExist "website" >>=
>     \l -> when(l) $ removeDirectoryRecursive "website"
>   createDirectory "website"
>   copyFile "docs/main.css" "website/main.css"-}
>   let v = "0.3.0-pre"  --- todo: read from cabal
>   let hd = wheader v
>   t <- getCurrentTime
>   let ft = wfooter v (formatDateTime "%D %T" t)
>   let pd1 = htmlize cssLink hd ft
>       pf = pd1 Txt
>       plhs = pd1 Lhs
>   pf "HsSqlPpp documentation"
>      (File "docs/index.txt")
>      "index.html"
>   pf "HsSqlPpp examples"
>      (File "docs/examples.txt")
>      "examples.html"
>   {-plhs "HsSqlPpp parser examples"
>        (Str parserTestsTable)
>        "ParserTests.html"
>   plhs "HsSqlPpp type checking examples"
>        (Str typeCheckTestsTable)
>        "TypeCheckTests.html"
>   qq <- quasiQuoteTestsTable
>   plhs "HsSqlPpp quasiquotation examples"
>        (Str qq)
>        "QuasiQuoteTests.html"
>   doSourceFiles pd1
>   doHaddock-}
>   doChaosSql pd1
>   return ()

-------------------------------------------------------------------------------

> doSourceFiles :: (PandocType -> String -> Input -> [Char] -> IO ()) -> IO ()
> doSourceFiles pdize = do
>   sf <- sourceFiles
>   let index = concatMap (\s -> let s1 = s ++ ".html"
>                                in "* [" ++ s ++ "](" ++ s1 ++ ")\n") sf
>   pdize Txt "HsSqlPpp source files" (Str index) "pandoc_source/index.html"
>   moveDTCOut
>   mapM_ pandocIt sf
>   moveDTCBack
>   where
>     pandocIt fn = do
>            let target = "pandoc_source/" ++ fn ++ ".html"
>                title = snd $ splitFileName fn
>            pdize (if takeExtension fn `elem` [".lhs", ".lag"]
>                   then Lhs
>                   else Highlight) title (File fn) target
>     sourceFiles = do
>       l <- find always sourceFileP "examples/"
>       l1 <- find always sourceFileP "src/"
>       return $ l ++ l1
>     sourceFileP = extension ==? ".hs" ||? extension ==? ".lhs"
>                     ||? extension ==? ".ag"
>                     ||? extension ==? ".lag"
>

todo: add filenames at top of these pages or// duplicate the title as
the top header for each page


------------------------------------------------------------------------------

> wheader :: String -> String
> wheader v = [$here|
>
> <div class='header'>[HsSqlPpp-|] ++ v ++
>             [$here|](website/index.html)</div>|] ++ "\n\n"

todo: add the last modified time for each file individually

> wfooter :: String -> String -> String
> wfooter v d = [$here|
> <br/>
> <br/>
> <hr/>
> <div class='footer'>Copyright 2010 Jake Wheat, generated on |]
>               ++ d ++ ", hssqlppp-" ++ v ++ "</div>"
>
> cssLink :: String
> cssLink = "<link href=\"website/main.css\" rel='stylesheet' type='text/css'>"

-------------------------------------------------------------------------------

> doHaddock :: IO ()
> doHaddock = do
>   --cos hscolour can't handle the large defaulttemplate1catalog,
>   --just move it out the way temporarily
>   moveDTCOut
>   _ <- rawSystem "cabal" ["configure"]
>   _ <- rawSystem "cabal" ["haddock"] --, "--hyperlink-source"]
>   renameDirectory "dist/doc/html/hssqlppp/" "website/haddock"
>   moveDTCBack
>   --want to use the pandoc source files instead of the hscolour ones
>   --mainly to avoid duplication
>   --not great that we don't avoid building the hscolour files which takes a while
>   {-removeDirectoryRecursive "website/haddock/src"
>   sf <- htmlSourceFiles
>   hf <- map ("website/haddock/"++) <$> getDirectoryContents "website/haddock/"
>   hf1 <- filterM doesFileExist hf
>   mapM_ (changeHaddockLinks sf) hf1-}
>   -- now add the anchor links to the pandoc files
>   --mapM_ filterSource sf
>   return ()
>   {-where
>     {-filterSource f = do
>       let bkp = f ++ ".bak"
>       renameFile f bkp
>       fc <- readFile bkp
>       let fc1 = addAnchors fc
>       writeFile f fc1
>       removeFile bkp-}

>     {-addAnchors xs@(y:ys) =
>         let p = "<span class=\"Function FunctionDefinition"
>         in case L.stripPrefix p xs of
>                    Nothing -> y : addAnchors ys
>                    Just ys' -> let (a,_) = span (/=':') ys'
>                                    (_,d) = span (/='>') a
>                                    e = trim $ drop 1 d
>                                    f ="<a name='" ++ e ++ "' />"
>                                in f ++ (y : addAnchors ys)
>     addAnchors [] = []-}

>     {-changeHaddockLinks sf f = do
>       let bkp = f ++ ".bak"
>       --renameFile f bkp
>       fc <- readFile f --bkp
>       writeFile "/dev/null" {-f-} $ gsubRegexPRBy "\"src[^\"]*\"" (c1 sf) fc
>       --removeFile bkp
>       return ()-}

>     htmlSourceFiles =
>       find always sourceFileP "website/pandoc_source/src"
>     sourceFileP = extension ==? ".html"-}

> {-changeHaddockLinks sf f = do
>   let bkp = f ++ ".bak"
>   --renameFile f bkp
>   fc <- readFile f --bkp
>   writeFile "/dev/null" {-f-} $ gsubRegexPRBy "\"src[^\"]*\"" (c1 sf) fc
>   --removeFile bkp
>   return ()-}


want to take a string like
src/Database-HsSqlPpp-Ast-Ast.html#something

and convert it to

../pandoc_sources/Database/HsSqlPpp/Ast/Ast.lhs.html#something

since we don't know whether it is an lhs, hs or ag file, take a list
of the target file names to find a match in

 > c1 :: [String] -> String -> String
 > c1 sf s = let s1 = changeSourceLink sf s
 >           in trace (s ++ "->\n    " ++ s1) s1

> {-changeSourceLink :: [String] -> String -> String
> changeSourceLink sf =
>     dropSrc |> replaceDash |> toLhs
>     where
>       dropSrc = drop 5
>       replaceDash = map (\l -> if l == '-' then '/' else l)
>       toLhs s = let an = snd (span (/='#') s)
>                 in findMatching (dropExtensions s) ++ an
>       findMatching f =
>          case L.find (L.isPrefixOf $ "website/pandoc_source/src/" ++ f) sf of
>            Just s -> s
>            Nothing -> error $ "no match for" ++ show f-}

-------------------------------------------------------------------------------

> moveDTCOut :: IO()
> moveDTCOut = do
>   renameFile "src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs"
>              "src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs.moved"
>   copyFile "src/Database/HsSqlPpp/AstInternals/Catalog/ShortDefaultTemplate1Catalog.lhs"
>            "src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs"
>
> moveDTCBack :: IO ()
> moveDTCBack = do
>   renameFile "src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs.moved"
>              "src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs"

