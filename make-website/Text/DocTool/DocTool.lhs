
Plan to fix the documentation:

the current entire website:
the website-source files rendered
+ 3 generated files from the automated tests

1. read the website-source files to pandoc internal
2. generate the automated tests files to pandoc internal
   refactor the tests completely: how to share stuff between hssqlppp and
   hssqlppp-th?
3. generate html from these
4. add the transforms and tweaks at the pandoc internal level:
  add header and footer
  process the non standard markdown headers with =
5. fix the syntax highlighting, pandoc should be fast enough now?

6. get h7c working somehow, and redo the crazy rendering of this. I
think this is more performance art than real programming because it is
mental.

> {-# LANGUAGE PatternGuards #-}
> module Text.DocTool.DocTool
>     (OutputFile(..)
>     ,Type(..)
>     ,Source(..)
>     ,Title
>     ,docify) where

> import Text.Pandoc
> --import Text.XHtml.Transitional hiding (toHtml)
> import System.FilePath
> import System.Directory
> import Debug.Trace
> --import Text.Highlighting.Illuminate
> import Data.DateTime
> import Control.Concurrent
> import Data.List
> import Control.Exception
> import Text.Groom
> import qualified Text.Pandoc.Builder as P
> import qualified Data.Set as S
> import Data.String
> import Text.Blaze.Html

> import Text.DocTool.Parser as Parser

> docify :: FilePath -> [OutputFile] -> IO ()
> docify b ofs = do
>   t <- getCurrentTime
>   let tm = formatDateTime "%D %T" t
>   children <- (newMVar [])
>   mapM_ (\f -> forkChild children (process "0.5.0" tm b f >> putStrLn (showOf f))) ofs
>   waitForChildren children
>          where
>            showOf (OutputFile (Text _) _ fp _) = fp
>            showOf (OutputFile s _ _ _) = show s

====================================================================

> data OutputFile = OutputFile Source Type FilePath Title

> instance Show OutputFile where
>     show (OutputFile (Text _) a b c) = "OutputFile (Text \"...\") " ++ show a ++ " " ++ show b ++ " " ++ show c
>     show (OutputFile a b c d) = "OutputFile " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d

> data Type = Sql | Lhs | Hs | Txt | Ag | Css | C | H
>             deriving Show

> data Source = File FilePath
>             | Text String
>               deriving Show

> type Title = String

> --type ProcessText = String -> String
> --type ProcessPandoc = Pandoc -> Pandoc
> --type ProcessHtml = Html -> Html

=========================================================

> ppPandoc :: OutputFile -> IO String
> ppPandoc (OutputFile s t _ _) =
>     asText s >>= return . (toPandoc t |> groom)

> asText :: Source -> IO String
> asText (File fp) = readFile fp
> asText (Text a) = return a

> toPandoc :: Type -> String -> Pandoc
> toPandoc t s = {-trace ("toPandoc " ++ show t ++ s) $-}
>                case t of
>                  Lhs -> readLhs s
>                  Sql -> readSource "/*" "*/" "sql" s
>                  Hs -> readSource "{-" "-}" "haskell" s
>                  Ag -> readSource "{-" "-}" "haskell" s
>                  Txt -> readMd s
>                  Css -> readSource "/*" "*/" "css" s
>                  C -> readSource "/*" "*/" "c" s
>                  H -> readSource "/*" "*/" "c" s

> setTitle :: String -> Pandoc -> Pandoc
> setTitle t m = P.setTitle (P.fromList [Str t]) m

> toHtml :: Pandoc -> Html
> toHtml pa = writeHtml def {writerExtensions = S.fromList [Ext_literate_haskell]} highlightCode
>     where
>       highlightCode = case pa of
>                           Pandoc m bs -> Pandoc m (map hi bs)
>       hi (CodeBlock a b) | Just t <- getType a b =
>          case illuminate t b of
>            Right result -> RawBlock (fromString "html") $ getPres a ++ result ++ getClosePres a
>            Left  err    -> error $ "Could not parse input: " ++ err
>       hi x = x
>       getClasses (_,x,_) = x
>       getPres a = concatMap (\x -> "<div class='" ++ x ++ "'>")  $ getClasses a
>       getClosePres a = concatMap (const "</div>") $ getClasses a
>       getType (_,x,_) b =
>          case x of
>            [] -> Nothing
>            _ | "sql" `elem` x
>                || "SqlPostgresql" `elem` x -> Just "sql"
>              | "haskell" `elem` x -> Just "haskell"
>              | "sh" `elem` x -> Just "sh"
>              | "c" `elem` x -> Just "c"
>              | "chs" `elem` x -> Just "haskell"
>              | otherwise -> trace ("unknown:" ++ show x
>                                    ++ "\n" ++ show b) Nothing

> wrapHtmlFragment :: String -> Html -> Html
> wrapHtmlFragment ti h =
>   P.header << [t,c]
>   +++ body << h
>   where
>     t = thetitle << ti
>     c = thelink ! [href "/website/main.css"
>                   ,rel "stylesheet"
>                   ,thetype "text/css"] << ""

bit dodgy

> filterLinks :: String -> String -> String
> filterLinks path = replace
>                           "\"/website/"
>                           ('"': path)


> toText :: Html -> String
> toText = renderHtml

> process :: String -> String -> FilePath -> OutputFile -> IO ()
> process _ _ _ (OutputFile (File f) Css fp _) =
>   copyFile f fp
> process v tm b (OutputFile s t fp ti) = do
>   let hd = wheader v
>       ft = wfooter v tm
>   flip fmap (asText s)
>     (toPandoc t
>       |> setTitle ti
>       |> toHtml
>       |> (\ h -> hd +++ h +++ ft)
>       |> wrapHtmlFragment ti
>       |> toText
>       |> filterLinks back)
>     >>= writeFolderFile fp
>   where
>     relpath = makeRelative b fp
>     back = concat $ replicate (length $
>                       splitDirectories $
>                       dropFileName relpath)
>                      ""

> wheader :: String -> Html
> wheader v =
>   thediv ! [theclass "header"]
>       << a
>   +++ [br,br,br]
>   where
>     a = anchor ! [href "/website/index.html"]
>                    << ("HsSqlPpp-" ++ v)

todo: add the last modified time for each file individually

> wfooter :: String -> String -> Html
> wfooter v d =
>     [br,br,br] +++ di
>   where
>     s = "generated on " ++ d ++ ", hssqlppp-" ++ v
>     di = thediv ! [theclass "footer"] << s




> writeFolderFile :: FilePath -> String -> IO ()
> writeFolderFile fp t = do
>   createDirectoryIfMissing True $ dropFileName fp
>   writeFile fp t

> infixl 9 |>
> (|>) :: (a -> b) -> (b -> c) -> a -> c
> (|>) = flip (.)



add some sample files: ag lhs hs txt sql
show the pandoc ast
try illuminate, need to write sql highlighter?


> -- pure wrappers to do various rendering
> readMd :: String -> Pandoc
> readMd = readMarkdown def
>
> readLhs :: String -> Pandoc
> readLhs = readMarkdown ropt
>   where
>     ropt = def {
>               readerExtensions = S.fromList [Ext_literate_haskell]
>            }


> readSource :: String -> String -> String -> String -> Pandoc
> readSource sc ec ty txt = either err id $ do
>     ccs <- parseSource sc ec txt
>     return $ convl ccs
>     where
>       err e = trace (show e) $ w $ cb txt
>       convl :: [Cc] -> Pandoc
>       convl cs = w $ concatMap conv cs
>       conv :: Cc -> [Block]
>       conv (Parser.Code c) = cb c
>       conv (Comments m) = case readMarkdown def m of
>                            Pandoc _ b -> b
>       cb s = [CodeBlock ("", ["sourceCode", "literate", ty], []) s]
>       w = Pandoc nullMeta

todo:
do hack for =,==,etc. headers
do hack for sql strings in haskell?
add css
add anchors
filter relative links
titles
copyright
header
footer
navigation, sitemap, breadcrumbs

================================================

> illuminate :: String -> String -> Either String String
> illuminate ty txt = Right txt {- do
>     let lexer = lexerByName ty
>     tokens <- tokenize lexer txt
>     let html = toXHtmlCSS defaultOptions tokens
>     return $ showHtmlFragment html -}


> replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
> replace _ _ [] = []
> replace old new xs@(y:ys) =
>   case stripPrefix old xs of
>     Nothing -> y : replace old new ys
>     Just ys' -> new ++ replace old new ys'


> waitForChildren :: MVar [MVar ()] -> IO ()
> waitForChildren children = do
>   cs <- takeMVar children
>   case cs of
>        []   -> return ()
>        m:ms -> do
>           putMVar children ms
>           takeMVar m
>           waitForChildren children

> forkChild :: MVar [MVar ()] -> IO () -> IO ThreadId
> forkChild children io = do
>        mvar <- newEmptyMVar
>        childs <- takeMVar children
>        putMVar children (mvar:childs)
>        forkIO (io `finally` putMVar mvar ())
