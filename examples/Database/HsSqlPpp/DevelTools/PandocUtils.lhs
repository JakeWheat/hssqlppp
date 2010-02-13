Copyright 2010 Jake Wheat

some dodgy wrappers around pandoc, highlighting-kate

> module Database.HsSqlPpp.DevelTools.PandocUtils
>     where

> import System.Directory
> import Text.Pandoc hiding (Str)
> import System.FilePath
> import Text.Highlighting.Kate
> import Debug.Trace
> import Text.XHtml.Strict hiding (title,src)
>
> import Database.HsSqlPpp.Utils.Utils

-------------------------------------------------------------------------------

pandoc/highlight wrappers

> data PandocType = Lhs
>                 | Highlight
>                 | Txt
>                 | Sql
>                   deriving Show
> data Input = Str String
>            | File String
>
>
> -- main utility funtion
> htmlize :: String -> String -> String -> PandocType -> String -> Input -> String -> IO ()
> htmlize cssLink hdr ftr pt title src tgt = do
>   putStrLn tgt
>   let tgt1 = "website/" ++ tgt
>   createDirectoryIfMissing True $ dropFileName tgt1
>   src1 <- case src of
>             Str s -> return s
>             File f -> readFile f
>   let filterRelativeLinks = filterLinks (concat $
>                           replicate (length $
>                                      splitDirectories $
>                                      dropFileName tgt)
>                           "../")
>   let src2 = hdr ++ src1 ++ ftr
>   writeFile tgt1 $ filterRelativeLinks $
>                 case pt of
>                            Lhs -> pandocLhs cssLink title $ wrapSqlCode $ addAnchors src2
>                            Highlight -> highlight "Haskell" cssLink hdr ftr title $ addAnchors src1
>                            Sql -> highlight "SqlPostgresql" cssLink hdr ftr title $ addAnchors src1
>                            Txt -> pandoc cssLink title src2
>
>
> addAnchors :: String -> String
> addAnchors f = f
>
> -- pure wrappers to do various rendering
> pandoc :: String -> String -> String -> String
> pandoc cssLink hd = (writeHtmlString wopt) . (readMarkdown defaultParserState)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               ,writerTitlePrefix = hd
>               ,writerTableOfContents = False
>               ,writerHeader = cssLink
>              }
>
> pandocFrag :: String -> String
> pandocFrag = (writeHtmlString wopt) . (readMarkdown defaultParserState)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = False
>              }
>
>
> pandocLhs :: String -> String -> String -> String
> pandocLhs cssLink hd = (writeHtmlString wopt) . (readMarkdown ropt)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               ,writerTitlePrefix = hd
>               ,writerTableOfContents = False
>               ,writerHeader = cssLink
>               ,writerLiterateHaskell=True
>              }
>     ropt = defaultParserState {
>             stateLiterateHaskell = True
>            }
>
> -- hack to render some quasiquoted sql using sql highlighting
> wrapSqlCode :: String -> String
> wrapSqlCode = replace
>               "\n\\begin{code}\n"
>               "\n~~~~~{.SqlPostgresql}\n"
>               . replace
>               "\n\\end{code}\n"
>               "\n~~~~~\n"

> filterLinks :: String -> String -> String
> filterLinks path = replace
>                    "\"website/"
>                    ("\"" ++ path)



pandoc won't render haskell source which isn't literate nicely at all,
so run it though highlighting-kate only

> highlight :: String -> String -> String -> String -> String -> String -> String
> highlight h cssLink hdr ftr title s = do
>   case highlightAs h s of
>     Right r -> "<html><head><title>" ++ title ++ "</title>" ++
>                cssLink ++ "</head><body>" ++
>                pandocFrag hdr ++
>                renderHtmlFragment (formatAsXHtml [OptTitleAttributes] h r) ++
>                pandocFrag ftr ++
>                "</body></html>"
>     Left err -> trace ("highlight error: " ++ err) s
