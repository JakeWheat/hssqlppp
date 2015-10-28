
convert error messages to show source text fragment with little hat,
plus output error location in emacs friendly format.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.ParseErrors
>     (toParseErrorExtra
>     ,ParseErrorExtra(..)) where
>
> import Text.Parsec
> import qualified Data.Text.Lazy as L
>
> showPE :: ParseError -> Maybe (Int,Int) -> L.Text -> String
> showPE pe sp src = show pe ++ "\n" ++ pePosToEmacs pe
>                       ++ "\n" ++ peToContext pe sp src
>
> pePosToEmacs :: ParseError -> String
> pePosToEmacs pe = let p = errorPos pe
>                       f = sourceName p
>                       l = sourceLine p
>                       c = sourceColumn p
>                   in f ++ ":" ++ show l ++ ":" ++ show c ++ ":"
>
> peToContext :: ParseError -> Maybe (Int,Int) -> L.Text -> String
> peToContext pe sp src =
>      let ls = L.lines src
>          line = safeGet ls (lineNo - 1)
>          prelines = map (safeGet ls) [(lineNo - 5) .. (lineNo - 2)]
>          postlines = map (safeGet ls) [lineNo .. (lineNo + 5)]
>          caretLine = L.pack (replicate (colNo - 1) ' ' ++ "^")
>          erLine = let s = "ERROR HERE"
>                   in L.pack (replicate (colNo - 1 - (length s `div` 2)) ' ' ++ s)
>          errorHighlightText = prelines
>                               ++ [line, caretLine, erLine]
>                               ++ postlines
>     in "\nContext:\n"
>        ++ L.unpack (L.unlines (trimLines errorHighlightText)) ++ "\n"
>     where
>       safeGet a i = if i < 0 || i >= length a
>                       then ""
>                       else a !! i
>       trimLines = trimStartLines . reverse . trimStartLines . reverse
>       trimStartLines = dropWhile (=="")
>       pos = errorPos pe
>       lineNo = sourceLine pos - adjLine
>       colNo = sourceColumn pos
>       adjLine = case sp of
>                         Just (l, _) -> l - 1
>                         Nothing -> 0
>
> -- | Simple wrapper to allow showing the source context of a ParseError
> data ParseErrorExtra =
>        ParseErrorExtra {
>                        -- | wrapped error
>                        parseErrorError :: ParseError
>                        -- | source position
>                        -- adjustment to get the
>                        -- context bit in error
>                        -- messages right - this is
>                        -- the same as what is passed
>                        -- into parseSqlWithPosition
>                        ,parseErrorPosition :: Maybe (Int, Int)
>                        -- | sql source
>                        ,parseErrorSqlSource :: L.Text
>                        }
>
> instance Show ParseErrorExtra where
>     show (ParseErrorExtra pe sp src) = showPE pe sp src
>
> toParseErrorExtra :: L.Text -> Maybe (Int,Int) -> ParseError -> ParseErrorExtra
> toParseErrorExtra src sp e = ParseErrorExtra e sp src
