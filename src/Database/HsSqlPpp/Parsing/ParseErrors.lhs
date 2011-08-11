
convert error messages to show source text fragment with little hat,
plus output error location in emacs friendly format.

>
> module Database.HsSqlPpp.Parsing.ParseErrors
>     (toParseErrorExtra
>     ,ParseErrorExtra(..)) where
>
> import Text.Parsec
> import Control.Monad.Error
>
> showPE :: ParseError -> Maybe (Int,Int) -> String -> String
> showPE pe sp src = show pe ++ "\n" ++ pePosToEmacs pe
>                    ++ "\n" ++ peToContext pe sp src
>
> pePosToEmacs :: ParseError -> String
> pePosToEmacs pe = let p = errorPos pe
>                       f = sourceName p
>                       l = sourceLine p
>                       c = sourceColumn p
>                   in f ++ ":" ++ show l ++ ":" ++ show c ++ ":"
>
> peToContext :: ParseError -> Maybe (Int,Int) -> String -> String
> peToContext pe sp src =
>      let ls = lines src
>          line = safeGet ls(lineNo - 1)
>          prelines = map (safeGet ls) [(lineNo - 5) .. (lineNo - 2)]
>          postlines = map (safeGet ls) [lineNo .. (lineNo + 5)]
>          caretLine = replicate (colNo - 1) ' ' ++ "^"
>          errorHighlightText = prelines
>                               ++ [line, caretLine, "ERROR HERE"]
>                               ++ postlines
>     in "\nContext:\n"
>        ++ unlines (trimLines errorHighlightText) ++ "\n"
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
>                        ,parseErrorSqlSource :: String
>                        }
>
> instance Show ParseErrorExtra where
>     show (ParseErrorExtra pe sp src) = showPE pe sp src
>
> instance Error ParseErrorExtra where
>   noMsg = ParseErrorExtra (error "instance Error ParseErrorExtra") Nothing "unknown"
>   strMsg = ParseErrorExtra (error "instance Error ParseErrorExtra") Nothing
>
> toParseErrorExtra :: Either ParseError b -> Maybe (Int,Int) -> String
>                   -> Either ParseErrorExtra b
> toParseErrorExtra a sp src = case a of
>                                Left pe -> Left $ ParseErrorExtra pe sp src
>                                Right x -> Right x
