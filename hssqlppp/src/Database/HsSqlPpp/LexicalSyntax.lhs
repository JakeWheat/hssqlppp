
> {-# LANGUAGE TupleSections,OverloadedStrings #-}
> module Database.HsSqlPpp.LexicalSyntax
>     (Token(..)
>     ,prettyToken
>     ,Position
>     ,sqlToken
>     ,sqlTokens
>     ,addPosition
>     ) where

> import qualified Data.Text as T
> import qualified Data.Text.Lazy as LT
> import Data.Attoparsec.Text as AP
> import Control.Applicative
> import Data.Char
> import Database.HsSqlPpp.SqlDialect
> import Control.Monad

> -- | Represents a lexed token
> data Token
>     -- | a symbol in postgresql dialect is one of the following:
>     --
>     -- * one of the characters (),;[]
>     --
>     -- * \'..\' or \':=\' or \'.\' or \':\'
>     --
>     -- * a compound symbol, which starts with one of \'*\/\<>=~!\@#%^&|\`?+-' and follows with 0 or more of '*\/<>=~!\@#%^&|\`?'
>     --
>     -- things that are not lexed as symbols:
>     --
>     -- * [] used in quoted identifiers, prefix \@,#,: used in identifiers
>     --
>     -- * $n positional arg
>     --
>     = Symbol T.Text
>
>     -- | This is an identifier or keyword.
>     --
>     -- The 'Maybe (Char,Char)' selects the quoted style - 'Nothing' means the
>     -- identifier was unquoted
>     -- otherwise the two characters are the start and end quote.
>     --
>     -- \'\"\' is used to quote identifiers in standard sql, sql server also uses [brackets]
>     -- to quote identifiers.
>     --
>     -- The identifier also includes the \'variable marker prefix\'
>     -- used in sql server (e.g. \@identifier, #identifier), and oracle
>     -- (e.g. :identifier)
>     | Identifier (Maybe (Char,Char)) T.Text
>
>     -- | This is a string literal.
>     --
>     -- The first field is the quotes used: single quote (\')
>     -- for normal strings, E' for escape supporting strings,
>     -- and $$ delimiter for postgresql dollar quoted strings.
>     --
>     -- The lexer doesn't process the escapes in strings, but passes
>     -- on the literal source e.g. E\'\\n\' parses to SqlString \"E\'\" \"\\n\"
>     -- with the literal characters \'\\\' and \'n\' in the string, not a newline character.
>     -- quotes within a string (\'\') or escaped string (\'\' or \\\') are passed through unchanged
>     | SqlString T.Text T.Text
>
>     -- | a number literal (integral or otherwise), stored in original format
>     -- unchanged
>     | SqlNumber T.Text
>
>     -- | non-significant whitespace (space, tab, newline) (strictly speaking,
>     -- it is up to the client to decide whether the whitespace is significant
>     -- or not)
>     | Whitespace T.Text
>
>     -- | a postgresql positional arg, e.g. $1
>     | PositionalArg Int
>
>     -- | a commented line using --, contains every character starting with the
>     -- \'--\' and including the terminating newline character if there is one
>     -- - this will be missing if the last line in the source is a line comment
>     -- with no trailing newline
>     | LineComment T.Text
>
>     -- | a block comment, \/* stuff *\/, includes the comment delimiters
>     | BlockComment T.Text
>
>     -- | an antiquotation splice, e.g. $x(stuff)
>     | Splice Char T.Text
>
>     -- | the copy data in a copy from stdin
>     | CopyPayload T.Text
>       deriving (Eq,Show)

> -- | Accurate pretty printing, if you lex a bunch of tokens,
> -- then pretty print them, should should get back exactly the
> -- same string
> prettyToken :: SQLSyntaxDialect -> Token -> LT.Text
> prettyToken _ (Symbol s) = LT.fromChunks [s]
> prettyToken _ (Identifier Nothing t) = LT.fromChunks [t]
> prettyToken _ (Identifier (Just (a,b)) t) =
>     LT.fromChunks [T.singleton a, t, T.singleton b]
> prettyToken _ (SqlString "E'" t) = LT.fromChunks ["E'",t,"'"]
> prettyToken _ (SqlString q t) = LT.fromChunks [q,t,q]
> prettyToken _ (SqlNumber r) = LT.fromChunks [r]
> prettyToken _ (Whitespace t) = LT.fromChunks [t]
> prettyToken _ (PositionalArg n) = LT.fromChunks [T.singleton '$', T.pack $ show n]
> prettyToken _ (LineComment l) = LT.fromChunks [l]
> prettyToken _ (BlockComment c) = LT.fromChunks [c]
> prettyToken _ (Splice c t) =
>     LT.fromChunks [T.singleton '$'
>                   ,T.singleton c
>                   ,T.singleton '('
>                   ,t
>                   ,T.singleton ')']
> prettyToken _ (CopyPayload s) = LT.fromChunks [s,"\\.\n"]


not sure how to get the position information in the parse errors

TODO: try to make all parsers applicative only
investigate what is missing for postgresql
investigate differences for sql server, oracle, maybe db2 and mysql
  also

> type Position = (String,Int,Int)

> addPosition :: Position -> LT.Text -> Position
> addPosition p s = addPosition' p $ LT.unpack s

> addPosition' :: Position -> String -> Position
> addPosition' (f,l,c) [] = (f,l,c)
> addPosition' (f,l,_) ('\n':xs) = addPosition' (f,l+1,0) xs
> addPosition' (f,l,c) (_:xs) = addPosition' (f,l,c+1) xs

> sqlTokens :: SQLSyntaxDialect -> Position -> T.Text -> Either String [(Position,Token)]
> sqlTokens dialect pos txt = parseOnly (many_p pos) txt
>    where

pretty hacky, want to switch to a different lexer for copy from stdin
statements

if we see 'from stdin;' then try to lex a copy payload

>      many_p pos' = some_p pos' `mplus` return []
>      some_p pos' = do
>        tok <- sqlToken dialect pos'
>        let pos'' = advancePos dialect pos' (snd tok)
>        case tok of
>          (_, Identifier Nothing t) | T.map toLower t == "from" -> (tok:) <$> seeStdin pos''
>          _ -> (tok:) <$> many_p pos''
>      seeStdin pos' = do
>        tok <- sqlToken dialect pos'
>        let pos'' = advancePos dialect pos' (snd tok)
>        case tok of
>          (_,Identifier Nothing t) | T.map toLower t == "stdin" -> (tok:) <$> seeColon pos''
>          (_,x) | isWs x -> (tok:) <$> seeStdin pos''
>          _ -> (tok:) <$> many_p pos''
>      seeColon pos' = do
>        tok <- sqlToken dialect pos'
>        let pos'' = advancePos dialect pos' (snd tok)
>        case tok of
>          (_,Symbol ";") -> (tok:) <$> copyPayload pos''
>          _ -> (tok:) <$> many_p pos''
>      copyPayload pos' = do
>        tok <- char '\n' *>
>             ((\x -> (pos', CopyPayload $ T.pack $ x ++ "\n"))
>              <$> manyTill anyChar (string "\n\\.\n"))
>        --let (_,CopyPayload t) = tok
>        --trace ("payload is '" ++ T.unpack t ++ "'") $ return ()
>        let pos'' = advancePos dialect pos' (snd tok)
>        (tok:) <$> many_p pos''

> advancePos :: SQLSyntaxDialect -> Position -> Token -> Position
> advancePos dialect pos tok =
>     let pt = prettyToken dialect tok
>     in addPosition pos pt

> isWs :: Token -> Bool
> isWs (Whitespace {}) = True
> isWs (BlockComment {}) = True
> isWs (LineComment {}) = True
> isWs _ = False

> -- | attoparsec parser for a sql token
> sqlToken :: SQLSyntaxDialect -> Position -> Parser (Position,Token)
> sqlToken d p =
>     (p,) <$> choice [sqlString d
>                     ,identifier d
>                     ,lineComment d
>                     ,blockComment d
>                     ,sqlNumber d
>                     ,symbol d
>                     ,sqlWhitespace d
>                     ,positionalArg d
>                     ,splice d]

> identifier :: SQLSyntaxDialect -> Parser Token

sql server: identifiers can start with @ or #
quoting uses [] or ""

> identifier SQLServerDialect =
>     choice
>     [Identifier (Just ('[',']'))
>      <$> (char '[' *> takeWhile1 (/=']') <* char ']')
>     ,Identifier (Just ('"','"'))
>      <$> (char '"' *> takeWhile1 (/='"') <* char '"')
>     ,Identifier Nothing <$> identifierStringPrefix '@'
>     ,Identifier Nothing <$> identifierStringPrefix '#'
>     ,Identifier Nothing <$> identifierString
>     ]

oracle: identifiers can start with :
quoting uses ""
(todo: check other possibilities)

> identifier OracleDialect =
>     choice
>     [Identifier (Just ('"','"'))
>      <$> (char '"' *> takeWhile1 (/='"') <* char '"')
>     ,Identifier Nothing <$> identifierStringPrefix ':'
>     ,Identifier Nothing <$> identifierString
>     ]

> identifier PostgreSQLDialect =
>     choice
>     [Identifier (Just ('"','"'))
>      <$> (char '"' *> takeWhile1 (/='"') <* char '"')
>     ,Identifier Nothing <$> identifierString
>     ]


> identifierStringPrefix :: Char  -> Parser T.Text
> identifierStringPrefix p = do
>     void $ char p
>     i <- identifierString
>     return $ T.cons p i

> identifierString :: Parser T.Text
> identifierString =
>     startsWith (\c -> c == '_' || isAlpha c)
>                (\c -> c == '_' || isAlphaNum c)

Strings in sql:
postgresql dialect:
strings delimited with single quotes
a literal quote is written ''
the lexer leaves the double quote in the string in the ast
strings can also be written like this:
E'string with quotes in \n \t'
the \n and \t are escape sequences. The lexer passes these through unchanged.
an 'E' escaped string can also contain \' for a literal single quote.
this are also passed into the ast unchanged
strings can be dollar quoted:
$$string$$
the dollar quote can contain an optional tag:
$tag$string$tag$
which allows nesting of dollar quoted strings with different tags

Not sure what behaviour in sql server and oracle, pretty sure they
don't have dollar quoting, but I think they have the other two
variants.

> sqlString :: SQLSyntaxDialect -> Parser Token
> sqlString _ =
>     choice [normalString
>            ,eString
>            ,dollarString]
>   where
>     normalString = SqlString "'" <$> (char '\'' *> normalStringSuffix "")
>     normalStringSuffix t = do
>         s <- takeTill (=='\'')
>         void $ char '\''
>         -- deal with '' as literal quote character
>         choice [do
>                 void $ char '\''
>                 normalStringSuffix $ T.concat [t,s,"''"]
>                ,return $ T.concat [t,s]]
>     eString = SqlString "E'" <$> (string "E'" *> eStringSuffix "")
>     eStringSuffix t = do
>         s <- takeTill (`elem` "\\'")
>         choice [do
>                 void $ string "\\'"
>                 eStringSuffix $ T.concat [t,s,"\\'"]
>                ,do
>                 void $ string "''"
>                 eStringSuffix $ T.concat [t,s,"''"]
>                ,do
>                 void $ char '\''
>                 return $ T.concat [t,s]
>                ,do
>                 c <- anyChar
>                 eStringSuffix $ T.concat [t,s,T.singleton c]]
>     dollarString = do
>         delim <- dollarDelim
>         y <- manyTill anyChar (string delim)
>         return $ SqlString delim $ T.pack y
>     dollarDelim :: Parser T.Text
>     dollarDelim = do
>       void $ char '$'
>       tag <- option "" identifierString
>       void $ char '$'
>       return $ T.concat ["$", tag, "$"]

> sqlNumber :: SQLSyntaxDialect -> Parser Token
> sqlNumber _ =

postgresql number parsing

digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits
where digits is one or more decimal digits (0 through 9). At least one digit must be before or after the decimal point, if one is used. At least one digit must follow the exponent marker (e), if one is present. There cannot be any spaces or other characters embedded in the constant. Note that any leading plus or minus sign is not actually considered part of the constant; it is an operator applied to the constant.

>    choice
>    [do
>     -- first char is a digit
>     d <- digits
>     -- try to read an fractional part or sci notation suffix
>     choice [do
>             s <- dotSuffix
>             return $ SqlNumber $ T.pack $ d ++ s
>            ,do
>             void $ char '.'
>             -- avoid parsing e.g. 4..5 as "4.",...
>             -- want to parse it as "4","..","5"
>             -- use choice to avoid impossible error
>             -- when peekCharing at end of input on parseonly
>             choice [do
>                     endOfInput
>                     return $ SqlNumber (T.pack $ d ++ ".")
>                    ,do
>                     x <- peekChar
>                     guard (x /= Just '.')
>                     return $ SqlNumber (T.pack $ d ++ ".")]
>            ,do
>             s <- eSuffix
>             return $ SqlNumber $ T.pack $ d ++ s
>            ,return $ SqlNumber $ T.pack d]
>    ,(SqlNumber . T.pack) <$> dotSuffix]
>  where
>    dotSuffix = do
>        void $ char '.'
>        d <- digits
>        choice [do
>                s <- eSuffix
>                return $ '.':(d ++ s)
>               ,return $ '.':d]
>    eSuffix = do
>        void $ char 'e'
>        sn <- option Nothing (Just <$> (char '+' <|> char '-'))
>        d <- digits
>        maybe (return $ 'e':d) (\sn' -> return $ 'e':sn':d) sn
>    digits = many1 digit

Symbols:

Copied from the postgresql manual:

An operator name is a sequence of up to NAMEDATALEN-1 (63 by default) characters from the following list:

+ - * / < > = ~ ! @ # % ^ & | ` ?

There are a few restrictions on operator names, however:
-- and /* cannot appear anywhere in an operator name, since they will be taken as the start of a comment.

A multiple-character operator name cannot end in + or -, unless the name also contains at least one of these characters:

~ ! @ # % ^ & | ` ?

For example, @- is an allowed operator name, but *- is not. This restriction allows PostgreSQL to parse SQL-compliant queries without requiring spaces between tokens.
When working with non-SQL-standard operator names, you will usually need to separate adjacent operators with spaces to avoid ambiguity. For example, if you have defined a left unary operator named @, you cannot write X*@Y; you must write X* @Y to ensure that PostgreSQL reads it as two operator names not one.

TODO: try to match this behaviour

> symbol :: SQLSyntaxDialect -> Parser Token
> symbol dialect = Symbol <$>
>     choice
>     [satisfyT (inClass simpleSymbols)
>     ,string ".."
>     ,string "."
>     ,string "::"
>     ,string ":="
>     ,string ":"
>     ,T.pack <$> anotherOp
>     ]
>   where
>     anotherOp = do
>       -- first char can be any, this is always a valid operator name
>       c0 <- satisfy (`elem` compoundFirst)
>       --recurse:
>       let r = choice
>               [do
>                c1 <- satisfy (`elem` compoundTail)
>                choice [do
>                        x <- r
>                        return $ c1 : x
>                       ,return [c1]]
>               ,try $ do
>                a <- satisfy (`elem` "+-")
>                b <- r
>                return $ a : b]
>       choice [do
>               tl <- r
>               return $ c0 : tl
>              ,return [c0]]
>     satisfyT p = T.singleton <$> satisfy p
>     {-biggerSymbol =
>         startsWith (inClass compoundFirst)
>                    (inClass compoundTail) -}
>     simpleSymbols | dialect == PostgreSQLDialect = "(),;[]"
>                   | otherwise = "(),;"
>     compoundFirst | dialect == PostgreSQLDialect = "*/<>=~!@#%^&|`?+-"
>                   | otherwise = "*/<>=~!%^&|`?+-"
>     compoundTail | dialect == PostgreSQLDialect = "*/<>=~!@#%^&|`?"
>                  | otherwise = "*/<>=~!%^&|`?"


> sqlWhitespace :: SQLSyntaxDialect -> Parser Token
> sqlWhitespace _ = (Whitespace . T.pack) <$> many1 (satisfy isSpace)

> positionalArg :: SQLSyntaxDialect -> Parser Token
> positionalArg PostgreSQLDialect =
>   PositionalArg <$> (char '$' *> decimal)

> positionalArg _ = satisfy (const False) >> error "positional arg unsupported"

> lineComment :: SQLSyntaxDialect -> Parser Token
> lineComment _ =
>     (\s -> LineComment $ T.concat ["--",s]) <$>
>     (string "--" *> choice
>                     [flip T.snoc '\n' <$> takeTill (=='\n') <* char '\n'
>                     ,AP.takeWhile (/='\n') <* endOfInput
>                     ])

> blockComment :: SQLSyntaxDialect -> Parser Token
> blockComment _ =
>     (\s -> BlockComment $ T.concat ["/*",s]) <$>
>     (string "/*" *> commentSuffix 0)
>   where
>     commentSuffix :: Int -> Parser T.Text
>     commentSuffix n = do
>       -- read until a possible end comment or nested comment
>       x <- AP.takeWhile (\e -> e /= '/' && e /= '*')
>       choice [-- close comment: if the nesting is 0, done
>               -- otherwise recurse on commentSuffix
>               string "*/" *> let t = T.concat [x,"*/"]
>                              in if n == 0
>                                 then return t
>                                 else (\s -> T.concat [t,s]) <$> commentSuffix (n - 1)
>               -- nested comment, recurse
>              ,string "/*" *> ((\s -> T.concat [x,"/*",s]) <$> commentSuffix (n + 1))
>               -- not an end comment or nested comment, continue
>              ,T.cons <$> anyChar <*> commentSuffix n]

> splice :: SQLSyntaxDialect -> Parser Token
> splice _ = do
>   Splice
>   <$> (char '$' *> letter)
>   <*> (char '(' *> identifierString <* char ')')


> startsWith :: (Char -> Bool) -> (Char -> Bool) -> Parser T.Text
> startsWith p ps = do
>   c <- satisfy p
>   choice [T.cons c <$> (AP.takeWhile1 ps)
>          ,return $ T.singleton c]
