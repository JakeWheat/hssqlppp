
> {-# LANGUAGE TupleSections,OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.LexInternal
>     (Token(..)
>     ,prettyToken
>     ,lexToken
>     ,lexTokens
>     ) where

> import qualified Data.Text as T
> import qualified Data.Text.Lazy as LT
> import Text.Parsec
> --Cimport Text.Parsec.String hdi
> import Text.Parsec.Text
> import Control.Applicative hiding ((<|>), many)
> import Data.Char
> import Database.HsSqlPpp.Internals.Dialect
> import Control.Monad
> import Prelude hiding (takeWhile)
> import Data.Maybe

> -- | Represents a lexed token
> data Token
>     -- | a symbol in postgresql dialect is one of the following:
>     --
>     -- * one of the characters (),;[]{}  (the {} is for odbc)
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
>     --  | This is a prefixed variable symbol, such as :var, @var or #var
>     -- (only :var is used in ansi dialect)
>     | PrefixedVariable Char T.Text
>
>     -- | a postgresql positional arg, e.g. $1
>     | PositionalArg Int
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
>     | SqlString T.Text T.Text T.Text
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
> prettyToken :: Dialect -> Token -> LT.Text
> prettyToken _ (Symbol s) = LT.fromChunks [s]
> prettyToken _ (Identifier Nothing t) = LT.fromChunks [t]
> prettyToken _ (Identifier (Just (a,b)) t) =
>     LT.fromChunks [T.singleton a, t, T.singleton b]
> prettyToken _ (PrefixedVariable c s) = LT.cons c (LT.fromChunks [s])
> prettyToken _ (SqlString q r t) = LT.fromChunks [q,t,r]
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

> lexTokens :: Dialect -> FilePath -> Maybe (Int,Int) -> T.Text -> Either ParseError [((FilePath,Int,Int),Token)]
> lexTokens dialect fn' mp txt =
>     let (l',c') = fromMaybe (1,1) mp
>     in runParser (setPos (fn',l',c') *> many_p <* eof) () "" txt
>   where

pretty hacky, want to switch to a different lexer for copy from stdin
statements

if we see 'from stdin;' then try to lex a copy payload

>      many_p = some_p `mplus` return []
>      some_p = do
>        tok <- lexToken dialect
>        case tok of
>          (_, Identifier Nothing t) | T.map toLower t == "from" -> (tok:) <$> seeStdin
>          _ -> (tok:) <$> many_p
>      seeStdin = do
>        tok <- lexToken dialect
>        case tok of
>          (_,Identifier Nothing t) | T.map toLower t == "stdin" -> (tok:) <$> seeColon
>          (_,x) | isWs x -> (tok:) <$> seeStdin
>          _ -> (tok:) <$> many_p
>      seeColon = do
>        tok <- lexToken dialect
>        case tok of
>          (_,Symbol ";") -> (tok:) <$> copyPayload
>          _ -> (tok:) <$> many_p
>      copyPayload = do
>        p' <- getPosition
>        let pos = (sourceName p',sourceLine p', sourceColumn p')
>        tok <- char '\n' *>
>             ((\x -> (pos, CopyPayload $ T.pack $ x ++ "\n"))
>              <$> manyTill anyChar (try $ string "\n\\.\n"))
>        --let (_,CopyPayload t) = tok
>        --trace ("payload is '" ++ T.unpack t ++ "'") $ return ()
>        (tok:) <$> many_p
>      setPos (fn,l,c) = do
>         fmap (flip setSourceName fn
>                . flip setSourceLine l
>                . flip setSourceColumn c) getPosition
>           >>= setPosition
>      isWs :: Token -> Bool
>      isWs (Whitespace {}) = True
>      isWs (BlockComment {}) = True
>      isWs (LineComment {}) = True
>      isWs _ = False

> -- | parser for a sql token
> lexToken :: Dialect -> Parser ((FilePath,Int,Int),Token)
> lexToken d = do
>     p' <- getPosition
>     let p = (sourceName p',sourceLine p', sourceColumn p')
>     (p,) <$> choice [sqlString d
>                     ,identifier d
>                     ,lineComment d
>                     ,blockComment d
>                     ,sqlNumber d
>                     ,symbol d
>                     ,sqlWhitespace d
>                     ,positionalArg d
>                     ,splice d]

> identifier :: Dialect -> Parser Token

sql server: identifiers can start with @ or #
quoting uses [] or ""

TODO: fix all the "qiden" parsers to allow "qid""en"

> identifier (Dialect {diSyntaxFlavour = SqlServer}) =
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

> identifier (Dialect {diSyntaxFlavour = Oracle}) =
>     choice
>     [Identifier (Just ('"','"'))
>      <$> (char '"' *> takeWhile1 (/='"') <* char '"')
>     ,Identifier Nothing <$> identifierStringPrefix ':'
>     ,Identifier Nothing <$> identifierString
>     ]

> identifier (Dialect {diSyntaxFlavour = Postgres}) = regularIdentifier
> identifier (Dialect {diSyntaxFlavour = Ansi}) = regularIdentifier

> regularIdentifier :: Parser Token
> regularIdentifier =
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

> positionalArg :: Dialect -> Parser Token
> -- uses try so we don't get confused with $splices
> positionalArg d =
>     guard (diSyntaxFlavour d == Postgres) >>
>     try (PositionalArg <$> (char '$' *> (read <$> many1 digit)))


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

> sqlString :: Dialect -> Parser Token
> sqlString _ =
>     choice [normalString
>            ,eString
>            ,dollarString]
>   where
>     normalString = SqlString "'" "'" <$> (char '\'' *> normalStringSuffix "")
>     normalStringSuffix t = do
>         s <- takeTill (=='\'')
>         void $ char '\''
>         -- deal with '' as literal quote character
>         choice [do
>                 void $ char '\''
>                 normalStringSuffix $ T.concat [t,s,"''"]
>                ,return $ T.concat [t,s]]
>     eString = SqlString "E'" "'" <$> (try (string "E'") *> eStringSuffix "")
>     eStringSuffix :: T.Text -> Parser T.Text
>     eStringSuffix t = do
>         s <- takeTill (`elem` ("\\'"::String))
>         choice [do
>                 try $ void $ string "\\'"
>                 eStringSuffix $ T.concat [t,s,"\\'"]
>                ,do
>                 void $ try $ string "''"
>                 eStringSuffix $ T.concat [t,s,"''"]
>                ,do
>                 void $ char '\''
>                 return $ T.concat [t,s]
>                ,do
>                 c <- anyChar
>                 eStringSuffix $ T.concat [t,s,T.singleton c]]
>     dollarString = do
>         delim <- dollarDelim
>         y <- manyTill anyChar (try $ string $ T.unpack delim)
>         return $ SqlString delim delim $ T.pack y
>     dollarDelim :: Parser T.Text
>     dollarDelim = try $ do
>       void $ char '$'
>       tag <- option "" identifierString
>       void $ char '$'
>       return $ T.concat ["$", tag, "$"]

postgresql number parsing

digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits
where digits is one or more decimal digits (0 through 9). At least one digit must be before or after the decimal point, if one is used. At least one digit must follow the exponent marker (e), if one is present. There cannot be any spaces or other characters embedded in the constant. Note that any leading plus or minus sign is not actually considered part of the constant; it is an operator applied to the constant.

> sqlNumber :: Dialect -> Parser Token
> sqlNumber _ =
>     (SqlNumber . T.pack) <$> completeNumber
>     -- this is for definitely avoiding possibly ambiguous source
>     -- with a special exception for a .. operator
>     <* choice
>        [void $ lookAhead (string "..")
>        ,void $ notFollowedBy (oneOf "eE.")
>        ]
>   where
>     completeNumber =
>       (int <??> (pp dot <??.> pp int)
>       -- try is used in case we read a dot
>       -- and it isn't part of a number
>       -- if there are any following digits, then we commit
>       -- to it being a number and not something else
>       <|> try ((++) <$> dot <*> int))
>       <??> pp expon

>     int = many1 digit
>     -- if we see two dots together, leave them
>     -- so we can parse things like 1..2 (used in postgres)
>     dot = try (string "." <* notFollowedBy (char '.'))
>     expon = (:) <$> oneOf "eE" <*> sInt
>     sInt = (++) <$> option "" (string "+" <|> string "-") <*> int
>     pp = (<$$> (++))

> (<??>) :: Parser a -> Parser (a -> a) -> Parser a
> p <??> q = p <**> option id q

> (<??.>) :: Parser (a -> a) -> Parser (a -> a) -> Parser (a -> a)
> (<??.>) pa pb = (.) `c` pa <*> option id pb
>   -- todo: fix this mess
>   where c = (<$>) . flip

> (<$$>) :: Applicative f =>
>       f b -> (a -> b -> c) -> f (a -> c)
> (<$$>) pa c = pa <**> pure (flip c)

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

inClass :: String -> Char -> Bool

> symbol :: Dialect -> Parser Token
> symbol dialect = Symbol <$> T.pack <$>
>     choice
>     [(:[]) <$> satisfy (`elem` simpleSymbols)
>     ,try $ string ".."
>     ,string "."
>     ,try $ string "::"
>     ,try $ string ":="
>     ,string ":"
>     ,anotherOp
>     ]
>   where
>     anotherOp :: Parser String
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
>                a <- satisfy (`elem` ("+-"::String))
>                b <- r
>                return $ a : b]
>       choice [do
>               tl <- r
>               return $ c0 : tl
>              ,return [c0]]
>     {-biggerSymbol =
>         startsWith (inClass compoundFirst)
>                    (inClass compoundTail) -}
>     isPostgres = diSyntaxFlavour dialect == Postgres
>     simpleSymbols :: String
>     simpleSymbols | isPostgres = "(),;[]{}"
>                   | otherwise = "(),;{}"
>     compoundFirst :: String
>     compoundFirst | isPostgres = "*/<>=~!@#%^&|`?+-"
>                   | otherwise = "*/<>=~!%^&|`?+-"
>     compoundTail :: String
>     compoundTail | isPostgres = "*/<>=~!@#%^&|`?"
>                  | otherwise = "*/<>=~!%^&|`?"


> sqlWhitespace :: Dialect -> Parser Token
> sqlWhitespace _ = (Whitespace . T.pack) <$> many1 (satisfy isSpace)

> lineComment :: Dialect -> Parser Token
> lineComment _ =
>     (\s -> (LineComment . T.pack) $ concat ["--",s]) <$>
>     -- try is used here in case we see a - symbol
>     -- once we read two -- then we commit to the comment token
>     (try (string "--") *> (
>      conc <$> manyTill anyChar (lookAhead lineCommentEnd) <*> lineCommentEnd))
>   where
>     conc a Nothing = a
>     conc a (Just b) = a ++ b
>     lineCommentEnd = Just "\n" <$ char '\n' <|> Nothing <$ eof

> blockComment :: Dialect -> Parser Token
> blockComment _ =
>     (\s -> BlockComment $ T.concat ["/*",s]) <$>
>     (try (string "/*") *> commentSuffix 0)
>   where
>     commentSuffix :: Int -> Parser T.Text
>     commentSuffix n = do
>       -- read until a possible end comment or nested comment
>       x <- takeWhile (\e -> e /= '/' && e /= '*')
>       choice [-- close comment: if the nesting is 0, done
>               -- otherwise recurse on commentSuffix
>               try (string "*/") *> let t = T.concat [x,"*/"]
>                                    in if n == 0
>                                       then return t
>                                       else (\s -> T.concat [t,s]) <$> commentSuffix (n - 1)
>               -- nested comment, recurse
>              ,try (string "/*") *> ((\s -> T.concat [x,"/*",s]) <$> commentSuffix (n + 1))
>               -- not an end comment or nested comment, continue
>              ,(\c s -> T.concat [x,T.pack [c], s]) <$> anyChar <*> commentSuffix n]

> splice :: Dialect -> Parser Token
> splice _ = do
>   Splice
>   <$> (char '$' *> letter)
>   <*> (char '(' *> identifierString <* char ')')

> startsWith :: (Char -> Bool) -> (Char -> Bool) -> Parser T.Text
> startsWith p ps = do
>   c <- satisfy p
>   choice [T.cons c <$> (takeWhile1 ps)
>          ,return $ T.singleton c]

> takeWhile1 :: (Char -> Bool) -> Parser T.Text
> takeWhile1 p = T.pack <$> many1 (satisfy p)

> takeWhile :: (Char -> Bool) -> Parser T.Text
> takeWhile p = T.pack <$> many (satisfy p)

> takeTill :: (Char -> Bool) -> Parser T.Text
> takeTill p =
>     T.pack <$> manyTill anyChar (peekSatisfy p)

> peekSatisfy :: (Char -> Bool) -> Parser ()
> peekSatisfy p = do
>     void $ lookAhead (satisfy p)
