
> {-# LANGUAGE TupleSections,OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.LexInternal
>     (Token(..)
>     ,prettyToken
>     ,lexToken
>     ,lexTokens
>     ) where

> --import qualified Data.Text as T
> import qualified Data.Text.Lazy as LT
> import Text.Parsec
> --Cimport Text.Parsec.String hdi
> import Text.Parsec.Text.Lazy
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
>     = Symbol String
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
>     | Identifier (Maybe (String,String)) String
>
>     --  | This is a prefixed variable symbol, such as :var, @var or #var
>     -- (only :var is used in ansi dialect)
>     | PrefixedVariable Char String
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
>     | SqlString String String String
>
>     -- | a number literal (integral or otherwise), stored in original format
>     -- unchanged
>     | SqlNumber String
>
>     -- | non-significant whitespace (space, tab, newline) (strictly speaking,
>     -- it is up to the client to decide whether the whitespace is significant
>     -- or not)
>     | Whitespace String
>
>     -- | a commented line using --, contains every character starting with the
>     -- \'--\' and including the terminating newline character if there is one
>     -- - this will be missing if the last line in the source is a line comment
>     -- with no trailing newline
>     | LineComment String
>
>     -- | a block comment, \/* stuff *\/, includes the comment delimiters
>     | BlockComment String
>
>     -- | an antiquotation splice, e.g. $x(stuff)
>     | Splice Char String
>
>     -- | the copy data in a copy from stdin
>     | CopyPayload String
>       deriving (Eq,Show)

> -- | Accurate pretty printing, if you lex a bunch of tokens,
> -- then pretty print them, should should get back exactly the
> -- same string
> prettyToken :: Dialect -> Token -> String
> prettyToken _ (Symbol s) = s
> prettyToken _ (Identifier Nothing t) = t
> prettyToken _ (Identifier (Just (a,b)) t) = a ++ t ++ b
> prettyToken _ (PrefixedVariable c s) = c:s
> prettyToken _ (SqlString q r t) = q ++ t ++ r
> prettyToken _ (SqlNumber r) = r
> prettyToken _ (Whitespace t) = t
> prettyToken _ (PositionalArg n) = '$':show n
> prettyToken _ (LineComment l) = l
> prettyToken _ (BlockComment c) = c
> prettyToken _ (Splice c t) = '$':c:'(':t ++ ")"
> prettyToken _ (CopyPayload s) = s ++ "\\.\n"


not sure how to get the position information in the parse errors

TODO: try to make all parsers applicative only
investigate what is missing for postgresql
investigate differences for sql server, oracle, maybe db2 and mysql
  also

> lexTokens :: Dialect -> FilePath -> Maybe (Int,Int) -> LT.Text -> Either ParseError [((FilePath,Int,Int),Token)]
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
>          (_, Identifier Nothing t) | map toLower t == "from" -> (tok:) <$> seeStdin
>          _ -> (tok:) <$> many_p
>      seeStdin = do
>        tok <- lexToken dialect
>        case tok of
>          (_,Identifier Nothing t) | map toLower t == "stdin" -> (tok:) <$> seeColon
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
>             ((\x -> (pos, CopyPayload $ x ++ "\n"))
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
>                     ,positionalArg d
>                     ,dontParseEndBlockComment d
>                     ,prefixedVariable d
>                     ,symbol d
>                     ,sqlWhitespace d
>                     ,splice d]

Parses identifiers:

simple_identifier_23
u&"unicode quoted identifier"
"quoted identifier"
"quoted identifier "" with double quote char"
`mysql quoted identifier`

> identifier :: Dialect -> Parser Token
> identifier d =
>     choice
>     [Identifier (Just ("\"","\"")) <$> qiden
>      -- try is used here to avoid a conflict with identifiers
>      -- and quoted strings which also start with a 'u'
>     ,Identifier (Just ("u&\"","\"")) <$> (try (string "u&") *> qiden)
>     ,Identifier (Just ("U&\"","\"")) <$> (try (string "U&") *> qiden)
>     ,Identifier Nothing <$> identifierString
>      -- todo: dialect protection
>     -- Identifier (Just ("`","`"))
>     -- <$> (char '`' *> takeWhile1 (/='`') <* char '`')
>     ,guard (diSyntaxFlavour d == SqlServer) >>
>      Identifier (Just ("[","]"))
>      <$> (char '[' *> takeWhile1 (`notElem` ("[]"::String)) <* char ']')
>     ]
>   where
>     qiden = char '"' *> qidenSuffix ""
>     qidenSuffix t = do
>         s <- takeTill (=='"')
>         void $ char '"'
>         -- deal with "" as literal double quote character
>         choice [do
>                 void $ char '"'
>                 qidenSuffix $ concat [t,s,"\"\""]
>                ,return $ concat [t,s]]


This parses a valid identifier without quotes.

> identifierString :: Parser String
> identifierString =
>     startsWith (\c -> c == '_' || isAlpha c) isIdentifierChar

this can be moved to the dialect at some point

> isIdentifierChar :: Char -> Bool
> isIdentifierChar c = c == '_' || isAlphaNum c

> prefixedVariable :: Dialect -> Parser Token
> prefixedVariable  d = try $ choice
>     [PrefixedVariable <$> char ':' <*> identifierString
>     ,guard (diSyntaxFlavour d == SqlServer) >>
>      PrefixedVariable <$> char '@' <*> identifierString
>     ,guard (diSyntaxFlavour d `elem` [Oracle,SqlServer]) >>
>      PrefixedVariable <$> char '#' <*> identifierString
>     ]


> positionalArg :: Dialect -> Parser Token
> -- uses try so we don't get confused with $splices
> positionalArg d =
>     guard (diSyntaxFlavour d == Postgres) >>
>     PositionalArg <$> try (char '$' *> (read <$> many1 digit))


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
> sqlString d = dollarString <|> csString <|> normalString
>   where
>     dollarString = do
>         -- need a solution for the dialect for quasi quotes
>         --guard $ diSyntaxFlavour d == Postgres
>         delim <- (\x -> concat ["$",x,"$"])
>                  <$> try (char '$' *> option "" identifierString <* char '$')
>         SqlString delim delim  <$> manyTill anyChar (try $ string delim)
>     normalString = SqlString "'" "'" <$> (char '\'' *> normalStringSuffix False "")
>     normalStringSuffix allowBackslash t = do
>         s <- takeTill $ if allowBackslash
>                         then (`elem` ("'\\"::String))
>                         else (== '\'')
>         -- deal with '' or \' as literal quote character
>         choice [do
>                 ctu <- choice ["''" <$ try (string "''")
>                               ,"\\'" <$ try (string "\\'")
>                               ,"\\" <$ char '\\']
>                 normalStringSuffix allowBackslash $ concat [t,s,ctu]
>                ,concat [t,s] <$ char '\'']
>     -- try is used to to avoid conflicts with
>     -- identifiers which can start with n,b,x,u
>     -- once we read the quote type and the starting '
>     -- then we commit to a string
>     -- it's possible that this will reject some valid syntax
>     -- but only pathalogical stuff, and I think the improved
>     -- error messages and user predictability make it a good
>     -- pragmatic choice
>     csString
>       | diSyntaxFlavour d == Postgres =
>         choice [SqlString <$> try (string "e'" <|> string "E'")
>                           <*> return "'" <*> normalStringSuffix True ""
>                ,csString']
>       | otherwise = csString'
>     csString' = SqlString
>                 <$> try cs
>                 <*> return "'"
>                 <*> normalStringSuffix False ""
>     csPrefixes = "nNbBxX"
>     cs = choice $ (map (\x -> string ([x] ++ "'")) csPrefixes)
>                   ++ [string "u&'"
>                      ,string "U&'"]

postgresql number parsing

digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits
where digits is one or more decimal digits (0 through 9). At least one digit must be before or after the decimal point, if one is used. At least one digit must follow the exponent marker (e), if one is present. There cannot be any spaces or other characters embedded in the constant. Note that any leading plus or minus sign is not actually considered part of the constant; it is an operator applied to the constant.

> sqlNumber :: Dialect -> Parser Token
> sqlNumber _ =
>     SqlNumber <$> completeNumber
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

A symbol is one of the two character symbols, or one of the single
character symbols in the two lists below.

> symbol :: Dialect -> Parser Token
> symbol d | diSyntaxFlavour d == Postgres =
>     Symbol <$> choice (otherSymbol ++ [singlePlusMinus,opMoreChars])

rules

An operator name is a sequence of up to NAMEDATALEN-1 (63 by default) characters from the following list:

+ - * / < > = ~ ! @ # % ^ & | ` ?

There are a few restrictions on operator names, however:
-- and /* cannot appear anywhere in an operator name, since they will be taken as the start of a comment.

A multiple-character operator name cannot end in + or -, unless the name also contains at least one of these characters:

~ ! @ # % ^ & | ` ?

>   where
>     -- other symbols are all the tokens which parse as symbols in
>     -- this lexer which aren't considered operators in postgresql
>     -- a single ? is parsed as a operator here instead of an other
>     -- symbol because this is the least complex way to do it
>     otherSymbol = many1 (char '.') :
>                   try (string ":=") :
>                   -- parse :: and : and avoid allowing ::: or more
>                   try (string "::" <* notFollowedBy (char ':')) :
>                   try (string ":" <* notFollowedBy (char ':')) :
>                   (map (string . (:[])) "[],;()"
>                    ++ if True -- allowOdbc d
>                       then [string "{", string "}"]
>                       else []
>                   )

exception char is one of:
~ ! @ # % ^ & | ` ?
which allows the last character of a multi character symbol to be + or
-

>     allOpSymbols = "+-*/<>=~!@#%^&|`?"
>     -- these are the symbols when if part of a multi character
>     -- operator permit the operator to end with a + or - symbol
>     exceptionOpSymbols = "~!@#%^&|`?"

>     -- special case for parsing a single + or - symbol
>     singlePlusMinus = try $ do
>       c <- oneOf "+-"
>       notFollowedBy $ oneOf allOpSymbols
>       return [c]

>     -- this is used when we are parsing a potentially multi symbol
>     -- operator and we have alread seen one of the 'exception chars'
>     -- and so we can end with a + or -
>     moreOpCharsException = do
>        c <- oneOf (filter (`notElem` ("-/*"::String)) allOpSymbols)
>             -- make sure we don't parse a comment starting token
>             -- as part of an operator
>             <|> try (char '/' <* notFollowedBy (char '*'))
>             <|> try (char '-' <* notFollowedBy (char '-'))
>             -- and make sure we don't parse a block comment end
>             -- as part of another symbol
>             <|> try (char '*' <* notFollowedBy (char '/'))
>        (c:) <$> option [] moreOpCharsException

>     opMoreChars = choice
>        [-- parse an exception char, now we can finish with a + -
>         (:)
>         <$> oneOf exceptionOpSymbols
>         <*> option [] moreOpCharsException
>        ,(:)
>         <$> (-- parse +, make sure it isn't the last symbol
>              try (char '+' <* lookAhead (oneOf allOpSymbols))
>              <|> -- parse -, make sure it isn't the last symbol
>                  -- or the start of a -- comment
>              try (char '-'
>                   <* notFollowedBy (char '-')
>                   <* lookAhead (oneOf allOpSymbols))
>              <|> -- parse / check it isn't the start of a /* comment
>              try (char '/' <* notFollowedBy (char '*'))
>              <|> -- make sure we don't parse */ as part of a symbol
>              try (char '*' <* notFollowedBy (char '/'))
>              <|> -- any other ansi operator symbol
>              oneOf "<>=")
>         <*> option [] opMoreChars
>        ]

> symbol d | diSyntaxFlavour d == SqlServer =
>    Symbol <$> choice (otherSymbol ++ regularOp)
>  where
>    otherSymbol = string "." :
>                  (map (string . (:[])) ",;():?"
>                   ++ if True -- allowOdbc d
>                      then [string "{", string "}"]
>                      else [])

try is used because most of the first characters of the two character
symbols can also be part of a single character symbol

>    regularOp = map (try . string) [">=","<=","!=","<>"]
>                ++ map (string . (:[])) "+-^*/%~&<>="
>                ++ [char '|' *>
>                    choice ["||" <$ char '|' <* notFollowedBy (char '|')
>                           ,return "|"]]

> symbol _d =
>    Symbol <$> choice (otherSymbol ++ regularOp)
>  where
>    otherSymbol = many1 (char '.') :
>                  (map (string . (:[])) "[],;():?"
>                   ++ if True -- allowOdbc d
>                      then [string "{", string "}"]
>                      else [])

try is used because most of the first characters of the two character
symbols can also be part of a single character symbol

>    regularOp = map (try . string) [">=","<=","!=","<>"]
>                ++ map (string . (:[])) "+-^*/%~&<>=[]"
>                ++ [char '|' *>
>                    choice ["||" <$ char '|' <* notFollowedBy (char '|')
>                           ,return "|"]]


> sqlWhitespace :: Dialect -> Parser Token
> sqlWhitespace _ = Whitespace <$> many1 (satisfy isSpace)

> lineComment :: Dialect -> Parser Token
> lineComment _ =
>     (\s -> LineComment $ concat ["--",s]) <$>
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
>     (\s -> BlockComment $ concat ["/*",s]) <$>
>     (try (string "/*") *> commentSuffix 0)
>   where
>     commentSuffix :: Int -> Parser String
>     commentSuffix n = do
>       -- read until a possible end comment or nested comment
>       x <- takeWhile (\e -> e /= '/' && e /= '*')
>       choice [-- close comment: if the nesting is 0, done
>               -- otherwise recurse on commentSuffix
>               try (string "*/") *> let t = concat [x,"*/"]
>                                    in if n == 0
>                                       then return t
>                                       else (\s -> concat [t,s]) <$> commentSuffix (n - 1)
>               -- nested comment, recurse
>              ,try (string "/*") *> ((\s -> concat [x,"/*",s]) <$> commentSuffix (n + 1))
>               -- not an end comment or nested comment, continue
>              ,(\c s -> concat [x, [c], s]) <$> anyChar <*> commentSuffix n]

> dontParseEndBlockComment :: Dialect -> Parser Token
> dontParseEndBlockComment _ =
>     -- don't use try, then it should commit to the error
>     try (string "*/") *> fail "comment end without comment start"

> splice :: Dialect -> Parser Token
> splice _ = do
>   Splice
>   <$> (char '$' *> letter)
>   <*> (char '(' *> identifierString <* char ')')

> startsWith :: (Char -> Bool) -> (Char -> Bool) -> Parser String
> startsWith p ps = do
>   c <- satisfy p
>   choice [(c:) <$> (takeWhile1 ps)
>          ,return [c]]

> takeWhile1 :: (Char -> Bool) -> Parser String
> takeWhile1 p = many1 (satisfy p)

> takeWhile :: (Char -> Bool) -> Parser String
> takeWhile p = many (satisfy p)

> takeTill :: (Char -> Bool) -> Parser String
> takeTill p =
>     manyTill anyChar (peekSatisfy p)

> peekSatisfy :: (Char -> Bool) -> Parser ()
> peekSatisfy p = do
>     void $ lookAhead (satisfy p)
