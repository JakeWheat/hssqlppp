
This file contains the lexer for sql source text.

Lexicon:

~~~~
string
identifier or keyword
symbols - operators and ;,()[]
positional arg
int
float
copy payload (used to lex copy from stdin data)
~~~~

> {-# LANGUAGE FlexibleContexts,OverloadedStrings,NoMonomorphismRestriction #-}
> module Database.HsSqlPpp.Parsing.Lexer (
>               Token
>              ,Tok(..)
>              ,lexSql
>              ,identifierString
>              ,LexState
>              ) where
> import Text.Parsec hiding(many, optional, (<|>),string)
> import qualified Text.Parsec as TP
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language
> --import Text.Parsec.String
> import Text.Parsec.Pos
>
> import Control.Applicative
> import Control.Monad.Identity

> import Data.Maybe
>
> import Database.HsSqlPpp.Parsing.ParseErrors
> import Database.HsSqlPpp.Utils.Utils
> -- import Database.HsSqlPpp.Ast.Name
> import Database.HsSqlPpp.SqlDialect
> {-import Prelude (String,Integer,Char,Eq,Show,FilePath,Either(..)
>                ,Int,either,($),(==),(&&),otherwise,(++)
>                ,replicate,concat,(.),Bool(..))-}
> import qualified Data.Text as T
> import qualified Data.Text.Lazy as LT

================================================================================

= data types

> type Token = (SourcePos, Tok)
>
> -- | the token type for lexing
> data Tok = StringTok T.Text T.Text -- ^ delim, value ,delim will one of
>                                    -- ', $$, $[stuff]$

>          | IdStringTok T.Text -- ^ a name component
>          | QIdStringTok T.Text -- ^ quoted namecomponent, also used
>                                -- when parsing '@local', '#temp' in sql server dialect

>          | SymbolTok T.Text -- ^ operators, and *()[],;: and also .
>          | PositionalArgTok Integer -- ^ used for $1, etc.

Use a numbertok with a string to parse numbers. This is mainly so that
numeric constants can be parsed accurately - if they are parsed to
floats in the ast then converted back to numeric, then the accuracy
can be lost (e.g. something like "0.2" parsing to 0.199999999 float.

>          | NumberTok T.Text -- ^ number
>          | CopyPayloadTok LT.Text -- ^ hacky support support copy from stdin; with inline data
>          | SpliceTok Char T.Text -- ^ a splice token, the splice char and the string
>                                  -- e.g. $e(stuff) -> SpliceTok \'e\' \"stuff\"
>            deriving (Eq,Show)
>
> type LexState = [Tok]
> type SParser s = ParsecT s LexState Identity
>
> lexSql :: Stream s Identity Char =>
>           SQLSyntaxDialect -- ^ dialect
>        -> FilePath -- ^ filename to use in errors
>        -> Maybe (Int,Int) -- ^ starting line and column no for positions
>        -> s -- ^ source to lex
>        -> Either ParseErrorExtra [Token]
> lexSql d f sp src =
>   either (Left . toParseErrorExtra f sp) Right
>   $ runParser lx [] f src
>   where
>     --lx :: SParser str [Token]
>     lx = maybe (return ()) (\(l,c) -> setPosition (newPos f l c)) sp
>          >> sqlTokens d

================================================================================

= lexers

lexer for tokens, contains a hack for copy from stdin with inline
table data.

> sqlTokens :: Stream s Identity Char =>
>              SQLSyntaxDialect -> SParser s [Token]
> sqlTokens d =
>   setState [] >>
>   whiteSpace >>
>   many (sqlToken d) <* eof

Lexer for an individual token.

Could lex lazily and when the lexer reads a copy from stdin statement,
it switches lexers to lex the inline table data, then switches
back. Don't know how to do this in parsec, or even if it is possible,
so as a work around, you use the state to trap if we've just seen 'from
stdin;', if so, you read the copy payload as one big token, otherwise
we read a normal token.

> sqlToken :: Stream s Identity Char =>
>             SQLSyntaxDialect -> SParser s Token
> sqlToken d = do
>   sp <- getPosition
>   sta <- getState
>   t <- if sta == [ft,st,mt]
>        then copyPayload
>        else choice
>             [try sqlNumber
>             ,try positionalArg
>             ,try splice
>             ,try sqlString
>             ,try (idString d)
>             ,try (qidString d)
>             ,sqlSymbol d]
>   updateState $ \stt ->
>              case () of
>                      _ | stt == [] && t == ft -> [ft]
>                        | stt == [ft] && t == st -> [ft,st]
>                        | stt == [ft,st] && t == mt -> [ft,st,mt]
>                        | otherwise -> []
>
>   return (sp,t)
>   where
>     ft = IdStringTok "from"
>     st = IdStringTok "stdin"
>     mt = SymbolTok ";"

> splice :: Stream s Identity Char =>
>           SParser s Tok
> splice = lexeme $
>   SpliceTok <$> (char '$' *> letter)
>     <*> (char '(' *> identifierString <* char ')')

== specialized token parsers

> sqlString :: Stream s Identity Char =>
>              SParser s Tok
> sqlString = stringQuotes <|> stringLD
>   where
>     --parse a string delimited by single quotes
>     stringQuotes = StringTok "\'" <$> stringPar
>     stringPar = optional (char 'E') *> char '\''
>                            *> (T.pack <$> readQuoteEscape) <* whiteSpace
>     --(readquoteescape reads the trailing ')

have to read two consecutive single quotes as a quote character
instead of the end of the string, probably an easier way to do this

other escapes (e.g. \n \t) are left unprocessed

>     readQuoteEscape = do
>                       x <- anyChar
>                       if x == '\''
>                         then try ((x:) <$> (char '\'' *> readQuoteEscape))
>                              <|> return ""
>                         else (x:) <$> readQuoteEscape

parse a dollar quoted string

>     stringLD = do
>                -- cope with $$ as well as $[identifier]$
>                tag <- try (char '$' *> ((char '$' *> return "")
>                                    <|> (identifierString <* char '$')))
>                s <- lexeme $ manyTill anyChar
>                       (try $ char '$' <* string tag <* char '$')
>                return $ StringTok (T.concat ["$",tag,"$"]) $ T.pack s
>
> idString :: Stream s Identity Char =>
>             SQLSyntaxDialect -> SParser s Tok
> idString d =
>   choice
>   [do
>    guard (d == SQLServerDialect)
>    IdStringTok <$> tsqlPrefix identifierString
>   ,IdStringTok <$> identifierString
>   ]

> tsqlPrefix :: Stream s Identity Char =>
>               SParser s T.Text -> SParser s T.Text
> tsqlPrefix p =
>    choice
>    [char '@' *> (T.cons '@' <$> p)
>    ,char '#' *> (T.cons '#' <$> p)]

> qidString :: Stream s Identity Char =>
>              SQLSyntaxDialect -> SParser s Tok
> qidString d =
>   choice
>   [do
>    guard (d == SQLServerDialect)
>    QIdStringTok <$> tsqlPrefix identifierString
>   ,QIdStringTok <$> qidentifierString d]



> positionalArg :: Stream s Identity Char =>
>                  SParser s Tok
> positionalArg = char '$' >> PositionalArgTok <$> integer


Lexing symbols:

~~~~
approach 1:
try to keep multi symbol operators as single lexical items
(e.g. "==", "~=="

approach 2:
make each character a separate element
e.g. == lexes to ['=', '=']
then the parser sorts this out

Sort of using approach 1 at the moment, see below

== notes on symbols in pg operators
pg symbols can be made from:

=_*/<>=~!@#%^&|`?

no --, /* in symbols

can't end in + or - unless contains
~!@#%^&|?

Most of this isn't relevant for the current lexer.

== sql symbols for this lexer:

sql symbol is one of
()[],; - single character
+-*/<>=~!@#%^&|`? string - one or more of these, parsed until hit char
which isn't one of these (including whitespace). This will parse some
standard sql expressions wrongly at the moment, work around is to add
whitespace e.g. i think 3*-4 is valid sql, should lex as '3' '*' '-'
'4', but will currently lex as '3' '*-' '4'. This is planned to be
fixed in the parser.
.. := :: : - other special cases
A single * will lex as an identifier rather than a symbol, the parser
deals with this.

~~~~

> sqlSymbol :: Stream s Identity Char =>
>              SQLSyntaxDialect -> SParser s Tok
> sqlSymbol d =
>   SymbolTok <$> lexeme (choice [
>                          T.replicate 1 . T.singleton
>                            <$> oneOf (if d == SQLServerDialect
>                                       then "(),;"
>                                       else "()[],;")
>                         ,try $ string ".."
>                         ,string "."
>                         ,try $ string "::"
>                         ,try $ string ":="
>                         ,string ":"
>                         --,try $ string "$(" -- antiquote standard splice
>                         --,try $ string "$s(" -- antiquote string splice
>                         --,string "$i(" -- antiquote identifier splice
>                          --cut down version: don't allow operator to contain + or -
>                         ,T.pack <$> anotherOp d
>                         ])
>   where
>     anotherOp PostgreSQLDialect = do
>       -- first char can be any, this is always a valid operator name
>       c0 <- oneOf "*/<>=~!@#%^&|`?+-"
>       --recurse:
>       let r = choice
>               [do
>                c1 <- oneOf "*/<>=~!@#%^&|`?"
>                choice [do
>                        x <- r
>                        return $ c1 : x
>                       ,return [c1]]
>               ,try $ do
>                a <- oneOf "+-"
>                b <- r
>                return $ a : b]
>       choice [do
>               tl <- r
>               return $ c0 : tl
>              ,return [c0]]

todo: just hacked copy and paste of pg version (removed @,#), but sql
server has a much more limited range of operators

>     anotherOp SQLServerDialect = do
>       -- first char can be any, this is always a valid operator name
>       c0 <- oneOf "*/<>=~!%^&|`?+-"
>       --recurse:
>       let r = choice
>               [do
>                c1 <- oneOf "*/<>=~!%^&|`?"
>                choice [do
>                        x <- r
>                        return $ c1 : x
>                       ,return [c1]]
>               ,try $ do
>                a <- oneOf "+-"
>                b <- r
>                return $ a : b]
>       choice [do
>               tl <- r
>               return $ c0 : tl
>              ,return [c0]]



parse a number:
digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits

I'm sure the implementation can be simpler than this

> sqlNumber :: Stream s Identity Char =>
>              SParser s Tok
> sqlNumber = NumberTok <$> lexeme (
>   choice [do
>           -- starts with digits
>           d <- digits
>           suff <- choice [-- complete fractional part
>                           try fracPart
>                          ,-- dot followed by optional exp
>                           -- check for .. symbol
>                           choice [try $ do
>                                         _ <- lookAhead $ string ".."
>                                         return []
>                                  ,do
>                                   _ <- char '.'
>                                   e <- optionMaybe expn
>                                   return $ concat $ catMaybes
>                                     [Just "."
>                                     ,e]
>                                   ]
>                          ,--no dot then expn
>                           expn
>                           -- just an integer
>                          ,return ""
>                          ]
>           return $ T.pack $ d ++ suff
>          ,T.pack <$> fracPart
>          ])
>   where
>      fracPart = do
>           _ <- char '.'
>           d <- digits
>           e <- optionMaybe expn
>           return $ concat $ catMaybes
>             [Just "."
>             ,Just d
>             ,e]
>      expn = do
>        _ <- char 'e'
>        s <- optionMaybe (char '+' <|> char '-')
>        d <- digits
>        return $ concat $ catMaybes [Just "e"
>                                    ,fmap (:[]) s
>                                    ,Just d]
>      digits = many1 digit

================================================================================

additional parser bits and pieces

include * in identifier strings during lexing. This parser is also
used for keywords, so identifiers and keywords aren't distinguished
until during proper parsing, and * isn't really examined until type
checking

> identifierString :: (Stream str Identity Char) =>
>                     SParser str T.Text
> identifierString = T.pack <$>
>                    (lexeme $ (letter <|> char '_')
>                             <:> many (alphaNum <|> char '_'))

todo:
select adrelid as "a""a" from pg_attrdef;
creates a column named: 'a"a' with a double quote in it

> qidentifierString :: Stream s Identity Char =>
>                      SQLSyntaxDialect -> SParser s T.Text
> qidentifierString d =
>   T.pack <$> choice
>   [do
>    guard (d == SQLServerDialect)
>    lexeme $ char '[' *> many (noneOf "]") <* char ']'
>   ,lexeme $ char '"' *> many (noneOf "\"") <* char '"']


parse the block of inline data for a copy from stdin, ends with \. on
its own on a line

> copyPayload :: (Stream str Identity Char) =>
>                SParser str Tok
> copyPayload = CopyPayloadTok <$> lexeme (LT.pack <$> getLinesTillMatches "\\.\n")
>   where
>     getLinesTillMatches s = do
>                             x <- getALine
>                             if x == s
>                               then return ""
>                               else (x++) <$> getLinesTillMatches s
>     getALine = (++"\n") <$> manyTill anyChar (try newline)
>

================================================================================

= parsec pass throughs

> --symbol :: String -> SParser String
> --symbol = P.symbol lexer
>

> string :: Stream s m Char => T.Text -> ParsecT s u m T.Text
> string t = do
>   s <- TP.string $ T.unpack t
>   return $ T.pack s

> integer :: (Stream str Identity Char) =>
>            SParser str Integer
> integer = lexeme $ P.integer lexer

> whiteSpace :: (Stream str Identity Char) =>
>               SParser str ()
> whiteSpace = P.whiteSpace lexer
>
> --lexeme :: (Show a, Stream str Identity a) =>
> --          SParser str a -> SParser str
> lexeme :: (Stream str Identity Char)
>           => SParser str a -> SParser str a
> lexeme = P.lexeme lexer

this lexer isn't really used as much as it could be, probably some of
the fields are not used at all (like identifier and operator stuff)

> lexer :: (Stream str Identity Char) =>
>          P.GenTokenParser str LexState Identity
> lexer = P.makeTokenParser sqlDef

> sqlDef :: (Stream str Identity Char) =>
>           GenLanguageDef str st Identity
> sqlDef = P.LanguageDef
>                { P.commentStart   = "/*"
>                , P.commentEnd     = "*/"
>                , P.commentLine    = "--"
>                , P.nestedComments = False
>                , P.identStart     = letter <|> char '_'
>                , P.identLetter    = alphaNum <|> oneOf "_'"
>                , P.opStart        = P.opLetter sqlDef
>                , P.opLetter       = oneOf ".:^*/%+-<>=|!"
>                , P.reservedOpNames= []
>                , P.reservedNames  = []
>                , P.caseSensitive  = False
>                }


