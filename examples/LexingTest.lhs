

test program to show using the new incremental lexer
The idea is to use getline to get lines of sql on the console

when a complete statement is read, call a function. Want to support
sql statements which cover multiple lines and pasting in multiple
statements at once/ entering multiple statements on one line.

> {-# LANGUAGE OverloadedStrings,TupleSections #-}

> import qualified Data.Text.IO as T
> import qualified Data.Text as T
> import qualified Data.Text.Lazy as LT
> import qualified Data.Text.Lazy.IO as LT
> import Data.Attoparsec.Text
> import Control.Applicative
> import Data.Char
> import Debug.Trace
> import System.IO
> --import Data.Ratio
> import Database.HsSqlPpp.SqlDialect
> import Data.List
> import Control.Monad
> --import System.Environment

> type Partial = T.Text -> SResult
> type SResult = Result [(Position,Token)]

> dialect :: SQLSyntaxDialect
> dialect = PostgreSQLDialect

> main :: IO ()
> main = do
>   let readloop :: Maybe Partial -> IO ()
>       readloop mpr = do
>           putStr $ maybe ("> ") (const ". ") mpr
>           hFlush stdout
>           line <- T.getLine
>           loopText mpr (T.snoc line '\n')
>       loopText :: Maybe Partial -> T.Text -> IO ()
>       loopText mpr txt = do
>           let y = case mpr of
>                     Nothing -> --trace ("parse '" ++ T.unpack txt ++ "'") $
>                                parse (sqlStatement ("",1,0)) txt
>                     Just pr -> --trace ("feed '" ++ T.unpack txt ++ "'") $
>                                feed (Partial pr) txt
>           case y of
>             Done leftover st -> do
>                    runStatement st
>                    -- keep processing the partial results
>                    -- in case there are multiple statements
>                    -- todo: checks the leftover to see if
>                    -- it is just whitespace, add to this
>                    -- checking to see if it is just comments
>                    -- and whitespace
>                    if not (T.all isSpace leftover)
>                       then loopText Nothing leftover
>                       else readloop Nothing
>             Partial pr -> readloop (Just pr)
>             Fail _leftover ctx err -> do
>                 putStrLn $ err ++ " " ++ show ctx
>                 readloop Nothing
>   readloop Nothing

> runStatement :: [(Position,Token)] -> IO ()
> runStatement s = do --trace "runstatement" $
>                  putStr "Statement: "
>                  LT.putStrLn $ LT.concat (map (prettyToken dialect . snd) s)
>                  putStrLn $ intercalate "\n" $ map show s

> sqlStatement :: Position -> Parser [(Position,Token)]
> sqlStatement p = sqlStatement' p []
> sqlStatement' :: Position -> [(Position,Token)] -> Parser [(Position,Token)]
> sqlStatement' p acc = do
>     -- keep lexing until get a ';' symbol
>     x <- sqlToken dialect p
>     let ts = x:acc
>     if snd x == Symbol ";"
>       then return $ reverse ts
>       else
>           -- dodgy: the work out the position
>           -- by pretty printing the previous token
>           -- and adding it to the current position
>           let pt = prettyToken dialect (snd x)
>               p' = addPosition p pt
>           in sqlStatement' p' ts

-----------------------------------------------

= new sql lexer

goals:
try out attoparsec
support partial parsing
support 100% accurate pretty printing from lexed tokens back to source
  (including whitespace and comments)


TODO:
add tests: manual, quickcheck
string parsing: E '' $$
symbol parsing for dialects
nested comments
support copy from stdin for postgresql (later)
split files: lexical syntax, lexer, pretty-lexicalsyntax, automated
  tests
replace the existing lexer in hssqlppp
public api in hssqlppp

= Syntax

> -- | Represents a lexed token
> data Token
>     -- | a symbol in postgresql dialect is one of the following:
>     -- * one of the characters (),;[]
>     -- * '..' or ':=' or '.' or ':'
>     -- * a compound symbol, which starts with one of '*/<>=~!@#%^&|`?+-'
>     -- and follows with 0 or more of '*/<>=~!@#%^&|`?'
>     -- things that are not lexed as symbols:
>     -- * [] used in quoted identifiers, prefix @,#,: used in identifiers
>     -- * $n positional arg
>     = Symbol T.Text
>     -- | this is an identifier or keyword
>     -- the maybe char,char selects the quoted style Nothing is unquoted
>     -- otherwise the two characters are the start and end quote
>     -- '"' is used in standard sql, sql server also uses [brackets]
>     -- to quote identifiers.
>     -- the identifier also includes the 'variable marker prefix'
>     -- used in sql server (e.g. @identifier, #identifier), and oracle
>     -- (e.g. :identifier)
>     | Identifier (Maybe (Char,Char)) T.Text
>     -- | this is a string,
>     -- the first field is the quotes used: single quote (')
>     -- for normal strings, and $$ delimiter for postgresql quotes
>     -- todo: want to parse exact numbers
>     -- change from Double to something better
>     | SqlString T.Text T.Text
>     -- | a number literal (integral or otherwise), stored in original format
>     -- unchanged
>     | SqlNumber T.Text
>     -- | non-significant whitespace (space, tab, newline) (strictly speaking,
>     -- it is up to the client to decide whether the whitespace is significant
>     -- or not)
>     | WhiteSpace T.Text
>     -- | a postgresql positional arg, e.g. $1
>     | PositionalArg Int
>     -- | a commented line using --, contains every character starting with the
>     -- '--' and including the terminating newline character
>     | LineComment T.Text
>     -- | a block comment, /* stuff */, includes the comment delimiters
>     | BlockComment T.Text
>     -- | an antiquotation splice, e.g. $x(stuff)
>     | Splice Char T.Text
>       deriving (Eq,Show)

-------------------------------------------

= Pretty printing

> -- | Accurate pretty printing, if you lex a bunch of tokens,
> -- then pretty print them, should should get back exactly the
> -- same string
> prettyToken :: SQLSyntaxDialect -> Token -> LT.Text
> prettyToken _ (Symbol s) = LT.fromChunks [s]
> prettyToken _ (Identifier Nothing t) = LT.fromChunks [t]
> prettyToken _ (Identifier (Just (a,b)) t) =
>     LT.fromChunks [T.singleton a, t, T.singleton b]
> prettyToken _ (SqlString q t) = LT.fromChunks [q,t,q]
> prettyToken _ (SqlNumber r) = LT.fromChunks [r]
> prettyToken _ (WhiteSpace t) = LT.fromChunks [t]
> prettyToken _ (PositionalArg n) = LT.fromChunks [T.singleton '$', T.pack $ show n]
> prettyToken _ (LineComment l) = LT.fromChunks [l]
> prettyToken _ (BlockComment c) = LT.fromChunks [c]
> prettyToken _ (Splice c t) =
>     LT.fromChunks [T.singleton '$'
>                   ,T.singleton c
>                   ,T.singleton '('
>                   ,t
>                   ,T.singleton ')']


= parsing

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

> sqlToken :: SQLSyntaxDialect -> Position -> Parser (Position,Token)
> sqlToken d p =
>     (p,) <$> choice [sqlString d
>                     ,identifier d
>                     ,symbol d
>                     ,sqlNumber d
>                     ,sqlWhitespace d
>                     ,positionalArg d
>                     ,lineComment d
>                     ,blockComment d
>                     ,splice d]

> identifier :: SQLSyntaxDialect -> Parser Token

sql server: identifiers can start with @ or #
quoting uses []

> identifier SQLServerDialect =
>     choice
>     [Identifier (Just ('[',']'))
>      <$> (char '[' *> takeWhile1 (/=']') <* char ']')
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

> sqlString :: SQLSyntaxDialect -> Parser Token
> sqlString _ =
>     SqlString "'" <$>
>     (char '\'' *> takeTill (=='\'') <* char '\'')

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

> symbol :: SQLSyntaxDialect -> Parser Token
> symbol _ = Symbol <$>
>     choice
>     [satisfyT (inClass "(),;[]")
>     ,string ".."
>     ,string "."
>     ,string ":="
>     ,string ":"
>     ,biggerSymbol]
>   where
>     satisfyT p = T.singleton <$> satisfy p
>     biggerSymbol =
>         startsWith (inClass "*/<>=~!@#%^&|`?+-")
>                    (inClass "*/<>=~!@#%^&|`?")


> sqlWhitespace :: SQLSyntaxDialect -> Parser Token
> sqlWhitespace _ = (WhiteSpace . T.pack) <$> many1 (satisfy isSpace)

> positionalArg :: SQLSyntaxDialect -> Parser Token
> positionalArg PostgreSQLDialect =
>   PositionalArg <$> (char '$' *> (read <$> many1 digit))

> positionalArg _ = satisfy (const False) >> error "positional arg unsupported"

> lineComment :: SQLSyntaxDialect -> Parser Token
> lineComment _ =
>     (\s -> LineComment $ T.concat ["--",s]) <$>
>     (string "--" *>  takeTill (=='\n'))

> blockComment :: SQLSyntaxDialect -> Parser Token
> blockComment _ =
>     (\s -> BlockComment $ T.pack ("/*" ++  s ++ "*/")) <$>
>     (string "/*" *> manyTill anyChar (string "*/"))

> splice :: SQLSyntaxDialect -> Parser Token
> splice _ = do
>   Splice
>   <$> (char '$' *> letter)
>   <*> (char '(' *> identifierString <* char ')')


> startsWith :: (Char -> Bool) -> (Char -> Bool) -> Parser T.Text
> startsWith p ps = do
>   c <- satisfy p
>   choice [T.cons c <$> (takeWhile1 ps)
>          ,return $ T.singleton c]

--------------------------------------

tests:

manually written

strings
'string'
E'string\n'
E'quote\''
'normal '' quote'
$$dollar quoting$$
$x$dollar $$ quoting$x$

identifiers
test
_test
"test test"
test123
[test]
@test
 #test
:test

symbols

numbers
10
.1
5e3
5e+3
5e-3
10.2
10.2e7

whitespace
" "
"  "
"\n"
"\t"

positional arg
$1

line comment
-- this is a comment

block comment

/* block
comment */

/* nested /*block*/ comment */

splice
$c(splice)


quickcheck:
generate random symbols
print then parse, check equal
generate random strings

try to parse, if parses, print and check equal to original string
