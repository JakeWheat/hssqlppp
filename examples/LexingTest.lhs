

test program to show using the new incremental lexer
The idea is to use getline to get lines of sql on the console

when a complete statement is read, call a function. Want to support
sql statements which cover multiple lines and pasting in multiple
statements at once/ entering multiple statements on one line.

> {-# LANGUAGE OverloadedStrings,TupleSections,ScopedTypeVariables #-}

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
> import Data.List hiding (takeWhile)
> import Prelude hiding (takeWhile)
> import Control.Monad
> import System.Environment
> import Test.Framework
> import Test.HUnit
> import Test.Framework.Providers.HUnit



> type Partial = T.Text -> SResult
> type SResult = Result [(Position,Token)]

> dialect :: SQLSyntaxDialect
> dialect = PostgreSQLDialect

> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     ("test":as) -> runTests as
>     _ -> do
>         let readloop :: Maybe Partial -> IO ()
>             readloop mpr = do
>                 putStr $ maybe ("> ") (const ". ") mpr
>                 hFlush stdout
>                 line <- T.getLine
>                 loopText mpr (T.snoc line '\n')
>             loopText :: Maybe Partial -> T.Text -> IO ()
>             loopText mpr txt = do
>                 let y = case mpr of
>                           Nothing -> --trace ("parse '" ++ T.unpack txt ++ "'") $
>                                      parse (sqlStatement ("",1,0)) txt
>                           Just pr -> --trace ("feed '" ++ T.unpack txt ++ "'") $
>                                      feed (Partial pr) txt
>                 case y of
>                   Done leftover st -> do
>                          runStatement st
>                          -- keep processing the partial results
>                          -- in case there are multiple statements
>                          -- todo: checks the leftover to see if
>                          -- it is just whitespace, add to this
>                          -- checking to see if it is just comments
>                          -- and whitespace
>                          if not (T.all isSpace leftover)
>                             then loopText Nothing leftover
>                             else readloop Nothing
>                   Partial pr -> readloop (Just pr)
>                   Fail _leftover ctx err -> do
>                       putStrLn $ err ++ " " ++ show ctx
>                       readloop Nothing
>         readloop Nothing

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
add quickcheck tests (see below)
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
>     -- for normal strings, E' for escape supporting strings,
>     -- and $$ delimiter for postgresql quotes
>     -- the lexer doesn't process the escapes in strings, but passes
>     -- on the literal source e.g. E'\n' parses to SqlString "E'" "\n"
>     -- with the literal character '\' and 'n' in the string, not a newline character
>     -- todo: want to parse exact numbers
>     -- change from Double to something better
>     | SqlString T.Text T.Text
>     -- | a number literal (integral or otherwise), stored in original format
>     -- unchanged
>     | SqlNumber T.Text
>     -- | non-significant whitespace (space, tab, newline) (strictly speaking,
>     -- it is up to the client to decide whether the whitespace is significant
>     -- or not)
>     | Whitespace T.Text
>     -- | a postgresql positional arg, e.g. $1
>     | PositionalArg Int
>     -- | a commented line using --, contains every character starting with the
>     -- '--' and including the terminating newline character if there is one
>     -- - this will be missing if the last line in the source is a line comment
>     -- with no trailing newline
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
>                     ,lineComment d
>                     ,blockComment d
>                     ,sqlNumber d
>                     ,symbol d
>                     ,sqlWhitespace d
>                     ,positionalArg d
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
> symbol dialect = Symbol <$>
>     choice
>     [satisfyT (inClass simpleSymbols)
>     ,string ".."
>     ,string "."
>     ,string ":="
>     ,string ":"
>     ,biggerSymbol]
>   where
>     satisfyT p = T.singleton <$> satisfy p
>     biggerSymbol =
>         startsWith (inClass compoundFirst)
>                    (inClass compoundTail)
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
>                     ,takeWhile (/='\n') <* endOfInput
>                     ])

> blockComment :: SQLSyntaxDialect -> Parser Token
> blockComment _ =
>     (\s -> BlockComment $ T.concat ["/*",s]) <$>
>     (string "/*" *> commentSuffix 0)
>   where
>     commentSuffix :: Int -> Parser T.Text
>     commentSuffix n = do
>       -- read until a possible end comment or nested comment
>       x <- takeWhile (\e -> e /= '/' && e /= '*')
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
>   choice [T.cons c <$> (takeWhile1 ps)
>          ,return $ T.singleton c]

--------------------------------------

tests:

manually written

> data ManualTest = MT T.Text [Token]
>                 | MTSS T.Text [Token] -- sql server syntax
>                 | MTO T.Text [Token] -- oracle syntax

> lexTestData :: [ManualTest]
> lexTestData =

strings

>     [MT "'string'" [SqlString "'" "string"]
>     ,MT "E'string\\n'" [SqlString "E'" "string\\n"] -- the \\n is to put a literal \ and n in the string
>     ,MT "E'bsquoteend\\''" [SqlString "E'" "bsquoteend\\'"]
>     ,MT "E'bsquote\\'xx'" [SqlString "E'" "bsquote\\'xx"]
>     ,MT "E'quoteend'''" [SqlString "E'" "quoteend''"]
>     ,MT "E'quote''x'" [SqlString "E'" "quote''x"]
>     ,MT "'normal '' quote'" [SqlString "'" "normal '' quote"]
>     ,MT "'normalendquote '''" [SqlString "'" "normalendquote ''"]
>     ,MT "$$dollar quoting$$" [SqlString "$$" "dollar quoting"]
>     ,MT "$x$dollar $$ quoting$x$" [SqlString "$x$" "dollar $$ quoting"]

identifiers

>     ,MT "test" [Identifier Nothing "test"]
>     ,MT "_test" [Identifier Nothing "_test"]
>     ,MT "\"test test\"" [Identifier (Just ('"','"')) "test test"]
>     ,MT "test123" [Identifier Nothing "test123"]
>     ,MTSS "[test \"]" [Identifier (Just ('[',']')) "test \""]
>     ,MTSS "@test" [Identifier Nothing "@test"]
>     ,MTSS "#test" [Identifier Nothing "#test"]
>     ,MTO ":test" [Identifier Nothing ":test"]

symbols

>     ,MT "+" [Symbol "+"]
>     ,MT "*" [Symbol "*"]

numbers

>     ,MT "10" [SqlNumber "10"]
>     ,MT ".1" [SqlNumber ".1"]
>     ,MT "5e3" [SqlNumber "5e3"]
>     ,MT "5e+3" [SqlNumber "5e+3"]
>     ,MT "5e-3" [SqlNumber "5e-3"]
>     ,MT "10.2" [SqlNumber "10.2"]
>     ,MT "10.2e7" [SqlNumber "10.2e7"]

whitespace

>     ,MT " " [Whitespace " "]
>     ,MT "  " [Whitespace "  "]
>     ,MT "\n" [Whitespace "\n"]
>     ,MT "\t" [Whitespace "\t"]

positional arg

>     ,MT "$1" [PositionalArg 1]

line comment

>     ,MT "-- this is a comment\n" [LineComment "-- this is a comment\n"]
>     -- check eof with no trailing newline
>     ,MT "-- this is a comment" [LineComment "-- this is a comment"]

block comment

>     ,MT "/* block\ncomment */" [BlockComment "/* block\ncomment */"]
>     ,MT "/* nested /*block*/ comment */" [BlockComment "/* nested /*block*/ comment */"]

splice

>     ,MT "$c(splice)" [Splice 'c' "splice"]

>     ]

> lexerTests :: Test.Framework.Test
> lexerTests = testGroup "lexertests" $ map itemToTft lexTestData

> itemToTft :: ManualTest -> Test.Framework.Test
> itemToTft (MT a b) = testLex PostgreSQLDialect a b
> itemToTft (MTSS a b) = testLex SQLServerDialect a b
> itemToTft (MTO a b) = testLex OracleDialect a b
>
> testLex :: SQLSyntaxDialect -> T.Text -> [Token] -> Test.Framework.Test
> testLex d t r = testCase ("lex "++ T.unpack t) $ do
>     let x = parseOnly (many1 (sqlToken d ("",1,0)) <* endOfInput) t
>         y = either (error . show) id x
>     assertEqual "lex" r (map snd y)
>     let t' = LT.concat $ map (prettyToken d) r
>     assertEqual "lex . pretty" (LT.fromChunks [t]) t'

> runTests :: [String] -> IO ()
> runTests as = defaultMainWithArgs [lexerTests] as

quickcheck:
generate random symbols
print then parse, check equal
generate random strings

try to parse, if parses, print and check equal to original string
