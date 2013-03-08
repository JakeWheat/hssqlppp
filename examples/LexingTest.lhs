

test program to show using the new incremental lexer
The idea is to use getline to get lines of sql on the console

when a complete statement is read, call a function. Want to support
sql statements which cover multiple lines and pasting in multiple
statements at once/ entering multiple statements on one line.

> {-# LANGUAGE OverloadedStrings #-}
> --import Database.HsSqlPpp.Parsing.Lexer2
> import qualified Data.Text.IO as T
> import qualified Data.Text as T
> import qualified Data.Text.Lazy as LT
> import qualified Data.Text.Lazy.IO as LT
> import Data.Attoparsec.Text
> import Control.Applicative
> import Data.Char
> import Debug.Trace
> import System.IO
> import Data.Ratio
> import Database.HsSqlPpp.SqlDialect

> type Partial = T.Text -> SResult
> type SResult = Result [Token]

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
>                                parse sqlStatement txt
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
>                      then loopText Nothing leftover
>                      else readloop Nothing
>             Partial pr -> readloop (Just pr)
>             Fail _leftover ctx err -> do
>                 putStrLn $ err ++ " " ++ show ctx
>                 readloop Nothing
>   readloop Nothing

> runStatement :: [Token] -> IO ()
> runStatement s = do --trace "runstatement" $
>                  putStr "Statement: "
>                  LT.putStrLn $ LT.concat (map (prettyToken dialect) s)

> sqlStatement :: Parser [Token]
> sqlStatement = sqlStatement' []
> sqlStatement' :: [Token] -> Parser [Token]
> sqlStatement' acc = do
>     -- keep lexing until get a ';' symbol
>     x <- sqlToken dialect
>     let ts = x:acc
>     if x == Symbol ";"
>       then return $ reverse ts
>       else sqlStatement' ts

-----------------------------------------------

= new sql lexer

goals:
try out attoparsec
support partial parsing
support 100% accurate pretty printing from lexed tokens back to source
  (including whitespace and comments)


TODO:
support copy from stdin for postgresql (later)
add dialect support:
  sqlserver: [quoted identifier]
    @identifier, #identifier
  oracle: same as postgres except :identifier for now
accurate number parsing
postgresql extended string delimiters
new tokens: quoted identifier
  add positional arg to symbol
  splices
  add comments as tokens
positional information
replace the existing lexer in hssqlppp
public api: use for source highlighting? or cut down sql parsers

automated tests:
manually written: parse examples of each type of symbol
quickcheck: generate strings and parse them
with all tests, check pretty . parse === id

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
>     -- | a number literal (integral or otherwise)
>     | SqlNumber Rational
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

accurate pretty printer:

> prettyToken :: SQLSyntaxDialect -> Token -> LT.Text
> prettyToken _ (Symbol s) = LT.fromChunks [s]
> prettyToken _ (Identifier Nothing t) = LT.fromChunks [t]
> prettyToken _ (Identifier (Just (a,b)) t) =
>     LT.fromChunks [T.singleton a, t, T.singleton b]
> prettyToken _ (SqlString q t) = LT.fromChunks [q,t,q]
> prettyToken _ (SqlNumber r) =
>     -- todo: how to make this work correctly?
>     if denominator r == 1
>     then LT.pack $ show $ numerator r
>     else LT.pack $ show $ ((realToFrac r) :: Double)
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


-------------------------------------------

parsing code

> sqlToken :: SQLSyntaxDialect -> Parser Token
> sqlToken d =
>     choice [sqlString d
>            ,identifier d
>            ,symbol d
>            ,sqlNumber d
>            ,sqlWhitespace d
>            ,positionalArg d
>            ,lineComment d
>            ,blockComment d
>            ,splice d]

> identifier :: SQLSyntaxDialect -> Parser Token
> identifier _ =
>     Identifier Nothing <$>
>     startsWith (\c -> c == '_' || isAlpha c)
>                (\c -> c == '_' || isAlphaNum c)

> sqlString :: SQLSyntaxDialect -> Parser Token
> sqlString _ =
>     SqlString "'" <$>
>     (char '\'' *> takeTill (=='\'') <* char '\'')

> sqlNumber :: SQLSyntaxDialect -> Parser Token
> sqlNumber _ = (SqlNumber . realToFrac) <$> double

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
> splice _ = satisfy (const False) >> error "splice unsupported"


> startsWith :: (Char -> Bool) -> (Char -> Bool) -> Parser T.Text
> startsWith p ps = do
>   c <- satisfy p
>   choice [T.cons c <$> (takeWhile1 ps)
>          ,return $ T.singleton c]
