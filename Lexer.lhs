#! /usr/bin/env runhaskell

> import Text.Parsec hiding(many, optional, (<|>))
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language
> --import Text.Parsec.Expr
> import Text.Parsec.String
> --import Text.Parsec.Error

> import Control.Applicative
> import Control.Monad.Identity

> --import Data.Maybe
> import Data.Char
> import System

> --import Tree

lexicon:

eof
string
identifier or keyword
() []
other symbols
positional arg

notes on symbols
treat enclosing symbols separately - [] ()
what are the default sql symbols?
pg symbols can be made from:

=_*/<>=~!@#%^&|`?

no --, /* in symbols

can't end in + or - unless contains
~!@#%^&|?

approach 1:
try to keep whole symbols as single lexical items
(e.g. ==, ~==

approach 2:
make each character a separate element
e.g. == parses to ['=', '=']
then the parser sorts this out

try approach 2 for now

> main :: IO ()
> main = do
>   args <- getArgs
>   let f = head args
>   putStrLn $ "parsing " ++ show f
>   x <- parseFromFile sqlTokens f
>   return ()
>   case x of
>        Left er -> do
>            src <- readFile f
>            putStrLn $ showEr er f src
>        Right l -> print l


> type Token = (SourcePos, Tok)


> data Tok = SqlString String String --delim, value (delim will one of ', $$, $[stuff]$
>          | IdString String --includes . and x.y.* type stuff
>          | Symbol Char
>          | PositionalArg Integer -- $1, etc.
>          | SqlFloat Double
>          | SqlInteger Integer
>            deriving (Eq,Show)

> sqlTokens = (whiteSpace *> many sqlToken <* eof)

> sqlToken = sqlString
>            <|> idString
>            <|> positionalArg
>            <|> sqlSymbol
>            <|> try sqlFloat
>            <|> sqlInteger

> sqlString = stringQuotes <|> stringLD
>   where
>     --parse a string delimited by single quotes
>     stringQuotes = SqlString "\'" <$> stringPar
>     stringPar = optional (char 'E') *> char '\''
>                 *> readQuoteEscape <* whiteSpace
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
>                return $ SqlString ("$" ++ tag ++ "$") s



> idString = IdString <$> identifierString

> sqlSymbol = Symbol <$> oneOf "=_*/<>=~!@#%^&|`?()[],"

> positionalArg = char '$' >> PositionalArg <$> integer

> lexeme :: ParsecT String u Identity a
>           -> ParsecT String u Identity a
> lexeme = P.lexeme lexer


> identifierString = lexeme $ choice [
>                     "*" <$ symbol "*"
>                    ,do
>                      a <- nonStarPart
>                      b <- tryMaybeP ((++) <$> symbol "." <*> identifierString)
>                      case b of Nothing -> return a
>                                Just c -> return $ a ++ c]

>   where
>     nonStarPart = letter <:> secondOnwards
>     secondOnwards = many (alphaNum <|> char '_')

> symbol :: String -> ParsecT String u Identity String
> symbol = P.symbol lexer

> sqlFloat = SqlFloat <$> float

> sqlInteger = SqlInteger <$> integer

> lexer :: P.GenTokenParser String u Identity
> lexer = P.makeTokenParser (emptyDef {
>                             P.commentStart = "/*"
>                            ,P.commentEnd = "*/"
>                            ,P.commentLine = "--"
>                            ,P.nestedComments = False
>                            ,P.identStart = letter <|> char '_'
>                            ,P.identLetter    = alphaNum <|> oneOf "_"
>                            ,P.opStart        = P.opLetter emptyDef
>                            ,P.opLetter       = oneOf opLetters
>                            ,P.reservedOpNames= []
>                            ,P.reservedNames  = []
>                            ,P.caseSensitive  = False
>                            })

> opLetters :: String
> opLetters = ".:^*/%+-<>=|!"

> whiteSpace :: ParsecT String u Identity ()
> whiteSpace= P.whiteSpace lexer

> tryMaybeP :: GenParser tok st a
>           -> ParsecT [tok] st Identity (Maybe a)
> tryMaybeP p = try (optionMaybe p) <|> return Nothing

doesn't seem too gratuitous, comes up a few times

> (<:>) :: (Applicative f) =>
>          f a -> f [a] -> f [a]
> (<:>) a b = (:) <$> a <*> b

> integer :: ParsecT String u Identity Integer
> integer = lexeme $ P.integer lexer

> float :: ParsecT String u Identity Double
> float = lexeme $ P.float lexer



> showEr :: ParseError -> String -> String -> String
> showEr er fn src =
>     let  pos  = errorPos er
>          lineNo = sourceLine pos
>          ls = lines src
>          line = safeGet ls(lineNo - 1)
>          prelines = map (safeGet ls) [(lineNo - 5) .. (lineNo - 2)]
>          postlines = map (safeGet ls) [lineNo .. (lineNo + 5)]
>          colNo = sourceColumn pos
>          highlightLine = replicate (colNo - 1) ' ' ++ "^"
>          errorHighlightText = prelines
>                               ++ [line, highlightLine, "ERROR HERE"]
>                               ++ postlines
>     in "\n---------------------\n" ++ show er
>        ++ "\n" ++ fn ++ ":" ++ show lineNo ++ ":" ++ show colNo
>        ++ "\n------------\nCheck it out:\n"
>        ++ unlines (trimLines errorHighlightText)
>        ++ "\n-----------------\n"
>     where
>       safeGet a i = if i < 0 || i >= length a
>                       then ""
>                       else a !! i
>       trimLines s = trimStartLines $ reverse $ trimStartLines $ reverse s
>       trimStartLines = dropWhile (=="")
