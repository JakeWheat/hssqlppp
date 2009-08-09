> module Parser where

> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec.Token as P
> import Text.ParserCombinators.Parsec.Language
> import qualified Text.Parsec.Prim
> import Control.Monad.Identity

> import Grammar

> parseSql :: String -> Either ParseError Select
> parseSql s = parse select "(unknown)" s

> select :: Text.Parsec.Prim.ParsecT String () Identity Select
> select = do
>   string "select"
>   whitespace
>   e <- expression
>   char ';'
>   return $ Select e

> expression :: Text.Parsec.Prim.ParsecT [Char] () Identity Expression
> expression = do
>   try binaryOperator
>     <|> try functionCall
>     <|> identifier
>     <|> stringLiteral
>     <|> integerLiteral

> binaryOperator :: Text.Parsec.Prim.ParsecT String u Identity Expression
> binaryOperator = do
>   e1 <- integerLiteral
>   op <- many1 (oneOf "+-")
>   e2 <- integerLiteral
>   return $ BinaryOperatorCall op e1 e2

> functionCall :: Text.Parsec.Prim.ParsecT String () Identity Expression
> functionCall = do
>   name <- word
>   whitespace
>   char '('
>   args <- commaSep expression
>   char ')'
>   return $ FunctionCall name args

> commaSep p  = p `sepBy` (do
>                          whitespace
>                          char ','
>                          whitespace)

> stringLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
> stringLiteral = do
>   --whitespace
>   char '\''
>   name <- many1 (noneOf "'")
>   char '\''
>   return $ StringLiteral name

> integerLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
> integerLiteral = do
>   i <- integer
>   return $ IntegerLiteral i

> identifier :: Text.Parsec.Prim.ParsecT String () Identity Expression
> identifier = do
>   i <- word
>   return $ StringLiteral i

> word :: Parser String
> word = many1 letter

> integer :: Text.Parsec.Prim.ParsecT String u Identity Integer
> integer = P.integer lexer

> whitespace :: Text.Parsec.Prim.ParsecT String u Identity ()
> whitespace = P.whiteSpace lexer

> lexer :: P.GenTokenParser String u Identity
> lexer = P.makeTokenParser haskellDef
