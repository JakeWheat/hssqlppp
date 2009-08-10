> module Parser where

> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec.Token as P
> import Text.ParserCombinators.Parsec.Language
> import qualified Text.Parsec.Prim
> import Control.Monad.Identity

> import Grammar

> parseSql :: String -> Either ParseError [Statement]
> parseSql s = parse statements "(unknown)" s

> statements = do
>   whitespace
>   s <- many statement
>   eof
>   return s

> statement :: Text.Parsec.Prim.ParsecT [Char] () Identity Statement
> statement = do
>   try select
>   <|>  createTable

> createTable :: Text.Parsec.Prim.ParsecT String () Identity Statement
> createTable = do
>   lexeme (string "create" <?> "create")
>   lexeme (string "table" <?> "table")
>   n <- identifierString <?> "identifier"
>   lexeme $ char '('
>   atts <- commaSep tableAtt
>   lexeme $ char ')'
>   lexeme $ char ';'
>   return $ CreateTable n atts

> tableAtt :: Text.Parsec.Prim.ParsecT String () Identity AttributeDef
> tableAtt = do
>   name <- identifierString <?> "identifier"
>   typ <- identifierString <?> "identifier"
>   return $ AttributeDef name typ

> select :: Text.Parsec.Prim.ParsecT String () Identity Statement
> select = do
>   lexeme $ string "select"
>   e <- expression
>   lexeme $ char ';'
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
>   op <- lexeme $ many1 $ oneOf "+-"
>   e2 <- integerLiteral
>   return $ BinaryOperatorCall op e1 e2

> functionCall :: Text.Parsec.Prim.ParsecT String () Identity Expression
> functionCall = do
>   name <- identifierString
>   lexeme $ char '('
>   args <- commaSep expression
>   lexeme $ char ')'
>   return $ FunctionCall name args

> commaSep p  = p `sepBy` (lexeme $ char ',')

> stringLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
> stringLiteral = do
>   char '\''
>   name <- many1 (noneOf "'")
>   lexeme $ char '\''
>   return $ StringLiteral name

> integerLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
> integerLiteral = do
>   liftM IntegerLiteral $ integer

> identifier :: Text.Parsec.Prim.ParsecT String () Identity Expression
> identifier = liftM Identifier $ lexeme word

> identifierString :: Parser String
> identifierString = word

> word :: Parser String
> word = lexeme (many1 letter)

> integer :: Text.Parsec.Prim.ParsecT String u Identity Integer
> integer = lexeme $ P.integer lexer

> whitespace = spaces

> lexer :: P.GenTokenParser String u Identity
> lexer = P.makeTokenParser haskellDef

> lexeme = P.lexeme lexer


Select (BinaryOperatorCall "+" (BinaryOperatorCall "+" (StringLiteral "x") (BinaryOperatorCall "-" (BinaryOperatorCall "-" (IntegerLiteral 1) (IntegerLiteral 1)) (StringLiteral "T"))) (BinaryOperatorCall "-" (BinaryOperatorCall "-" (StringLiteral "3") (BinaryOperatorCall "+" (IntegerLiteral 1) (FunctionCall "e" []))) (FunctionCall "me" [Identifier "vc",StringLiteral "&)"])))