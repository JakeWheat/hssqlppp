
> module P1 where

> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec as P (parse)
> import qualified Text.ParserCombinators.Parsec.Token as P
> import Text.ParserCombinators.Parsec.Language
> import qualified Text.Parsec.Prim
> import Control.Monad.Identity
> import Text.ParserCombinators.Parsec.Expr

 > import Text.ParserCombinators.Parsec.Language (haskellStyle)


> import Grammar

> expr :: Parser Expression
> expr =
>   buildExpressionParser table factor
>   <?> "expression"

> parens :: Text.Parsec.Prim.ParsecT String u Identity a
>           -> Text.Parsec.Prim.ParsecT String u Identity a
> parens = P.parens lexer

> lexer :: P.GenTokenParser String u Identity
> lexer = P.makeTokenParser (haskellDef
>                            { reservedOpNames = ["*","/","+","-"]
>                            })

>   -- Recognizes a factor in an expression
> factor :: GenParser Char () Expression
> factor  = parens expr
>           <|> integer
>           <|> try booleanLiteral
>           <|> stringLiteral
>           <|> functionCall
>           <?> "simple expression"

> booleanLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
> booleanLiteral = do
>   x <- ((lexeme $ string "true")
>         <|> (lexeme $ string "false"))
>   return $ BooleanL (if x == "true" then True else False)

> integer :: Text.Parsec.Prim.ParsecT String u Identity Expression
> integer = liftM IntegerL $ lexeme $ P.integer lexer

>   -- Specifies operator, associativity, precendence, and constructor to execute
>   -- and built AST with.
> table :: [[Operator Char u Expression]]
> table =
>       [[prefix "-" (Exp Mult (IntegerL (-1)))]
>       ,[binary "^" (Exp Pow) AssocRight]
>       ,[binary "*" (Exp Mult) AssocLeft
>        ,binary "/" (Exp Div) AssocLeft
>        ,binary "%" (Exp Mod) AssocLeft]
>       ,[binary "+" (Exp Plus) AssocLeft
>        ,binary "-" (Exp Minus) AssocLeft]
>       ]
>     where
>       binary s f assoc
>          = Infix (do{ string s; return f}) assoc
>       prefix s f
>          = Prefix (do{ string s; return f})
>

> stringLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
> stringLiteral = do
>   char '\''
>   name <- many1 (noneOf "'")
>   lexeme $ char '\''
>   return $ StringL name

> functionCall :: Text.Parsec.Prim.ParsecT String () Identity Expression
> functionCall = do
>   name <- identifierString
>   args <- parens $ commaSep factor
>   return $ FunctionCall name args


> -- Parses a string into an AST, using the parser defined above
> parse :: String -> Expression
> parse s = case P.parse expr "" s of
>   Right ast -> ast
>   Left e -> error $ show e
>

> lexeme :: Text.Parsec.Prim.ParsecT String u Identity a
>           -> Text.Parsec.Prim.ParsecT String u Identity a
> lexeme = P.lexeme lexer

> identifierString :: Parser String
> identifierString = word

> word :: Parser String
> word = lexeme (many1 letter)

> commaSep :: Text.Parsec.Prim.ParsecT String u Identity a
>             -> Text.Parsec.Prim.ParsecT String u Identity [a]
> commaSep = P.commaSep lexer

> commaSep1 :: Text.Parsec.Prim.ParsecT String u Identity a
>             -> Text.Parsec.Prim.ParsecT String u Identity [a]
> commaSep1 = P.commaSep lexer
