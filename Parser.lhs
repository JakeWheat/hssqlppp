> module Parser where

> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec.Token as P
> import Text.ParserCombinators.Parsec.Language
> import qualified Text.Parsec.Prim
> import Control.Monad.Identity
> import Text.ParserCombinators.Parsec.Expr

 > import Text.ParserCombinators.Parsec.Language (haskellStyle)


> import Grammar

> parseSql :: String -> Either ParseError [Statement]
> parseSql s = parse statements "(unknown)" s

> statements :: Text.Parsec.Prim.ParsecT [Char] () Identity [Statement]
> statements = do
>   whitespace
>   s <- many statement
>   eof
>   return s

> statement :: Text.Parsec.Prim.ParsecT [Char] () Identity Statement
> statement = do
>   select
>   <|> insert
>   <|> update
>   <|> createTable

> insert :: Text.Parsec.Prim.ParsecT String () Identity Statement
> insert = do
>   keyword "insert"
>   keyword "into"
>   tableName <- identifierString
>   atts <- parens $ commaSep1 identifierString
>   keyword "values"
>   exps <- parens $ commaSep1 expr
>   semi
>   return $ Insert tableName atts exps
 
> update :: Text.Parsec.Prim.ParsecT String () Identity Statement
> update = do
>   keyword "update"
>   tableName <- identifierString
>   keyword "set"
>   scs <- commaSep1 setClause
>   wh <- maybeP whereClause
>   semi
>   return $ Update tableName scs wh

> setClause :: Text.Parsec.Prim.ParsecT String () Identity SetClause
> setClause = do
>   ref <- identifierString
>   symbol "="
>   ex <- expr
>   return $ SetClause ref ex

> whereClause :: Text.Parsec.Prim.ParsecT String () Identity Where
> whereClause = do
>   keyword "where"
>   ex <- expr
>   return $ Where ex

> maybeP :: GenParser tok st a
>           -> Text.Parsec.Prim.ParsecT [tok] st Identity (Maybe a)
> maybeP p = do
>   (do a <- try p
>       return $ Just a)
>   <|> return Nothing

> createTable :: Text.Parsec.Prim.ParsecT String () Identity Statement
> createTable = do
>   keyword "create"
>   keyword "table"
>   n <- identifierString
>   atts <- parens $ commaSep1 tableAtt
>   semi
>   return $ CreateTable n atts

> tableAtt :: Text.Parsec.Prim.ParsecT String () Identity AttributeDef
> tableAtt = do
>   name <- identifierString
>   typ <- identifierString
>   return $ AttributeDef name typ

> select :: Text.Parsec.Prim.ParsecT String () Identity Statement
> select = do
>   keyword "select"
>   (do try selExpression
>    <|> selQuerySpec)

> selQuerySpec :: Text.Parsec.Prim.ParsecT String () Identity Statement
> selQuerySpec = do
>   sl <- (do
>          symbol "*"
>          return Star
>         ) <|> selectList
>   keyword "from"
>   tb <- word
>   semi
>   return $ Select sl tb

> selectList :: Text.Parsec.Prim.ParsecT String () Identity SelectList
> selectList = do
>   liftM SelectList $ commaSep1 identifierString

> selExpression :: Text.Parsec.Prim.ParsecT [Char] () Identity Statement
> selExpression = do
>   e <- expr
>   semi
>   return $ SelectE e

 > expression :: Text.Parsec.Prim.ParsecT [Char] () Identity Expression
 > expression = do
 >   try binaryOperator
 >     <|> try functionCall
 >     <|> identifierExpr
 >     <|> stringLiteral
 >     <|> integerLiteral

 > binaryOperator :: GenParser Char () Expression
 > binaryOperator = do
 >   e1 <- expression
 >   op <- lexeme $ many1 $ oneOf "+-="
 >   e2 <- expression
 >   return $ BinaryOperatorCall op e1 e2

 > functionCall :: Text.Parsec.Prim.ParsecT String () Identity Expression
 > functionCall = do
 >   name <- identifierString
 >   args <- parens $ commaSep expression
 >   return $ FunctionCall name args

 > stringLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
 > stringLiteral = do
 >   char '\''
 >   name <- many1 (noneOf "'")
 >   lexeme $ char '\''
 >   return $ StringLiteral name

 > integerLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
 > integerLiteral = do
 >   liftM IntegerLiteral $ integer

 > identifierExpr :: Text.Parsec.Prim.ParsecT String () Identity Expression
 > identifierExpr = liftM Identifier $ lexeme word

> keyword :: String -> Text.Parsec.Prim.ParsecT String u Identity String
> keyword k = lexeme $ string k

> identifierString :: Parser String
> identifierString = word

> word :: Parser String
> word = lexeme (many1 letter)

 > integer :: Text.Parsec.Prim.ParsecT String u Identity Integer
 > integer = lexeme $ P.integer lexer

> whitespace :: Text.Parsec.Prim.ParsecT String u Identity ()
> whitespace = spaces --skipMany space ?

===============================================================================

> lexer :: P.GenTokenParser String u Identity
> lexer = P.makeTokenParser (haskellDef
>                            { reservedOpNames = ["*","/","+","-"]
>                            })

 > natural   = P.natural lexer

 > identifier= P.identifier lexer

 > reserved  = P.reserved lexer
 > reservedOp= P.reservedOp lexer

> lexeme :: Text.Parsec.Prim.ParsecT String u Identity a
>           -> Text.Parsec.Prim.ParsecT String u Identity a
> lexeme = P.lexeme lexer

> commaSep :: Text.Parsec.Prim.ParsecT String u Identity a
>             -> Text.Parsec.Prim.ParsecT String u Identity [a]
> commaSep = P.commaSep lexer

> commaSep1 :: Text.Parsec.Prim.ParsecT String u Identity a
>             -> Text.Parsec.Prim.ParsecT String u Identity [a]
> commaSep1 = P.commaSep lexer


> semi :: Text.Parsec.Prim.ParsecT String u Identity String
> semi = P.semi lexer

> parens :: Text.Parsec.Prim.ParsecT String u Identity a
>           -> Text.Parsec.Prim.ParsecT String u Identity a
> parens = P.parens lexer

> symbol :: String -> Text.Parsec.Prim.ParsecT String u Identity String
> symbol = P.symbol lexer

================================================================================

> expr :: Parser Expression
> expr =
>   buildExpressionParser table factor
>   <?> "expression"

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
>       [[prefix "-" (BinaryOperatorCall Mult (IntegerL (-1)))]
>       ,[binary "^" (BinaryOperatorCall Pow) AssocRight]
>       ,[binary "*" (BinaryOperatorCall Mult) AssocLeft
>        ,binary "/" (BinaryOperatorCall Div) AssocLeft
>        ,binary "%" (BinaryOperatorCall Mod) AssocLeft]
>       ,[binary "+" (BinaryOperatorCall Plus) AssocLeft
>        ,binary "-" (BinaryOperatorCall Minus) AssocLeft]
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
> parseExpression :: String -> Expression
> parseExpression s = case parse expr "" s of
>   Right ast -> ast
>   Left e -> error $ show e


