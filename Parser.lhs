> module Parser where

> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec.Token as P
> import Text.ParserCombinators.Parsec.Language
> import qualified Text.Parsec.Prim
> import Control.Monad.Identity
> import Text.ParserCombinators.Parsec.Expr
> import Data.Maybe

> import Grammar

================================================================================

Top level parsing functions

Parse fully formed sql

> parseSql :: String -> Either ParseError [Statement]
> parseSql s = parse statements "(unknown)" s

> parseSqlFile :: String -> IO (Either ParseError [Statement])
> parseSqlFile f = parseFromFile statements f

Parse expression fragment, used for testing purposes

> parseExpression :: String -> Either ParseError Expression
> parseExpression s = parse expr' "" s
>   where expr' = do
>                 x <- expr
>                 eof
>                 return x

================================================================================

Parsing top level statements

> statements :: Text.Parsec.Prim.ParsecT [Char] () Identity [Statement]
> statements = do
>   whitespace
>   s <- many statement
>   eof
>   return s

> statement :: Text.Parsec.Prim.ParsecT [Char] () Identity Statement
> statement = do
>   s <- (
>         try genSelect
>         <|> try insert
>         <|> try update
>         <|> try delete
>         <|> try (do
>               keyword "create"
>               (createTable
>                <|> createFunction
>                <|> createView))
>         <|> try assignment
>         <|> try returnSt
>         <|> try raise
>         <|> try forStatement
>         <|> nullStatement)
>   semi
>   return s

statement types

> insert :: Text.Parsec.Prim.ParsecT String () Identity Statement
> insert = do
>   keyword "insert"
>   keyword "into"
>   tableName <- identifierString
>   atts <- parens $ commaSep1 identifierString
>   keyword "values"
>   exps <- parens $ commaSep1 expr
>   return $ Insert tableName atts exps

> update :: Text.Parsec.Prim.ParsecT String () Identity Statement
> update = do
>   keyword "update"
>   tableName <- identifierString
>   keyword "set"
>   scs <- commaSep1 setClause
>   wh <- maybeP whereClause
>   return $ Update tableName scs wh

> delete :: Text.Parsec.Prim.ParsecT String () Identity Statement
> delete = do
>   keyword "delete"
>   keyword "from"
>   tableName <- identifierString
>   wh <- maybeP whereClause
>   return $ Delete tableName wh

> createTable :: Text.Parsec.Prim.ParsecT String () Identity Statement
> createTable = do
>   keyword "table"
>   n <- identifierString
>   atts <- parens $ commaSep1 tableAtt
>   return $ CreateTable n atts

> genSelect :: Text.Parsec.Prim.ParsecT [Char] () Identity Statement
> genSelect = do
>   (try exceptSelect)
>   <|> select

> select :: Text.Parsec.Prim.ParsecT String () Identity Statement
> select = do
>   keyword "select"
>   (do try selQuerySpec
>    <|> selExpression)

> exceptSelect :: Text.Parsec.Prim.ParsecT String () Identity Statement
> exceptSelect = do
>   s1 <- select
>   keyword "except"
>   s2 <- select
>   return $ ExceptSelect s1 s2

> createFunction :: GenParser Char () Statement
> createFunction = do
>   keyword "function"
>   fnName <- identifierString
>   params <- parens $ commaSep param
>   keyword "returns"
>   retType <- identifierString
>   keyword "as"
>   symbol "$$"
>   (decls, stmts) <- functionBody
>   symbol "$$"
>   keyword "language"
>   keyword "plpgsql"
>   keyword "volatile"
>   return $ CreateFunction fnName params retType decls stmts

> createView :: Text.Parsec.Prim.ParsecT String () Identity Statement
> createView = do
>   keyword "view"
>   vName <- identifierString
>   keyword "as"
>   sel <- genSelect
>   return $ CreateView vName sel

> nullStatement :: Text.Parsec.Prim.ParsecT String u Identity Statement
> nullStatement = do
>   keyword "null"
>   return NullStatement

> forStatement :: GenParser Char () Statement
> forStatement = do
>   keyword "for"
>   i <- identifierString
>   keyword "in"
>   st <- genSelect
>   keyword "loop"
>   stmts <- many statement
>   keyword "end"
>   keyword "loop"
>   return $ ForStatement i st stmts

> selExpression :: Text.Parsec.Prim.ParsecT [Char] () Identity Statement
> selExpression = do
>   e <- expr
>   return $ SelectE e

plpgsql stements

> assignment :: Text.Parsec.Prim.ParsecT String () Identity Statement
> assignment = do
>   n <- identifierString
>   symbol ":="
>   ex <- expr
>   return $ Assignment n ex

> returnSt :: Text.Parsec.Prim.ParsecT String () Identity Statement
> returnSt = do
>   keyword "return"
>   ex <- expr
>   return $ Return ex

> raise :: Text.Parsec.Prim.ParsecT String () Identity Statement
> raise = do
>   keyword "raise"
>   keyword "notice"
>   s <- stringPar
>   exps <- maybeP (do
>                    symbol ","
>                    commaSep expr)
>   return $ Raise RNotice s (fromMaybe [] exps)

Statement components

> functionBody :: Text.Parsec.Prim.ParsecT [Char] () Identity ([VarDef], [Statement])
> functionBody =
>   (do
>      keyword "declare"
>      decls <- manyTill (try varDef) (try $ keyword "begin")
>      stmts <- many statement
>      keyword "end"
>      semi
>      return (decls,stmts)
>   ) <|> (do
>      keyword "begin"
>      stmts <- many statement
>      keyword "end"
>      semi
>      return ([],stmts))

> varDef :: Text.Parsec.Prim.ParsecT String () Identity VarDef
> varDef = do
>   name <- identifierString
>   tp <- identifierString
>   semi
>   return $ VarDef name tp

> param :: Text.Parsec.Prim.ParsecT String () Identity ParamDef
> param = do
>   name <- identifierString
>   tp <- identifierString
>   return $ ParamDef name tp

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

> tableAtt :: Text.Parsec.Prim.ParsecT String () Identity AttributeDef
> tableAtt = do
>   name <- identifierString
>   typ <- identifierString
>   check <- maybeP (do
>                     keyword "check"
>                     expr)
>   return $ AttributeDef name typ check

> selQuerySpec :: Text.Parsec.Prim.ParsecT String () Identity Statement
> selQuerySpec = do
>   sl <- (do
>          symbol "*"
>          return Star
>         ) <|> selectList
>   keyword "from"
>   tb <- identifierString
>   wh <- maybeP whereClause
>   return $ Select sl tb wh

> selectList :: Text.Parsec.Prim.ParsecT String () Identity SelectList
> selectList = do
>   liftM SelectList $ commaSep1 identifierString

================================================================================

expressions

> expr :: Parser Expression
> expr =
>   buildExpressionParser table factor
>   <?> "expression"

> factor :: GenParser Char () Expression
> factor  = parens expr
>           <|> stringLiteral
>           <|> integer
>           <|> try booleanLiteral
>           <|> try inPredicate
>           <|> try functionCall
>           <|> identifier
>           <?> "simple expression"

>   -- Specifies operator, associativity, precendence, and constructor to execute
>   -- and built AST with.
> table :: [[Operator Char u Expression]]
> table =
>       [ --[prefix "-" (BinaryOperatorCall Mult (IntegerL (-1)))]
>       --,
>        [binary "^" (BinaryOperatorCall Pow) AssocRight]
>       ,[binary "*" (BinaryOperatorCall Mult) AssocLeft
>        ,binary "/" (BinaryOperatorCall Div) AssocLeft
>        ,binary "=" (BinaryOperatorCall Eql) AssocLeft
>        ,binary "%" (BinaryOperatorCall Mod) AssocLeft]
>       ,[binary "+" (BinaryOperatorCall Plus) AssocLeft
>        ,binary "-" (BinaryOperatorCall Minus) AssocLeft
>        ,binary "and" (BinaryOperatorCall And) AssocLeft]
>       ]
>     where
>       binary s f assoc
>          = Infix (symbol s >> return f) assoc
>       --prefix s f
>       --   = Prefix (symbol s >> return f)
>

> inPredicate :: Text.Parsec.Prim.ParsecT [Char] () Identity Expression
> inPredicate = do
>   vexp <- identifierString
>   keyword "in"
>   e <- parens $ commaSep1 expr
>   return $ InPredicate vexp e

> identifier :: Text.Parsec.Prim.ParsecT String () Identity Expression
> identifier = liftM Identifier identifierString

> booleanLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
> booleanLiteral = do
>   x <- ((lexeme $ string "true")
>         <|> (lexeme $ string "false"))
>   return $ BooleanL (x == "true")

> integer :: Text.Parsec.Prim.ParsecT String u Identity Expression
> integer = liftM IntegerL $ lexeme $ P.integer lexer

> stringLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
> stringLiteral = do
>   liftM StringL stringPar

> stringPar :: Text.Parsec.Prim.ParsecT String u Identity String
> stringPar = do
>   char '\''
>   name <- many (noneOf "'")
>   lexeme $ char '\''
>   return name


> functionCall :: Text.Parsec.Prim.ParsecT String () Identity Expression
> functionCall = do
>   name <- identifierString
>   args <- parens $ commaSep factor
>   return $ FunctionCall name args



================================================================================

Utility parsers

> whitespace :: Text.Parsec.Prim.ParsecT String u Identity ()
> whitespace = skipMany ((space >> return ())
>                        <|> blockComment
>                        <|> lineComment)

> keyword :: String -> Text.Parsec.Prim.ParsecT String u Identity String
> keyword k = lexeme $ string k

> identifierString :: Parser String
> identifierString = do
>   s <- letter
>   p <- many (alphaNum <|> char '_')
>   whitespace
>   return $ s : p

 > word :: Parser String
 > word = lexeme (many1 letter)

> maybeP :: GenParser tok st a
>           -> Text.Parsec.Prim.ParsecT [tok] st Identity (Maybe a)
> maybeP p = do
>   (do a <- try p
>       return $ Just a)
>   <|> return Nothing

> blockComment :: Text.Parsec.Prim.ParsecT [Char] st Identity ()
> blockComment = do
>   try (char '/' >> char '*')
>   manyTill anyChar (try (string "*/"))
>   return ()

> lineComment :: Text.Parsec.Prim.ParsecT [Char] st Identity ()
> lineComment = do
>   try (char '-' >> char '-')
>   manyTill anyChar ((try (char '\n') >> return ()) <|> eof)
>   return ()

================================================================================

pass through stuff from parsec

> lexer :: P.GenTokenParser String u Identity
> lexer = P.makeTokenParser (haskellDef
>                            { reservedOpNames = ["*","/","+","-"],
>                              commentStart = "/*",
>                              commentEnd = "*/",
>                              commentLine = "--"
>                            })

> lexeme :: Text.Parsec.Prim.ParsecT String u Identity a
>           -> Text.Parsec.Prim.ParsecT String u Identity a
> lexeme = P.lexeme lexer

> commaSep :: Text.Parsec.Prim.ParsecT String u Identity a
>             -> Text.Parsec.Prim.ParsecT String u Identity [a]
> commaSep = P.commaSep lexer

> commaSep1 :: Text.Parsec.Prim.ParsecT String u Identity a
>             -> Text.Parsec.Prim.ParsecT String u Identity [a]
> commaSep1 = P.commaSep lexer

> semiSep :: Text.Parsec.Prim.ParsecT String u Identity a
>             -> Text.Parsec.Prim.ParsecT String u Identity [a]
> semiSep = P.semiSep lexer


> semi :: Text.Parsec.Prim.ParsecT String u Identity String
> semi = P.semi lexer

> parens :: Text.Parsec.Prim.ParsecT String u Identity a
>           -> Text.Parsec.Prim.ParsecT String u Identity a
> parens = P.parens lexer

> symbol :: String -> Text.Parsec.Prim.ParsecT String u Identity String
> symbol = P.symbol lexer
