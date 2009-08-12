> module Parser where

> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec.Token as P
> import Text.ParserCombinators.Parsec.Language
> import qualified Text.Parsec.Prim
> import Control.Monad.Identity
> import Text.Parsec.Expr
> import Data.Maybe

> import Grammar

================================================================================

Top level parsing functions

Parse fully formed sql

> parseSql :: String -> Either ParseError [Statement]
> parseSql = parse statements "(unknown)"

> parseSqlFile :: String -> IO (Either ParseError [Statement])
> parseSqlFile = parseFromFile statements

Parse expression fragment, used for testing purposes

> parseExpression :: String -> Either ParseError Expression
> parseExpression s = parse expr' "" s
>   where expr' = do
>                 x <- expr
>                 eof
>                 return x

================================================================================

Parsing top level statements

> statements :: Text.Parsec.Prim.ParsecT String () Identity [Statement]
> statements = do
>   whitespace
>   s <- many statement
>   eof
>   return s

> statement :: Text.Parsec.Prim.ParsecT String () Identity Statement
> statement = do
>   (do
>    s <- (
>         try select
>         <|> try insert
>         <|> try update
>         <|> try delete
>         <|> try (do
>               keyword "create"
>               (createTable
>                <|> createFunction
>                <|> createView
>                <|> createDomain))
>         <|> try assignment
>         <|> try returnSt
>         <|> try raise
>         <|> try forStatement
>         <|> try perform
>         <|> nullStatement)
>    semi
>    return s)
>    <|> copy

statement types

> copy :: Text.Parsec.Prim.ParsecT [Char] u Identity Statement
> copy = do
>   keyword "copy"
>   --x <- manyTill anyChar (try (string "END OF COPY"))
>   x <- getLinesTillMatches "\\.\n"
>   whitespace
>   return $ Copy x


> getLinesTillMatches :: [Char] -> Text.Parsec.Prim.ParsecT [Char] u Identity [Char]

 > getLinesTillMatches :: (Num a) =>
 >                        [Char]
 >                            -> a
 >                            -> Text.Parsec.Prim.ParsecT [Char] u Identity [Char]

> getLinesTillMatches s = do
>   x <- getALine
>   if x == s
>     then return x
>     else liftM (x++) $ getLinesTillMatches s

> getALine :: Text.Parsec.Prim.ParsecT [Char] u Identity [Char]
> getALine = do
>   x <- manyTill anyChar (try newline)
>   return $ x ++ "\n"

> insert :: Text.Parsec.Prim.ParsecT String () Identity Statement
> insert = do
>   keyword "insert"
>   keyword "into"
>   tableName <- identifierString
>   atts <- maybeP (parens $ commaSep1 identifierString)
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

 > genSelect :: Text.Parsec.Prim.ParsecT String () Identity Statement
 > genSelect =
 >   try exceptSelect <|> select

> select :: Text.Parsec.Prim.ParsecT String () Identity Statement
> select = do
>   keyword "select"
>   s1 <- selQuerySpec
>   (do
>     (try (do keyword "except"
>              s2 <- select
>              return $ CombineSelect Except s1 s2))
>     <|> (try (do keyword "union"
>                  s3 <- select
>                  return $ CombineSelect Union s1 s3))
>     <|> (return s1))

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
>   sel <- select
>   return $ CreateView vName sel

> createDomain :: Text.Parsec.Prim.ParsecT String () Identity Statement
> createDomain = do
>   keyword "domain"
>   nm <- identifierString
>   keyword "as"
>   tp <- identifierString
>   check <- maybeP (do
>                     keyword "check"
>                     expr)
>   return $ CreateDomain nm tp check

> nullStatement :: Text.Parsec.Prim.ParsecT String u Identity Statement
> nullStatement = do
>   keyword "null"
>   return NullStatement

> forStatement :: GenParser Char () Statement
> forStatement = do
>   keyword "for"
>   i <- identifierString
>   keyword "in"
>   st <- select
>   keyword "loop"
>   stmts <- many statement
>   keyword "end"
>   keyword "loop"
>   return $ ForStatement i st stmts

> perform :: Text.Parsec.Prim.ParsecT String () Identity Statement
> perform = do
>   keyword "perform"
>   ex <- expr
>   return $ Perform ex

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

> functionBody :: Text.Parsec.Prim.ParsecT String () Identity ([VarDef], [Statement])
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
>   nl <- maybeP ((do
>                   try (keyword "null")
>                   return NullL)
>                 <|>
>                 (do
>                   keyword "not"
>                   keyword "null"
>                   return $ BinaryOperatorCall Not (NullL) (NullL)))
>   check <- maybeP (do
>                     keyword "check"
>                     expr)
>   return $ if isJust nl
>              then AttributeDef name typ nl
>              else AttributeDef name typ check

> selQuerySpec :: Text.Parsec.Prim.ParsecT String () Identity Statement
> selQuerySpec = do
>   sl <- selectList
>   tb <- maybeP from
>   wh <- maybeP whereClause
>   return $ Select sl tb wh

> from :: GenParser Char () From
> from = do
>        keyword "from"
>        liftM From tref

> tref :: Text.Parsec.Prim.ParsecT String () Identity TableRef
> tref = do
>        a <- identifierString
>        b <- maybeP (do
>                     whitespace
>                     x <- identifierString
>                     if x `elem` ["where", "except", "union", "loop", "inner", "on"]
>                       then fail "not keyword"
>                       else return x)
>        jn <- maybeP joinPart
>        let tr1 = case b of
>                         Nothing -> Tref a
>                         Just b1 -> TrefAlias a b1
>        case jn of
>          Nothing -> return tr1
>          Just (jt,tr2,ex) -> return  $ JoinedTref tr1 jt tr2 ex

> joinPart :: GenParser Char () (JoinType, TableRef, Maybe Expression)
> joinPart = do
>   keyword "inner"
>   keyword "join"
>   tr2 <- tref
>   ex <- maybeP (do
>                  keyword "on"
>                  expr)
>   return (Inner,tr2,ex)


> selectList :: Text.Parsec.Prim.ParsecT String () Identity SelectList
> selectList = liftM SelectList $ commaSep1 selectItem

> selectItem :: Text.Parsec.Prim.ParsecT String () Identity SelectItem
> selectItem = do
>        ex <- expr
>        i <- maybeP (do
>                     keyword "as"
>                     identifierString)
>        return $ case i of
>                   Nothing -> SelExp ex
>                   Just iden -> SelectItem ex iden

================================================================================

expressions

> expr :: Parser Expression
> expr = buildExpressionParser table factor
>        <?> "expression"

> factor :: GenParser Char () Expression
> factor  = try scalarSubQuery
>           <|> parens expr
>           <|> stringLiteral
>           <|> integer
>           <|> try booleanLiteral
>           <|> try inPredicate
>           <|> try nullL
>           <|> try array
>           <|> try functionCall
>           <|> try identifier
>           <?> "simple expression"

> scalarSubQuery :: GenParser Char () Expression
> scalarSubQuery = do
>   x <- parens select
>   return $ ScalarSubQuery x

>   -- Specifies operator, associativity, precendence, and constructor to execute
>   -- and built AST with.

> table :: [[Operator [Char] u Identity Expression]]
> table =
>       [[--prefix "-" (BinaryOperatorCall Mult (IntegerL (-1)))
>         prefixk "not" (BinaryOperatorCall Not (NullL))
>        ,binary "." (BinaryOperatorCall Qual) AssocLeft]
>       ,[binary "::" (BinaryOperatorCall Cast) AssocLeft
>        ,binary "^" (BinaryOperatorCall Pow) AssocRight]
>       ,[binary "*" (BinaryOperatorCall Mult) AssocLeft
>        ,binary "/" (BinaryOperatorCall Div) AssocLeft
>        ,binary "=" (BinaryOperatorCall Eql) AssocLeft
>        ,binary "like" (BinaryOperatorCall Like) AssocLeft
>        ,postfixk "is not null" (BinaryOperatorCall IsNotNull (NullL))
>        ,postfixk "is null" (BinaryOperatorCall IsNull (NullL))
>        ,binary "%" (BinaryOperatorCall Mod) AssocLeft]
>       ,[binary "+" (BinaryOperatorCall Plus) AssocLeft
>        ,binary "-" (BinaryOperatorCall Minus) AssocLeft
>        ,binaryk "and" (BinaryOperatorCall And) AssocLeft
>        ,binary "||" (BinaryOperatorCall Conc) AssocLeft]
>       ]
>     where
>       binary s f
>          = Infix (try (symbol s >> return f))
>       binaryk s f
>          = Infix (try (keyword s >> return f))
>       prefixk s f
>          = Prefix (try (keyword s >> return f))
>       postfixk s f
>          = Postfix (try (keyword s >> return f))
>

> array :: GenParser Char () Expression
> array = do
>   keyword "array"
>   liftM ArrayL $ squares $ commaSep expr

> inPredicate :: Text.Parsec.Prim.ParsecT String () Identity Expression
> inPredicate = do
>   vexp <- identifierString
>   keyword "in"
>   e <- parens $ commaSep1 expr
>   return $ InPredicate vexp e

> nullL :: Text.Parsec.Prim.ParsecT String u Identity Expression
> nullL = do
>   keyword "null"
>   return NullL

> identifier :: Text.Parsec.Prim.ParsecT String () Identity Expression
> identifier = liftM Identifier identifierString

> booleanLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
> booleanLiteral = do
>   x <- lexeme (string "true")
>        <|> lexeme (string "false")
>   return $ BooleanL (x == "true")

> integer :: Text.Parsec.Prim.ParsecT String u Identity Expression
> integer = liftM IntegerL $ lexeme $ P.integer lexer

> stringLiteral :: Text.Parsec.Prim.ParsecT String u Identity Expression
> stringLiteral = liftM StringL stringPar

> stringPar :: Text.Parsec.Prim.ParsecT String u Identity String
> stringPar = do
>   char '\''
>   name <- many (noneOf "'")
>   lexeme $ char '\''
>   return name


> functionCall :: Text.Parsec.Prim.ParsecT String () Identity Expression
> functionCall = do
>   name <- identifierString
>   args <- parens $ commaSep expr
>   return $ FunctionCall name args



================================================================================

Utility parsers

> whitespace :: Text.Parsec.Prim.ParsecT String u Identity ()
> whitespace = skipMany ((space >> return ())
>                        <|> blockComment
>                        <|> lineComment)

 > keyword :: String -> Text.Parsec.Prim.ParsecT String u Identity String

> keyword :: String -> Text.Parsec.Prim.ParsecT String u Identity ()
> keyword k = do
>   lexeme $ do
>     string k
>     notFollowedBy alphaNum

> identifierString :: Parser String
> identifierString =
>   (do
>     string "*"
>     whitespace
>     return "*")
>   <|> do
>       s <- letter
>       p <- many (alphaNum <|> char '_')
>       whitespace
>       return $ s : p

 > word :: Parser String
 > word = lexeme (many1 letter)

> maybeP :: GenParser tok st a
>           -> Text.Parsec.Prim.ParsecT [tok] st Identity (Maybe a)
> maybeP p =
>   (do a <- try p
>       return $ Just a)
>   <|> return Nothing

> blockComment :: Text.Parsec.Prim.ParsecT String st Identity ()
> blockComment = do
>   try (char '/' >> char '*')
>   manyTill anyChar (try (string "*/"))
>   return ()

> lineComment :: Text.Parsec.Prim.ParsecT String st Identity ()
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

> squares :: Text.Parsec.Prim.ParsecT String u Identity a
>           -> Text.Parsec.Prim.ParsecT String u Identity a
> squares = P.squares lexer

> symbol :: String -> Text.Parsec.Prim.ParsecT String u Identity String
> symbol = P.symbol lexer
