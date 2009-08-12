> module Parser where

> import Text.Parsec
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language
> import qualified Text.Parsec.Prim
> import Control.Monad.Identity
> import Text.Parsec.Expr
> import Data.Maybe
> import Text.Parsec.String

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

> statements :: ParsecT String () Identity [Statement]
> statements = do
>   whitespace
>   s <- many statement
>   eof
>   return s

> statement :: ParsecT String () Identity Statement
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

> copy :: ParsecT [Char] u Identity Statement
> copy = do
>   keyword "copy"
>   --x <- manyTill anyChar (try (string "END OF COPY"))
>   x <- getLinesTillMatches "\\.\n"
>   whitespace
>   return $ Copy x


> getLinesTillMatches :: [Char] -> ParsecT [Char] u Identity [Char]
> getLinesTillMatches s = do
>   x <- getALine
>   if x == s
>     then return x
>     else liftM (x++) $ getLinesTillMatches s

> getALine :: ParsecT [Char] u Identity [Char]
> getALine = do
>   x <- manyTill anyChar (try newline)
>   return $ x ++ "\n"

> insert :: ParsecT String () Identity Statement
> insert = do
>   keyword "insert"
>   keyword "into"
>   tableName <- identifierString
>   atts <- maybeP (parens $ commaSep1 identifierString)
>   keyword "values"
>   exps <- parens $ commaSep1 expr
>   return $ Insert tableName atts exps

> update :: ParsecT String () Identity Statement
> update = do
>   keyword "update"
>   tableName <- identifierString
>   keyword "set"
>   scs <- commaSep1 setClause
>   wh <- maybeP whereClause
>   return $ Update tableName scs wh

> delete :: ParsecT String () Identity Statement
> delete = do
>   keyword "delete"
>   keyword "from"
>   tableName <- identifierString
>   wh <- maybeP whereClause
>   return $ Delete tableName wh

> createTable :: ParsecT String () Identity Statement
> createTable = do
>   keyword "table"
>   n <- identifierString
>   atts <- parens $ commaSep1 tableAtt
>   return $ CreateTable n atts

> select :: ParsecT String () Identity Statement
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

> createView :: ParsecT String () Identity Statement
> createView = do
>   keyword "view"
>   vName <- identifierString
>   keyword "as"
>   sel <- select
>   return $ CreateView vName sel

> createDomain :: ParsecT String () Identity Statement
> createDomain = do
>   keyword "domain"
>   nm <- identifierString
>   keyword "as"
>   tp <- identifierString
>   check <- maybeP (do
>                     keyword "check"
>                     expr)
>   return $ CreateDomain nm tp check

> nullStatement :: ParsecT String u Identity Statement
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

> perform :: ParsecT String () Identity Statement
> perform = do
>   keyword "perform"
>   ex <- expr
>   return $ Perform ex

plpgsql stements

> assignment :: ParsecT String () Identity Statement
> assignment = do
>   n <- identifierString
>   symbol ":="
>   ex <- expr
>   return $ Assignment n ex

> returnSt :: ParsecT String () Identity Statement
> returnSt = do
>   keyword "return"
>   ex <- expr
>   return $ Return ex

> raise :: ParsecT String () Identity Statement
> raise = do
>   keyword "raise"
>   keyword "notice"
>   s <- stringPar
>   exps <- maybeP (do
>                    symbol ","
>                    commaSep expr)
>   return $ Raise RNotice s (fromMaybe [] exps)

Statement components

> functionBody :: ParsecT String () Identity ([VarDef], [Statement])
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

> varDef :: ParsecT String () Identity VarDef
> varDef = do
>   name <- identifierString
>   tp <- identifierString
>   semi
>   return $ VarDef name tp

> param :: ParsecT String () Identity ParamDef
> param = do
>   name <- identifierString
>   tp <- identifierString
>   return $ ParamDef name tp

> setClause :: ParsecT String () Identity SetClause
> setClause = do
>   ref <- identifierString
>   symbol "="
>   ex <- expr
>   return $ SetClause ref ex

> whereClause :: ParsecT String () Identity Where
> whereClause = do
>   keyword "where"
>   ex <- expr
>   return $ Where ex

> tableAtt :: ParsecT String () Identity AttributeDef
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
>   def <- maybeP (do
>                   keyword "default"
>                   expr)
>   check <- maybeP (do
>                     keyword "check"
>                     expr)
>   return $ if isJust nl
>              then AttributeDef name typ def nl
>              else AttributeDef name typ def check

> selQuerySpec :: ParsecT String () Identity Statement
> selQuerySpec = do
>   sl <- selectList
>   tb <- maybeP from
>   wh <- maybeP whereClause
>   return $ Select sl tb wh

> from :: GenParser Char () From
> from = do
>        keyword "from"
>        liftM From tref

> tref :: ParsecT String () Identity TableRef
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


> selectList :: ParsecT String () Identity SelectList
> selectList = liftM SelectList $ commaSep1 selectItem

> selectItem :: ParsecT String () Identity SelectItem
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
>           <|> stringLD
>           <|> integer
>           <|> try booleanLiteral
>           <|> try inPredicate
>           <|> try nullL
>           <|> try array
>           <|> try windowFn
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

> inPredicate :: ParsecT String () Identity Expression
> inPredicate = do
>   vexp <- identifierString
>   keyword "in"
>   e <- parens $ commaSep1 expr
>   return $ InPredicate vexp e

> nullL :: ParsecT String u Identity Expression
> nullL = do
>   keyword "null"
>   return NullL

> identifier :: ParsecT String () Identity Expression
> identifier = liftM Identifier identifierString

> booleanLiteral :: ParsecT String u Identity Expression
> booleanLiteral = do
>   x <- lexeme (string "true")
>        <|> lexeme (string "false")
>   return $ BooleanL (x == "true")

> integer :: ParsecT String u Identity Expression
> integer = liftM IntegerL $ lexeme $ P.integer lexer

> stringLiteral :: ParsecT String u Identity Expression
> stringLiteral = liftM StringL stringPar

> stringLD :: ParsecT String () Identity Expression
> stringLD = do
>   char '$'
>   tag <- ((do
>            lookAhead $ char '$'
>            return "") <|>
>            identifierString)
>   char '$'
>   s <- manyTill anyChar (try $ do
>                                char '$'
>                                string tag
>                                char '$')
>   whitespace
>   return $ StringLD tag s

> stringPar :: ParsecT String u Identity [Char]
> stringPar = do
>   char '\''
>   name <- readQuoteEscape

-- >   name <- manyTill anyChar $ lookAhead $ try $ do
-- >                                               char '\''
-- >                                               noneOf "'"
-- >                                               return ()

 >   ((try $ do
 >         char '\''
 >         fail "it's gone wrong again")
 >    <|> return $ replace "''" "'" name))

>   whitespace
>   return name

> readQuoteEscape :: ParsecT String u Identity [Char]
> readQuoteEscape = do
>   x <- anyChar
>   if x == '\''
>      then do
>          (try $ do
>             char '\''
>             l <- readQuoteEscape
>             return $ x:l)
>          <|> return ""
>      else do
>          l <- readQuoteEscape
>          return $ x:l


> functionCall :: ParsecT String () Identity Expression
> functionCall = do
>   name <- identifierString
>   args <- parens $ commaSep expr
>   return $ FunctionCall name args

> windowFn :: GenParser Char () Expression
> windowFn = do
>   fn <- functionCall
>   keyword "over"
>   os <- parens (maybeP (do
>                         keyword "order"
>                         keyword "by"
>                         commaSep1 expr))
>   return $ WindowFn fn os


================================================================================

Utility parsers

> whitespace :: ParsecT String u Identity ()
> whitespace = skipMany ((space >> return ())
>                        <|> blockComment
>                        <|> lineComment)

 > keyword :: String -> ParsecT String u Identity String

> keyword :: String -> ParsecT String u Identity ()
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

> maybeP :: GenParser tok st a
>           -> ParsecT [tok] st Identity (Maybe a)
> maybeP p =
>   (do a <- try p
>       return $ Just a)
>   <|> return Nothing

> blockComment :: ParsecT String st Identity ()
> blockComment = do
>   try (char '/' >> char '*')
>   manyTill anyChar (try (string "*/"))
>   return ()

> lineComment :: ParsecT String st Identity ()
> lineComment = do
>   try (char '-' >> char '-')
>   manyTill anyChar ((try (char '\n') >> return ()) <|> eof)
>   return ()

================================================================================

pass through stuff from parsec

> lexer :: P.GenTokenParser String u Identity
> lexer = P.makeTokenParser (haskellDef
>                            { P.reservedOpNames = ["*","/","+","-"],
>                              P.commentStart = "/*",
>                              P.commentEnd = "*/",
>                              P.commentLine = "--"
>                            })

> lexeme :: ParsecT String u Identity a
>           -> ParsecT String u Identity a
> lexeme = P.lexeme lexer

> commaSep :: ParsecT String u Identity a
>             -> ParsecT String u Identity [a]
> commaSep = P.commaSep lexer

> commaSep1 :: ParsecT String u Identity a
>             -> ParsecT String u Identity [a]
> commaSep1 = P.commaSep lexer

> semiSep :: ParsecT String u Identity a
>             -> ParsecT String u Identity [a]
> semiSep = P.semiSep lexer


> semi :: ParsecT String u Identity String
> semi = P.semi lexer

> parens :: ParsecT String u Identity a
>           -> ParsecT String u Identity a
> parens = P.parens lexer

> squares :: ParsecT String u Identity a
>           -> ParsecT String u Identity a
> squares = P.squares lexer

> symbol :: String -> ParsecT String u Identity String
> symbol = P.symbol lexer
