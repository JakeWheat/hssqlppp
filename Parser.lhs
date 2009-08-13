> module Parser where

> import Text.Parsec
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language
> import qualified Text.Parsec.Prim
> import Control.Monad.Identity
> import Text.Parsec.Expr
> import Data.Maybe
> import Text.Parsec.String
> import Text.Parsec.Error

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
>               (try createTable
>                <|> createType
>                <|> createFunction
>                <|> createView
>                <|> createDomain))
>         <|> try assignment
>         <|> try ifStatement
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
>   where
>     getLinesTillMatches s = do
>                             x <- getALine
>                             if x == s
>                               then return x
>                               else liftM (x++) $ getLinesTillMatches s
>     getALine = do
>                x <- manyTill anyChar (try newline)
>                return $ x ++ "\n"

> insert :: ParsecT String () Identity Statement
> insert = do
>   keyword "insert"
>   keyword "into"
>   tableName <- identifierString
>   atts <- maybeP (parens $ commaSep1 identifierString)
>   ida <- (do
>           keyword "values"
>           exps <- commaSep1 $ parens $ commaSep1 expr
>           return $ InsertData exps) <|>
>          (do
>           s1 <- select
>           return $ InsertQuery s1)
>   return $ Insert tableName atts ida

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

> createType :: ParsecT String () Identity Statement
> createType = do
>   keyword "type"
>   n <- identifierString
>   keyword "as"
>   atts <- parens $ commaSep1 typeAtt
>   return $ CreateType n atts


> select :: ParsecT String () Identity Statement
> select = do
>   keyword "select"
>   i <- maybeP (do
>                 keyword "into"
>                 commaSep1 identifierString)
>   s1t <- selQuerySpec
>   let s1 = case i of
>              Nothing -> s1t
>              Just i1 -> SelectInto i1 s1t
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
>   body <- stringChoose
>   keyword "language"
>   lang <- (keyword "plpgsql" >> return Plpgsql)
>           <|> (keyword "sql" >> return Sql)

>   case parse (functionBody lang) ("function " ++ fnName) (extrStr body) of
>     Left e -> do
>       sp <- getPosition
>       error $ "in " ++ show sp ++ ", " ++ showEr e (extrStr body)
>     Right body' -> do
>                     vol <- (keyword "volatile" >> return Volatile)
>                            <|> (keyword "stable" >> return Stable)
>                            <|> (keyword "immutable" >> return Immutable)
>                     return $ CreateFunction lang fnName params retType (quoteOfString body) body' vol

> functionBody :: Language -> ParsecT String () Identity FnBody
> functionBody Sql = liftM SqlFnBody $ (whitespace >> many statement)
> functionBody Plpgsql = whitespace >>
>   ((do
>      keyword "declare"
>      decls <- manyTill (try varDef) (try $ keyword "begin")
>      stmts <- many statement
>      keyword "end"
>      semi
>      eof
>      return $ PlpgsqlFnBody decls stmts
>   ) <|> (do
>      keyword "begin"
>      stmts <- many statement
>      keyword "end"
>      semi
>      eof
>      return $ PlpgsqlFnBody [] stmts))

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
>   maybeP (keyword "as")
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
>   tp <- ((keyword "notice" >> return RNotice)
>          <|> (try (keyword "exception" >> return RException))
>          <|> (keyword "error" >> return RError))
>   s <- stringPar
>   exps <- maybeP (do
>                    symbol ","
>                    commaSep expr)
>   return $ Raise tp s (fromMaybe [] exps)

> ifStatement :: ParsecT String () Identity Statement
> ifStatement = do
>   keyword "if"
>   e <- expr
>   keyword "then"
>   st <- many statement
>   elsSts <- maybeP (do
>                      keyword "else"
>                      many statement)
>   keyword "end"
>   keyword "if"
>   return $ If e st elsSts

Statement components

> varDef :: ParsecT String () Identity VarDef
> varDef = do
>   name <- identifierString
>   tp <- identifierString
>   semi
>   return $ VarDef name tp

> param :: ParsecT String () Identity ParamDef
> param = do
>   name <- identifierString
>   t <- maybeP identifierString
>   case t of
>     Just tp -> return $ ParamDef name tp
>     Nothing -> return $ ParamDefTp name

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

> typeAtt :: ParsecT String () Identity TypeAttributeDef
> typeAtt = liftM2 TypeAttDef identifierString identifierString


> selQuerySpec :: ParsecT String () Identity Statement
> selQuerySpec = liftM5 Select selectList (maybeP from) (maybeP whereClause)
>                (maybeP orderBy) (maybeP limit)

> limit :: GenParser Char () Expression
> limit = keyword "limit" >> expr

> from :: GenParser Char () From
> from = do
>        keyword "from"
>        liftM From tref

> tref :: ParsecT String () Identity TableRef
> tref = do
>        tr1 <- (do
>                sub <- parens select
>                keyword "as"
>                alias <- identifierString
>                return $ SubTref sub alias
>               ) <|>
>               (do
>                a <- identifierString
>                b <- maybeP (do
>                             whitespace
>                             x <- identifierString
>                             if x `elem` ["where"
>                                         ,"except"
>                                         ,"union"
>                                         ,"loop"
>                                         ,"inner"
>                                         ,"on"
>                                         ,"left"
>                                         ,"right"
>                                         ,"full"
>                                         ,"cross"
>                                         ,"natural"
>                                         ,"order"
>                                         ,"limit"]
>                               then fail "not keyword"
>                               else return x)
>                return $ case b of
>                                Nothing -> Tref a
>                                Just b1 -> TrefAlias a b1)
>        jn <- maybeP $ joinPart tr1
>        case jn of
>          Nothing -> return tr1
>          Just jn1 -> return jn1

> joinPart :: TableRef -> GenParser Char () TableRef
> joinPart tr1 = do
>   nat <- maybeP $ keyword "natural"
>   typ <- ((do
>             keyword "inner"
>             return Inner) <|>
>            (do
>              keyword "left"
>              keyword "outer"
>              return LeftOuter) <|>
>            (do
>              keyword "right"
>              keyword "outer"
>              return RightOuter) <|>
>            (do
>              keyword "full"
>              keyword "outer"
>              return FullOuter) <|>
>            (do
>              keyword "cross"
>              return Cross))
>   keyword "join"
>   tr2 <- tref
>   ex <- maybeP (do
>                  keyword "on"
>                  expr)
>   let jp1 = JoinedTref tr1 (isJust nat) typ tr2 ex
>   jp2 <- maybeP $ joinPart jp1
>   case jp2 of
>     Nothing -> return jp1
>     Just j -> return j


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
>           <|> try rowCtor
>           <|> parens expr
>           <|> stringLiteral
>           <|> try stringLD
>           <|> try positionalArg
>           <|> integer
>           <|> try caseParse
>           <|> try exists
>           <|> try booleanLiteral
>           <|> try inPredicate
>           <|> try nullL
>           <|> try array
>           <|> try windowFn
>           <|> try functionCall
>           <|> try identifier
>           <?> "simple expression"

> positionalArg :: ParsecT String u Identity Expression
> positionalArg = do
>   char '$'
>   i <- lexeme $ P.integer lexer
>   return $ PositionalArg ((fromInteger i)::Int)

> scalarSubQuery :: GenParser Char () Expression
> scalarSubQuery = liftM ScalarSubQuery $ parens select

> rowCtor :: ParsecT [Char] () Identity Expression
> rowCtor = do
>   (do
>     keyword "row"
>     liftM Row $ parens $ commaSep expr)
>   <|> (liftM Row $ parens $ commaSep2 expr)

> commaSep2 :: ParsecT String u Identity t -> ParsecT String u Identity [t]
> commaSep2 p = sepBy2 p (symbol ",")

> sepBy2 :: (Stream s m t1) => ParsecT s u m t -> ParsecT s u m a -> ParsecT s u m [t]
> sepBy2 p sep = do
>   x <- p
>   sep
>   xs <- sepBy1 p sep
>   return (x:xs)

>   -- Specifies operator, associativity, precendence, and constructor to execute
>   -- and built AST with.

> table :: [[Operator [Char] u Identity Expression]]
> table = [[binary "." (BinaryOperatorCall Qual) AssocLeft]
>         ,[binary "::" (BinaryOperatorCall Cast) AssocLeft]
>          --missing []
>          --missing unary -
>         ,[binary "^" (BinaryOperatorCall Pow) AssocLeft]
>         ,[binary "*" (BinaryOperatorCall Mult) AssocLeft
>          ,binary "/" (BinaryOperatorCall Div) AssocLeft
>          ,binary "%" (BinaryOperatorCall Mod) AssocLeft]
>         ,[binary "+" (BinaryOperatorCall Plus) AssocLeft
>          ,binary "-" (BinaryOperatorCall Minus) AssocLeft]
>          --should be is isnull and notnull
>         ,[postfixk "is not null" (BinaryOperatorCall IsNotNull (NullL))
>          ,postfixk "is null" (BinaryOperatorCall IsNull (NullL))]
>          --other operators added in this list:
>         ,[binary "<=" (BinaryOperatorCall Lte) AssocRight
>          ,binary ">=" (BinaryOperatorCall Gte) AssocRight
>          ,binary "||" (BinaryOperatorCall Conc) AssocLeft]
>          --in should be here
>          --between
>          --overlaps
>         ,[binary "like" (BinaryOperatorCall Like) AssocNone
>           --moved <> temporarily since it doesn't parse when it
>           --is in the correct place, possibly cos it starts
>           --the same as '<' TODO: fix this properly
>          ,binary "<>" (BinaryOperatorCall NotEql) AssocNone]
>          --(also ilike similar)
>         ,[binary "<" (BinaryOperatorCall Lt) AssocNone
>          ,binary ">" (BinaryOperatorCall Gt) AssocNone]
>         ,[binary "=" (BinaryOperatorCall Eql) AssocRight
>           -- <> should be here
>          ]
>         ,[prefixk "not" (BinaryOperatorCall Not (NullL))]
>         ,[binaryk "and" (BinaryOperatorCall And) AssocLeft]]
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

> exists :: ParsecT [Char] () Identity Expression
> exists = do
>   keyword "exists"
>   liftM Exists $ parens select

> inPredicate :: ParsecT String () Identity Expression
> inPredicate = do
>   vexp <- identifierString
>   keyword "in"
>   e <- parens ((liftM InSelect select)
>                <|>
>                (liftM InList $ commaSep1 expr))
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

> stringChoose :: ParsecT String () Identity Expression
> stringChoose = (liftM StringL stringPar) <|> stringLD

> extrStr :: Expression -> String
> extrStr (StringLD _ s) = s
> extrStr (StringL s) = s
> extrStr x = error $ "extrStr not supported for this type " ++ show x

> quoteOfString :: Expression -> String
> quoteOfString (StringLD tag _) = "$" ++ tag ++ "$"
> quoteOfString (StringL _) = "'"
> quoteOfString x = error $ "quoteType not supported for this type " ++ show x

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
> functionCall = liftM2 FunctionCall identifierString (parens $ commaSep expr)

> windowFn :: GenParser Char () Expression
> windowFn = do
>   fn <- functionCall
>   keyword "over"

 >   os <- parens orderBy
 >   return $ WindowFn fn Nothing (Just os)

>   symbol "("
>   (ps,os) <- (try $ do
>                       p <- partitionBy
>                       symbol ","
>                       q <- orderBy
>                       return (Just p, Just q)
>                      ) <|> (do
>                             p <- partitionBy
>                             return (Just p, Nothing)
>                      ) <|> (do

>                             q <- orderBy
>                             return (Nothing, Just q))
>   symbol ")"

-->                      ) <|> (return (Nothing,Nothing)))

>   return $ WindowFn fn ps os

> orderBy :: GenParser Char () [Expression]
> orderBy = do
>           keyword "order"
>           keyword "by"
>           commaSep1 expr

> partitionBy :: GenParser Char () [Expression]
> partitionBy = do
>           keyword "partition"
>           keyword "by"
>           do
>             x <- p
>             do
>                 (do
>                     lookAhead (do
>                                 sep
>                                 keyword "order")
>                     fail "no order"
>                     )
>                 sep
>                 xs <- sepEndBy p sep
>                 return (x:xs)
>               <|> return [x]
>             where
>                 p = expr
>                 sep = expr

> caseParse :: ParsecT [Char] () Identity Expression
> caseParse = do
>   keyword "case"
>   wh <- many whenParse
>   ex <- maybeP (do
>                  keyword "else"
>                  e <- expr
>                  return $ Else e)
>   keyword "end"
>   return $ Case wh ex

> whenParse :: ParsecT String () Identity When
> whenParse = do
>   keyword "when"
>   e1 <- expr
>   keyword "then"
>   e2 <- expr
>   return $ When e1 e2

================================================================================

Utility parsers

> whitespace :: ParsecT String u Identity ()
> whitespace = skipMany ((space >> return ())
>                        <|> blockComment
>                        <|> lineComment)

> keyword :: String -> ParsecT String u Identity ()
> keyword k = do
>   (lexeme $ do
>     string k
>     notFollowedBy alphaNum) <?> k

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

> showEr :: ParseError -> String -> String
> showEr er src =
>     let  pos  = errorPos er
>          lineNo = sourceLine pos
>          ls = lines src
>          line = safeGet ls(lineNo - 1)
>          preline = safeGet ls (lineNo - 2)
>          postline = safeGet ls lineNo
>          colNo = sourceColumn pos
>          highlightLine = (take (colNo -1) (repeat ' ')) ++ "^"
>     in "\n---------------------\n" ++ show er
>        ++ "\n------------\nCheck it out:\n" ++ preline ++ "\n"
>        ++ line ++ "\n" ++ highlightLine ++ "\n" ++ postline
>        ++ "\n-----------------\n"
>          where
>            safeGet a i = if i < 0
>                            then ""
>                            else if i >= length a
>                                   then ""
>                                   else a !! i

show
