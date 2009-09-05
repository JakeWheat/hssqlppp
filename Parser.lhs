Copyright 2009 Jake Wheat

The main file for parsing sql, uses parsec (badly). Not sure if parsec
is the right choice.

For syntax reference see
http://savage.net.au/SQL/sql-2003-2.bnf.html
and
http://savage.net.au/SQL/sql-92.bnf.html
for some online sql grammar guides
and
http://www.postgresql.org/docs/8.4/interactive/sql-syntax.html
for some notes on postgresql syntax (the rest of that manual is also helpful)

Some further reference/reading:

parsec tutorial:
http://legacy.cs.uu.nl/daan/download/parsec/parsec.html

parsec reference:
http://hackage.haskell.org/package/parsec-3.0.0

pdf about parsing, uses haskell and parser combinators for examples
and exercises:
http://www.cs.uu.nl/docs/vakken/gont/diktaat.pdf

Crash course on applicative/point free parsing:

 delete = keyword "delete" >> keyword "from" >>
          Delete
          <$> idString
          <*> whereClause
          <*> returning

means the same as:

 delete = do
          keyword "delete"
          keyword "from"
          i <- idString
          w <- whereClause
          r <- returning
          return $ Delete i w r

and

 substring = keyword "substring" >> symbol '(' >>
             Substring
             <$> expr
             <*> (keyword "from" *> expr)
             <*> (keyword "for" *> expr <* symbol ')')

is same as

 substring = do
             keyword "substring"
             symbol '('
             e <- expr
             keyword "from"
             e1 <- expr
             keyword <- "for"
             e2 <- expr
             symbol ')'
             return $ Substring e e1 e2

(Note how >> is the same as *> but has different precedence)


Notes on source positions:
The constraints to try to satify are:
Don't change the ast node datatypes
Don't make the individual parsers in this file look like crap by
putting source pos stuff all over them
Plan:
whilst parsing, store the token positions in the parser state, which
will be some sort of tree
individual terminal parsers will add individual tokensource positions
to the state in a generic way

the tree structure will appear by having a token position acceptor
custom for each nonterminal parser which is saved in the state and
filters the incoming tokens from the terminal parsers into the right
tree structure then the parser will return the list of statements and
a parallel tree of token positions (sounds mental?)


> module Parser (
>               --parse fully formed sql statements from a string
>               parseSql
>               --parse a file containing sql statements only
>              ,parseSqlFile
>               --parse a file and return the parse tree and the
>               --parser state also
>              ,parseSqlFileWithState
>               --parse an expression (one expression plus whitespace
>               --only allowed
>              ,parseExpression
>              --parse fully formed plpgsql statements from a string
>              ,parsePlpgsql
>              )
>     where

> import Text.Parsec hiding(many, optional, (<|>), string)
> import Text.Parsec.Expr
> import Text.Parsec.String

> import Control.Applicative
> import Control.Monad.Identity

> import Data.Maybe
> import Data.Char

> import Lexer
> import ParseErrors
> import Ast

> type ParseState = [MySourcePos]

> startState :: ParseState
> startState = []

> toMySp :: SourcePos -> MySourcePos
> toMySp sp = (sourceName sp, sourceLine sp, sourceColumn sp)

=====================================W==========================================

= Top level parsing functions

parse fully formed sql

> parseSql :: String -> Either ExtendedError StatementList
> parseSql s = statementsOnly $ parseIt (lexSqlText s) sqlStatements "" s startState

> parseSqlFile :: String -> IO (Either ExtendedError StatementList)
> parseSqlFile fn = do
>   sc <- readFile fn
>   x <- lexSqlFile fn
>   return $ statementsOnly $ parseIt x sqlStatements fn sc startState

> parseSqlFileWithState :: String -> IO (Either ExtendedError StatementList)
> parseSqlFileWithState fn = do
>   sc <- readFile fn
>   x <- lexSqlFile fn
>   return $ parseIt x sqlStatements fn sc startState


Parse expression fragment, used for testing purposes

> parseExpression :: String -> Either ExtendedError Expression
> parseExpression s = parseIt (lexSqlText s) (expr <* eof) "" s startState


parse plpgsql statements, used for testing purposes

> parsePlpgsql :: String -> Either ExtendedError StatementList
> parsePlpgsql s =  parseIt (lexSqlText s) (many plPgsqlStatement <* eof) "" s startState

utility function to do error handling in one place

> parseIt lexed parser fn src ss =
>     case lexed of
>                Left er -> Left er
>                Right toks -> convertToExtendedError
>                                (runParser parser ss fn toks) fn src

> statementsOnly :: Either ExtendedError StatementList
>                -> Either ExtendedError StatementList
> statementsOnly s = case s of
>                      Left er -> Left er
>                      Right st -> Right st

================================================================================

= Parsing top level statements

> sqlStatements :: ParsecT [Token] ParseState Identity [(MySourcePos,Statement)]
> sqlStatements = many (sqlStatement True) <* eof

parse a statement

> sqlStatement :: Bool -> ParsecT [Token] ParseState Identity (MySourcePos,Statement)
> sqlStatement reqSemi = do
>    p <- getAdjustedPosition
>    st <- ((choice [
>                          selectStatement
>                         ,insert
>                         ,update
>                         ,delete
>                         ,truncateSt
>                         ,copy
>                         ,keyword "create" *>
>                                    choice [
>                                       createTable
>                                      ,createType
>                                      ,createFunction
>                                      ,createView
>                                      ,createDomain]
>                         ,keyword "drop" *>
>                                    choice [
>                                       dropSomething
>                                      ,dropFunction]
>                         ]
>        <* (if reqSemi
>              then symbol ";" >> return ()
>              else optional (symbol ";") >> return ()))
>       <|> copyData)
>    return (p, st)

> getAdjustedPosition :: ParsecT [Token] ParseState Identity MySourcePos
> getAdjustedPosition = do
>   p <- toMySp <$> getPosition
>   s <- getState
>   case s of
>     [] -> return p
>     x:_ -> return $ adjustPosition x p

> adjustPosition :: MySourcePos -> MySourcePos -> MySourcePos
> adjustPosition (fn,pl,_) (_,l,c) = (fn,pl+l-1,c)

================================================================================

statement flavour parsers

top level/sql statements first

= select

select parser, parses things starting with the keyword 'select'

supports plpgsql 'select into' only for the variants which look like
'select into ([targets]) [columnNames] from ...
or
'select [columnNames] into ([targets]) from ...

recurses to support parsing excepts, unions, etc

> selectStatement :: ParsecT [Token] ParseState Identity Statement
> selectStatement = SelectStatement <$> selectExpression

> selectExpression :: ParsecT [Token] ParseState Identity SelectExpression
> selectExpression =
>   choice [selectE, values]
>   where
>     selectE = do
>       keyword "select"
>       s1 <- selQuerySpec
>       choice [
>         --don't know if this does associativity in the correct order for
>         --statements with multiple excepts/ intersects and no parens
>         CombineSelect Except s1 <$> (keyword "except" *> selectExpression)
>        ,CombineSelect Intersect s1 <$> (keyword "intersect" *> selectExpression)
>        ,CombineSelect UnionAll s1 <$> (try (keyword "union"
>                                              *> keyword "all") *> selectExpression)
>        ,CombineSelect Union s1 <$> (keyword "union" *> selectExpression)
>        ,return s1]
>       where
>         selQuerySpec = Select
>                    <$> option Dupes (Distinct <$ keyword "distinct")
>                    <*> selectList
>                    <*> tryOptionMaybe from
>                    <*> tryOptionMaybe whereClause
>                    <*> option [] groupBy
>                    <*> tryOptionMaybe having
>                    <*> option [] orderBy
>                    <*> option Asc (choice [
>                                     Asc <$ keyword "asc"
>                                    ,Desc <$ keyword "desc"])
>                    <*> tryOptionMaybe limit
>                    <*> tryOptionMaybe offset
>         from = keyword "from" *> tref
>         groupBy = keyword "group" *> keyword "by"
>                   *> commaSep1 expr
>         having = keyword "having" *> expr
>         orderBy = keyword "order" *> keyword "by"
>                   *> commaSep1 expr
>         limit = keyword "limit" *> expr
>         offset = keyword "offset" *> expr

>         -- table refs
>         -- have to cope with:
>         -- a simple tableref i.e just a name
>         -- an aliased table ref e.g. select a.b from tbl as a
>         -- a sub select e.g. select a from (select b from c)
>         --  - these are handled in tref
>         -- then cope with joins recursively using joinpart below
>         tref = threadOptionalSuffix getFirstTref joinPart
>         getFirstTref = choice [
>                         SubTref
>                         <$> parens selectExpression
>                         <*> (optional (keyword "as") *> idString)
>                        ,optionalSuffix
>                           TrefFun (try $ identifier >>= functionCallSuffix)
>                           TrefFunAlias () (optional (keyword "as") *> idString)
>                        ,optionalSuffix
>                           Tref nkwid
>                           TrefAlias () (optional (keyword "as") *> nkwid)]
>         --joinpart: parse a join after the first part of the tableref
>         --(which is a table name, aliased table name or subselect) -
>         --takes this tableref as an arg so it can recurse to multiple
>         --joins
>         joinPart tr1 = threadOptionalSuffix (readOneJoinPart tr1) joinPart
>         readOneJoinPart tr1 = JoinedTref tr1
>              --look for the join flavour first
>              <$> option Unnatural (Natural <$ keyword "natural")
>              <*> choice [
>                 Inner <$ keyword "inner"
>                ,LeftOuter <$ try (keyword "left" *> keyword "outer")
>                ,RightOuter <$ try (keyword "right" *> keyword "outer")
>                ,FullOuter <$ try (keyword "full" >> keyword "outer")
>                ,Cross <$ keyword "cross"]
>              --recurse back to tref to read the table
>              <*> (keyword "join" *> tref)
>              --now try and read the join condition
>              <*> choice [
>                  Just <$> (JoinOn <$> (keyword "on" *> expr))
>                 ,Just <$> (JoinUsing <$> (keyword "using" *> columnNameList))
>                 ,return Nothing]
>         nkwid = try $ do
>                  x <- idString
>                  --avoid all these keywords as aliases since they can
>                  --appear immediately following a tableref as the next
>                  --part of the statement, if we don't do this then lots
>                  --of things don't parse. Seems a bit inelegant but
>                  --works for the tests and the test sql files don't know
>                  --if these should be allowed as aliases without "" or
>                  --[]
>                  if map toLower x `elem` ["as"
>                              ,"where"
>                              ,"except"
>                              ,"union"
>                              ,"intersect"
>                              ,"loop"
>                              ,"inner"
>                              ,"on"
>                              ,"left"
>                              ,"right"
>                              ,"full"
>                              ,"cross"
>                              ,"natural"
>                              ,"order"
>                              ,"group"
>                              ,"limit"
>                              ,"using"
>                              ,"from"]
>                    then fail "not keyword"
>                    else return x
>     values = keyword "values" >>
>              Values <$> commaSep1 (parens $ commaSep1 expr)


= insert, update and delete

insert statement: supports option column name list,
multiple rows to insert and insert from select statements

> insert :: ParsecT [Token] ParseState Identity Statement
> insert = keyword "insert" >> keyword "into" >>
>          Insert <$> idString
>                 <*> option [] (try columnNameList)
>                 <*> selectExpression
>                 <*> tryOptionMaybe returning

> update :: ParsecT [Token] ParseState Identity Statement
> update = keyword "update" >>
>          Update
>          <$> idString
>          <*> (keyword "set" *> commaSep1 setClause)
>          <*> tryOptionMaybe whereClause
>          <*> tryOptionMaybe returning
>     where
>       setClause = choice
>             [RowSetClause <$> parens (commaSep1 idString)
>                           <*> (symbol "=" *> parens (commaSep1 expr))
>             ,SetClause <$> idString
>                        <*> (symbol "=" *> expr)]

> delete :: ParsecT [Token] ParseState Identity Statement
> delete = keyword "delete" >> keyword "from" >>
>          Delete
>          <$> idString
>          <*> tryOptionMaybe whereClause
>          <*> tryOptionMaybe returning

> truncateSt :: ParsecT [Token] ParseState Identity Statement
> truncateSt = keyword "truncate" >> optional (keyword "table") >>
>            Truncate
>            <$> commaSep1 idString
>            <*> option ContinueIdentity (choice [
>                                 ContinueIdentity <$ (keyword "continue"
>                                                      <* keyword "identity")
>                                ,RestartIdentity <$ (keyword "restart"
>                                                     <* keyword "identity")])
>            <*> cascade

> copy :: ParsecT [Token] ParseState Identity Statement
> copy = do
>        keyword "copy"
>        tableName <- idString
>        cols <- option [] (parens $ commaSep1 idString)
>        keyword "from"
>        src <- choice [
>                CopyFilename <$> extrStr <$> stringLit
>               ,Stdin <$ keyword "stdin"]
>        return $ Copy tableName cols src

> copyData :: ParsecT [Token] ParseState Identity Statement
> copyData = CopyData <$> mytoken (\tok ->
>                                         case tok of
>                                                  CopyPayloadTok n -> Just n
>                                                  _ -> Nothing)

= ddl

> createTable :: ParsecT [Token] ParseState Identity Statement
> createTable = do
>   keyword "table"
>   tname <- idString
>   choice [
>      CreateTableAs tname <$> (keyword "as" *> selectExpression)
>     ,uncurry (CreateTable tname) <$> readAttsAndCons]
>   where
>     --parse our unordered list of attribute defs or constraints for
>     --each line, want to try the constraint parser first, then the
>     --attribute parser, so we need the swap to feed them in the
>     --right order into createtable
>     readAttsAndCons = parens (swap <$> multiPerm
>                                          (try tableConstr)
>                                          tableAtt
>                                          (symbol ","))
>                       where swap (a,b) = (b,a)
>     tableAtt = AttributeDef
>                <$> idString
>                <*> typeName
>                <*> tryOptionMaybe (keyword "default" *> expr)
>                <*> many rowConstraint
>     tableConstr = choice [
>                    UniqueConstraint
>                    <$> try (keyword "unique" *> columnNameList)
>                    ,PrimaryKeyConstraint
>                    <$> try (keyword "primary" *> keyword "key"
>                             *> choice [
>                                     (:[]) <$> idString
>                                    ,parens (commaSep1 idString)])
>                    ,CheckConstraint
>                    <$> try (keyword "check" *> parens expr)
>                    ,ReferenceConstraint
>                    <$> try (keyword "foreign" *> keyword "key"
>                             *> parens (commaSep1 idString))
>                    <*> (keyword "references" *> idString)
>                    <*> option [] (parens $ commaSep1 idString)
>                    <*> onDelete
>                    <*> onUpdate]
>     rowConstraint =
>        choice [
>           RowUniqueConstraint <$ keyword "unique"
>          ,RowPrimaryKeyConstraint <$ keyword "primary" <* keyword "key"
>          ,RowCheckConstraint <$> (keyword "check" *> parens expr)
>          ,NullConstraint <$ keyword "null"
>          ,NotNullConstraint <$ (keyword "not" *> keyword "null")
>          ,RowReferenceConstraint
>          <$> (keyword "references" *> idString)
>          <*> option Nothing (try $ parens $ Just <$> idString)
>          <*> onDelete
>          <*> onUpdate
>          ]
>     onDelete = onSomething "delete"
>     onUpdate = onSomething "update"
>     onSomething k = option Restrict $ try $ keyword "on"
>                    *> keyword k *> cascade


> createType :: ParsecT [Token] ParseState Identity Statement
> createType = keyword "type" >>
>              CreateType
>              <$> idString
>              <*> (keyword "as" *> parens (commaSep1 typeAtt))
>   where
>     typeAtt = TypeAttDef <$> idString <*> typeName


create function, support sql functions and
plpgsql functions. Actually parses the body in both cases
and provides a statement list for the body rather than just
a string

> createFunction :: ParsecT [Token] ParseState Identity Statement
> createFunction = do
>   keyword "function"
>   fnName <- idString
>   params <- parens $ commaSep param
>   retType <- keyword "returns" *> typeName
>   keyword "as"
>   bodypos <- getAdjustedPosition
>   body <- stringLit
>   lang <- readLang
>   let (q, b) = parseBody lang body fnName bodypos
>   CreateFunction lang fnName params retType q b <$> pVol
>     where
>         pVol = matchAKeyword [("volatile", Volatile)
>                              ,("stable", Stable)
>                              ,("immutable", Immutable)]
>         readLang = keyword "language" *> matchAKeyword [("plpgsql", Plpgsql)
>                                                        ,("sql",Sql)]
>         parseBody lang body fnName bodypos =
>             case (parseIt
>                   (lexSqlText (extrStr body))
>                   (functionBody lang)
>                   ("function " ++ fnName)
>                   (extrStr body)
>                   [bodypos]) of
>                      Left er@(ExtendedError e _) ->
>                               -- don't know how to change the
>                               --position of the error we've been
>                               --given, so add some information to
>                               --show the position of the containing
>                               --function and the adjusted absolute
>                               --position of this error
>                               let ep = toMySp $ errorPos e
>                                   (fn,fp,fc) = bodypos
>                                   fnBit = "in function " ++ fnName ++ "\n"
>                                           ++ fn ++ ":" ++ show fp ++ ":" ++ show fc ++ ":\n"
>                                   (_,lp,lc) = adjustPosition bodypos ep
>                                   lineBit = "on line\n"
>                                             ++ fn ++ ":" ++ show lp ++ ":" ++ show lc ++ ":\n"
>                               in error $ show er ++ "\n" ++ fnBit ++ lineBit
>                      Right body' -> (quoteOfString body, body')

sql function is just a list of statements, the last one has the
trailing semicolon optional

>         functionBody Sql = do
>            a <- many (try $ sqlStatement True)
>            -- this makes my head hurt, should probably write out
>            -- more longhand
>            SqlFnBody <$> option a ((\b -> (a++[b])) <$> sqlStatement False)

plpgsql function has an optional declare section, plus the statements
are enclosed in begin ... end; (semi colon after end is optional

>         functionBody Plpgsql =
>             PlpgsqlFnBody
>             <$> option [] declarePart
>             <*> statementPart
>             where
>               statementPart = keyword "begin"
>                     *> many plPgsqlStatement
>                     <* keyword "end" <* optional (symbol ";") <* eof
>               declarePart = keyword "declare"
>                   *> manyTill (try varDef) (lookAhead $ keyword "begin")

params to a function

> param :: ParsecT [Token] ParseState Identity ParamDef
> param = choice [
>          try (ParamDef <$> idString <*> typeName)
>         ,ParamDefTp <$> typeName]

variable declarations in a plpgsql function

> varDef :: ParsecT [Token] ParseState Identity VarDef
> varDef = VarDef
>          <$> idString
>          <*> typeName
>          <*> tryOptionMaybe ((symbol ":=" <|> symbol "=")*> expr) <* symbol ";"


> createView :: ParsecT [Token] ParseState Identity Statement
> createView = keyword "view" >>
>              CreateView
>              <$> idString
>              <*> (keyword "as" *> selectExpression)

> createDomain :: ParsecT [Token] ParseState Identity Statement
> createDomain = keyword "domain" >>
>                CreateDomain
>                <$> idString
>                <*> (tryOptionMaybe (keyword "as") *> typeName)
>                <*> tryOptionMaybe (keyword "check" *> parens expr)

> dropSomething :: ParsecT [Token] ParseState Identity Statement
> dropSomething = do
>   x <- try (choice [
>                  Domain <$ keyword "domain"
>                 ,Type <$ keyword "type"
>                 ,Table <$ keyword "table"
>                 ,View <$ keyword "view"
>             ])
>   (i,e,r) <- parseDrop idString
>   return $ DropSomething x i e r

> dropFunction :: ParsecT [Token] ParseState Identity Statement
> dropFunction = do
>                keyword "function"
>                (i,e,r) <- parseDrop pFun
>                return $ DropFunction i e r
>                where
>                  pFun = (,) <$> idString
>                             <*> parens (many idString)

> parseDrop :: ParsecT [Token] ParseState Identity a
>           -> ParsecT [Token] ParseState Identity (IfExists, [a], Cascade)
> parseDrop p = (,,)
>               <$> ifExists
>               <*> commaSep1 p
>               <*> cascade
>     where
>       ifExists = option Require
>                  (try $ IfExists <$ (keyword "if"
>                                      *> keyword "exists"))

================================================================================

= component parsers for sql statements

> whereClause :: ParsecT [Token] ParseState Identity Expression
> whereClause = keyword "where" *> expr

selectlist and selectitem: the bit between select and from
check for into either before the whole list of select columns
or after the whole list

> selectList :: ParsecT [Token] ParseState Identity SelectList
> selectList =
>     choice [
>         flip SelectList <$> readInto <*> itemList
>        ,SelectList <$> itemList <*> option [] readInto]
>   where
>     readInto = keyword "into" *> commaSep1 idString
>     itemList = commaSep1 selectItem
>     selectItem = optionalSuffix
>                    SelExp expr
>                    SelectItem () (keyword "as" *> idString)

> returning :: ParsecT [Token] ParseState Identity SelectList
> returning = keyword "returning" *> selectList

> columnNameList :: ParsecT [Token] ParseState Identity [String]
> columnNameList = parens $ commaSep1 idString

> typeName :: ParsecT [Token] ParseState Identity TypeName
> typeName = choice [
>             SetOfTypeName <$> (keyword "setof" *> typeName)
>            ,do
>              s <- (map toLower) <$> idString
>              choice [
>                PrecTypeName s <$> parens integer
>               ,ArrayTypeName (SimpleTypeName s) <$ symbol "[" <* symbol "]"
>               ,return $ SimpleTypeName s]]

> cascade :: ParsecT [Token] ParseState Identity Cascade
> cascade = option Restrict (choice [
>                             Restrict <$ keyword "restrict"
>                            ,Cascade <$ keyword "cascade"])

================================================================================

= plpgsql statements

> plPgsqlStatement :: ParsecT [Token] ParseState Identity (MySourcePos,Statement)
> plPgsqlStatement = do
>    p <- getAdjustedPosition
>    ((sqlStatement True)
>     <|> (,) p <$> (choice [
>                          continue
>                         ,execute
>                         ,caseStatement
>                         ,assignment
>                         ,ifStatement
>                         ,returnSt
>                         ,raise
>                         ,forStatement
>                         ,whileStatement
>                         ,perform
>                         ,nullStatement]
>                         <* symbol ";"))

> nullStatement :: ParsecT [Token] ParseState Identity Statement
> nullStatement = NullStatement <$ keyword "null"

> continue :: ParsecT [Token] ParseState Identity Statement
> continue = ContinueStatement <$ keyword "continue"

> perform :: ParsecT [Token] ParseState Identity Statement
> perform = keyword "perform" >>
>           Perform <$> expr

> execute :: ParsecT [Token] ParseState Identity Statement
> execute = keyword "execute" >>
>           optionalSuffix
>             Execute expr
>             ExecuteInto () readInto
>     where
>       readInto = keyword "into" *> commaSep1 idString

> assignment :: ParsecT [Token] ParseState Identity Statement
> assignment = Assignment
>              -- put the := in the first try to attempt to get a
>              -- better error if the code looks like malformed
>              -- assignment statement
>              <$> try (idString <* (symbol ":=" <|> symbol "="))
>              <*> expr

> returnSt :: ParsecT [Token] ParseState Identity Statement
> returnSt = keyword "return" >>
>            choice [
>             ReturnNext <$> (keyword "next" *> expr)
>            ,ReturnQuery <$> (keyword "query" *> selectExpression)
>            ,Return <$> tryOptionMaybe expr]

> raise :: ParsecT [Token] ParseState Identity Statement
> raise = keyword "raise" >>
>         Raise
>         <$> raiseType
>         <*> (extrStr <$> stringLit)
>         <*> option [] (symbol "," *> commaSep1 expr)
>         where
>           raiseType = matchAKeyword [("notice", RNotice)
>                                      ,("exception", RException)
>                                      ,("error", RError)]

> forStatement :: ParsecT [Token] ParseState Identity Statement
> forStatement = do
>                keyword "for"
>                start <- idString
>                keyword "in"
>                choice [(ForSelectStatement start <$> try selectExpression <*> theRest)
>                       ,(ForIntegerStatement start
>                               <$> expr
>                               <*> (symbol ".." *> expr)
>                               <*> theRest)]
>   where
>     theRest = keyword "loop" *> many plPgsqlStatement
>               <* keyword "end" <* keyword "loop"

> whileStatement :: ParsecT [Token] ParseState Identity Statement
> whileStatement = keyword "while" >>
>                  WhileStatement
>                  <$> (expr <* keyword "loop")
>                  <*> many plPgsqlStatement <* keyword "end" <* keyword "loop"

> ifStatement :: ParsecT [Token] ParseState Identity Statement
> ifStatement = keyword "if" >>
>               If
>               <$> (ifPart <:> elseifParts)
>               <*> (elsePart <* endIf)
>   where
>     ifPart = expr <.> (thn *> many plPgsqlStatement)
>     elseifParts = many ((elseif *> expr) <.> (thn *> many plPgsqlStatement))
>     elsePart = option [] (keyword "else" *> many plPgsqlStatement)
>     endIf = keyword "end" <* keyword "if"
>     thn = keyword "then"
>     elseif = keyword "elseif"
>     --might as well these in as well after all that
>     -- can't do <,> unfortunately, so use <.> instead
>     (<.>) a b = (,) <$> a <*> b

> caseStatement :: ParsecT [Token] ParseState Identity Statement
> caseStatement = keyword "case" >>
>     CaseStatement <$> expr
>                   <*> many whenSt
>                   <*> option [] (keyword "else" *> many plPgsqlStatement)
>                           <* keyword "end" <* keyword "case"
>     where
>       whenSt = keyword "when" >>
>                (,) <$> commaSep1 expr
>                    <*> (keyword "then" *> many plPgsqlStatement)

================================================================================

= expressions

This is the bit that makes it the most obvious that I don't really
know haskell, parsing theory or parsec ... robbed a parsing example
from haskell-cafe and mainly just kept changing it until it seemed to
work

> expr :: ParsecT [Token] ParseState Identity Expression
> expr = buildExpressionParser table factor
>        <?> "expression"

> factor :: ParsecT [Token] ParseState Identity Expression
> factor =

First job is to take care of forms which start as a regular
expression, and then add a suffix on

>          threadOptionalSuffixes fct [castSuffix
>                                     ,betweenSuffix
>                                     ,arraySubSuffix]
>          where
>            fct = choice [

order these so the ones which can be valid prefixes of others
appear further down the list

probably want to refactor this to use the optionalsuffix parsers
to improve speed

One little speed optimisation, to help with pretty printed code which
can contain a lot of parens - check for nested ((
This little addition speeds up ./ParseFile.lhs sqltestfiles/system.sql on my system
from ~4 minutes to ~4 seconds

>                try (lookAhead (symbol "(" >> symbol "(")) >> parens expr

start with the factors which start with parens - eliminate scalar
subquerys since they're easy to distinguish from the others then do in
predicate before row constructor, since an in predicate can start with
a row constructor, then finally vanilla parens

>               ,scalarSubQuery
>               ,try $ threadOptionalSuffix rowCtor inPredicateSuffix
>               ,parens expr

we have two things which can start with a $,
do the position arg first, then we can unconditionally
try the dollar quoted string next

>               ,positionalArg

string using quotes don't start like anything else and we've
already tried the other thing which starts with a $, so can
parse without a try

>               ,stringLit

>               ,floatLit
>               ,integerLit

put the factors which start with keywords before the ones which start
with a function

>               ,caseParse
>               ,exists
>               ,booleanLit
>               ,nullLit
>               ,arrayLit
>               ,castKeyword
>               ,substring

now do identifiers, functions, and window functions (each is a prefix
to the next one)

>               ,threadOptionalSuffixes
>                 identifier
>                 [inPredicateSuffix
>                 ,\l -> threadOptionalSuffix (functionCallSuffix l) windowFnSuffix]]

== operator table

proper hacky, but sort of does the job
the 'missing' notes refer to pg operators which aren't yet supported
pg's operator table is on this page:
http://www.postgresql.org/docs/8.4/interactive/sql-syntax-lexical.html#SQL-SYNTAX-OPERATORS

will probably need something more custom to handle full range of sql
syntactical novelty

> table :: [[Operator [Token] ParseState Identity Expression]]
> table = [--[binary "::" (BinOpCall Cast) AssocLeft]
>          --missing [] for array element select
>          [prefix "-" (FunCall (Operator "u-"))]
>         ,[binary "^" AssocLeft]
>         ,[binary "*" AssocLeft
>          ,idHackBinary "*" AssocLeft
>          ,binary "/" AssocLeft
>          ,binary "%" AssocLeft]
>         ,[binary "+" AssocLeft
>          ,binary "-" AssocLeft]
>          --should be is isnull and notnull
>         ,[postfixks ["is", "not", "null"] (FunCall (KOperator IsNotNull))
>          ,postfixks ["is", "null"] (FunCall (KOperator IsNull))]
>          --other operators all added in this list according to the pg docs:
>         ,[binary "<->" AssocNone
>          ,binary "<=" AssocRight
>          ,binary ">=" AssocRight
>          ,binary "||" AssocLeft
>          ,prefix "@" (FunCall (Operator "@"))
>          ]
>          --in should be here, but is treated as a factor instead
>          --between
>          --overlaps
>         ,[binaryk "like" Like AssocNone
>          ,binarycust "!=" "<>" AssocNone]
>          --(also ilike similar)
>         ,[lt "<" AssocNone
>          ,binary ">" AssocNone]
>          ,[binary "=" AssocRight
>          ,binary "<>" AssocNone]
>         ,[prefixk "not" (FunCall (KOperator Not))]
>         ,[binaryk "and" And AssocLeft
>          ,binaryk "or" Or AssocLeft]]
>     where
>       --use different parsers for symbols and keywords to get the
>       --right whitespace behaviour
>       binary s
>          = Infix (try (symbol s >> (return $ binaryF s)))
>       binarycust s t
>          = Infix (try (symbol s >> (return $ binaryF t)))
>       -- * ends up being lexed as an id token rather than a symbol
>       -- * token, so work around here
>       idHackBinary s
>           = Infix (try (keyword s >> (return $ binaryF s)))
>       prefix s f
>          = Prefix (try (symbol s >> (return $ unaryF f)))
>       binaryk s o
>          = Infix (try (keyword s >> (return $ binaryFk o)))
>       prefixk s f
>          = Prefix (try (keyword s >> (return $ unaryF f)))
>       --postfixk s f
>       --   = Postfix (try (keyword s >> return f))
>       postfixks ss f
>          = Postfix (try (mapM_ keyword ss >> (return $ unaryF f)))
>       unaryF f = \l -> f [l]
>       binaryF s = \l -> \m -> (FunCall (Operator s)) [l,m]
>       binaryFk s = \l -> \m -> (FunCall (KOperator s)) [l,m]

some custom parsers

fix problem parsing <> - don't parse as "<" if it is immediately
followed by ">"

>       lt _ = Infix (dontFollowWith "<" ">" >>
>                     (return $ (\l -> (\m -> (FunCall (Operator "<")) [l,m]))))

>       dontFollowWith c1 c2 =
>         try $ symbol c1 *> ((do
>                                lookAhead $ symbol c2
>                                fail "dont follow")
>                             <|> return ())

the first argument to these twp above is ignored, it is there so the
symbol can appear in the operator table above for readability purposes

== factor parsers

> scalarSubQuery :: ParsecT [Token] ParseState Identity Expression
> scalarSubQuery = try (symbol "(" *> lookAhead (keyword "select")) >>
>                  ScalarSubQuery
>                  <$> selectExpression <* symbol ")"

in predicate - an identifier or row constructor followed by 'in'
then a list of expressions or a subselect

> inPredicateSuffix :: Expression -> ParsecT [Token] ParseState Identity Expression
> inPredicateSuffix e =
>   InPredicate e
>   <$> option True (False <$ keyword "not")
>   <*> (keyword "in" *> parens ((InSelect <$> selectExpression)
>                                <|>
>                                (InList <$> commaSep1 expr)))

row ctor: one of
row ()
row (expr)
row (expr, expr1, ...)
(expr, expr2,...) [implicit (no row keyword) version, at least two elements
                   must be present]
notes:
(expr) parses to just expr rather than row(expr)
and () is a syntax error.

> rowCtor :: ParsecT [Token] ParseState Identity Expression
> rowCtor = FunCall RowCtor <$> choice [
>            keyword "row" *> parens (commaSep expr)
>           ,parens $ commaSep2 expr]

> floatLit :: ParsecT [Token] ParseState Identity Expression
> floatLit = FloatLit <$> float

> integerLit :: ParsecT [Token] ParseState Identity Expression
> integerLit = IntegerLit <$> integer

case - only supports 'case when condition' flavour and not 'case
expression when value' currently

> caseParse :: ParsecT [Token] ParseState Identity Expression
> caseParse = keyword "case" >>
>             Case <$> many whenParse
>                  <*> tryOptionMaybe (keyword "else" *> expr)
>                       <* keyword "end"
>   where
>     whenParse = (,) <$> (keyword "when" *> commaSep1 expr)
>                     <*> (keyword "then" *> expr)

> exists :: ParsecT [Token] ParseState Identity Expression
> exists = keyword "exists" >>
>          Exists <$> parens selectExpression

> booleanLit :: ParsecT [Token] ParseState Identity Expression
> booleanLit = BooleanLit <$> (True <$ keyword "true"
>                                <|> False <$ keyword "false")

> nullLit :: ParsecT [Token] ParseState Identity Expression
> nullLit = NullLit <$ keyword "null"

> arrayLit :: ParsecT [Token] ParseState Identity Expression
> arrayLit = keyword "array" >>
>            FunCall ArrayCtor <$> squares (commaSep expr)

when you put expr instead of identifier in arraysub, it stack
overflows, not sure why.

> arraySubSuffix :: Expression -> ParsecT [Token] ParseState Identity Expression
> arraySubSuffix e = if e == Identifier "array"
>                      then fail "can't use array as identifier name"
>                      else FunCall ArraySub <$> ((e:) <$> squares (commaSep1 expr))

supports basic window functions of the form
fn() over ([partition bit]? [order bit]?)

> windowFnSuffix :: Expression -> ParsecT [Token] ParseState Identity Expression
> windowFnSuffix e = WindowFn e
>                    <$> (keyword "over" *> (symbol "(" *> option [] partitionBy))
>                    <*> option [] orderBy1
>                    <*> option Asc (try $ choice [
>                                             Asc <$ keyword "asc"
>                                            ,Desc <$ keyword "desc"])
>                            <* symbol ")"
>   where
>     orderBy1 = keyword "order" *> keyword "by" *> commaSep1 expr
>     partitionBy = keyword "partition" *> keyword "by" *> commaSep1 expr

> betweenSuffix :: Expression -> ParsecT [Token] ParseState Identity Expression
> betweenSuffix a = do
>   keyword "between"
>   b <- dodgyParseElement
>   keyword "and"
>   c <- dodgyParseElement
>   return $ FunCall Between [a,b,c]
>              --can't use the full expression parser at this time
>              --because of a conflict between the operator 'and' and
>              --the 'and' part of a between

From postgresql src/backend/parser/gram.y

 * We have two expression types: a_expr is the unrestricted kind, and
 * b_expr is a subset that must be used in some places to avoid shift/reduce
 * conflicts.  For example, we can't do BETWEEN as "BETWEEN a_expr AND a_expr"
 * because that use of AND conflicts with AND as a boolean operator.  So,
 * b_expr is used in BETWEEN and we remove boolean keywords from b_expr.
 *
 * Note that '(' a_expr ')' is a b_expr, so an unrestricted expression can
 * always be used by surrounding it with parens.

Thanks to Sam Mason for the heads up on this.

>              where
>                dodgyParseElement =
>                    choice [
>                       threadOptionalSuffix identifier functionCallSuffix
>                      ,parens dodgyParseElement
>                      ,stringLit
>                      ,integerLit]

> functionCallSuffix :: Expression -> ParsecT [Token] ParseState Identity Expression
> functionCallSuffix (Identifier fnName) = FunCall (SimpleFun fnName) <$> parens (commaSep expr)
> functionCallSuffix s = error $ "cannot make functioncall from " ++ show s

> castKeyword :: ParsecT [Token] ParseState Identity Expression
> castKeyword = keyword "cast" *> symbol "(" >>
>               Cast <$> expr
>                    <*> (keyword "as" *> typeName <* symbol ")")

> castSuffix :: Expression -> ParsecT [Token] ParseState Identity Expression
> castSuffix ex = Cast ex <$> (symbol "::" *> typeName)


> substring :: ParsecT [Token] ParseState Identity Expression
> substring = do
>             keyword "substring"
>             symbol "("
>             a <- expr
>             keyword "from"
>             b <- expr
>             keyword "for"
>             c <- expr
>             symbol ")"
>             return $ FunCall Substring [a,b,c]

> identifier :: ParsecT [Token] ParseState Identity Expression
> identifier = Identifier <$> idString

================================================================================

= Utility parsers

== tokeny things

keyword has to not be immediately followed by letters or numbers
(symbols and whitespace are ok) so we know that we aren't reading an
identifier which happens to start with a complete keyword

> keyword :: String -> ParsecT [Token] ParseState Identity String
> keyword k = mytoken (\tok ->
>                                case tok of
>                                IdStringTok i | lcase k == lcase i -> Just k
>                                _ -> Nothing)
>                       where
>                         lcase = map toLower

> idString :: MyParser String
> idString = mytoken (\tok -> case tok of
>                                      IdStringTok i -> Just i
>                                      _ -> Nothing)

> symbol :: String -> ParsecT [Token] ParseState Identity String
> symbol c = mytoken (\tok -> case tok of
>                                    SymbolTok s | c==s -> Just c
>                                    _           -> Nothing)

> integer :: MyParser Integer
> integer = mytoken (\tok -> case tok of
>                                     IntegerTok n -> Just n
>                                     _ -> Nothing)

> positionalArg :: ParsecT [Token] ParseState Identity Expression
> positionalArg = PositionalArg <$> mytoken (\tok -> case tok of
>                                     PositionalArgTok n -> Just n
>                                     _ -> Nothing)

> float :: MyParser Double
> float = mytoken (\tok -> case tok of
>                                     FloatTok n -> Just n
>                                     _ -> Nothing)

> stringLit :: MyParser Expression
> stringLit = mytoken (\tok ->
>                   case tok of
>                            StringTok d s -> Just $ StringLit d s
>                            _ -> Nothing)

couple of helper functions which extract the actual string
from a StringLD or StringL, and the delimiters which were used
(either ' or a dollar tag)

> extrStr :: Expression -> String
> extrStr (StringLit _ s) = s
> extrStr x = error $ "extrStr not supported for this type " ++ show x

> quoteOfString :: Expression -> String
> quoteOfString (StringLit tag _) = tag
> quoteOfString x = error $ "quoteType not supported for this type " ++ show x

== combinatory things

> parens :: ParsecT [Token] ParseState Identity a
>        -> ParsecT [Token] ParseState Identity a
> parens = between (symbol "(") (symbol ")")

> squares :: ParsecT [Token] ParseState Identity a
>        -> ParsecT [Token] ParseState Identity a
> squares = between (symbol "[") (symbol "]")

> tryOptionMaybe :: (Stream s m t) =>
>              ParsecT s u m a -> ParsecT s u m (Maybe a)
> tryOptionMaybe p = try (optionMaybe p) <|> return Nothing

> commaSep2 :: ParsecT [Token] ParseState Identity a
>           -> ParsecT [Token] ParseState Identity [a]
> commaSep2 p = sepBy2 p (symbol ",")

> sepBy2 :: (Stream s m t) =>
>           ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
> sepBy2 p sep = (p <* sep) <:> sepBy1 p sep

> commaSep :: ParsecT [Token] ParseState Identity a
>          -> ParsecT [Token] ParseState Identity [a]
> commaSep p = sepBy p (symbol ",")

> commaSep1 :: ParsecT [Token] ParseState Identity a
>           -> ParsecT [Token] ParseState Identity [a]
> commaSep1 p = sepBy1 p (symbol ",")

doesn't seem too gratuitous, comes up a few times

> (<:>) :: (Applicative f) =>
>          f a -> f [a] -> f [a]
> (<:>) a b = (:) <$> a <*> b

pass a list of pairs of strings and values
try each pair k,v in turn,
if keyword k matches then return v
doesn't really add a lot of value

> matchAKeyword :: [(String, a)] -> ParsecT [Token] ParseState Identity a
> matchAKeyword [] = fail "no matches"
> matchAKeyword ((k,v):kvs) = v <$ keyword k <|> matchAKeyword kvs

parseOptionalSuffix

parse the start of something -> parseResultA,
then parse an optional suffix -> parseResultB
if this second parser succeeds, return fn2 parseResultA parseResultB
else return fn1 parseResultA

e.g.
parsing an identifier in a select list can be
fieldName
or
fieldName as alias
so we can pass
* IdentifierCtor
* identifier (returns aval)
* AliasedIdentifierCtor
* () - looks like a place holder, probably a crap idea
* parser for (as b) (returns bval)
as the args, which I like to ident like:
parseOptionalSuffix
  IdentifierCtor identifierParser
  AliasedIdentifierCtor () asAliasParser
and we get either
* IdentifierCtor identifierValue
or
* AliasedIdentifierCtor identifierValue aliasValue
as the result depending on whether the asAliasParser
succeeds or not.

probably this concept already exists under a better name in parsing
theory

> optionalSuffix :: (Stream s m t2) =>
>                   (t1 -> b)
>                -> ParsecT s u m t1
>                -> (t1 -> a -> b)
>                -> ()
>                -> ParsecT s u m a
>                -> ParsecT s u m b
> optionalSuffix c1 p1 c2 _ p2 = do
>   x <- p1
>   option (c1 x) (c2 x <$> try p2)

threadOptionalSuffix

parse the start of something -> parseResultA,
then parse an optional suffix, passing parseResultA
  to this parser -> parseResultB
return parseResultB is it succeeds, else return parseResultA

sort of like a suffix operator parser where the suffixisable part
is parsed, then if the suffix is there it wraps the suffixisable
part in an enclosing tree node.

parser1 -> tree1
(parser2 tree1) -> maybe tree2
tree2 isnothing ? tree1 : tree2

> threadOptionalSuffix :: ParsecT [tok] st Identity a
>                      -> (a -> GenParser tok st a)
>                      -> ParsecT [tok] st Identity a
> threadOptionalSuffix p1 p2 = do
>   x <- p1
>   option x (try $ p2 x)

I'm pretty sure this is some standard monad operation but I don't know
what. It's a bit like the maybe monad but when you get nothing it
returns the previous result instead of nothing
- if you take the parsing specific stuff out you get:

p1 :: (Monad m) =>
      m b -> (b -> m (Maybe b)) -> m b
p1 = do
   x <- p1
   y <- p2 x
   case y of
     Nothing -> return x
     Just z -> return z
=====

couldn't work how to to perms so just did this hack instead
e.g.
a1,a2,b1,b2,a2,b3,b4 parses to ([a1,a2,a3],[b1,b2,b3,b4])

> threadOptionalSuffixes :: ParsecT [tok] st Identity a
>                        -> [a -> GenParser tok st a]
>                        -> ParsecT [tok] st Identity a
> threadOptionalSuffixes p1 p2s = do
>   x <- p1
>   option x (try $ choice (map (\l -> l x) p2s))


> multiPerm :: (Stream s m t) =>
>                ParsecT s u m a1
>             -> ParsecT s u m a
>             -> ParsecT s u m sep
>             -> ParsecT s u m ([a1], [a])

> multiPerm p1 p2 sep = do
>   (r1, r2) <- unzip <$> sepBy1 parseAorB sep
>   return (catMaybes r1, catMaybes r2)
>   where
>     parseAorB = choice [
>                   (\x -> (Just x,Nothing)) <$> p1
>                  ,(\y -> (Nothing, Just y)) <$> p2]

== lexer stuff

> type MyParser = GenParser Token ParseState

> mytoken :: (Tok -> Maybe a) -> MyParser a
> mytoken test
>   = token showToken posToken testToken
>   where
>   showToken (_,tok)   = show tok
>   posToken  (pos,_)   = pos
>   testToken (_,tok)   = test tok
