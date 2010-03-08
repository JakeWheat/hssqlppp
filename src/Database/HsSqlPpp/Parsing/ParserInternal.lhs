Copyright 2009 Jake Wheat

The main file for parsing sql, uses parsec. Not sure if parsec is the
right choice, but it seems to do the job pretty well at the moment.

> {-# LANGUAGE RankNTypes,FlexibleContexts #-}
>
> -- | Functions to parse SQL.
> module Database.HsSqlPpp.Parsing.ParserInternal
>     (-- * Main
>      parseSql
>     ,parseSqlWithPosition
>     ,parseSqlFile
>      -- * Testing
>     ,parseExpression
>     ,parsePlpgsql
>      -- * errors
>     ,ParseErrorExtra(..)
>      -- * quasiquotation support
>     ,parseAntiSql
>     ,parseAntiPlpgsql
>     ,parseAntiExpression
>      -- other helpers for internal use
>     ,tableAttribute
>     ,keyword
>     ,parens
>     ,symbol
>     ,idString
>     ,commaSep1
>     ,commaSep
>     ) where
>
> import Text.Parsec hiding(many, optional, (<|>), string, label)
> import Text.Parsec.Expr
> import Text.Parsec.String
> import Text.Parsec.Perm
>
> import Control.Applicative
> import Control.Monad.Identity
>
> import Data.Maybe
> import Data.Char
>
> import Data.Generics.PlateData
> import Data.Generics hiding (Prefix,Infix)
>
> import Database.HsSqlPpp.Parsing.Lexer
> import Database.HsSqlPpp.Parsing.ParseErrors
> import Database.HsSqlPpp.AstInternals.AstAnti
> --import Database.HsSqlPpp.Ast
> import qualified Database.HsSqlPpp.Ast as A
> import Database.HsSqlPpp.Annotation as A
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Catalog

--------------------------------------------------------------------------------

Top level parsing functions
===========================

To support antiquotation, the following approach is used:

* makeantinodes processes the generated astinternal.hs, and extracts
  the ast node types.
* it modifies these to add anti ctors to the appropriate types,
  creates a set of trivial conversion functions to convert between anti
  nodes and regular nodes, and this is written to astanti.hs.
* astanti then contains exactly the same set of ast nodes as
  astinternal, but with a few additions to support antiquotes.
* this parsing code parses to the antinodes, then converts to regular
  nodes for the public api, and returns antinodes for the sql
  quasiquoter to use.
* todo: add a flag so that if we are not parsing for the quasiquoter,
  any splice syntax is rejected as a parse error.

> parseSql :: String -- ^ filename to use in errors
>          -> String -- ^ a string containing the sql to parse
>          -> Either ParseErrorExtra A.StatementList
> parseSql f s =
>   deAS $ parseIt l sqlStatements f Nothing s startState
>   where l = lexSqlText f s
>
> parseSqlWithPosition :: FilePath -- ^ filename to use in errors
>                      -> Int -- ^ adjust line number in errors by adding this
>                      -> Int -- ^ adjust column in errors by adding this
>                      -> String -- ^ a string containing the sql to parse
>                      -> Either ParseErrorExtra A.StatementList
> parseSqlWithPosition f l c s = deAS $ parseAntiSql f l c s
>
> parseSqlFile :: FilePath -- ^ file name of file containing sql
>              -> IO (Either ParseErrorExtra A.StatementList)
> parseSqlFile fn = do
>   sc <- readFile fn
>   x <- lexSqlFile fn
>   return $ deAS $ parseIt x sqlStatements fn Nothing sc startState
>
> -- | Parse expression fragment, used for testing purposes
> parseExpression :: String -- ^ filename for error messages
>                 -> String -- ^ sql string containing a single expression,
>                           -- with no trailing ';'
>                 -> Either ParseErrorExtra A.Expression
> parseExpression f s =
>   deAE $ parseIt l (expr <* eof) f Nothing s startState
>   where l = lexSqlText f s
>
> -- | Parse plpgsql statements, used for testing purposes -
> -- this can be used to parse a list of plpgsql statements which
> -- aren't contained in a create function.
> -- (The produced ast won't pass a type check.)
> parsePlpgsql :: String
>              -> String
>              -> Either ParseErrorExtra A.StatementList
> parsePlpgsql f s =
>   deAS $ parseIt l p f Nothing s startState
>   where
>     l = lexSqlText f s
>     p = many plPgsqlStatement <* eof
>
> parseAntiSql :: FilePath
>              -> Int
>              -> Int
>              -> String
>              -> Either ParseErrorExtra StatementList
> parseAntiSql f l c s =
>   parseIt lx sqlStatements f ps s startState
>   where
>     lx = lexSqlTextWithPosition f l c s
>     ps = Just (l,c)
>
> parseAntiPlpgsql :: String
>                  -> Int
>                  -> Int
>                  -> String
>                  -> Either ParseErrorExtra [Statement]
> parseAntiPlpgsql f l c s =
>   parseIt lx p f ps s startState
>   where
>     lx = lexSqlText f s
>     p = many plPgsqlStatement <* eof
>     ps = Just (l,c)
>
> parseAntiExpression :: String
>                     -> Int
>                     -> Int
>                     -> String
>                     -> Either ParseErrorExtra Expression
> parseAntiExpression f l c s =
>   parseIt lx p f ps s startState
>   where
>     lx = lexSqlText f s
>     p = expr <* eof
>     ps = Just (l,c)
>
> --utility function to do error handling in one place
> parseIt :: forall t s u b.(Stream s Identity t, Data b) =>
>            Either ParseErrorExtra s
>         -> Parsec s u b
>         -> SourceName
>         -> Maybe (Int,Int)
>         -> String
>         -> u
>         -> Either ParseErrorExtra b
> parseIt lexed parser fn sp src ss =
>     case lexed of
>                Left er -> Left er
>                Right toks -> let r1 = runParser parser ss fn toks
>                              in case toParseErrorExtra r1 sp src of
>                                   Left er -> Left er
>                                   Right t -> Right $ fixupTree t
>
> deAE :: Either ParseErrorExtra Expression
>      -> Either ParseErrorExtra A.Expression
> deAE x = case x of
>                 Left e -> Left e
>                 Right ex -> Right $ convertExpression ex
> deAS :: Either ParseErrorExtra [Statement]
>      -> Either ParseErrorExtra [A.Statement]
> deAS x = case x of
>                 Left e -> Left e
>                 Right ex -> Right $ convertStatements ex

--------------------------------------------------------------------------------

> type SParser =  GenParser Token ParseState

Parsing top level statements
============================

> sqlStatements :: SParser [Statement]
> sqlStatements = many (sqlStatement True) <* eof
>
> sqlStatement :: Bool -> SParser Statement
> sqlStatement reqSemi =
>    (choice [
>      antiStatement
>     ,selectStatement
>     ,insert
>     ,update
>     ,delete
>     ,truncateSt
>     ,copy
>     ,set
>     ,notify
>     ,keyword "create" *>
>              choice [
>                 createTable
>                ,createSequence
>                ,createType
>                ,createFunction
>                ,createView
>                ,createDomain
>                ,createLanguage
>                ,createTrigger]
>     ,keyword "alter" *>
>              choice [
>                 alterSequence
>                ,alterTable]
>     ,keyword "drop" *>
>              choice [
>                 dropSomething
>                ,dropFunction]]
>     <* (if reqSemi
>           then symbol ";" >> return ()
>           else optional (symbol ";") >> return ()))
>    <|> copyData

--------------------------------------------------------------------------------

statement flavour parsers
=========================

top level/sql statements first

select
------

select parser, parses things starting with the keyword 'select'

supports plpgsql 'select into' only for the variants which look like
'select into ([targets]) [columnNames] from ...
or
'select [columnNames] into ([targets]) from ...
This should be changed so it can only parse an into clause when
expecting a plpgsql statement.

recurses to support parsing excepts, unions, etc.
this recursion needs refactoring cos it's a mess

> selectStatement :: SParser Statement
> selectStatement = SelectStatement <$> pos <*> selectExpression
>
> selectExpression :: SParser SelectExpression
> selectExpression =
>   with <|>
>   buildExpressionParser combTable selFactor
>   where
>         selFactor = try (parens selectExpression) <|> selQuerySpec <|> values
>         with = WithSelect <$> (pos <* keyword "with")
>                           <*> commaSep1 withQuery
>                           <*> selectExpression
>         withQuery = WithQuery <$> pos
>                               <*> (idString <* keyword "as")
>                               <*> parens selectExpression
>         combTable = [map (\(c,p) -> Infix (CombineSelect
>                                            <$> pos
>                                            <*> (c <$ p)) AssocLeft)
>                         [(Except, keyword "except")
>                         ,(Intersect, keyword "intersect")
>                         ,(UnionAll, try (keyword "union" *> keyword "all"))
>                         ,(Union, keyword "union")]]
>         selQuerySpec = Select <$> (pos <* keyword "select")
>                    <*> option Dupes (Distinct <$ keyword "distinct")
>                    <*> selectList
>                    <*> option [] from
>                    <*> optionMaybe whereClause
>                    <*> option [] groupBy
>                    <*> optionMaybe having
>                    <*> option [] orderBy
>                    <*> optionMaybe limit
>                    <*> optionMaybe offset
>         from = keyword "from" *> commaSep1 tableRef
>         groupBy = keyword "group" *> keyword "by"
>                   *> commaSep1 expr
>         having = keyword "having" *> expr
>         orderBy = keyword "order" *> keyword "by"
>                     *> commaSep1 oneOrder
>         oneOrder = (,) <$> expr
>                        <*> option Asc (choice [
>                                         Asc <$ keyword "asc"
>                                        ,Desc <$ keyword "desc"])
>         limit = keyword "limit" *> expr
>         offset = keyword "offset" *> expr
>         values = Values <$> (pos <* keyword "values")
>                         <*> commaSep1 (parens $ commaSep1 expr)

table refs
have to cope with:
a simple tableref i.e just a name
an aliased table ref e.g. select a.b from tbl as a
a sub select e.g. select a from (select b from c)
 - these are handled in nonJoinTref
then we combine by seeing if there is a join looking prefix

> tableRef :: SParser TableRef
> tableRef =
>   trefTerm >>= maybeParseAnotherJoin
>   where
>         maybeParseAnotherJoin tr1 =
>           choice [
>                 do
>                   p2 <- pos
>                   (nat,jt) <- joinKw
>                   JoinedTref p2 tr1 nat jt
>                                    <$> trefTerm
>                                    <*> onExpr
>                                    <*> palias
>                     >>= maybeParseAnotherJoin
>                ,return tr1]
>         trefTerm = nonJoinTref
>                    <|> try (parens tableRef)
>         nonJoinTref = try $ optParens $ do
>                   p2 <- pos
>                   choice [
>                          SubTref p2
>                          <$> parens selectExpression
>                          <*> palias
>                         ,TrefFun p2
>                          <$> try (identifier >>= functionCallSuffix)
>                          <*> palias
>                         ,Tref p2
>                          <$> nkwid
>                          <*> palias]
>         joinKw :: SParser (Natural, JoinType)
>         joinKw = do
>              --look for the join flavour first
>              n <- option Unnatural (Natural <$ keyword "natural")
>              jt <- choice [
>                     LeftOuter <$ try (keyword "left"
>                                       *> optional (keyword "outer"))
>                    ,RightOuter <$ try (keyword "right"
>                                        *> optional (keyword "outer"))
>                    ,FullOuter <$ try (keyword "full"
>                                       *> optional (keyword "outer"))
>                    ,Cross <$ keyword "cross"
>                    ,Inner <$ optional (keyword "inner")]
>              --recurse back to tref to read the table
>              keyword "join"
>              return (n,jt)
>         onExpr = choice [
>                  Just <$> (JoinOn <$> pos <*> (keyword "on" *> expr))
>                 ,Just <$> (JoinUsing <$> pos
>                            <*> (keyword "using" *> columnNameList))
>                 ,return Nothing]
>         palias = option NoAlias
>                    (optionalSuffix
>                       TableAlias (optional (keyword "as") *> nkwid)
>                       FullAlias () (parens $ commaSep1 idString))
>         nkwid = try $ do
>                  x <- idString
>                  --avoid all these keywords as aliases since they can
>                  --appear immediately following a tableref as the next
>                  --part of the statement, if we don't do this then lots
>                  --of things don't parse. Seems a bit inelegant but
>                  --works for the tests and the test sql files don't know
>                  --if these should be allowed as aliases without "" or
>                  --[]
>                  -- TODO find out what the correct behaviour here is.
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
>                              ,"join"
>                              ,"natural"
>                              ,"order"
>                              ,"group"
>                              ,"limit"
>                              ,"using"
>                              ,"from"]
>                    then fail "not keyword"
>                    else return x
>
> optParens :: SParser a
>           -> SParser a
> optParens p = try (parens p) <|> p

insert, update and delete
-------------------------

insert statement: supports option column name list,
multiple rows to insert and insert from select statements

> insert :: SParser Statement
> insert = Insert
>          <$> pos <* keyword "insert" <* keyword "into"
>          <*> idString
>          <*> option [] (try columnNameList)
>          <*> selectExpression
>          <*> tryOptionMaybe returning
>
> update :: SParser Statement
> update = Update
>          <$> pos <* keyword "update"
>          <*> idString
>          <*> (keyword "set" *> commaSep1 setClause)
>          <*> option [] (keyword "from" *> commaSep1 tableRef)
>          <*> tryOptionMaybe whereClause
>          <*> tryOptionMaybe returning
>     where
>       setClause = expr
>       -- todo: restrict this to a = expr or (a,b...) = expr forms only

> delete :: SParser Statement
> delete = Delete
>          <$> pos <* keyword "delete" <* keyword "from"
>          <*> idString
>          <*> option [] (keyword "using" *> commaSep1 tableRef)
>          <*> tryOptionMaybe whereClause
>          <*> tryOptionMaybe returning
>

other dml-type stuff
--------------------

> truncateSt :: SParser Statement
> truncateSt =
>            Truncate
>            <$> pos <* keyword "truncate" <* optional (keyword "table")
>            <*> commaSep1 idString
>            <*> option ContinueIdentity (choice [
>                                 ContinueIdentity <$ (keyword "continue"
>                                                      <* keyword "identity")
>                                ,RestartIdentity <$ (keyword "restart"
>                                                     <* keyword "identity")])
>            <*> cascade
>
> copy :: SParser Statement
> copy = do
>        p <- pos
>        keyword "copy"
>        tableName <- idString
>        cols <- option [] (parens $ commaSep1 idString)
>        keyword "from"
>        src <- choice [
>                CopyFilename <$> extrStr <$> stringLit
>               ,Stdin <$ keyword "stdin"]
>        return $ Copy p tableName cols src
>
> copyData :: SParser Statement
> copyData = CopyData <$> pos <*> mytoken (\tok ->
>                                         case tok of
>                                                  CopyPayloadTok n -> Just n
>                                                  _ -> Nothing)
>

--------------------------------------------------------------------------------

misc
====

> set :: SParser Statement
> set = Set <$> pos
>           <*> (keyword "set" *> idString)
>           <*> ((keyword "to" <|> symbol "=") *>
>               commaSep1 sv)
>       where
>         sv = choice [
>               SetStr <$> pos <*> stringN
>              ,SetId <$> pos <*> idString
>              ,SetNum <$> pos <*> ((fromInteger <$> integer) <|> float)]
>
> notify :: SParser Statement
> notify = Notify <$> pos
>                 <*> (keyword "notify" *> idString)

--------------------------------------------------------------------------------

ddl
===

> createTable :: SParser Statement
> createTable = do
>   p <- pos
>   keyword "table"
>   tname <- idString
>   choice [
>      CreateTableAs p tname <$> (keyword "as" *> selectExpression)
>     ,uncurry (CreateTable p tname) <$> readAttsAndCons]
>   where
>     --parse our unordered list of attribute defs or constraints, for
>     --each line want to try the constraint parser first, then the
>     --attribute parser, so we need the swap to feed them in the
>     --right order into createtable
>     readAttsAndCons = parens (swap <$> multiPerm
>                                          (try tableConstraint)
>                                          tableAttribute
>                                          (symbol ","))
>                       where swap (a,b) = (b,a)
>
> tableAttribute :: SParser AttributeDef
> tableAttribute = AttributeDef
>                <$> pos
>                <*> idString
>                <*> typeName
>                <*> tryOptionMaybe (keyword "default" *> expr)
>                <*> many rowConstraint
>   where
>     rowConstraint = do
>        p <- pos
>        cn <- option "" (keyword "constraint" *> idString)
>        choice [
>           RowUniqueConstraint p cn <$ keyword "unique"
>          ,RowPrimaryKeyConstraint p cn <$ keyword "primary" <* keyword "key"
>          ,RowCheckConstraint p cn <$> (keyword "check" *> parens expr)
>          ,NullConstraint p cn <$ keyword "null"
>          ,NotNullConstraint p cn <$ (keyword "not" <* keyword "null")
>          ,RowReferenceConstraint p cn
>          <$> (keyword "references" *> idString)
>          <*> option Nothing (try $ parens $ Just <$> idString)
>          <*> onDelete
>          <*> onUpdate
>          ]


>
> onDelete,onUpdate :: SParser Cascade
> onDelete = onSomething "delete"
> onUpdate = onSomething "update"
>
> onSomething :: String -> SParser Cascade
> onSomething k = option Restrict $ try $ keyword "on"
>                 *> keyword k *> cascade
>
> tableConstraint :: SParser Constraint
> tableConstraint = do
>                 p <- pos
>                 cn <- option "" (keyword "constraint" *> option "" conName)
>                 choice [
>                    UniqueConstraint p cn
>                    <$> try (keyword "unique" *> optParens columnNameList)
>                    ,PrimaryKeyConstraint p cn
>                    <$> try (keyword "primary" *> keyword "key"
>                                     *> choice [
>                                             (:[]) <$> idString
>                                            ,parens (commaSep1 idString)])
>                    ,CheckConstraint p cn
>                    <$>try (keyword "check" *> parens expr)
>                    ,ReferenceConstraint p cn
>                    <$> try (keyword "foreign" *> keyword "key"
>                             *> parens (commaSep1 idString))
>                    <*> (keyword "references" *> idString)
>                    <*> option [] (parens $ commaSep1 idString)
>                    <*> onUpdate
>                    <*> onDelete]
>                 where
>                   conName = try $ do
>                             x <- idString
>                             if map toLower x `elem` [
>                                     "unique"
>                                    ,"primary"
>                                    ,"check"
>                                    ,"foreign"
>                                    ,"references"]
>                               then fail "not keyword"
>                               else return x
>
> alterTable :: SParser Statement
> alterTable = AlterTable <$> (pos <* keyword "table"
>                              <* optional (keyword "only"))
>                         <*> idString
>                         <*> many1 action
>              where action = choice [
>                              AlterColumnDefault
>                              <$> (pos <* keyword "alter" <* keyword "column")
>                              <*> idString
>                              <*> (keyword "set" *> keyword "default" *> expr)
>                             ,AddConstraint
>                              <$> (pos <* keyword "add")
>                              <*> tableConstraint]
>
> createType :: SParser Statement
> createType = CreateType
>              <$> pos <* keyword "type"
>              <*> idString
>              <*> (keyword "as" *> parens (commaSep1 typeAtt))
>   where
>     typeAtt = TypeAttDef <$> pos <*> idString <*> typeName
>
> createSequence :: SParser Statement
> createSequence = do
>   p <- pos
>   keyword "sequence"
>   nm <- idString
>   (stw, incr, mx, mn, c) <-
>      permute ((,,,,) <$?> (1,startWith)
>                      <|?> (1,increment)
>                      <|?> ((2::Integer) ^ (63::Integer) - 1, maxi)
>                      <|?> (1, mini)
>                      <|?> (1, cache))
>   return $ CreateSequence p nm incr mn mx stw c
>   where
>     startWith = keyword "start" *> optional (keyword "with") *> integer
>     increment = keyword "increment" *> optional (keyword "by") *> integer
>     maxi = (2::Integer) ^ (63::Integer) - 1
>            <$ try (keyword "no" <* keyword "maxvalue")
>     mini = 1 <$ try (keyword "no" <* keyword "minvalue")
>     cache = keyword "cache" *> integer
>
> alterSequence :: SParser Statement
> alterSequence = AlterSequence <$> pos
>                               <*> (keyword "sequence" *> idString)
>                               <*> (keyword "owned"
>                                    *> keyword "by"
>                                    *> idString)

create function, support sql functions and plpgsql functions. Parses
the body in both cases and provides a statement list for the body
rather than just a string.

> createFunction :: SParser Statement
> createFunction = do
>   p <- pos
>   rep <- choice [NoReplace <$ keyword "function"
>                 ,Replace <$ mapM_ keyword ["or", "replace", "function"]
>                 ]
>   fnName <- idString
>   params <- parens $ commaSep param
>   retType <- keyword "returns" *> typeName
>   ((bodypos,body), lang,vol) <-
>     permute ((,,) <$$> parseAs
>                   <||> readLang
>                   <|?> (Volatile,pVol))
>   case parseBody lang body bodypos of
>        Left er -> fail er
>        Right b ->
>          return $ CreateFunction p fnName params retType rep lang b vol
>     where
>         parseAs = do
>                    keyword "as"
>                    bodypos <- toMySp <$> getPosition
>                    body <- stringLit
>                    return (bodypos,body)
>         pVol = matchAKeyword [("volatile", Volatile)
>                              ,("stable", Stable)
>                              ,("immutable", Immutable)]
>         readLang = keyword "language" *> matchAKeyword [("plpgsql", Plpgsql)
>                                                        ,("sql",Sql)]
>         parseBody :: Language -> Expression -> MySourcePos
>                   -> Either String FnBody
>         parseBody lang body (fileName,line,col) =
>             case (parseIt
>                   (lexSqlTextWithPosition fileName line col (extrStr body))
>                   (functionBody lang)
>                   fileName
>                   (Just (line,col))
>                   (extrStr body)
>                   ()) of
>                      Left er@(ParseErrorExtra _ _ _) -> Left $ show er
>                      Right body' -> Right body'
>         -- sql function is just a list of statements, the last one
>         -- has the trailing semicolon optional
>         functionBody Sql = do
>            p <- pos
>            a <- many (try $ sqlStatement True)
>            -- this makes my head hurt, should probably write out
>            -- more longhand
>            SqlFnBody p <$> option a ((\b -> (a++[b])) <$> sqlStatement False)
>         -- plpgsql function has an optional declare section, plus
>         -- the statements are enclosed in begin ... end; (semi colon
>         -- after end is optional)
>         functionBody Plpgsql =
>             PlpgsqlFnBody
>             <$> pos
>             <*> option [] declarePart
>             <*> statementPart
>             where
>               statementPart = keyword "begin"
>                     *> many plPgsqlStatement
>                     <* keyword "end" <* optional (symbol ";") <* eof
>               declarePart = keyword "declare"
>                   *> manyTill (try varDef) (lookAhead $ keyword "begin")

params to a function

> param :: SParser ParamDef
> param = choice [
>          try (ParamDef <$> pos <*> idString <*> typeName)
>         ,ParamDefTp <$> pos <*> typeName]

variable declarations in a plpgsql function

> varDef :: SParser VarDef
> varDef = VarDef
>          <$> pos
>          <*> idString
>          <*> typeName
>          <*> tryOptionMaybe ((symbol ":=" <|> symbol "=")*> expr)
>              <* symbol ";"
>
> createView :: SParser Statement
> createView = CreateView
>              <$> pos <* keyword "view"
>              <*> idString
>              <*> (keyword "as" *> selectExpression)
>
> createDomain :: SParser Statement
> createDomain = CreateDomain
>                <$> pos <* keyword "domain"
>                <*> idString
>                <*> (tryOptionMaybe (keyword "as") *> typeName)
>                <*> option "" (keyword "constraint" *> idString)
>                <*> tryOptionMaybe (keyword "check" *> parens expr)
>
> dropSomething :: SParser Statement
> dropSomething = do
>   p <- pos
>   x <- try (choice [
>                  Domain <$ keyword "domain"
>                 ,Type <$ keyword "type"
>                 ,Table <$ keyword "table"
>                 ,View <$ keyword "view"
>             ])
>   (i,e,r) <- parseDrop idString
>   return $ DropSomething p x i e r
>
> dropFunction :: SParser Statement
> dropFunction = do
>                p <- pos
>                keyword "function"
>                (i,e,r) <- parseDrop pFun
>                return $ DropFunction p i e r
>                where
>                  pFun = (,) <$> idString
>                             <*> parens (many typeName)
>
> parseDrop :: SParser a
>           -> SParser (IfExists, [a], Cascade)
> parseDrop p = (,,)
>               <$> ifExists
>               <*> commaSep1 p
>               <*> cascade
>     where
>       ifExists = option Require
>                  (try $ IfExists <$ (keyword "if"
>                                      *> keyword "exists"))
>
> createLanguage :: SParser Statement
> createLanguage =
>   CreateLanguage <$> pos
>                  <*> (optional (keyword "procedural") *>
>                       keyword "language" *>
>                       idString)
>
> createTrigger :: SParser Statement
> createTrigger =
>   CreateTrigger <$> pos
>                 <*> (keyword "trigger" *> idString)
>                 <*> twhen
>                 <*> tevents
>                 <*> (keyword "on" *> idString)
>                 <*> tfiring
>                 <*> (keyword "execute" *> keyword "procedure"
>                      *> idString)
>                 <*> parens (commaSep expr)
>   where
>     twhen = choice [TriggerBefore <$ keyword "before"
>                    ,TriggerAfter <$ keyword "after"]
>     tevents :: SParser [TriggerEvent]
>     tevents = sepBy1 (choice [
>                          AntiTriggerEvent <$> splice
>                         ,TInsert <$ keyword "insert"
>                         ,TUpdate <$ keyword "update"
>                         ,TDelete <$ keyword "delete"]) (keyword "or")
>     tfiring = option EachStatement
>                 (keyword "for" *> optional (keyword "each") *>
>                 choice [
>                   EachRow <$ keyword "row"
>                  ,EachStatement <$ keyword "statement"])

anti statement
--------------

> antiStatement :: SParser Statement
> antiStatement = AntiStatement <$> splice

--------------------------------------------------------------------------------

component parsers for sql statements
====================================

> whereClause :: SParser Expression
> whereClause = keyword "where" *> expr

selectlist and selectitem: the bit between select and from
check for into either before the whole list of select columns
or after the whole list

> selectList :: SParser SelectList
> selectList =
>     pos >>= \p ->
>     choice [
>         flip (SelectList p) <$> readInto <*> itemList
>        ,SelectList p  <$> itemList <*> option [] readInto]
>   where
>     readInto = keyword "into" *> commaSep1 idString
>     itemList = commaSep1 selectItem
>     selectItem = pos >>= \p ->
>                  optionalSuffix
>                    (SelExp p) expr
>                    (SelectItem p) () (keyword "as" *> idString)
>
> returning :: SParser SelectList
> returning = keyword "returning" *> selectList
>
> columnNameList :: SParser [String]
> columnNameList = parens $ commaSep1 idString
>
> typeName :: SParser TypeName
> typeName =
>   choice [
>      SetOfTypeName <$> pos <*> (keyword "setof" *> typeName)
>     ,otherTypeName]
>   where
>     otherTypeName = do
>        p <- pos
>        s <- map toLower <$> pTypeNameString
>        choice [PrecTypeName p s <$> parens integer
>               ,arrayTypeName p s
>               ,return $ SimpleTypeName p s]
>     arrayTypeName p s = ArrayTypeName p (SimpleTypeName p s)
>                         <$ symbol "[" <* symbol "]"
>     --todo: add special cases for the other type names with spaces in them
>     pTypeNameString = ("double precision" <$ try (keyword "double"
>                                                   <* keyword "precision"))
>                       <|> idString
>
> cascade :: SParser Cascade
> cascade = option Restrict (choice [
>                             Restrict <$ keyword "restrict"
>                            ,Cascade <$ keyword "cascade"])

--------------------------------------------------------------------------------

plpgsql statements
==================

> plPgsqlStatement :: SParser Statement
> plPgsqlStatement =
>    sqlStatement True
>     <|> (choice [
>           continue
>          ,execute
>          ,caseStatement
>          ,assignment
>          ,ifStatement
>          ,returnSt
>          ,raise
>          ,forStatement
>          ,whileStatement
>          ,loopStatement
>          ,perform
>          ,nullStatement
>          ,exitStatement]
>          <* symbol ";")
>     <|> label
>
> label :: SParser Statement
> label = Label <$> pos <*> (symbol "<<" *> idString <* symbol ">>")
>
> nullStatement :: SParser Statement
> nullStatement = NullStatement <$> (pos <* keyword "null")
>
> exitStatement :: SParser Statement
> exitStatement = ExitStatement <$> (pos <* keyword "exit")
>                               <*> optional idString
>
>
> continue :: SParser Statement
> continue = ContinueStatement <$> (pos <* keyword "continue")
>                              <*> optional idString
>
> perform :: SParser Statement
> perform = Perform <$> (pos <* keyword "perform") <*> expr
>
> execute :: SParser Statement
> execute = pos >>= \p -> keyword "execute" >>
>           optionalSuffix
>             (Execute p) expr
>             (ExecuteInto p) () readInto
>     where
>       readInto = keyword "into" *> commaSep1 idString
>
> assignment :: SParser Statement
> assignment = Assignment
>              <$> pos
>              -- put the := in the first try to attempt to get a
>              -- better error if the code looks like malformed
>              -- assignment statement
>              <*> try (idString <* (symbol ":=" <|> symbol "="))
>              <*> expr
>
> returnSt :: SParser Statement
> returnSt = pos >>= \p -> keyword "return" >>
>            choice [
>             ReturnNext p <$> (keyword "next" *> expr)
>            ,ReturnQuery p <$> (keyword "query" *> selectExpression)
>            ,Return p <$> tryOptionMaybe expr]
>
> raise :: SParser Statement
> raise = pos >>= \p -> keyword "raise" >>
>         Raise p
>         <$> raiseType
>         <*> (extrStr <$> stringLit)
>         <*> option [] (symbol "," *> commaSep1 expr)
>         where
>           raiseType = matchAKeyword [("notice", RNotice)
>                                      ,("exception", RException)
>                                      ,("error", RError)]
>
> forStatement :: SParser Statement
> forStatement = do
>                p <- pos
>                keyword "for"
>                start <- idString
>                keyword "in"
>                choice [ForSelectStatement p start
>                        <$> try selectExpression <*> theRest
>                       ,ForIntegerStatement p start
>                               <$> expr
>                               <*> (symbol ".." *> expr)
>                               <*> theRest]
>   where
>     theRest = keyword "loop" *> many plPgsqlStatement
>               <* keyword "end" <* keyword "loop"
>
> whileStatement :: SParser Statement
> whileStatement = WhileStatement
>                  <$> (pos <* keyword "while")
>                  <*> (expr <* keyword "loop")
>                  <*> many plPgsqlStatement <* keyword "end" <* keyword "loop"
> loopStatement :: SParser Statement
> loopStatement = LoopStatement
>                  <$> (pos <* keyword "loop")
>                  <*> many plPgsqlStatement <* keyword "end" <* keyword "loop"
>

>
> ifStatement :: SParser Statement
> ifStatement = If
>               <$> (pos <* keyword "if")
>               <*> (ifPart <:> elseifParts)
>               <*> (elsePart <* endIf)
>   where
>     ifPart = expr <.> (thn *> many plPgsqlStatement)
>     elseifParts = many ((elseif *> expr) <.> (thn *> many plPgsqlStatement))
>     elsePart = option [] (keyword "else" *> many plPgsqlStatement)
>     endIf = keyword "end" <* keyword "if"
>     thn = keyword "then"
>     elseif = keyword "elseif"
>     --might as well throw this in as well after all that
>     -- can't do <,> unfortunately, so use <.> instead
>     (<.>) a b = (,) <$> a <*> b
>
> caseStatement :: SParser Statement
> caseStatement = do
>     p <- pos
>     keyword "case"
>     choice [try (CaseStatementSimple p
>                  <$> expr
>                  <*> many whenSt
>                  <*> option [] (keyword "else" *> many plPgsqlStatement)
>                         <* keyword "end" <* keyword "case")
>            ,CaseStatement p
>                  <$> many whenSt
>                  <*> option [] (keyword "else" *> many plPgsqlStatement)
>                         <* keyword "end" <* keyword "case"]
>     where
>       whenSt = keyword "when" >>
>                (,) <$> commaSep1 expr
>                    <*> (keyword "then" *> many plPgsqlStatement)

--------------------------------------------------------------------------------

expressions
===========

This is the bit that makes it the most obvious that I don't really
know haskell, parsing theory or parsec ... robbed a parsing example
from haskell-cafe and mainly just kept changing it until it seemed to
work

> expr :: SParser Expression
> expr = buildExpressionParser table factor
>        <?> "expression"
>
> factor :: SParser Expression
> factor =

First job is to take care of forms which start like a vanilla
expression, and then add a suffix on

>   threadOptionalSuffixes fct [castSuffix
>                              ,betweenSuffix
>                              ,arraySubSuffix]
>   where
>     fct = choice [

order these so the ones which can be valid prefixes of others appear
further down the list (used to be a lot more important when there
wasn't a separate lexer), probably want to refactor this to use the
optionalsuffix parsers to improve speed.

One little speed optimisation, to help with pretty printed code which
can contain a lot of parens - check for nested ((
This little addition speeds up ./ParseFile.lhs sqltestfiles/system.sql
on my system from ~4 minutes to ~4 seconds (most of the 4s is probably
compilation overhead).

>        try (lookAhead (symbol "(" >> symbol "(")) >> parens expr

start with the factors which start with parens - eliminate scalar
subqueries since they're easy to distinguish from the others then do in
predicate before row constructor, since an in predicate can start with
a row constructor looking thing, then finally vanilla parens

>       ,scalarSubQuery
>       ,try $ threadOptionalSuffix rowCtor inPredicateSuffix
>       ,parens expr

try a few random things which can't start a different expression

>       ,positionalArg
>       ,placeholder
>       ,stringLit
>       ,floatLit
>       ,integerLit

put the factors which start with keywords before the ones which start
with a function, so we don't try an parse a keyword as a function name

>       ,caseExpression
>       ,exists
>       ,booleanLit
>       ,nullLit
>       ,arrayLit
>       ,castKeyword
>       ,substring

now do identifiers, functions, and window functions (each is a prefix
to the next one)

want to allow splices in e.g. function calls: $(fnname)(). To do this,
don't want to parse anti expression above, but need to parse these
following suffixes starting with a splice, but if there is no suffix,
want to parse as an antiexpression rather than an antiidentifier

>       ,try $ do
>              i <- antiIdentifier
>              choice [inPredicateSuffix i
>                     ,threadOptionalSuffix (functionCallSuffix i)
>                                           windowFnSuffix]

>       ,antiExpression
>       ,antiIdentifier1
>       ,threadOptionalSuffixes identifier
>                               [inPredicateSuffix
>                               ,\l -> threadOptionalSuffix
>                                        (functionCallSuffix l)
>                                        windowFnSuffix]]



operator table
--------------

proper hacky, but sort of does the job
the 'missing' notes refer to pg operators which aren't yet supported,
or supported in a different way (e.g. cast uses the type name parser
for one of it's argument, not the expression parser - I don't know if
there is a better way of doing this but there usually is in parsec)

pg's operator table is on this page:
http://www.postgresql.org/docs/8.4/interactive/sql-syntax-lexical.html#SQL-SYNTAX-OPERATORS

will probably need something more custom to handle full range of sql
syntactical novelty, in particular the precedence rules mix these
operators up with irregular syntax operators, you can create new
operators during parsing, and some operators are prefix/postfix or
binary depending on the types of their operands (how do you parse
something like this?)

The full list of operators from a standard template1 database should
be used here.

> table :: [[Operator [Token] ParseState Identity Expression]]
> table = [--[binary "::" (BinOpCall Cast) AssocLeft]
>          --missing [] for array element select
>          [prefix "-" "u-"]
>         ,[binary "^" AssocLeft]
>         ,[binary "*" AssocLeft
>          ,idHackBinary "*" AssocLeft
>          ,binary "/" AssocLeft
>          ,binary "%" AssocLeft]
>         ,[binary "+" AssocLeft
>          ,binary "-" AssocLeft]
>          --should be is isnull and notnull
>         ,[postfixks ["is", "not", "null"] "!isnotnull"
>          ,postfixks ["is", "null"] "!isnull"]
>          --other operators all added in this list according to the pg docs:
>         ,[binary "<->" AssocNone
>          ,binary "<=" AssocRight
>          ,binary ">=" AssocRight
>          ,binary "||" AssocLeft
>          ,prefix "@" "@"
>          ]
>          --in should be here, but is treated as a factor instead
>          --between
>          --overlaps
>         ,[binaryk "like" "!like" AssocNone
>          ,binarycust (symbol "!=") "<>" AssocNone]
>          --(also ilike similar)
>         ,[binary "<" AssocNone
>          ,binary ">" AssocNone]
>         ,[binary "=" AssocRight
>          ,binary "<>" AssocNone]
>         ,[notNot
>          ,prefixk "not" "!not"
>          ]
>         ,[binaryk "and" "!and" AssocLeft
>          ,binaryk "or" "!or" AssocLeft]]
>     where
>       binary s = binarycust (symbol s) s
>       -- '*' is lexed as an id token rather than a symbol token, so
>       -- work around here
>       idHackBinary s = binarycust (keyword s) s
>       binaryk = binarycust . keyword
>       prefix = unaryCust Prefix . symbol
>       prefixk = unaryCust Prefix . keyword
>       postfixks = unaryCust Postfix . mapM_ keyword
>       binarycust opParse t =
>         Infix $ try $ do
>              f <- FunCall <$> pos <*> (t <$ opParse)
>              return (\l m -> f [l,m])
>       unaryCust ctor opParse t =
>         ctor $ try $ do
>           f <- FunCall <$> pos <*> (t <$ opParse)
>           return (\l -> f [l])
>       -- hack - haven't worked out why parsec buildexpression parser won't
>       -- parse something like "not not EXPR" without parens so hack here
>       notNot =
>         Prefix (try $ do
>                       p1 <- pos
>                       keyword "not"
>                       p2 <- pos
>                       keyword "not"
>                       return (\l -> FunCall p1 "!not"
>                                       [FunCall p2 "!not" [l]]))

factor parsers
--------------

I think the lookahead is used in an attempt to help the error messages.

> scalarSubQuery :: SParser Expression
> scalarSubQuery = try (symbol "(" *> lookAhead (keyword "select"
>                                                <|> keyword "with")) >>
>                  ScalarSubQuery
>                  <$> pos
>                  <*> selectExpression <* symbol ")"

in predicate - an identifier or row constructor followed by 'in'
then a list of expressions or a subselect

> inPredicateSuffix :: Expression -> SParser Expression
> inPredicateSuffix e =
>   InPredicate
>   <$> pos
>   <*> return e
>   <*> option True (False <$ keyword "not")
>   <*> (keyword "in" *> parens ((InSelect <$> pos <*> selectExpression)
>                                <|>
>                                (InList <$> pos <*> commaSep1 expr)))

row ctor: one of

* row ()
* row (expr)
* row (expr, expr1, ...)
* (expr, expr2,...) [implicit (no row keyword) version, at least two elements
                   must be present]

* (expr) parses to just expr rather than row(expr)
* and () is a syntax error.

> rowCtor :: SParser Expression
> rowCtor = FunCall
>           <$> pos
>           <*> return "!rowctor"
>           <*> choice [
>            keyword "row" *> parens (commaSep expr)
>           ,parens $ commaSep2 expr]
>
> floatLit :: SParser Expression
> floatLit = FloatLit <$> pos <*> float
>
> integerLit :: SParser Expression
> integerLit = IntegerLit <$> pos <*> integer
>
> caseExpression :: SParser Expression
> caseExpression = do
>   p <- pos
>   keyword "case"
>   choice [
>              try $ CaseSimple p <$> expr
>                                 <*> many whenParse
>                                 <*> tryOptionMaybe (keyword "else" *> expr)
>                                         <* keyword "end"
>             ,Case p <$> many whenParse
>                     <*> tryOptionMaybe (keyword "else" *> expr)
>                             <* keyword "end"]
>   where
>     whenParse = (,) <$> (keyword "when" *> commaSep1 expr)
>                     <*> (keyword "then" *> expr)
>
> exists :: SParser Expression
> exists = Exists <$> pos <* keyword "exists" <*> parens selectExpression
>
> booleanLit :: SParser Expression
> booleanLit = BooleanLit <$> pos <*> (True <$ keyword "true"
>                                      <|> False <$ keyword "false")
>
> nullLit :: SParser Expression
> nullLit = NullLit <$> pos <* keyword "null"
>
> arrayLit :: SParser Expression
> arrayLit = FunCall <$> pos <* keyword "array"
>                    <*> return "!arrayctor"
>                    <*> squares (commaSep expr)
>
> arraySubSuffix :: Expression -> SParser Expression
> arraySubSuffix e = case e of
>                      Identifier _ "array" -> fail "can't use array as \
>                                                   \identifier name"
>                      _ -> FunCall <$> pos
>                                   <*> return "!arraysub"
>                                   <*> ((e:) <$> squares (commaSep1 expr))
>
> windowFnSuffix :: Expression -> SParser Expression
> windowFnSuffix e = WindowFn <$> pos <*> return e
>                    <*> (keyword "over"
>                         *> (symbol "(" *> option [] partitionBy))
>                    <*> option [] orderBy1
>                    <*> option Asc (try $ choice [
>                                             Asc <$ keyword "asc"
>                                            ,Desc <$ keyword "desc"])
>                    <*> frm
>                    <* symbol ")"
>   where
>     orderBy1 = keyword "order" *> keyword "by" *> commaSep1 expr
>     partitionBy = keyword "partition" *> keyword "by" *> commaSep1 expr
>     frm = option FrameUnboundedPreceding $ choice
>                                          $ map (\(a,b) -> a <$ try (ks b)) [
>            (FrameUnboundedPreceding, ["range","unbounded","preceding"])
>           ,(FrameUnboundedPreceding, ["range"
>                                      ,"between"
>                                      ,"unbounded"
>                                      ,"preceding"
>                                      ,"and"
>                                      ,"current"
>                                      ,"row"])
>           ,(FrameUnboundedFull, ["range"
>                                 ,"between"
>                                 ,"unbounded"
>                                 ,"preceding"
>                                 ,"and"
>                                 ,"unbounded"
>                                 ,"following"])
>           ,(FrameUnboundedFull, ["rows"
>                                 ,"between"
>                                 ,"unbounded"
>                                 ,"preceding"
>                                 ,"and"
>                                 ,"unbounded"
>                                 ,"following"])
>           ,(FrameRowsUnboundedPreceding, ["rows","unbounded","preceding"])
>           ,(FrameRowsUnboundedPreceding, ["rows"
>                                          ,"between"
>                                          ,"unbounded"
>                                          ,"preceding"
>                                          ,"and"
>                                          ,"current"
>                                          ,"row"])]
>     ks = mapM keyword
>
> betweenSuffix :: Expression -> SParser Expression
> betweenSuffix a = do
>   p <- pos
>   keyword "between"
>   b <- dodgyParseElement
>   keyword "and"
>   c <- dodgyParseElement
>   return $ FunCall p "!between" [a,b,c]
>              --can't use the full expression parser at this time
>              --because of a conflict between the operator 'and' and
>              --the 'and' part of a between
>   where
>     dodgyParseElement = factor

From postgresql src/backend/parser/gram.y

~~~~~

 * We have two expression types: a_expr is the unrestricted kind, and
 * b_expr is a subset that must be used in some places to avoid shift/reduce
 * conflicts.  For example, we can't do BETWEEN as "BETWEEN a_expr AND a_expr"
 * because that use of AND conflicts with AND as a boolean operator.  So,
 * b_expr is used in BETWEEN and we remove boolean keywords from b_expr.
 *
 * Note that '(' a_expr ')' is a b_expr, so an unrestricted expression can
 * always be used by surrounding it with parens.

~~~~~

TODO: copy this approach here.


> functionCallSuffix :: Expression -> SParser Expression
> functionCallSuffix (Identifier _ fnName) =
>   pos >>= \p -> FunCall p fnName
>                 <$> parens (optional (keyword "all"
>                                       <|> keyword "distinct")
>                             *> commaSep expr)
> functionCallSuffix s =
>   error $ "internal error: cannot make functioncall from " ++ show s
>
> castKeyword :: SParser Expression
> castKeyword = Cast
>               <$> pos <* keyword "cast" <* symbol "("
>               <*> expr
>               <*> (keyword "as" *> typeName <* symbol ")")
>
> castSuffix :: Expression -> SParser Expression
> castSuffix ex = pos >>= \p -> Cast p ex <$> (symbol "::" *> typeName)
>
> substring :: SParser Expression
> substring = do
>             p <- pos
>             keyword "substring"
>             symbol "("
>             a <- expr
>             keyword "from"
>             b <- expr
>             keyword "for"
>             c <- expr
>             symbol ")"
>             return $ FunCall p "!substring" [a,b,c]
>
> identifier :: SParser Expression
> identifier = Identifier <$> pos <*> (idString <|> splice)
>

> antiIdentifier :: SParser Expression
> antiIdentifier = Identifier <$> pos <*> spliceD

> antiIdentifier1 :: SParser Expression
> antiIdentifier1 = Identifier <$> pos <*> ssplice
>                   where
>                     ssplice = (\s -> "$i(" ++ s ++ ")") <$>
>                               (symbol "$i(" *> idString <* symbol ")")


--------------------------------------------------------------------------------

Utility parsers
===============

tokeny things
-------------

keyword has to not be immediately followed by letters or numbers
(symbols and whitespace are ok) so we know that we aren't reading an
identifier which happens to start with a complete keyword

> keyword :: String -> SParser ()
> keyword k = mytoken (\tok ->
>                                case tok of
>                                IdStringTok i | lcase k == lcase i -> Just ()
>                                _ -> Nothing)
>                       where
>                         lcase = map toLower
>
> idString :: SParser String
> idString =
>     choice [(\l -> "$(" ++ l ++ ")")
>             <$> (symbol "$(" *> idString <* symbol ")")
>            ,ids
>            ]
>   where
>     ids = mytoken (\tok -> case tok of
>                                      IdStringTok "not" -> Nothing
>                                      IdStringTok i -> Just i
>                                      _ -> Nothing)
>
> spliceD :: SParser String
> spliceD = (\x -> "$(" ++ x ++ ")") <$> splice
>
> splice :: SParser String
> splice = symbol "$(" *> idString <* symbol ")"
>
> symbol :: String -> SParser ()
> symbol c = mytoken (\tok -> case tok of
>                                    SymbolTok s | c==s -> Just ()
>                                    _           -> Nothing)
>
> integer :: SParser Integer
> integer = mytoken (\tok -> case tok of
>                                     IntegerTok n -> Just n
>                                     _ -> Nothing)
>
> liftPositionalArgTok :: SParser Integer
> liftPositionalArgTok =
>   mytoken (\tok -> case tok of
>                    PositionalArgTok n -> Just n
>                    _ -> Nothing)

> positionalArg :: SParser Expression
> positionalArg = PositionalArg <$> pos <*> liftPositionalArgTok
>
> antiExpression :: SParser Expression
> antiExpression = AntiExpression <$> splice
>
> placeholder :: SParser Expression
> placeholder = (Placeholder <$> pos) <* symbol "?"
>
> float :: SParser Double
> float = mytoken (\tok -> case tok of
>                                     FloatTok n -> Just n
>                                     _ -> Nothing)
>
> liftStringTok :: SParser String
> liftStringTok = mytoken (\tok ->
>                   case tok of
>                            StringTok _ s -> Just s
>                            _ -> Nothing)

> stringLit :: SParser Expression
> stringLit = (StringLit <$> pos <*> liftStringTok)
>             <|>
>             (StringLit <$> pos <*> ssplice)
>              where
>                ssplice = (\s -> "$s(" ++ s ++ ")") <$>
>                            (symbol "$s(" *> idString <* symbol ")")
>
> stringN :: SParser String
> stringN = mytoken (\tok ->
>                   case tok of
>                            StringTok _ s -> Just s
>                            _ -> Nothing)

> extrStr :: Expression -> String
> extrStr (StringLit _ s) = s
> extrStr x =
>   error $ "internal error: extrStr not supported for this type " ++ show x

== combinatory things

> parens :: SParser a
>        -> SParser a
> parens = between (symbol "(") (symbol ")")
>
> squares :: SParser a
>        -> SParser a
> squares = between (symbol "[") (symbol "]")
>
> tryOptionMaybe :: (Stream s m t) =>
>              ParsecT s u m a -> ParsecT s u m (Maybe a)
> tryOptionMaybe p = try (optionMaybe p) <|> return Nothing
>
> commaSep2 :: SParser a
>           -> SParser [a]
> commaSep2 p = sepBy2 p (symbol ",")
>
> sepBy2 :: (Stream s m t) =>
>           ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
> sepBy2 p sep = (p <* sep) <:> sepBy1 p sep
>
> commaSep :: SParser a
>          -> SParser [a]
> commaSep p = sepBy p (symbol ",")
>
> commaSep1 :: SParser a
>           -> SParser [a]
> commaSep1 p = sepBy1 p (symbol ",")

pass a list of pairs of strings and values
try each pair k,v in turn,
if keyword k matches then return v
doesn't really add a lot of value

> matchAKeyword :: [(String, a)] -> SParser a
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

like thread optional suffix, but we pass a list of suffixes in, not
much of a shorthand

> threadOptionalSuffixes :: ParsecT [tok] st Identity a
>                        -> [a -> GenParser tok st a]
>                        -> ParsecT [tok] st Identity a
> threadOptionalSuffixes p1 p2s = do
>   x <- p1
>   option x (try $ choice (map (\l -> l x) p2s))

couldn't work how to to perms so just did this hack instead
e.g.
a1,a2,b1,b2,a2,b3,b4 parses to ([a1,a2,a3],[b1,b2,b3,b4])

> multiPerm :: (Stream s m t) =>
>                ParsecT s u m a1
>             -> ParsecT s u m a
>             -> ParsecT s u m sep
>             -> ParsecT s u m ([a1], [a])
>
> multiPerm p1 p2 sep = do
>   (r1, r2) <- unzip <$> sepBy1 parseAorB sep
>   return (catMaybes r1, catMaybes r2)
>   where
>     parseAorB = choice [
>                   (\x -> (Just x,Nothing)) <$> p1
>                  ,(\y -> (Nothing, Just y)) <$> p2]

== position stuff

simple wrapper for parsec source positions, probably not really useful

> type MySourcePos = (String,Int,Int)
>
> toMySp :: SourcePos -> MySourcePos
> toMySp sp = (sourceName sp,sourceLine sp,sourceColumn sp)

parser combinator to return the current position as an ast annotation

> pos :: SParser Annotation
> pos = do
>   (\a -> emptyAnnotation {asrc=Just a}) <$> toMySp <$> getPosition

== lexer stuff

> mytoken :: (Tok -> Maybe a) -> SParser a
> mytoken test
>   = token showToken posToken testToken
>   where
>   showToken (_,tok)   = show tok
>   posToken  (posi,_)  = posi
>   testToken (_,tok)   = test tok

--------------------------------------------------------------------------------

= fixup tree

this is where some generics code is used to transform the parse trees
to alter the nodes used where it's too difficult to do whilst
parsing. The only item at the moment that needs this treatment is the
any/some/all construct which looks like this:
expr operator [any|some|all] (expr)

This gets parsed as
funcall operator [expr1,funcall [any|some|all] [expr2,...]]
and we want to transform it to
liftoperator operator any|some|all [expr1, expr2,...]
not doing anything if the funcall name isn't any,some,all
any other checks are left to the type checking stage
(e.g. there can only be one expression in the expr2 part, and it must
be an array or subselect, etc)

> fixupTree :: Data a => a -> a
> fixupTree =
>     transformBi $ \x ->
>       case x of
>              FunCall an op (expr1:FunCall _ nm expr2s:expr3s)
>                | isOperatorName op
>                  && map toLower nm `elem` ["any", "some", "all"]
>                -> LiftOperator an op flav (expr1:expr2s ++ expr3s)
>                   where flav = case (map toLower nm) of
>                                  "any" -> LiftAny
>                                  "some" -> LiftAny
>                                  "all" -> LiftAll
>                                  z -> error $ "internal error in parsing \
>                                               \lift transform: " ++ z
>              x1 -> x1

--------------------------------------------------------------------------------

Parse state not currently used. Use these placeholders to add some.

> type ParseState = ()
>
> startState :: ()
> startState = ()
