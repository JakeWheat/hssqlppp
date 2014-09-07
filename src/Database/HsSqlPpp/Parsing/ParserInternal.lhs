
The main file for parsing sql, uses parsec. Not sure if parsec is the
right choice, but it seems to do the job pretty well at the moment.

> {-# LANGUAGE FlexibleContexts,ExplicitForAll,TupleSections #-}
> -- | Functions to parse SQL.
> module Database.HsSqlPpp.Parsing.ParserInternal
>     (-- * Main
>      parseStatements
>     ,parseStatementsWithPosition
>     ,parseStatementsFromFile
>     ,parseQueryExpr
>     ,parsePlpgsqlWithPosition
>      -- * Testing
>     ,parseScalarExpr
>     ,parseScalarExprWithPosition
>     ,parsePlpgsql
>      -- * errors
>     ,ParseErrorExtra(..)
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
> import Data.Generics.Uniplate.Data
> import Data.Data hiding (Prefix,Infix)
>
> import Database.HsSqlPpp.Parsing.Lexer
> import Database.HsSqlPpp.Parsing.ParseErrors
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation as A
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Catalog
> --import Debug.Trace

--------------------------------------------------------------------------------

Top level parsing functions
===========================

> parseStatements :: String -- ^ filename to use in errors
>                 -> String -- ^ a string containing the sql to parse
>                 -> Either ParseErrorExtra [Statement]
> parseStatements f s =
>   parseIt l sqlStatements f Nothing s startState
>   where l = lexSqlText f s
>
> parseStatementsWithPosition :: FilePath -- ^ filename to use in errors
>                             -> Int -- ^ adjust line number in errors by adding this
>                             -> Int -- ^ adjust column in errors by adding this
>                             -> String -- ^ a string containing the sql to parse
>                             -> Either ParseErrorExtra [Statement]
> parseStatementsWithPosition f l c s =
>   parseIt lx sqlStatements f (Just (l,c)) s startState
>   where lx = lexSqlText f s
> --parseAntiSql f l c s
>
> parseStatementsFromFile :: FilePath -- ^ file name of file containing sql
>                         -> IO (Either ParseErrorExtra [Statement])
> parseStatementsFromFile fn = do
>   sc <- readFile fn
>   x <- lexSqlFile fn
>   return $ parseIt x sqlStatements fn Nothing sc startState
>

> parseQueryExpr :: String -- ^ filename to use in errors
>                -> String -- ^ a string containing the sql to parse
>                -> Either ParseErrorExtra QueryExpr
> parseQueryExpr f s =
>   parseIt l pqe f Nothing s startState
>   where
>     l = lexSqlText f s
>     pqe :: SParser QueryExpr
>     pqe = do
>           (QueryStatement _ q) <- queryStatement
>           _ <- optional (symbol ";")
>           eof
>           return q

> -- | Parse expression fragment, used for testing purposes
> parseScalarExpr :: String -- ^ filename for error messages
>                 -> String -- ^ sql string containing a single expression,
>                           -- with no trailing ';'
>                 -> Either ParseErrorExtra ScalarExpr
> parseScalarExpr f s =
>   parseIt l (expr <* eof) f Nothing s startState
>   where l = lexSqlText f s
>
> -- | Parse plpgsql statements, used for testing purposes -
> -- this can be used to parse a list of plpgsql statements which
> -- aren't contained in a create function.
> -- (The produced ast won't pass a type check.)
> parsePlpgsql :: String
>              -> String
>              -> Either ParseErrorExtra [Statement]
> parsePlpgsql f s =
>   parseIt l p f Nothing s startState
>   where
>     l = lexSqlText f s
>     p = many plPgsqlStatement <* eof
>
> parsePlpgsqlWithPosition :: String
>                  -> Int
>                  -> Int
>                  -> String
>                  -> Either ParseErrorExtra [Statement]
> parsePlpgsqlWithPosition f l c s =
>   parseIt lx p f ps s startState
>   where
>     lx = lexSqlText f s
>     p = many plPgsqlStatement <* eof
>     ps = Just (l,c)
>
> parseScalarExprWithPosition :: String
>                     -> Int
>                     -> Int
>                     -> String
>                     -> Either ParseErrorExtra ScalarExpr
> parseScalarExprWithPosition f l c s =
>   parseIt lx p f ps s startState
>   where
>     lx = lexSqlText f s
>     p = expr <* eof
>     ps = Just (l,c)
>
> -- utility function to do error handling in one place
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
>     ,queryStatement
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

> queryStatement :: SParser Statement
> queryStatement = QueryStatement <$> pos <*> pQueryExpr
>
> into :: SParser (Statement -> Statement)
> into = do
>   p <- pos <* keyword "into"
>   st <- option False (True <$ keyword "strict")
>   is <- commaSep1 name
>   return $ \s -> Into p st is s

> intoQueryStatement :: SParser Statement
> intoQueryStatement = do
>   (i,_) <- pQueryExprX True
>   case i of
>     Nothing -> fail "not into"
>     Just s -> return s

> pQueryExpr :: SParser QueryExpr
> pQueryExpr = snd <$> pQueryExprX False

bit convoluted to parse the into part

> pQueryExprX :: Bool -> SParser (Maybe Statement, QueryExpr)
> pQueryExprX allowInto =
>   ((Nothing,) <$> with)
>   <|>  buildExpressionParser combTable selFactor
>   where
>         selFactor = choice [try ((Nothing,) <$> (parens pQueryExpr))
>                            ,selQuerySpec
>                            ,(Nothing,) <$> values]
>         with = WithQueryExpr <$> (pos <* keyword "with")
>                              <*> commaSep1 withQuery
>                              <*> pQueryExpr
>         withQuery = WithQuery <$> pos
>                               <*> nameComponent
>                               <*> tryOptionMaybe (parens $ commaSep nameComponent)
>                               <*> (keyword "as" *> parens pQueryExpr)
>         combTable =
>            [map makeOp [(Except, keyword "except")
>                        ,(Intersect, keyword "intersect")
>                        ,(UnionAll, try (keyword "union" *> keyword "all"))
>                        ,(Union, keyword "union")]]
>         makeOp (c,p) =
>            Infix (do
>                   cmb <- CombineQueryExpr
>                          <$> pos
>                          <*> (c <$ p)
>                   return $ \s0 s1 -> (Nothing, cmb (snd s0) (snd s1))
>                  ) AssocLeft
>         selQuerySpec = do
>           p <- pos <* keyword "select"
>           d <- option Dupes (Distinct <$ keyword "distinct")
>           -- hacky parsing of sql server 'top n' style select
>           -- quiz: what happens when you use top n and limit at the same time?
>           tp <- optionMaybe $ try
>                 $ keyword "top" *> (NumberLit <$> pos <*> (show <$> integer))
>           -- todo: work out how to make this work properly - need to return
>           -- the into
>           (sl,intoBit) <- if allowInto
>                           then permute ((,)
>                                         <$$> try selectList
>                                         <|?> (Nothing, Just <$> into))
>                           else (,Nothing) <$> selectList
>           s <- Select p d sl
>                    <$> option [] from
>                    <*> optionMaybe whereClause
>                    <*> option [] groupBy
>                    <*> optionMaybe having
>                    <*> orderBy
>                    <*> option tp (Just <$> limit)
>                    <*> optionMaybe offset
>           return (case intoBit of
>                       Just f -> Just $ f $ QueryStatement p s
>                       Nothing -> Nothing
>                  ,s)
>         from = keyword "from" *> commaSep1 tableRef
>         groupBy = keyword "group" *> keyword "by"
>                   *> commaSep1 expr
>         having = keyword "having" *> expr
>         limit = keyword "limit" *> expr
>         offset = keyword "offset" *> expr
>         values = Values <$> (pos <* keyword "values")
>                         <*> commaSep1 (parens $ commaSep1 expr)

> orderBy :: SParser [(ScalarExpr,Direction)]
> orderBy = option []
>             (keyword "order" *> keyword "by"
>                              *> commaSep1 oneOrder)
>           where
>             oneOrder = (,) <$> expr
>                        <*> option Asc (choice [
>                                         Asc <$ keyword "asc"
>                                        ,Desc <$ keyword "desc"])


table refs
have to cope with:
a simple tableref i.e just a name
an aliased table ref e.g. select a.b from tbl as a
a sub select e.g. select a from (select b from c)
 - these are handled in nonJoinTref
then you combine by seeing if there is a join looking prefix

> tableRef :: SParser TableRef
> tableRef =
>   trefTerm >>= maybeParseAnotherJoin
>   where
>         maybeParseAnotherJoin tr1 =
>           choice [
>                 do
>                   p2 <- pos
>                   (nat,jt) <- joinKw
>                   JoinTref p2 tr1 nat jt
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
>                          <$> parens pQueryExpr
>                          <*> palias
>                         ,FunTref p2
>                          <$> try (identifier >>= functionCallSuffix)
>                          <*> palias
>                         ,Tref p2
>                          <$> nonKeywordName
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
>         palias = do
>            p <- pos
>            option (NoAlias p)
>                    (try $ optionalSuffix
>                       (TableAlias p) (optional (keyword "as") *> nonKeywordNc)
>                       (FullAlias p) () (parens $ commaSep1 nameComponent))
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
>          <*> name
>          <*> option [] (try columnNameList)
>          <*> pQueryExpr
>          <*> tryOptionMaybe returning
>
> update :: SParser Statement
> update = Update
>          <$> pos <* keyword "update"
>          <*> name
>          <*> (keyword "set" *> commaSep1 setClause)
>          <*> option [] (keyword "from" *> commaSep1 tableRef)
>          <*> tryOptionMaybe whereClause
>          <*> tryOptionMaybe returning
>     where
>       setClause =
>         choice [do
>           p <- pos
>           l <- parens (commaSep1 nameComponent)
>           symbol "="
>           r <- parens (commaSep1 expr)
>           return $ MultiSetClause p l $ FunCall p (nm p "!rowctor") r
>         ,do
>           p <- pos
>           l <- nameComponent
>           symbol "="
>           r <- expr
>           return $ SetClause p l r]

> nm :: Annotation -> String -> Name
> nm a s = Name a [Nmc s]

> delete :: SParser Statement
> delete = Delete
>          <$> pos <* keyword "delete" <* keyword "from"
>          <*> name
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
>            <*> commaSep1 name
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
>        tableName <- name
>        cols <- option [] (parens $ commaSep1 nameComponent)
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
>              ,SetNum <$> pos <*> (try (fromInteger <$> integer)
>                                   <|> (read <$> numString))]
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
>   tname <- name
>   choice [
>      CreateTableAs p tname <$> (keyword "as" *> pQueryExpr)
>     ,uncurry (CreateTable p tname) <$> readAttsAndCons]
>   where
>     --parse our unordered list of attribute defs or constraints, for
>     --each line want to try the constraint parser first, then the
>     --attribute parser, so you need the swap to feed them in the
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
>                <*> nameComponent
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
>          <$> (keyword "references" *> name)
>          <*> option Nothing (try $ parens $ Just <$> nameComponent)
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
>                                             (:[]) <$> nameComponent
>                                            ,parens (commaSep1 nameComponent)])
>                    ,CheckConstraint p cn
>                    <$>try (keyword "check" *> parens expr)
>                    ,ReferenceConstraint p cn
>                    <$> try (keyword "foreign" *> keyword "key"
>                             *> parens (commaSep1 nameComponent))
>                    <*> (keyword "references" *> name)
>                    <*> option [] (parens $ commaSep1 nameComponent)
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
>                               then fail "not keyword (constraint name)"
>                               else return x
>
> alterTable :: SParser Statement
> alterTable = AlterTable <$> (pos <* keyword "table"
>                              <* optional (keyword "only"))
>                         <*> name
>                         <*> many1 action
>              where action = choice [
>                              AlterColumnDefault
>                              <$> (pos <* keyword "alter" <* keyword "column")
>                              <*> nameComponent
>                              <*> (keyword "set" *> keyword "default" *> expr)
>                             ,AddConstraint
>                              <$> (pos <* keyword "add")
>                              <*> tableConstraint]
>
> createType :: SParser Statement
> createType = CreateType
>              <$> pos <* keyword "type"
>              <*> name
>              <*> (keyword "as" *> parens (commaSep1 typeAtt))
>   where
>     typeAtt = TypeAttDef <$> pos <*> nameComponent <*> typeName
>
> createSequence :: SParser Statement
> createSequence = do
>   p <- pos
>   keyword "sequence"
>   snm <- name
>   (stw, incr, mx, mn, c) <-
>      permute ((,,,,) <$?> (1,startWith)
>                      <|?> (1,increment)
>                      <|?> ((2::Integer) ^ (63::Integer) - 1, maxi)
>                      <|?> (1, mini)
>                      <|?> (1, cache))
>   return $ CreateSequence p snm incr mn mx stw c
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
>                               <*> (keyword "sequence" *> name)
>                               <*> (keyword "owned"
>                                    *> keyword "by"
>                                    *> name)

create function, support sql functions and plpgsql functions. Parses
the body in both cases and provides a statement list for the body
rather than just a string.

> createFunction :: SParser Statement
> createFunction = do
>   p <- pos
>   rep <- choice [NoReplace <$ keyword "function"
>                 ,Replace <$ mapM_ keyword ["or", "replace", "function"]
>                 ]
>   fnName <- name
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
>         parseBody :: Language -> ScalarExpr -> MySourcePos
>                   -> Either String FnBody
>         parseBody lang body (fileName,line,col) =
>             case parseIt
>                   (lexSqlTextWithPosition fileName line col (extrStr body))
>                   (functionBody lang)
>                   fileName
>                   (Just (line,col))
>                   (extrStr body)
>                   () of
>                      Left er@(ParseErrorExtra _ _ _) -> Left $ show er
>                      Right body' -> Right body'
>         -- sql function is just a list of statements, the last one
>         -- has the trailing semicolon optional
>         functionBody Sql = do
>            p <- pos
>            a <- many (try $ sqlStatement True)
>            -- this makes my head hurt, should probably write out
>            -- more longhand
>            SqlFnBody p <$> option a ((\b -> a++[b]) <$> sqlStatement False)
>         -- plpgsql function has an optional declare section, plus
>         -- the statements are enclosed in begin ... end; (semi colon
>         -- after end is optional)
>         functionBody Plpgsql =
>             PlpgsqlFnBody <$> pos <*> do
>                    p <- pos
>                    l <- label
>                    block p l <* optional (symbol ";") <* eof

params to a function

> param :: SParser ParamDef
> param = choice [
>          try (ParamDef <$> pos <*> nameComponent <*> typeName)
>         ,ParamDefTp <$> pos <*> typeName]

variable declarations in a plpgsql function

> varDef :: SParser VarDef
> varDef = do
>   p <- pos
>   a <- nameComponent
>   choice [do
>           keyword "alias"
>           keyword "for"
>           choice [
>             VarAlias p a <$> name
>            ,ParamAlias p a <$> liftPositionalArgTok]
>          ,VarDef p a
>           <$> typeName
>           <*> tryOptionMaybe ((symbol ":=" <|> symbol "=")*> expr)
>           ]
>     <* symbol ";"
>
> createView :: SParser Statement
> createView = CreateView
>              <$> pos <* keyword "view"
>              <*> name
>              <*> tryOptionMaybe (parens $ commaSep nameComponent)
>              <*> (keyword "as" *> pQueryExpr)

>
> createDomain :: SParser Statement
> createDomain = CreateDomain
>                <$> pos <* keyword "domain"
>                <*> name
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
>   (i,e,r) <- parseDrop name
>   return $ DropSomething p x i e r
>
> dropFunction :: SParser Statement
> dropFunction = do
>                p <- pos
>                keyword "function"
>                (i,e,r) <- parseDrop pFun
>                return $ DropFunction p i e r
>                where
>                  pFun = (,) <$> name
>                             <*> parens (commaSep typeName)
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
>                 <*> (keyword "trigger" *> nameComponent)
>                 <*> twhen
>                 <*> tevents
>                 <*> (keyword "on" *> name)
>                 <*> tfiring
>                 <*> (keyword "execute" *> keyword "procedure"
>                      *> name)
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

> whereClause :: SParser ScalarExpr
> whereClause = keyword "where" *> expr

selectlist and selectitem: the bit between select and from
check for into either before the whole list of select columns
or after the whole list

> selectList :: SParser SelectList
> selectList = SelectList <$> pos
>              <*> itemList
>     {-pos >>= \p ->
>     choice [
>         --flip (SelectList p) <$> readInto <*> itemList
>        SelectList p <$> itemList] -- <*> option [] readInto]-}
>   where
>     --readInto = keyword "into" *> commaSep1 qName
>     itemList = commaSep1 selectItem
>     selectItem = pos >>= \p ->
>                  optionalSuffix
>                    (SelExp p) (starExpr <|> expr)
>                    (SelectItem p) () (keyword "as" *> nameComponent)

should try to factor this into the standard expr parse (use a flag) so
that can left factor the 'name component . '  part and avoid the try

> starExpr :: SParser ScalarExpr
> starExpr = choice [Star <$> pos <* symbol "*"
>                   ,try $ do
>                          p <- pos
>                          nc <- nameComponent
>                          symbol "."
>                          symbol "*"
>                          return $ QStar p nc]

>
> returning :: SParser SelectList
> returning = keyword "returning" *> selectList
>
> columnNameList :: SParser [NameComponent]
> columnNameList = parens $ commaSep1 nameComponent
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
>        choice [try (Prec2TypeName p s
>                     <$> (symbol "(" *> integer)
>                     <*> (symbol "," *> integer <* symbol ")"))
>               ,PrecTypeName p s <$> parens integer
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
>    choice [
>      -- modified sql statements
>      choice [
>         try intoQueryStatement
>        ,choice [insert
>                ,update
>                ,delete] >>= intoSuffix
>        ] <* symbol ";"
>     -- regular sql statements
>     ,sqlStatement True
>     -- regular plpgsql statements
>     ,choice [
>           continue
>          ,execute >>= intoSuffix
>          ,caseStatement
>          ,assignment
>          ,ifStatement
>          ,returnSt
>          ,raise
>          ,perform
>          ,labelPrefixed
>          ,nullStatement
>          ,exitStatement]
>          <* symbol ";"
>     ]
>    where
>      intoSuffix e =
>        option e (try $ do
>                  i <- into
>                  return $ i e)
>      labelPrefixed = do
>        p <- pos
>        l <- label
>        choice [block p l
>               ,forStatement p l
>               ,whileStatement p l
>               ,loopStatement p l]

> label :: SParser (Maybe String)
> label = optional (symbol "<<" *> idString <* symbol ">>")
>
> block :: Annotation -> Maybe String -> SParser Statement
> block p l = Block p l
>             <$> option [] declarePart
>             <*> statementPart
>         where
>           statementPart = keyword "begin"
>                     *> many plPgsqlStatement
>                     <* keyword "end"
>           declarePart = keyword "declare"
>                         *> manyTill (try varDef) (lookAhead $ keyword "begin")
>
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
> execute = Execute <$> (pos <* keyword "execute")
>          <*> expr
>          {-pos >>= \p ->  >>
>           optionalSuffix
>             (Execute p) expr
>             (ExecuteInto p) () readInto
>     where
>       readInto = keyword "into" *> commaSep1 idString-}
>
> assignment :: SParser Statement
> assignment = Assignment
>              <$> pos
>              -- put the := in the first try to attempt to get a
>              -- better error if the code looks like malformed
>              -- assignment statement
>              <*> try (name <* (symbol ":=" <|> symbol "="))
>              <*> expr
>
> returnSt :: SParser Statement
> returnSt = pos >>= \p -> keyword "return" >>
>            choice [
>             ReturnNext p <$> (keyword "next" *> expr)
>            ,ReturnQuery p <$> (keyword "query" *> pQueryExpr)
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
> forStatement :: Annotation -> Maybe String -> SParser Statement
> forStatement p l = do
>                keyword "for"
>                start <- nameComponent
>                keyword "in"
>                choice [ForQueryStatement p l start
>                        <$> try pQueryExpr <*> theRest
>                       ,ForIntegerStatement p l start
>                               <$> expr
>                               <*> (symbol ".." *> expr)
>                               <*> theRest]
>   where
>     theRest = keyword "loop" *> many plPgsqlStatement
>               <* keyword "end" <* keyword "loop"
>
> whileStatement :: Annotation -> Maybe String -> SParser Statement
> whileStatement p l = WhileStatement p l
>                      <$> (keyword "while" *> expr <* keyword "loop")
>                      <*> many plPgsqlStatement <* keyword "end" <* keyword "loop"
> loopStatement :: Annotation -> Maybe String -> SParser Statement
> loopStatement p l = LoopStatement p l
>                     <$> (keyword "loop" *> many plPgsqlStatement <* keyword "end" <* keyword "loop")
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
>     elseif = keyword "elseif" <|> keyword "elsif"
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

> expr :: SParser ScalarExpr
> expr = buildExpressionParser table factor
>        <?> "expression"

>
> factor :: SParser ScalarExpr
> factor =

First job is to take care of forms which start like a vanilla
expression, and then add a suffix on

>   fct >>= tryExprSuffix
>   where
>     tryExprSuffix e =
>       option e (choice (map (\f -> f e)
>                                  [inPredicateSuffix
>                                  ,functionCallSuffix
>                                  ,windowFnSuffix
>                                  ,castSuffix
>                                  ,betweenSuffix
>                                  ,arraySubSuffix
>                                  ,qualIdSuffix])
>                 >>= tryExprSuffix)
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

>        --try (lookAhead (symbol "(" >> symbol "(")) >> parens expr

start with the factors which start with parens - eliminate scalar
subqueries since they're easy to distinguish from the others then do in
predicate before row constructor, since an in predicate can start with
a row constructor looking thing, then finally vanilla parens

>       --,
>        scalarSubQuery
>       ,try rowCtor
>       ,parens expr

try a few random things which can't start a different expression

>       ,positionalArg
>       ,placeholder
>       ,stringLit
>       ,numberLit

put the factors which start with keywords before the ones which start
with a function, so you don't try an parse a keyword as a function name

>       ,caseScalarExpr
>       ,exists
>       ,booleanLit
>       ,nullLit
>       ,arrayLit
>       ,castKeyword
>       ,try substring -- use try cos there is also a regular function called substring
>       ,extract

>       ,try interval
>       ,try typedStringLit
>       ,antiScalarExpr
>       ,identifier
>       ]

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

> tableAB :: Bool -> [[Operator [Token] ParseState Identity ScalarExpr]]
> tableAB isB = [[{-binary "." AssocLeft-}]
>          --[binary "::" (BinOpCall Cast) AssocLeft]
>          --missing [] for array element select
>         ,[prefix "-" "u-"]
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
>          ,binaryks ["not","like"] "!notlike" AssocNone
>          ,binarycust (symbol "!=") "<>" AssocNone]
>          --(also ilike similar)
>         ,[binary "<" AssocNone
>          ,binary ">" AssocNone]
>         ,[binary "=" AssocRight
>          ,binary "<>" AssocNone]
>         ,[notNot
>          ,prefixk "not" "!not"
>          ]
>         ,let x = [binaryk "or" "!or" AssocLeft]
>          in if isB
>             then x
>             else binaryk "and" "!and" AssocLeft : x
>          ]
>     where
>       binary s = binarycust (symbol s) s
>       -- '*' is lexed as an id token rather than a symbol token, so
>       -- work around here
>       idHackBinary s = binarycust (keyword s) s
>       binaryk = binarycust . keyword
>       binaryks = binarycust . mapM_ keyword
>       prefix = unaryCust Prefix . symbol
>       prefixk = unaryCust Prefix . keyword
>       postfixks = unaryCust Postfix . mapM_ keyword
>       binarycust opParse t =
>         Infix $ try $ do
>              f <- FunCall <$> pos <*> (nm emptyAnnotation t <$ opParse)
>              return (\l m -> f [l,m])
>       unaryCust ctor opParse t =
>         ctor $ try $ do
>           f <- FunCall <$> pos <*> (nm emptyAnnotation t <$ opParse)
>           return (\l -> f [l])
>       -- hack - haven't worked out why parsec buildexpression parser won't
>       -- parse something like "not not EXPR" without parens so hack here
>       notNot =
>         Prefix (try $ do
>                       p1 <- pos
>                       keyword "not"
>                       p2 <- pos
>                       keyword "not"
>                       return (\l -> FunCall p1 (nm p1 "!not")
>                                     [FunCall p2 (nm p2 "!not") [l]]))

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

> table :: [[Operator [Token] ParseState Identity ScalarExpr]]
> table = tableAB False

> tableB :: [[Operator [Token] ParseState Identity ScalarExpr]]
> tableB = tableAB True

use the same factors

> b_expr :: SParser ScalarExpr
> b_expr = buildExpressionParser tableB factor
>        <?> "expression"
>

factor parsers
--------------

I think the lookahead is used in an attempt to help the error messages.

> scalarSubQuery :: SParser ScalarExpr
> scalarSubQuery = try (symbol "(" *> lookAhead (keyword "select"
>                                                <|> keyword "with")) >>
>                  ScalarSubQuery
>                  <$> pos
>                  <*> pQueryExpr <* symbol ")"

in predicate - an identifier or row constructor followed by 'in'
then a list of expressions or a subselect

> inPredicateSuffix :: ScalarExpr -> SParser ScalarExpr
> inPredicateSuffix e = try $
>   InPredicate
>   <$> pos
>   <*> return e
>   <*> option True (False <$ keyword "not")
>   <*> (keyword "in" *> parens ((InQueryExpr <$> pos <*> pQueryExpr)
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

> rowCtor :: SParser ScalarExpr
> rowCtor = FunCall
>           <$> pos
>           <*> (nm <$> pos <*> return "!rowctor")
>           <*> choice [
>            keyword "row" *> parens (commaSep expr)
>           ,parens $ commaSep2 expr]
>
> numberLit :: SParser ScalarExpr
> numberLit = NumberLit <$> pos <*> numString
>

> integer :: SParser Integer
> integer = do
>   l <- numString
>   guard (all (`elem` digChars) l)
>   return $ read l
>   where
>     digChars = concatMap show [(0::Int)..9]

>
> caseScalarExpr :: SParser ScalarExpr
> caseScalarExpr = do
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
> exists :: SParser ScalarExpr
> exists = Exists <$> pos <* keyword "exists" <*> parens pQueryExpr
>

> booleanLit :: SParser ScalarExpr
> booleanLit = BooleanLit <$> pos <*> (True <$ keyword "true"
>                                      <|> False <$ keyword "false")

>
> nullLit :: SParser ScalarExpr
> nullLit = NullLit <$> pos <* keyword "null"
>
> arrayLit :: SParser ScalarExpr
> arrayLit = FunCall <$> pos <* keyword "array"
>                    <*> (nm <$> pos <*> return "!arrayctor")
>                    <*> squares (commaSep expr)
>
> arraySubSuffix :: ScalarExpr -> SParser ScalarExpr
> arraySubSuffix e = case e of
>                      Identifier _ (Nmc "array") -> fail "can't use array \
>                                                         \as identifier name"
>                      _ -> FunCall <$> pos
>                                   <*> (nm <$> pos <*> return "!arraysub")
>                                   <*> ((e:) <$> squares (commaSep1 expr))
>
> windowFnSuffix :: ScalarExpr -> SParser ScalarExpr
> windowFnSuffix e = WindowFn <$> pos <*> return e
>                    <*> (keyword "over"
>                         *> (symbol "(" *> option [] partitionBy))
>                    <*> orderBy
>                    <*> frm
>                    <* symbol ")"
>   where
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
> betweenSuffix :: ScalarExpr -> SParser ScalarExpr
> betweenSuffix a = do
>   p <- pos
>   keyword "between"
>   b <- b_expr
>   keyword "and"
>   c <- b_expr
>   return $ FunCall p (nm p "!between") [a,b,c]

handles aggregate business as well

can use a * in aggregate calls and in window functions.  This
represents a call to an aggregate which has no parameters, so count(*)
actually means count():

select count() from pg_attrdef;
ERROR:  count(*) must be used to call a parameterless aggregate function
LINE 1: select count() from pg_attrdef;

But you can't write it as count() ... ?

You cannot use * together with either distinct or order by, and it
cannot be qualified.

This parser is used as the prefix of the window function parser so
both are handled here. This will parse non aggregate calls containing
a single * argument without error - this will have to be caught during
typechecking. It also parses the aggregate extras (distinct and order
by) for non aggregate calls without error, so this also will need to
be caught during typechecking. The typechecker doesn't really do much
checking with aggregates at the moment so should fix it all together.

> functionCallSuffix :: ScalarExpr -> SParser ScalarExpr
> functionCallSuffix (Identifier _ (Nmc fnName)) = do
>   p <- pos
>   (di,as,ob) <- parens
>                 $ choice [ -- handle a single *
>                           (Nothing,,[]) <$> ((:[]) <$> (Star <$> pos <* symbol "*"))
>                          ,(,,)
>                           <$> optionMaybe
>                                (choice [Distinct <$ keyword "distinct"
>                                        ,Dupes <$ keyword "all"])
>                           <*> commaSep expr
>                           <*> orderBy]
>   return $ case (di,ob) of
>     (Nothing,[]) -> FunCall p (nm p fnName) as
>     (d,o) -> AggregateFn p (fromMaybe Dupes d) (FunCall p (nm p fnName) as) o
> --hack for antiquoted function name
> functionCallSuffix (AntiScalarExpr n) =
>   functionCallSuffix (Identifier emptyAnnotation (Nmc $ "$(" ++ n ++ ")"))
> functionCallSuffix s =
>   fail $ "cannot make functioncall from " ++ show s
>
> castKeyword :: SParser ScalarExpr
> castKeyword = Cast
>               <$> pos <* keyword "cast" <* symbol "("
>               <*> expr
>               <*> (keyword "as" *> typeName <* symbol ")")
>
> castSuffix :: ScalarExpr -> SParser ScalarExpr
> castSuffix ex = pos >>= \p -> Cast p ex <$> (symbol "::" *> typeName)

> typedStringLit :: SParser ScalarExpr
> typedStringLit = TypedStringLit
>                  <$> pos
>                  <*> typeName
>                  <*> (extrStr <$> stringLit)

> extract :: SParser ScalarExpr
> extract = try $ do
>   p <- pos
>   _ <- keyword "extract"
>   _ <- symbol "("
>   f <- extractField
>   _ <- keyword "from"
>   e <- expr
>   _ <- symbol ")"
>   return $ Extract p f e
>   where
>     extractField =
>       choice [ExtractCentury <$ keyword "century"
>              ,ExtractDay <$ keyword "day"
>              ,ExtractDecade <$ keyword "decade"
>              ,ExtractDow <$ keyword "dow"
>              ,ExtractDoy <$ keyword "doy"
>              ,ExtractEpoch <$ keyword "epoch"
>              ,ExtractHour <$ keyword "hour"
>              ,ExtractIsodow <$ keyword "isodow"
>              ,ExtractIsoyear <$ keyword "isoyear"
>              ,ExtractMicroseconds <$ keyword "microseconds"
>              ,ExtractMillennium <$ keyword "millennium"
>              ,ExtractMilliseconds <$ keyword "milliseconds"
>              ,ExtractMinute <$ keyword "minute"
>              ,ExtractMonth <$ keyword "month"
>              ,ExtractQuarter <$ keyword "quarter"
>              ,ExtractSecond <$ keyword "second"
>              ,ExtractTimezone <$ keyword "timezone"
>              ,ExtractTimezoneHour <$ keyword "timezone_hour"
>              ,ExtractTimezoneMinute <$ keyword "timezone_minute"
>              ,ExtractWeek <$ keyword "week"
>              ,ExtractYear <$ keyword "year"]

> interval :: SParser ScalarExpr
> interval = Interval
>            <$> pos
>            <*> (keyword "interval" *> (extrStr <$> stringLit))
>            <*> intervalField
>            <*> tryOptionMaybe (parens (fromInteger <$> integer))
>   where
>     intervalField =
>         choice [IntervalYear <$ keyword "year"
>                ,IntervalMonth <$ keyword "month"
>                ,IntervalDay <$ keyword "day"
>                ,IntervalHour <$ keyword "hour"
>                ,IntervalMinute <$ keyword "minut"
>                ,IntervalSecond <$ keyword "second"
>                {-,IntervalYearToMonth <$ keyword "day"
>                ,IntervalDayToHour <$ keyword "day"
>                ,IntervalDayToMinute <$ keyword "day"
>                ,IntervalDayToSecond <$ keyword "day"
>                ,IntervalHourToMinute <$ keyword "day"
>                ,IntervalHourToSecond <$ keyword "day"
>                ,IntervalMinuteToSecond <$ keyword "day"-}]
>
> substring :: SParser ScalarExpr
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
>             return $ FunCall p (nm p "!substring") [a,b,c]
>

------------------------------------------------------------

identifier wasteland

> qualIdSuffix :: ScalarExpr -> SParser ScalarExpr
> qualIdSuffix (Identifier p i) = do
>     i1 <- symbol "." *> nameComponent
>     return $ QIdentifier p [i,i1]
> qualIdSuffix e = do
>     p <- pos
>     i1 <- symbol "." *> nameComponent
>     return $ FunCall p (nm p ".") [e,Identifier p i1]


> identifier :: SParser ScalarExpr
> identifier = Identifier <$> pos <*> nameComponent
>

bit hacky, avoid a bunch of keywords. Not exactly sure which keywords
should be in the blacklist, and where this parser should be used
instead of the full parser which allows keywords. Also not sure if
keywords used in qualified names should be rejected the same as
keywords which are unqualified.

> nonKeywordNc :: SParser NameComponent
> nonKeywordNc = do
>   x <- nameComponent
>   if x `elem` badKeywords
>     then fail "not keyword (NameComponent)"
>     else return x
>   where
>     badKeywords = map Nmc
>                   ["as"
>                   ,"where"
>                   ,"except"
>                   ,"union"
>                   ,"intersect"
>                   ,"loop"
>                   ,"inner"
>                   ,"on"
>                   ,"left"
>                   ,"right"
>                   ,"full"
>                   ,"cross"
>                   ,"join"
>                   ,"natural"
>                   ,"order"
>                   ,"group"
>                   ,"limit"
>                   ,"using"
>                   ,"from"]


> nonKeywordNcs :: SParser [NameComponent]
> nonKeywordNcs = sepBy1 nonKeywordNc (symbol ".")

> ncs :: SParser [NameComponent]
> ncs = sepBy1 nameComponent (symbol ".")

> name :: SParser Name
> name = Name <$> pos <*> ncs

> nonKeywordName :: SParser Name
> nonKeywordName = Name <$> pos <*> nonKeywordNcs

> nameComponent :: SParser NameComponent
> nameComponent = choice [Nmc <$> idString
>                        ,QNmc <$> qidString
>                        ,Nmc <$> spliceD
>                        ,Nmc <$> ssplice]
>                 where
>                   ssplice = (\s -> "$i(" ++ s ++ ")") <$>
>                               (symbol "$i(" *> idString <* symbol ")")


--------------------------------------------------------------------------------

Utility parsers
===============

tokeny things
-------------

keyword has to not be immediately followed by letters or numbers
(symbols and whitespace are ok) so you know that we aren't reading an
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
>                                      -- have to go through and fix every
>                                      -- use of idString to make this work correctly
>                                      -- idstring is used LOADS
>                                      -- lots of places in the ast probably need fixing
>                                      _ -> Nothing)
> qidString :: SParser String
> qidString =
>     choice [(\l -> "$(" ++ l ++ ")")
>             <$> (symbol "$(" *> idString <* symbol ")")
>            ,ids
>            ]
>   where
>     ids = mytoken (\tok -> case tok of
>                                      QIdStringTok i -> Just i
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
> liftPositionalArgTok :: SParser Integer
> liftPositionalArgTok =
>   mytoken (\tok -> case tok of
>                    PositionalArgTok n -> Just n
>                    _ -> Nothing)

> positionalArg :: SParser ScalarExpr
> positionalArg = PositionalArg <$> pos <*> liftPositionalArgTok
>
> antiScalarExpr :: SParser ScalarExpr
> antiScalarExpr = AntiScalarExpr <$> splice
>
> placeholder :: SParser ScalarExpr
> placeholder = (Placeholder <$> pos) <* symbol "?"
>
> numString :: SParser String
> numString = mytoken (\tok -> case tok of
>                                     NumberTok n -> Just n
>                                     _ -> Nothing)

>
> liftStringTok :: SParser String
> liftStringTok = mytoken (\tok ->
>                   case tok of
>                            StringTok _ s -> Just s
>                            _ -> Nothing)

> stringLit :: SParser ScalarExpr
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

> extrStr :: ScalarExpr -> String
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

optionalSuffix

parse the start of something -> parseResultA,
then parse an optional suffix -> parseResultB
if this second parser succeeds, return fn2 parseResultA parseResultB
else return fn1 parseResultA

e.g.
parsing an identifier in a select list can be
fieldName
or
fieldName as alias
so you can pass
* IdentifierCtor
* identifier (returns aval)
* AliasedIdentifierCtor
* () - looks like a place holder, probably a crap idea
* parser for (as b) (returns bval)
as the args, which I like to ident like:
parseOptionalSuffix
  IdentifierCtor identifierParser
  AliasedIdentifierCtor () asAliasParser
and you get either
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
> pos =
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
and you want to transform it to
liftoperator operator any|some|all [expr1, expr2,...]
not doing anything if the funcall name isn't any,some,all
any other checks are left to the type checking stage
(e.g. there can only be one expression in the expr2 part, and it must
be an array or subselect, etc)

> fixupTree :: Data a => a -> a
> fixupTree =
>     transformBi $ \x ->
>       case x of
>              FunCall an op (expr1:FunCall _ fn expr2s:expr3s)
>                | Name _ [Nmc opnm] <- op
>                , isOperatorName opnm
>                , Name _ [Nmc fnm] <- fn
>                , Just flav <- case map toLower fnm of
>                                  "any" -> Just LiftAny
>                                  "some" -> Just LiftAny
>                                  "all" -> Just LiftAll
>                                  _ -> Nothing
>                -> LiftOperator an opnm flav (expr1:expr2s ++ expr3s)
>              x1 -> x1

--------------------------------------------------------------------------------

Parse state not currently used. Use these placeholders to add some.

> type ParseState = ()
>
> startState :: ()
> startState = ()
