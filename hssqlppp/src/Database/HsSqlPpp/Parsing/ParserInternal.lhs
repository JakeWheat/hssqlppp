
The main file for parsing sql, uses parsec. Not sure if parsec is the
right choice, but it seems to do the job pretty well at the moment.

> {-# LANGUAGE FlexibleContexts,ExplicitForAll,TupleSections,
>              NoMonomorphismRestriction,OverloadedStrings #-}
> -- | Functions to parse SQL.
> module Database.HsSqlPpp.Parsing.ParserInternal
>     (-- * Main
>      parseStatements
>     ,parseQueryExpr
>     ,parseScalarExpr
>     ,parsePlpgsql
>      -- * parsing flags
>     ,ParseFlags(..)
>     ,defaultParseFlags
>     ,SQLSyntaxDialect(..)
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
>     ,parseName
>     ,parseNameComponent
>     ) where
>
> import Text.Parsec hiding (many, optional, (<|>), string, label)
> import Text.Parsec.Expr
> import Text.Parsec.String
> import Text.Parsec.Perm
>
> import Control.Applicative
> import Control.Monad.Identity
> --import Control.Monad
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
> import Database.HsSqlPpp.SqlDialect
> import Data.Text (Text)
> import qualified Data.Text as T
> --import qualified Data.Text.Lazy as LT
> import Text.Parsec.Text ()
> --import Database.HsSqlPpp.Catalog
> --import Debug.Trace
> import qualified Data.Text.Lazy as L
> --import Database.HsSqlPpp.Internals.StringLike

--------------------------------------------------------------------------------

Top level parsing functions
===========================

> -- | Parse a list of statements
> parseStatements :: ParseFlags -- ^ parse options
>                 -> FilePath -- ^ filename to use in errors
>                 -> Maybe (Int,Int) -- ^ set the line number and column number
>                                    -- of the first char in the source (used in annotation)
>                 -> L.Text -- ^ a string containing the sql to parse
>                 -> Either ParseErrorExtra [Statement]
> parseStatements = parseIt' sqlStatements

> -- | Parse a single query expr
> parseQueryExpr :: ParseFlags -- ^ parse options
>                -> FilePath -- ^ filename to use in errors
>                -> Maybe (Int,Int) -- ^ set the line number and column number
>                -> L.Text -- ^ a string containing the sql to parse
>                -> Either ParseErrorExtra QueryExpr
> parseQueryExpr =
>   parseIt' $ pQueryExpr <* optional (symbol ";") <* eof

> -- | Parse a single scalar expr
> parseScalarExpr :: ParseFlags -- ^ parse options
>                 -> FilePath -- ^ filename to use in errors
>                 -> Maybe (Int,Int) -- ^ set the line number and column number
>                 -> L.Text -- ^ a string containing the sql to parse
>                 -> Either ParseErrorExtra ScalarExpr
> parseScalarExpr = parseIt' $ expr <* eof

> -- | Parse a list of plpgsql statements (or tsql if you are using
> -- sql server dialect)
> parsePlpgsql :: ParseFlags -- ^ parse options
>              -> FilePath -- ^ filename to use in errors
>              -> Maybe (Int,Int) -- ^ set the line number and column number
>              -> L.Text -- ^ a string containing the sql to parse
>              -> Either ParseErrorExtra [Statement]
> parsePlpgsql = parseIt' $ many plPgsqlStatement <* eof

> parseIt' :: Data a =>
>             SParser a
>          -> ParseFlags
>          -> FilePath
>          -> Maybe (Int,Int)
>          -> L.Text
>          -> Either ParseErrorExtra a
> parseIt' ps flg fn sp src = do
>   lxd <- lexSql (pfDialect flg) fn sp src
>   psd <- either (\e -> Left $ ParseErrorExtra e sp src) Right
>          $ runParser ps flg fn lxd
>   return $ fixupTree psd

Parse state used for parse flags. Might be better to use a readerT
somehow, but I'm not smart enough to work out how to do this. This
state is never updated during parsing

> -- | Settings to influence the parsing
> data ParseFlags = ParseFlags
>     {pfDialect :: SQLSyntaxDialect
>     }
>     deriving (Show,Eq)

> defaultParseFlags :: ParseFlags
> defaultParseFlags = ParseFlags {pfDialect = PostgreSQLDialect}


> type ParseState = ParseFlags

> isSqlServer :: SParser Bool
> isSqlServer = do
>   ParseFlags {pfDialect = d} <- getState
>   return $ d == SQLServerDialect

couple of wrapper functions for the quoting

> parseName :: ParseFlags
>           -> FilePath
>           -> Maybe (Int,Int)
>           -> L.Text
>           -> Either ParseErrorExtra Name
> parseName = parseIt' $ name <* eof

> parseNameComponent :: ParseFlags
>                    -> FilePath
>                    -> Maybe (Int,Int)
>                    -> L.Text
>                    -> Either ParseErrorExtra NameComponent
> parseNameComponent = parseIt' $ nameComponent <* eof


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
>     ,do
>      not <$> isSqlServer >>= guard
>      set
>     ,notify
>     ,keyword "create" *>
>              choice [
>                 createTable
>                ,createSequence
>                ,createType
>                ,createFunction
>                ,createView
>                ,createDomain
>                ,createDatabase
>                ,createLanguage
>                ,createTrigger
>                ,createIndex]
>     ,keyword "alter" *>
>              choice [
>                 alterSequence
>                ,alterTable]
>     ,keyword "drop" *>
>              choice [
>                 dropSomething
>                ,dropFunction]]
>     <* stmtEnd (not reqSemi))
>    <|> copyData

--------------------------------------------------------------------------------

statement flavour parsers
=========================

top level/sql statements first

query expr

> queryStatement :: SParser Statement
> queryStatement = QueryStatement <$> pos <*> pQueryExpr

this is the plpgsql into

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
TODO: used to parse the into in the select list parser,
maybe it should still do this since it would probably be a lot clearer


> pQueryExprX :: Bool -> SParser (Maybe Statement, QueryExpr)
> pQueryExprX allowInto =
>   ((Nothing,) <$> with)
>   <|>  buildExpressionParser combTable selFactor
>   where
>         selFactor = choice [try ((Nothing,) <$> parens pQueryExpr)
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
>           -- todo: support explicit all
>           d <- option All (Distinct <$ keyword "distinct")
>           -- hacky parsing of sql server 'top n' style select
>           -- quiz: what happens when you use top n and limit at the same time?
>           tp <- choice
>                 [do
>                  isSqlServer >>= guard
>                  optionMaybe $ try
>                      (keyword "top" *>
>                       choice [parens expr
>                              -- not sure why you can't write expr without parens
>                              -- but it doesn't work TODO: fix this
>                              -- the current hack only allows a single number
>                              -- if you don't also use parens
>                              ,NumberLit <$> pos <*> ((T.pack . show) <$> integer)])
>                 ,return Nothing]
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

only way of combining them is joins which parse left associative which
makes it easy

> tableRef :: SParser TableRef
> tableRef = nonJoin >>= optionalJoinSuffix
>   where
>     nonJoin = do
>               -- read parens and a subquery or tableref
>               -- or a funtref or just a tref
>               t <- choice
>                    [do
>                     p <- pos
>                     parens $ choice [try $ SubTref p <$> pQueryExpr
>                                     ,TableRefParens p <$> tableRef]
>                     -- should combine the funtref and tref parsing
>                    ,try $ FunTref <$> pos
>                                   <*> (identifier >>= functionCallSuffix)
>                    ,Tref <$> pos <*> name]
>               optionalAlias t
>     optionalAlias t = do
>               -- try and read an alias
>               a <- optionMaybe $ try alias
>               -- ignore an optional hint if this is sql server
>               _ <- choice [do isSqlServer >>= guard
>                               _ <- try $ optional tableHint
>                               return ()
>                           ,return ()]
>               -- wrap the tref from before with the alias
>               -- if got one
>               return $ maybe t (\f -> f t) a

>     alias :: SParser (TableRef -> TableRef)
>     alias = do
>             p <- pos
>             optionalSuffix
>                  (TableAlias p) (optional (keyword "as") *> nameComponent)
>                  (FullAlias p) (parens $ commaSep1 nameComponent)
>     optionalJoinSuffix tr =
>       choice [try (joinSuffix tr)
>               >>= optionalAlias
>               >>= optionalJoinSuffix
>              ,return tr]
>     joinSuffix tr = do
>       let p = getAnnotation tr
>       (nat,hint,jt) <- joinKw
>       JoinTref p tr nat jt hint
>           <$> nonJoin
>           <*> onExpr
>     joinKw = do
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
>              hint <- option Nothing (Just <$> choice [Merge <$ keyword "merge"
>                                                      ,Loop <$ keyword "loop"
>                                                      ,Hash <$ keyword "hash"])
>              keyword "join"
>              return (n,hint,jt)
>     onExpr = choice
>              [Just <$> (JoinOn <$> pos <*> (keyword "on" *> expr))
>              ,Just <$> (JoinUsing <$> pos
>                                   <*> (keyword "using" *> columnNameList))
>              ,return Nothing]

>     tableHint = do

just acts as an incomplete recogniser for table hints:
it doesn't accept all valid table hints
and doesn't reject all invalid table hints
and it doesn't return anything in the ast
but allows you to parse some queries containing with hints and get
everything else

>           try $ keyword "with"
>           -- just allows any list of identifiers
>           _ <- parens $ commaSep1 idString
>           return ()
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
>           return $ MultiSetClause p l $ SpecialOp p (nm p "rowctor") r
>         ,do
>           p <- pos
>           l <- nameComponent
>           symbol "="
>           r <- expr
>           return $ SetClause p l r]

> nm :: Annotation -> Text -> Name
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
>       <*> (keyword "set" *> idString)
>       <*> ((keyword "to" <|> symbol "=") *>
>            commaSep1 sv)
>   where
>         sv = choice [
>               SetStr <$> pos <*> stringN
>              ,SetId <$> pos <*> idString
>              ,SetNum <$> pos <*> (try (fromInteger <$> integer)
>                                   <|> (read <$> T.unpack <$> numString))]
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
>     --parse the unordered list of attribute defs or constraints, for
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
> onSomething :: Text -> SParser Cascade
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
>                             if T.map toLower x `elem` [
>                                     "unique"
>                                    ,"primary"
>                                    ,"check"
>                                    ,"foreign"
>                                    ,"references"]
>                               then fail $ "not keyword (constraint name): " ++ T.unpack x
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
rather than just a string. TODO: maybe support other languages by
returning the function body just as a string instead of attempting to
parse it

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
>   flg <- getState
>   case parseBody flg lang body bodypos of
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
>         parseBody :: ParseFlags -> Language -> ScalarExpr -> MySourcePos
>                   -> Either String FnBody
>         parseBody flg lang body (fileName,line,col) =
>             case parseIt'
>                   (functionBody lang)
>                   flg
>                   fileName
>                   (Just (line,col))
>                   (L.fromChunks [extrStr body]) of
>                      Left er@(ParseErrorExtra {}) -> Left $ show er
>                      Right body' -> Right body'
>         -- sql function is just a list of statements, the last one
>         -- has the trailing semicolon optional
>         functionBody Sql = do
>            p <- pos
>            a <- many (try $ sqlStatement True)
>            -- this makes my head hurt, should probably write out
>            -- more longhand
>            SqlFnBody p <$> option a (((a++) . (:[])) <$> sqlStatement False)
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

> createDatabase :: SParser Statement
> createDatabase = CreateDatabase
>                <$> pos <* keyword "database"
>                <*> name

>
> dropSomething :: SParser Statement
> dropSomething = do
>   p <- pos
>   x <- try (choice [
>                  Domain <$ keyword "domain"
>                 ,Type <$ keyword "type"
>                 ,Table <$ keyword "table"
>                 ,View <$ keyword "view"
>                 ,Database <$ keyword "database"
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
>                          AntiTriggerEvent <$> splice 't'
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
> antiStatement = AntiStatement <$> splice 's'

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
>   where
>     itemList = commaSep1 selectItem
>     selectItem = pos >>= \p ->
>                  optionalSuffix
>                    (SelExp p) (starExpr <|> expr)
>                    (SelectItem p) (keyword "as" *> asAlias)

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
>        s <- pTypeNameString
>        choice [try (Prec2TypeName p s
>                     <$> (symbol "(" *> integer)
>                     <*> (symbol "," *> integer <* symbol ")"))
>               ,PrecTypeName p s <$> parens integer
>               ,arrayTypeName p s
>               ,return $ SimpleTypeName p s]
>     arrayTypeName p s = ArrayTypeName p (SimpleTypeName p s)
>                         <$ symbol "[" <* symbol "]"
>     pTypeNameString :: SParser Name
>     pTypeNameString = (Name <$> pos
>                        <*> choice [[Nmc "double precision"]
>                                    <$ try (keyword "double"
>                                            <* keyword "precision")
>                                   ,[Nmc "character varying"]
>                                    <$ try (keyword "character"
>                                            <* keyword "varying")])
>                       <|> name
>
> cascade :: SParser Cascade
> cascade = option Restrict (choice [
>                             Restrict <$ keyword "restrict"
>                            ,Cascade <$ keyword "cascade"])

> stmtEnd :: Bool -> SParser ()
> stmtEnd alwaysOptional = do
>   ss <- isSqlServer
>   if alwaysOptional || ss
>     then optional (symbol ";") >>= \_ -> return ()
>     else symbol ";" >>= \_ -> return ()

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
>        ] <* stmtEnd False
>     -- regular sql statements
>     ,sqlStatement True
>     -- regular plpgsql statements
>     ,choice [
>           declareStatement
>          ,setAssign
>          ,continue
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
>          <* stmtEnd False
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

> label :: SParser (Maybe Text)
> label = optional (symbol "<<" *> idString <* symbol ">>")
>
> block :: Annotation -> Maybe Text -> SParser Statement
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
> execute =
>   choice
>   [do
>    isSqlServer >>= guard
>    _ <- keyword "exec" <|> keyword "execute"
>    ExecStatement <$> pos
>      <*> name
>      <*> commaSep expr
>   ,Execute <$> (pos <* keyword "execute")
>            <*> expr]
>
> assignment :: SParser Statement
> assignment = Assignment
>              <$> pos
>              -- put the := in the first try to attempt to get a
>              -- better error if the code looks like malformed
>              -- assignment statement
>              <*> try (name <* (symbol ":=" <|> symbol "="))
>              <*> expr

> setAssign :: SParser Statement
> setAssign = do
>   isSqlServer >>= guard
>   Assignment <$> (pos <* keyword "set")
>              <*> name
>              <*> (symbol "=" *> expr)

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
> forStatement :: Annotation -> Maybe Text -> SParser Statement
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
> whileStatement :: Annotation -> Maybe Text -> SParser Statement
> whileStatement p l = WhileStatement p l
>                      <$> (keyword "while" *> expr <* keyword "loop")
>                      <*> many plPgsqlStatement <* keyword "end" <* keyword "loop"
> loopStatement :: Annotation -> Maybe Text -> SParser Statement
> loopStatement p l = LoopStatement p l
>                     <$> (keyword "loop" *> many plPgsqlStatement <* keyword "end" <* keyword "loop")
>

>
> ifStatement :: SParser Statement
> ifStatement = do
>   ss <- isSqlServer
>   if ss
>     -- no else if in sql server
>     -- no end if in sql server
>     then If
>          <$> (pos <* keyword "if")
>          <*> ((:[]) <$> (expr <.> someStatements ss))
>          <*> elsePart ss
>     else If
>          <$> (pos <* keyword "if")
>          <*> (ifPart ss <:> elseifParts ss)
>          <*> (elsePart ss <* endIf)
>   where
>     ifPart ss =
>       if ss
>       -- no then keyword in tsql
>       then expr <.> someStatements ss
>       else expr <.> (thn *> someStatements ss)
>     elseifParts ss = many ((elseif *> expr) <.> (thn *> someStatements ss))
>     elsePart ss = option [] (keyword "else" *> someStatements ss)
>     endIf = keyword "end" <* keyword "if"
>     thn = keyword "then"
>     elseif = keyword "elseif" <|> keyword "elsif"
>     (<.>) a b = (,) <$> a <*> b
>     someStatements ss =
>       if ss
>       then
>           -- sql server only allows multiple statements if wrapped
>           -- in a begin end block
>           choice [keyword "begin"
>                   *> many plPgsqlStatement
>                   <* keyword "end"
>                  ,(:[]) <$> plPgsqlStatement]
>       else many plPgsqlStatement

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

> declareStatement :: SParser Statement
> declareStatement = do
>   isSqlServer >>= guard
>   DeclareStatement
>     <$> pos
>     <*> (keyword "declare"
>          *> commaSep1 de)
>   where
>     de = (,,) <$> localVarName
>               <*> typeName
>               <*> optional (symbol "=" *> expr)
>     localVarName = do
>       i <- idString
>       guard (T.head i == '@')
>       return i


only limited support for tsql create index atm

> createIndex :: SParser Statement
> createIndex =
>   CreateIndexTSQL
>   <$> try (pos <* flavs <* keyword "index")
>   <*> nameComponent
>   <*> (keyword "on" *> name)
>   <*> parens (commaSep1 nameComponent) <* opts
>   where
>     flavs = do
>       _ <- optional $ keyword "unique"
>       _ <- optional (keyword "clustered" <|> keyword "nonclustered")
>       return ()
>     opts = do
>       _ <- optional $ do
>              _ <- keyword "include"
>              _ <- parens (commaSep1 nameComponent)
>              return ()
>       return ()

--------------------------------------------------------------------------------

expressions
===========

This is the bit that makes it the most obvious that I don't really
know haskell, parsing theory or parsec ... robbed a parsing example
from haskell-cafe and mainly just kept changing it until it seemed to
work

> expr :: SParser ScalarExpr
> expr = do
>   ParseFlags {pfDialect = d} <- getState
>   buildExpressionParser (table d) factor
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

start with the factors which start with parens - eliminate scalar
subqueries since they're easy to distinguish from the others then do in
predicate before row constructor, since an in predicate can start with
a row constructor looking thing, then finally vanilla parens

>        scalarSubQuery
>       ,try rowCtor
>       ,Parens <$> pos <*> parens expr

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
>       ,keywordFunction
>       ,identifier
>       ,Identifier <$> pos <*> (AntiName <$> splice 'n')
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
binary depending on the types of their operands

The full list of operators from a standard template1 database should
be used here.

TODO: handle the completely different list of sql server operators a
bit better

> tableAB :: SQLSyntaxDialect
>         -> Bool
>         -> [[Operator [Token] ParseState Identity ScalarExpr]]
> tableAB d isB = [[{-binary "." AssocLeft-}]
>          --[binary "::" (BinOpCall Cast) AssocLeft]
>          --missing [] for array element select
>         ,[prefix "-" "-"]
>         ,[binary "^" AssocLeft]
>         ,[binary "*" AssocLeft
>          ,idHackBinary "*" AssocLeft
>          ,binary "/" AssocLeft
>          ,binary "%" AssocLeft]
>         ,[binary "+" AssocLeft
>          ,binary "-" AssocLeft]
>         ,[postfixks ["is", "not", "null"] "isnotnull"
>          ,postfixks ["is", "null"] "isnull"]
>          --other operators all added in this list according to the pg docs:
>         ,[binary "<->" AssocNone
>          ,binary "<=" AssocRight
>          ,binary ">=" AssocRight
>          ,binary "||" AssocLeft]
>          ++ [prefix "@" "@" | d == PostgreSQLDialect]
>          --in should be here, but is treated as a factor instead
>          --between
>          --overlaps
>         ,[binaryk "like" "like" AssocNone
>          ,binaryks ["not","like"] "notlike" AssocNone
>          ,binarycust (symbol "!=") "<>" AssocNone]
>          --(also ilike similar)
>         ,[binary "<" AssocNone
>          ,binary ">" AssocNone]
>         ,[binary "=" AssocRight
>          ,binary "<>" AssocNone]
>         ,[notNot
>          ,prefixk "not" "not"
>          ]
>         ,let x = [binaryk "or" "or" AssocLeft]
>          in if isB
>             then x
>             else binaryk "and" "and" AssocLeft : x
>          ]
>     where
>       binary s = binarycust (symbol s) s
>       -- '*' is lexed as an id token rather than a symbol token, so
>       -- work around here
>       idHackBinary s = binarycust (keyword s) s
>       binaryk = binarycust . keyword
>       binaryks = binarycust . mapM_ keyword
>       prefix = prefCust Prefix . symbol
>       prefixk = prefCust Prefix . keyword
>       postfixks = postCust Postfix . mapM_ keyword
>       binarycust opParse t =
>         Infix $ try $
>           BinaryOp <$> pos <*> (nm emptyAnnotation t <$ opParse)
>       prefCust ctor opParse t =
>         ctor $ try $
>           PrefixOp <$> pos <*> (nm emptyAnnotation t <$ opParse)
>       postCust ctor opParse t =
>         ctor $ try $
>           PostfixOp <$> pos <*> (nm emptyAnnotation t <$ opParse)
>       -- hack - haven't worked out why parsec buildexpression parser won't
>       -- parse something like "not not EXPR" without parens so hack here
>       notNot =
>         Prefix $ try $ do
>                       p1 <- pos
>                       keyword "not"
>                       p2 <- pos
>                       keyword "not"
>                       return $ PrefixOp p1 (nm p1 "not")
>                                . PrefixOp p2 (nm p2 "not")

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

> table :: SQLSyntaxDialect -> [[Operator [Token] ParseState Identity ScalarExpr]]
> table d = tableAB d False

> tableB :: SQLSyntaxDialect -> [[Operator [Token] ParseState Identity ScalarExpr]]
> tableB d = tableAB d True

use the same factors

> b_expr :: SParser ScalarExpr
> b_expr = do
>          ParseFlags {pfDialect = d} <- getState
>          buildExpressionParser (tableB d) factor
>          <?> "expression"
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

(expr) parses to just expr rather than row(expr)
and () is a syntax error.

> rowCtor :: SParser ScalarExpr
> rowCtor = SpecialOp
>           <$> pos
>           <*> (nm <$> pos <*> return "rowctor")
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
>   guard (T.all (`elem` digChars) l)
>   return $ read $ T.unpack l
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
> arrayLit = SpecialOp <$> pos <* keyword "array"
>                    <*> (nm <$> pos <*> return "arrayctor")
>                    <*> squares (commaSep expr)
>
> arraySubSuffix :: ScalarExpr -> SParser ScalarExpr
> arraySubSuffix e = SpecialOp <$> pos
>                    <*> (nm <$> pos <*> return "arraysub")
>                    <*> ((e:) <$> squares (commaSep1 expr))
>
> windowFnSuffix :: ScalarExpr -> SParser ScalarExpr
> windowFnSuffix e = WindowApp <$> pos <*> return e
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
>   return $ SpecialOp p (nm p "between") [a,b,c]

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
> functionCallSuffix ex =
>  -- bit hacky, if exp is a identifier or qualified, then
>  -- convert to function, also support antiquote for function name
>    case ex of
>      Identifier _ nm' -> fn nm'
>      s -> fail $ "cannot make functioncall from " ++ show s
>    where
>      fn :: Name -> SParser ScalarExpr
>      fn fnName = do
>        p <- pos
>        (di,as,ob) <- parens
>                      $ choice [ -- handle a single *
>                                (Nothing,,[]) <$> ((:[]) <$> (Star <$> pos <* symbol "*"))
>                               ,(,,)
>                                <$> optionMaybe
>                                     (choice [Distinct <$ keyword "distinct"
>                                             ,All <$ keyword "all"])
>                                <*> commaSep expr
>                                <*> orderBy]
>        return $ case (di,ob) of
>          (Nothing,[]) -> App p fnName as
>          (d,o) -> AggregateApp p (fromMaybe All d) (App p fnName as) o

these won't parse as normal functions because they use keywords so do
a special case for them

> keywordFunction :: SParser ScalarExpr
> keywordFunction = try $ do
>   p <- pos
>   i <- nameComponentAllows kfs
>   unless (iskfs i) $ fail "not any or all"
>   functionCallSuffix (Identifier p (Name p [i]))
>   where
>     kfs = ["any","all","isnull"]
>     iskfs (Nmc n) | T.map toLower n `elem` kfs = True
>     iskfs _ = False

>
> castKeyword :: SParser ScalarExpr
> castKeyword =
>    choice
>    [do
>     -- parse tsql convert function to cast ast
>     isSqlServer >>= guard
>     p <- pos
>     _ <- keyword "convert" <* symbol "("
>     tn <- typeName
>     _ <- symbol ","
>     e <- expr
>     -- ignores the style
>     _ <- optional $ symbol "," *> integer
>     _ <- symbol ")"
>     return $ Cast p e tn
>    ,Cast
>     <$> pos <* keyword "cast" <* symbol "("
>     <*> expr
>     <*> (keyword "as" *> typeName <* symbol ")")]
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
>             return $ SpecialOp p (nm p "substring") [a,b,c]
>

------------------------------------------------------------

identifier parsing, quite a few variations ...

parse x.y, x.y.z, etc.

> qualIdSuffix :: ScalarExpr -> SParser ScalarExpr
> qualIdSuffix (Identifier _p (Name _ is)) =
>   qualIdX is

> qualIdSuffix e = do
>     p <- pos
>     i1 <- symbol "." *> nameComponent
>     return $ BinaryOp p (nm p ".") e (Identifier p (Name p [i1]))

> qualIdX :: [NameComponent] -> SParser ScalarExpr
> qualIdX is = do
>   ss <- isSqlServer
>   if ss
>     then sqls
>     else regular
>   where
>     regular = do
>               p <- pos
>               i1 <- symbol "." *> nameComponent
>               return $ Identifier p (Name p (is ++ [i1]))
>     sqls = do
>            p <- pos
>            numDots <- readNDots
>            let eis = replicate (numDots - 1) (Nmc "")
>            i1 <- nameComponent
>            return $ Identifier p (Name p (is ++ eis ++ [i1]))

the cranky lexer lexes lots of dots ......
as mixtures of symbol "." and symbol ".."
we just want to know how many dots in a row there are

> readNDots :: SParser Int
> readNDots = sum <$> many1 (choice [1 <$ symbol "."
>                                   ,2 <$ symbol ".."])



> identifier :: SParser ScalarExpr
> identifier = Identifier <$> pos <*> name

see sql-keywords appendix in pg manual

doesn't deal with non-reserved, and reserved with qualifications yet
(e.g. the may be function or type catagory), and other categories

I think the categories should be:
reserved
reserved but can be function or type
not reserved but cannot be function or type

these categories affect the parser and typechecker differently

(the parser has to parse some reserved and 'not reserved but cannot be
function or type' keywords as function names, maybe there are others)

> reservedWords :: SParser [Text]
> reservedWords = do
>   ss <- isSqlServer
>   if not ss
>     then return
>        ["all"
>        ,"analyse"
>        ,"analyze"
>        ,"and"
>        ,"any"
>        ,"array"
>        ,"as"
>        ,"asc"
>        ,"symmetric"
>        ,"authorization"
>        ,"binary"
>        ,"both"
>        ,"case"
>        ,"cast"
>        ,"check"
>        ,"collate"
>        ,"column"
>        ,"concurrently"
>        ,"constraint"
>        ,"create"
>        ,"cross"
>        ,"current_catalog"
>        ,"current_date"
>        ,"current_role"
>        ,"current_time"
>        ,"current_timestamp"
>        ,"current_user"
>        ,"default"
>        ,"deferrable"
>        ,"desc"
>        ,"distinct"
>        ,"do"
>        ,"else"
>        ,"end"
>        ,"except"
>        ,"false"
>        ,"fetch"
>        ,"for"
>        ,"freeze"
>        ,"from"
>        ,"full"
>        ,"grant"
>        ,"group"
>        ,"having"
>        ,"ilike"
>        ,"in"
>        ,"initially"
>        ,"inner"
>        ,"intersect"
>        ,"into"
>        ,"is"
>        ,"isnull"
>        ,"join"
>        ,"leading"
>        ,"left"
>        ,"like"
>        ,"limit"
>        ,"localtime"
>        ,"localtimestamp"
>        ,"natural"
>        ,"not"
>        ,"notnull"
>        ,"null"
>        ,"offset"
>        ,"on"
>        ,"only"
>        ,"or"
>        ,"order"
>        ,"outer"
>        ,"over"
>        ,"overlaps"
>        ,"placing"
>        ,"primary"
>        ,"references"
>        ,"returning"
>        ,"right"
>        ,"select"
>        ,"session_user"
>        ,"similar"
>        ,"some"
>        ,"symmetric"
>        ,"table"
>        ,"then"
>        ,"to"
>        ,"trailing"
>        ,"true"
>        ,"union"
>        ,"unique"
>        ,"user"
>        ,"using"
>        ,"variadic"
>        ,"verbose"
>        ,"when"
>        ,"where"
>        ,"window"
>        ,"with"
>        --extras for hssqlppp: Todo: fix this
>        ,"loop"]

sql server keywords from this page:
http://msdn.microsoft.com/en-us/library/ms189822.aspx
(this is the list for 2008 R2)

>     else return
>        ["add"
>        ,"all"
>        ,"alter"
>        ,"and"
>        ,"any"
>        ,"as"
>        ,"asc"
>        ,"authorization"
>        ,"backup"
>        ,"begin"
>        ,"between"
>        ,"break"
>        ,"browse"
>        ,"bulk"
>        ,"by"
>        ,"cascade"
>        ,"case"
>        ,"check"
>        ,"checkpoint"
>        ,"close"
>        ,"clustered"
>        ,"coalesce"
>        ,"collate"
>        ,"column"
>        ,"commit"
>        ,"compute"
>        ,"constraint"
>        ,"contains"
>        ,"containstable"
>        ,"continue"
>        ,"convert"
>        ,"create"
>        ,"cross"
>        ,"current"
>        ,"current_date"
>        ,"current_time"
>        ,"current_timestamp"
>        ,"current_user"
>        ,"cursor"
>        ,"database"
>        ,"dbcc"
>        ,"deallocate"
>        ,"declare"
>        ,"default"
>        ,"delete"
>        ,"deny"
>        ,"desc"
>        ,"disk"
>        ,"distinct"
>        ,"distributed"
>        --,"double"
>        ,"drop"
>        ,"dump"
>        ,"else"
>        ,"end"
>        ,"errlvl"
>        ,"escape"
>        ,"except"
>        ,"exec"
>        ,"execute"
>        ,"exists"
>        ,"exit"
>        ,"external"
>        ,"fetch"
>        ,"file"
>        ,"fillfactor"
>        ,"for"
>        ,"foreign"
>        ,"freetext"
>        ,"freetexttable"
>        ,"from"
>        ,"full"
>        ,"function"
>        ,"goto"
>        ,"grant"
>        ,"group"
>        ,"having"
>        ,"holdlock"
>        ,"identity"
>        ,"identity_insert"
>        ,"identitycol"
>        ,"if"
>        ,"in"
>        ,"index"
>        ,"inner"
>        ,"insert"
>        ,"intersect"
>        ,"into"
>        ,"is"
>        ,"join"
>        ,"key"
>        ,"kill"
>        ,"left"
>        ,"like"
>        ,"lineno"
>        ,"load"
>        ,"merge"
>        ,"national"
>        ,"natural"
>        ,"nocheck"
>        ,"nonclustered"
>        ,"not"
>        ,"null"
>        ,"nullif"
>        ,"of"
>        ,"off"
>        ,"offsets"
>        ,"on"
>        ,"open"
>        ,"opendatasource"
>        ,"openquery"
>        ,"openrowset"
>        ,"openxml"
>        ,"option"
>        ,"or"
>        ,"order"
>        ,"outer"
>        ,"over"
>        ,"percent"
>        ,"pivot"
>        -- ,"plan" -- temporarily unkeyword this
>        ,"precision"
>        ,"primary"
>        ,"print"
>        ,"proc"
>        ,"procedure"
>        ,"public"
>        ,"raiserror"
>        ,"read"
>        ,"readtext"
>        ,"reconfigure"
>        ,"references"
>        ,"replication"
>        ,"restore"
>        ,"restrict"
>        ,"return"
>        ,"revert"
>        ,"revoke"
>        ,"right"
>        ,"rollback"
>        ,"rowcount"
>        ,"rowguidcol"
>        ,"rule"
>        ,"save"
>        ,"schema"
>        ,"securityaudit"
>        ,"select"
>        ,"session_user"
>        ,"set"
>        ,"setuser"
>        ,"shutdown"
>        ,"some"
>        ,"statistics"
>        ,"system_user"
>        ,"table"
>        ,"tablesample"
>        ,"textsize"
>        ,"then"
>        ,"to"
>        ,"top"
>        ,"tran"
>        ,"transaction"
>        ,"trigger"
>        ,"truncate"
>        ,"tsequal"
>        ,"union"
>        ,"unique"
>        ,"unpivot"
>        ,"update"
>        ,"updatetext"
>        ,"use"
>        ,"user"
>        ,"values"
>        ,"varying"
>        ,"view"
>        ,"waitfor"
>        ,"when"
>        ,"where"
>        ,"while"
>        ,"with"
>        ,"writetext"]

> ncs :: SParser [NameComponent]
> ncs = do
>   ss <- isSqlServer
>   if ss
>     then do
>       -- what a mess
>       let another = do
>             numDots <- readNDots
>             let empties = replicate (numDots - 1) (Nmc "")
>             nc <- nameComponent
>             suf <- choice [another
>                           ,return []]
>             return $ empties ++ [nc] ++ suf
>       i <- nameComponent
>       suf <- choice [another
>                     ,return []]
>       return (i:suf)
>     else
>       sepBy1 nameComponent (symbol ".")

parse a complete name

> name :: SParser Name
> name = choice [AntiName <$> splice 'n'
>               ,Name <$> pos <*> ncs
>               ]

> nameComponent :: SParser NameComponent
> nameComponent = nameComponentAllows []

parser for a name component where you supply the exceptions to the
reserved identifier list to say you want these to parse successfully
instead of failing with a no keyword error

> nameComponentAllows :: [Text] -> SParser NameComponent
> nameComponentAllows allows = do
>   p <- pos
>   x <- unrestrictedNameComponent
>   rw <- reservedWords
>   case x of
>     Nmc n | T.map toLower n `elem` allows -> return x
>           | T.map toLower n `elem` rw ->
>               fail $ "no keywords " ++ show p ++ " " ++ T.unpack n
>     _ -> return x

ignore reserved keywords completely

> unrestrictedNameComponent :: SParser NameComponent
> unrestrictedNameComponent =
>   choice [Nmc <$> idString
>          ,QNmc <$> qidString
>          ,AntiNameComponent <$> splice 'm']

quick hack to support identifiers like this: 'test' for sql server

not sure how to do this properly, you must not be able to write
strings in sql server using single quotes.

> asAlias :: SParser NameComponent
> asAlias = do
>   ss <- isSqlServer
>   if ss
>     then choice [nameComponent
>                 ,do
>                  s <- stringN
>                  return $ QNmc s]
>     else nameComponent

--------------------------------------------------------------------------------

Utility parsers
===============

> keyword :: Text -> SParser ()
> keyword k = mytoken (\tok -> case tok of
>                                IdStringTok i | lcase k == lcase i -> Just ()
>                                _ -> Nothing)
>                       where
>                         lcase = T.map toLower
>
> idString :: SParser Text
> idString = mytoken (\tok -> case tok of
>                                      IdStringTok i -> Just i
>                                      _ -> Nothing)
> qidString :: SParser Text
> qidString = mytoken (\tok -> case tok of
>                                      QIdStringTok i -> Just i
>                                      _ -> Nothing)

> splice :: Char -> SParser Text
> splice c = mytoken (\tok -> case tok of
>                                SpliceTok c' i | c == c' -> Just i
>                                _ -> Nothing)

>
> symbol :: Text -> SParser ()
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
> antiScalarExpr = AntiScalarExpr <$> splice 'e'
>
> placeholder :: SParser ScalarExpr
> placeholder = (Placeholder <$> pos) <* symbol "?"
>
> numString :: SParser Text
> numString = mytoken (\tok -> case tok of
>                                     NumberTok n -> Just n
>                                     _ -> Nothing)

>
> liftStringTok :: SParser Text
> liftStringTok = mytoken (\tok ->
>                   case tok of
>                            StringTok _ s -> Just s
>                            _ -> Nothing)

> stringLit :: SParser ScalarExpr
> stringLit = choice
>             [StringLit <$> pos <*> liftStringTok
>              -- bit crap at the moment, not sure how to fix without
>              -- mangling the ast types
>             ,StringLit <$> pos <*> ((\s -> T.concat ["$s(",s,")"]) <$> splice 's')
>             ]
>
> stringN :: SParser Text
> stringN = mytoken (\tok ->
>                   case tok of
>                            StringTok _ s -> Just s
>                            _ -> Nothing)

> extrStr :: ScalarExpr -> Text
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

> matchAKeyword :: [(Text, a)] -> SParser a
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

there must be a standard way of writing this

> optionalSuffix :: (Stream s m t2) =>
>                   (t1 -> b)
>                -> ParsecT s u m t1
>                -> (t1 -> a -> b)
>                -> ParsecT s u m a
>                -> ParsecT s u m b
> optionalSuffix c1 p1 c2 p2 = do
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

wrapper for parsec source positions, probably not really useful

> type MySourcePos = (String,Int,Int)
>
> toMySp :: SourcePos -> MySourcePos
> toMySp sp = (sourceName sp,sourceLine sp,sourceColumn sp)

parser combinator to return the current position as an ast annotation

> pos :: SParser Annotation
> pos =
>   (\a -> emptyAnnotation {anSrc = Just a}) <$> toMySp <$> getPosition

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
binop operator [expr1,app [any|some|all] [expr2,...]]

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
>              BinaryOp an op expr1 (App _ fn (expr2s:expr3s))
>                | Name _ [Nmc fnm] <- fn
>                , Just flav <- case T.map toLower fnm of
>                                  "any" -> Just LiftAny
>                                  "some" -> Just LiftAny
>                                  "all" -> Just LiftAll
>                                  _ -> Nothing
>                -> LiftApp an op flav ([expr1,expr2s] ++ expr3s)
>              x1 -> x1
