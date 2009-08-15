Copyright 2009 Jake Wheat

The main file for parsing sql, uses parsec (badly). Only uses a lexer
in a few places which may both be wrong and a massive design flaw. Not
sure if parsec is the right choice either. Whitespace is handled in
the lexeme style.

> module Parser(
>               --parse fully formed sql statements from a string
>               parseSql
>               --parse a file containing sql statements only
>              ,parseSqlFile
>               --parse an expression (one expression plus whitespace
>               --only allowed
>              ,parseExpression
>               --convert a parse error to string plus some source
>               --with highlights
>              ,showEr
>              )
>     where

> import Text.Parsec
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language
> import Text.Parsec.Expr
> import Text.Parsec.String
> import Text.Parsec.Error

> import Data.Maybe
> import Control.Monad.Identity


> import Tree

see
http://savage.net.au/SQL/sql-2003-2.bnf.html
and
http://savage.net.au/SQL/sql-92.bnf.html
for some online sql grammar guides
and
http://www.postgresql.org/docs/8.4/interactive/sql-syntax.html
for some notes on postgresql syntax (the rest of that manual is also helpful)

================================================================================

= Top level parsing functions

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

= Parsing top level statements

> statements :: ParsecT String () Identity [Statement]
> statements = do
>   whitespace
>   s <- many statement
>   eof
>   return s

parse a statement

a lot of trys are used because there is a lot of overlap - the main
problem with this is the error messages end up being a bit crap

factoring it to work better will probably proper mangle the code
readability-wise, maybe some sort of code generator can be used for
this bit

no attempt is made to reject plpgsql only statements outside of a
function or inside a sql function

> statement :: ParsecT String () Identity Statement
> statement = do
>   (do
>    s <- choice [
>          try select
>         ,try insert
>         ,try update
>         ,try delete
>         ,(try (keyword "create") >>
>           choice [createTable
>                  ,createType
>                  ,createFunction
>                  ,createView
>                  ,createDomain])
>         ,(try (keyword "drop") >> dropFunction)
>         ,try execute
>         ,try assignment
>         ,try ifStatement
>         ,try returnSt
>         ,try raise
>         ,try forStatement
>         ,try perform
>         ,nullStatement]
>    semi
>    return s)
>    <|> copy

quick hack to support sql functions where the semicolon on the last
statement is optional. We only bother with sql statements

> statementOptionalSemi :: ParsecT String () Identity Statement
> statementOptionalSemi = do
>   (do
>    s <- choice [try select
>         ,try insert
>         ,try update
>         ,try delete
>         ,(do
>               keyword "create"
>               choice [try createTable
>                ,createType
>                ,createFunction
>                ,createView
>                ,createDomain])
>         ,copy]
>    maybeP semi
>    eof
>    return s)

================================================================================

statement flavour parsers

top level/sql statements first

= select

select parser, parses things starting with the keyword 'select'

supports plpgsql 'select into' only for the variant which looks like
'select into ([targets]) [columnNames] from ...

recurses to support parsing excepts, unions, etc

> select :: ParsecT String () Identity Statement
> select = do
>   keyword "select"
>   s1 <- selQuerySpec
>   choice [

don't know if this does associativity in the correct order for
statements with multiple excepts/ intersects and no parens

>     try $ do keyword "except"
>              s2 <- select
>              return $ CombineSelect Except s1 s2
>    ,try $ do keyword "intersect"
>              s3 <- select
>              return $ CombineSelect Intersect s1 s3
>    ,try $ do keyword "union"
>              s3 <- select
>              return $ CombineSelect Union s1 s3
>    ,return s1]

= insert, update and delete

insert statement: supports option column name list,
multiple rows to insert and insert from select statements

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
>   rt <- maybeP returning
>   return $ Insert tableName atts ida rt

> update :: ParsecT String () Identity Statement
> update = do
>   keyword "update"
>   tableName <- identifierString
>   keyword "set"
>   scs <- commaSep1 setClause
>   wh <- maybeP whereClause
>   rt <- maybeP returning
>   return $ Update tableName scs wh rt

> delete :: ParsecT String () Identity Statement
> delete = do
>   keyword "delete"
>   keyword "from"
>   tableName <- identifierString
>   wh <- maybeP whereClause
>   rt <- maybeP returning
>   return $ Delete tableName wh rt

= copy statement

copy: just reads the string in for now - read lines until we get to
one with just a \. in the first two columns

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

= ddl

> createTable :: ParsecT String () Identity Statement
> createTable = do
>   try $ keyword "table"
>   n <- identifierString

parse our unordered list of attribute defs or constraints

>   (cons, atts) <- parens $ parseABsep1
>                   (try tableConstr)
>                   tableAtt
>                   (symbol ",")
>   return $ CreateTable n atts cons

couldn't work how to to perms so just did this hack instead
e.g.
a1,a2,b1,b2,a2,b3,b4 parses to ([a1,a2,a3],[b1,b2,b3,b4])

> parseABsep1 :: (Stream s m t) =>
>                ParsecT s u m a1
>             -> ParsecT s u m a
>             -> ParsecT s u m sep
>             -> ParsecT s u m ([a1], [a])

> parseABsep1 p1 p2 sep = do
>   rs <- sepBy1 parseAorB sep
>   let (r1, r2) = unzip rs
>   return (catMaybes r1, catMaybes r2)
>   where
>     parseAorB = choice [
>                   do
>                   x <- p1
>                   return (Just x,Nothing)
>                  ,do
>                   y <- p2
>                   return (Nothing, Just y)]

> createType :: ParsecT String () Identity Statement
> createType = do
>   keyword "type"
>   n <- identifierString
>   keyword "as"
>   atts <- parens $ commaSep1 typeAtt
>   return $ CreateType n atts

create function, support sql functions and
plpgsql functions. Actually parses the body in both cases
and provides a statement list for the body rather than just
a string

> createFunction :: GenParser Char () Statement
> createFunction = do
>   keyword "function"
>   fnName <- identifierString
>   params <- parens $ commaSep param
>   keyword "returns"
>   retType <- retTypeName
>   keyword "as"
>   body <- stringLiteral
>   keyword "language"
>   lang <- (keyword "plpgsql" >> return Plpgsql)
>           <|> (keyword "sql" >> return Sql)

if we have an error parsing the body, collect all the needed info
from that error and rethrow it

>   case parse (functionBody lang) ("function " ++ fnName) (extrStr body) of
>     Left e -> do
>       sp <- getPosition
>       error $ "in " ++ show sp ++ ", " ++ showEr e (extrStr body)
>     Right body' -> do
>                     vol <- choice [
>                             keyword "volatile" >> return Volatile
>                            ,keyword "stable" >> return Stable
>                            ,keyword "immutable" >> return Immutable]
>                     return $ CreateFunction lang fnName params
>                                retType (quoteOfString body) body' vol

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

> dropFunction :: ParsecT String () Identity Statement
> dropFunction = do
>   keyword "function"
>   nm <- identifierString
>   ts <- parens $ many identifierString
>   return $ DropFunction nm ts

================================================================================

= component parsers for sql statements

select bits

> selQuerySpec :: ParsecT String () Identity Statement
> selQuerySpec = do
>   sl <- selectList
>   fr <- maybeP from
>   wh <- maybeP whereClause
>   ord <- maybeP orderBy
>   li <- maybeP limit
>   return $ Select sl fr wh ord li

> orderBy :: GenParser Char () [Expression]
> orderBy = do
>           keyword "order"
>           keyword "by"
>           commaSep1 expr

> from :: GenParser Char () From
> from = do
>        keyword "from"
>        liftM From tref

> whereClause :: ParsecT String () Identity Where
> whereClause = do
>   keyword "where"
>   ex <- expr
>   return $ Where ex

> limit :: GenParser Char () Expression
> limit = keyword "limit" >> expr

== table refs
used in the from part of a select
have to cope with:
a simple tableref i.e just a name
an aliased table ref e.g. select a.b from tbl as a
a sub select e.g. select a from (select b from c)
 - these are handled in tref
then cope with joins recursively using joinpart below

> tref :: ParsecT String () Identity TableRef
> tref = do
>        tr1 <- choice [
>                do
>                sub <- parens select
>                keyword "as"
>                alias <- identifierString
>                return $ SubTref sub alias
>               ,do
>                  fc <- try $ functionCall
>                  alias <- maybeP $ do
>                             keyword "as"
>                             identifierString
>                  case alias of
>                    Nothing -> return $ TrefFun fc
>                    Just a -> return $ TrefFunAlias fc a
>               ,do
>                a <- identifierString
>                b <- maybeP (do
>                             whitespace
>                             x <- identifierString

avoid all these keywords as aliases since they can appear immediately
following a tableref as the next part of the statement, if we don't do
this then lots of things don't parse.

>                             if x `elem` ["where"
>                                         ,"except"
>                                         ,"union"
>                                         ,"intersect"
>                                         ,"loop"
>                                         ,"inner"
>                                         ,"on"
>                                         ,"left"
>                                         ,"right"
>                                         ,"full"
>                                         ,"cross"
>                                         ,"natural"
>                                         ,"order"
>                                         ,"limit"
>                                         ,"using"]
>                               then fail "not keyword"
>                               else return x)
>                return $ case b of
>                                Nothing -> Tref a
>                                Just b1 -> TrefAlias a b1]
>        jn <- maybeP $ joinPart tr1
>        case jn of
>          Nothing -> return tr1
>          Just jn1 -> return jn1

joinpart: parse a join after the first part of the tableref
(which is a table name, aliased table name or subselect)
 - takes this tableref as an arg so it can recurse to
multiple joins

> joinPart :: TableRef -> GenParser Char () TableRef
> joinPart tr1 = do

look for the join flavour first

>   nat <- maybeP $ keyword "natural"
>   typ <- choice [
>             keyword "inner" >> return Inner
>            ,keyword "left" >> keyword "outer" >> return LeftOuter
>            ,keyword "right" >> keyword "outer" >> return RightOuter
>            ,keyword "full" >> keyword "outer" >> return FullOuter
>            ,keyword "cross" >> return Cross]
>   keyword "join"

recurse back to tref to read the table

>   tr2 <- tref

now try and read the join condition

>   onex <- maybeP (do
>                  keyword "on"
>                  liftM JoinOn expr)
>   usingx <- maybeP (keyword "using" >>
>                     (liftM JoinUsing $ parens $ commaSep1 identifierString))
>   let jp1 = JoinedTref tr1 (isJust nat) typ tr2 $ case onex of
>                                                             Just a -> Just a
>                                                             Nothing -> usingx

see if there's another join waiting

>   jp2 <- maybeP $ joinPart jp1
>   case jp2 of
>     Nothing -> return jp1
>     Just j -> return j

selectlist and selectitem: the bit between select and from
check for into either before the whole list of select columns
or after the while list

> selectList :: ParsecT String () Identity SelectList
> selectList = do
>   i1 <- readInto
>   sl <- commaSep1 selectItem
>   i2 <- case i1 of
>           Just _ -> return i1
>           Nothing -> readInto
>   return $ SelectList sl i2
>   where
>     readInto = maybeP $ do
>                         keyword "into"
>                         commaSep1 identifierStringMaybeDot


> selectItem :: ParsecT String () Identity SelectItem
> selectItem = do
>        ex <- expr
>        i <- maybeP (do
>                     keyword "as"
>                     identifierString)
>        return $ case i of
>                   Nothing -> SelExp ex
>                   Just iden -> SelectItem ex iden

> returning :: ParsecT String () Identity SelectList
> returning = do
>   keyword "returning"
>   selectList

== update

set clause - the set a = 3, b=4 part of a update statement

> setClause :: ParsecT String () Identity SetClause
> setClause = do
>   ref <- identifierString
>   symbol "="
>   ex <- expr
>   return $ SetClause ref ex

== ddl

tableatt - an single attribute line in a create table

> tableAtt :: ParsecT String () Identity AttributeDef
> tableAtt = do
>   name <- identifierString
>   typ <- identifierString
>   def <- maybeP (do
>                   keyword "default"
>                   expr)
>   constr <- sepBy inlineConstraint whitespace
>   return $ AttributeDef name typ def constr

> tableConstr :: ParsecT String () Identity Constraint
> tableConstr = do
>   (try $ do
>      keyword "unique"
>      liftM UniqueConstraint $ parens $ commaSep1 identifierString)

> inlineConstraint :: ParsecT String () Identity InlineConstraint
> inlineConstraint =
>   choice [
>           keyword "unique" >> return InlineUniqueConstraint
>          ,keyword "check" >> liftM InlineCheckConstraint (parens expr)
>          ,try (keyword "null") >> return NullConstraint
>          ,keyword "not" >> keyword "null"  >> return NotNullConstraint
>          ]


typeatt: like a cut down version of tableatt, used in create type

> typeAtt :: ParsecT String () Identity TypeAttributeDef
> typeAtt = liftM2 TypeAttDef identifierString identifierString

> retTypeName :: ParsecT String () Identity Expression
> retTypeName =
>   choice [
>     try $ do
>       keyword "setof"
>       i <- parseBasicType
>       return $ UnOpCall SetOf i
>    ,parseBasicType]
>   where
>     parseBasicType = do
>       t <- identifierString
>       pr <- maybeP $ parens $ integer
>       case pr of
>               Nothing -> return $ Identifier t
>               Just p -> return $ FunCall t [IntegerL p]


================================================================================

= plpgsql statements

null statement is plpgsql nop, written 'null;'

> nullStatement :: ParsecT String u Identity Statement
> nullStatement = do
>   keyword "null"
>   return NullStatement

> perform :: ParsecT String () Identity Statement
> perform = do
>   keyword "perform"
>   ex <- expr
>   return $ Perform ex

> execute :: ParsecT String () Identity Statement
> execute = do
>   keyword "execute"
>   liftM Execute expr

> assignment :: ParsecT String () Identity Statement
> assignment = do
>   n <- identifierStringMaybeDot
>   symbol ":="
>   ex <- expr
>   return $ Assignment n ex

> returnSt :: ParsecT String () Identity Statement
> returnSt = do
>   keyword "return"
>   ((try $ keyword "next" >> liftM ReturnNext expr)
>    <|> (liftM Return $ maybeP expr))


> raise :: ParsecT String () Identity Statement
> raise = do
>   keyword "raise"
>   tp <- choice [
>          keyword "notice" >> return RNotice
>         ,try (keyword "exception" >> return RException)
>         ,keyword "error" >> return RError]
>   s <- stringLiteral
>   exps <- maybeP (do
>                    symbol ","
>                    commaSep expr)
>   return $ Raise tp (extrStr s) (fromMaybe [] exps)

for statement, only supports for x in [select statement]
flavour at the moment

> forStatement :: GenParser Char () Statement
> forStatement = do
>   keyword "for"
>   i <- identifierString
>   keyword "in"
>   choice [
>     do
>       st <- try select
>       stmts <- theRest
>       return $ ForSelectStatement i st stmts
>    ,do
>      st <- expr
>      symbol ".."
>      en <- expr
>      stmts <- theRest
>      return $ ForIntegerStatement i st en stmts]
>   where
>     theRest = do
>       keyword "loop"
>       stmts <- many statement
>       keyword "end"
>       keyword "loop"
>       return stmts


if statement, no support for elsif yet

> ifStatement :: ParsecT String () Identity Statement
> ifStatement = do
>   keyword "if"
>   e <- expr
>   keyword "then"
>   st <- many statement
>   sts <- many (try $ do
>                keyword "elseif"
>                e1 <- expr
>                keyword "then"
>                st1 <- many statement
>                return (e1, st1)
>                )
>   elsSts <- maybeP (do
>                      keyword "else"
>                      many statement)
>   keyword "end"
>   keyword "if"
>   return $ If ((e,st):sts) elsSts

================================================================================

= statement components for plpgsql

> functionBody :: Language -> ParsecT String () Identity FnBody

sql function is just a list of statements, the last one has the
trailing semicolon optional

> functionBody Sql = do
>   whitespace
>   a <- (many (try statement))
>   b <- maybeP statementOptionalSemi
>   return $ SqlFnBody $ case b of
>                               Nothing -> a
>                               Just e -> a ++ [e]

plpgsql function has an optional declare section, plus the statements
are enclosed in begin ... end;

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

params to a function

> param :: ParsecT String () Identity ParamDef
> param = do
>   name <- identifierString
>   t <- maybeP identifierString
>   case t of
>     Just tp -> return $ ParamDef name tp
>     Nothing -> return $ ParamDefTp name

variable declarations in a plpgsql function

> varDef :: ParsecT String () Identity VarDef
> varDef = do
>   name <- identifierString
>   tp <- identifierString
>   val <- maybeP (do
>                  symbol ":="
>                  expr)
>   semi
>   return $ VarDef name tp val

================================================================================

= expressions

This is the bit that makes it the most obvious that I don't really
know haskell, parsing theory or parsec ... robbed a parsing example
from haskell-cafe and mainly just kept changing it until it seemed to
work

> expr :: Parser Expression
> expr = buildExpressionParser table factor
>        <?> "expression"

> factor :: GenParser Char () Expression
> factor = choice [

order these so the ones which can be valid prefixes of others
appear further down the list

start with the ones which start with parens - eliminate scalar
subquerys since they're easy to distinguish from the others then do in
predicate before row constructor, since an in predicate can start with
a row constructor, then finally vanilla parens

>           try scalarSubQuery
>          ,try inPredicate
>          ,try rowCtor
>          ,parens expr

we have two things which can start with a $,
do the position arg first, then we can unconditionally
try the dollar quoted string next

>          ,try positionalArg

string using quotes don't start like anything else and we've
already tried the other thing which starts with a $, so can
parse without a try

>          ,stringLiteral

anything starting with a number has to be a number, so this
could probably appear anywhere in the list

>          ,integerLit

put the factors which start with keywords before the ones which start
with a function, I think these all need try because functions can
start with the same letters as these keywords, and they have to be
tried after these. This claim might be wrong

>          ,try caseParse
>          ,try exists
>          ,try booleanLiteral
>          ,try nullL
>          ,try array

now the ones starting with a function name, since a function call
looks like the start of a window expression, try the window expression
first

>          ,try windowFn

try function call before identifier for same reason

>          ,try functionCall
>          ,try identifier]

== operator table

proper hacky, but seems to do the job
the 'missing' notes refer to pg operators which aren't yet supported
pg's operator table is on this page:
http://www.postgresql.org/docs/8.4/interactive/sql-syntax-lexical.html#SQL-SYNTAX-OPERATORS

> table :: [[Operator [Char] u Identity Expression]]
> table = [[singleDot (BinOpCall Qual) AssocLeft]
>         ,[binary "::" (BinOpCall Cast) AssocLeft]
>          --missing [] for array element select
>          --missing unary -
>         ,[binary "^" (BinOpCall Pow) AssocLeft]
>         ,[binary "*" (BinOpCall Mult) AssocLeft
>          ,binary "/" (BinOpCall Div) AssocLeft
>          ,binary "%" (BinOpCall Mod) AssocLeft]
>         ,[binary "+" (BinOpCall Plus) AssocLeft
>          ,binary "-" (BinOpCall Minus) AssocLeft]
>          --should be is isnull and notnull
>         ,[postfixk "is not null" (UnOpCall IsNotNull)
>          ,postfixk "is null" (UnOpCall IsNull)]
>          --other operators all added in this list according to the pg docs:
>         ,[binary "<->" (BinOpCall DistBetween) AssocNone
>          ,binary "<=" (BinOpCall Lte) AssocRight
>          ,binary ">=" (BinOpCall Gte) AssocRight
>          ,binary "||" (BinOpCall Conc) AssocLeft
>          ]
>          --in should be here, but is treated as a factor instead
>          --between
>          --overlaps
>         ,[binary "like" (BinOpCall Like) AssocNone
>          ,binary "!=" (BinOpCall NotEql) AssocNone]
>          --(also ilike similar)
>         ,[binary "<" (BinOpCall Lt) AssocNone
>          ,binary ">" (BinOpCall Gt) AssocNone]
>         ,[binary "=" (BinOpCall Eql) AssocRight
>          ,binary "<>" (BinOpCall NotEql) AssocNone
>          ]
>         ,[prefixk "not" (UnOpCall Not)]
>         ,[binaryk "and" (BinOpCall And) AssocLeft
>          ,binaryk "or" (BinOpCall Or) AssocLeft]]
>     where

use different parsers for symbols and keywords to get the right
whitespace behaviour

>       binary s f
>          = Infix (try (operator s >> return f))

main problem is that .. in for can't be parsed properly since the
expression parser gets the . then barfs, so we put in a special
case to only parse as . if it isn't followed by another .

>       singleDot f
>          =  Infix (try (lexeme (char '.'
>                                 >> notFollowedBy (char '.'))
>                         >> return f))
>       binaryk s f
>          = Infix (try (keyword s >> return f))
>       prefixk s f
>          = Prefix (try (keyword s >> return f))
>       postfixk s f
>          = Postfix (try (keyword s >> return f))
>

== factor parsers

> scalarSubQuery :: GenParser Char () Expression
> scalarSubQuery = liftM ScalarSubQuery $ parens select

in predicate - an identifier or row constructor followed by 'in'
then a list of expressions or a subselect

> inPredicate :: ParsecT String () Identity Expression
> inPredicate = do
>   vexp <- (try rowCtor) <|> liftM Identifier identifierString
>   n <- maybeP $ keyword "not"
>   keyword "in"
>   e <- parens ((liftM InSelect select)
>                <|>
>                (liftM InList $ commaSep1 expr))
>   return $ let p = InPredicate vexp e
>            in case n of
>                 Nothing -> p
>                 Just _ -> UnOpCall Not p

row ctor: one of
row ()
row (expr)
row (expr, expr1, ...)
(expr, expr2,...) [implicit (no row keyword) version, at least two elements
                   must be present]
notes:
(expr) parses to just expr rather than row(expr)
and () is a syntax error.

> rowCtor :: ParsecT [Char] () Identity Expression
> rowCtor = do
>   (do
>     keyword "row"
>     liftM Row $ parens $ commaSep expr)
>   <|> (liftM Row $ parens $ commaSep2 expr)

> positionalArg :: ParsecT String u Identity Expression
> positionalArg = do
>   char '$'
>   i <- integer
>   return $ PositionalArg ((fromInteger i)::Int)

string parsing

> stringLiteral :: ParsecT String () Identity Expression
> stringLiteral = stringQuotes <|> stringLD
>   where

parse a string delimited by single quotes

>     stringQuotes = liftM StringL stringPar
>     stringPar = do
>                 char '\''
>                 name <- readQuoteEscape

when we get to here, we've already read the quote at the end of the string

>                 whitespace
>                 return name

have to read two consecutive single quotes as a quote character
instead of the end of the string, probably an easier way to do this
other escapes (e.g. \n \t) are left unprocessed

>     readQuoteEscape = do
>                         x <- anyChar
>                         if x == '\''
>                           then do
>                                (try $ do
>                                       char '\''
>                                       l <- readQuoteEscape
>                                       return $ x:l)
>                                <|> return ""
>                           else do
>                                l <- readQuoteEscape
>                                return $ x:l

parse a dollar quoted string

>     stringLD = do
>                char '$'
>                tag <- ((do

cope with $$ as well as $[identifier]$

>                         lookAhead $ char '$'
>                         return "") <|>
>                        identifierString)
>                char '$'
>                s <- manyTill anyChar (try $ do
>                                             char '$'
>                                             string tag
>                                             char '$')
>                whitespace
>                return $ StringLD tag s

couple of helper functions which extract the actual string
from a StringLD or StringL, and the delimiters which were used
(either ' or a dollar tag)

> extrStr :: Expression -> String
> extrStr (StringLD _ s) = s
> extrStr (StringL s) = s
> extrStr x = error $ "extrStr not supported for this type " ++ show x

> quoteOfString :: Expression -> String
> quoteOfString (StringLD tag _) = "$" ++ tag ++ "$"
> quoteOfString (StringL _) = "'"
> quoteOfString x = error $ "quoteType not supported for this type " ++ show x


> integerLit :: ParsecT String u Identity Expression
> integerLit = liftM IntegerL integer

case - only supports 'case when condition' flavour and not 'case
expression when value' currently

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
>   where
>     whenParse = do
>                 keyword "when"
>                 e1 <- expr
>                 keyword "then"
>                 e2 <- expr
>                 return $ When e1 e2

> exists :: ParsecT [Char] () Identity Expression
> exists = do
>   keyword "exists"
>   liftM Exists $ parens select

> booleanLiteral :: ParsecT String u Identity Expression
> booleanLiteral = do
>   x <- lexeme (string "true")
>        <|> lexeme (string "false")
>   return $ BooleanL (x == "true")

> nullL :: ParsecT String u Identity Expression
> nullL = do
>   keyword "null"
>   return NullL

> array :: GenParser Char () Expression
> array = do
>   keyword "array"
>   liftM ArrayL $ squares $ commaSep expr

supports basic window functions of the form
fn() over ([partition bit]? [order bit]?)

> windowFn :: GenParser Char () Expression
> windowFn = do
>   fn <- functionCall
>   keyword "over"
>   (ps, os) <- parens (do
>                       ps <- maybeP partitionBy
>                       os <- maybeP orderBy
>                       return (ps,os))
>   return $ WindowFn fn ps os
>   where
>     partitionBy = do
>           keyword "partition"
>           keyword "by"
>           commaSep1 expr

> functionCall :: ParsecT String () Identity Expression
> functionCall = liftM2 FunCall identifierString (parens $ commaSep expr)

> identifier :: ParsecT String () Identity Expression
> identifier = liftM Identifier identifierString


================================================================================

= Utility parsers

== tokeny things

> lexeme :: ParsecT String u Identity a
>           -> ParsecT String u Identity a
> lexeme = P.lexeme lexer

> semi :: ParsecT String u Identity String
> semi = P.semi lexer

> symbol :: String -> ParsecT String u Identity String
> symbol = P.symbol lexer

> integer :: ParsecT String u Identity Integer
> integer = lexeme $ P.integer lexer

> operator :: String -> ParsecT String u Identity String
> operator s = symbol s

keyword has to not be immediately followed by letters or numbers
(symbols and whitespace are ok) so we know that we aren't reading an
identifier which happens to start with a complete keyword

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
>       return (s : p)

> identifierStringMaybeDot :: Parser String
> identifierStringMaybeDot =
>   (do
>     string "*"
>     whitespace
>     return "*")
>   <|> do
>       s <- letter
>       p <- many (alphaNum <|> char '_' <|> char '.')
>       whitespace
>       return $ s : p

== combinatory things

> parens :: ParsecT String u Identity a
>           -> ParsecT String u Identity a
> parens = P.parens lexer

> squares :: ParsecT String u Identity a
>           -> ParsecT String u Identity a
> squares = P.squares lexer


> maybeP :: GenParser tok st a
>           -> ParsecT [tok] st Identity (Maybe a)
> maybeP p =
>   (do a <- try p
>       return $ Just a)
>   <|> return Nothing


> commaSep2 :: ParsecT String u Identity t -> ParsecT String u Identity [t]
> commaSep2 p = sepBy2 p (symbol ",")

> sepBy2 :: (Stream s m t1) =>
>           ParsecT s u m t -> ParsecT s u m a -> ParsecT s u m [t]
> sepBy2 p sep = do
>   x <- p
>   sep
>   xs <- sepBy1 p sep
>   return (x:xs)

> commaSep :: ParsecT String u Identity a
>             -> ParsecT String u Identity [a]
> commaSep = P.commaSep lexer

> commaSep1 :: ParsecT String u Identity a
>             -> ParsecT String u Identity [a]
> commaSep1 = P.commaSep lexer

== whitespacey things

> whitespace :: ParsecT String u Identity ()
> whitespace = skipMany ((space >> return ())
>                        <|> blockComment
>                        <|> lineComment)

not quite sure how comments are suppose to work, but these in the
whitespace parser and in the lexer below seem to cover all the bases

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

== lexerizer

bit piss poor at the moment, I think it only puts in real work
when it is used to allow the built expression parser to deal
with comments properly

> lexer :: P.GenTokenParser String u Identity
> lexer = P.makeTokenParser (emptyDef {
>                             P.commentStart = "/*"
>                            ,P.commentEnd = "*/"
>                            ,P.commentLine = "--"
>                            ,P.nestedComments = False
>                            ,P.identStart = letter <|> char '_'
>                            ,P.identLetter    = alphaNum <|> oneOf "_"
>                            ,P.opStart        = P.opLetter emptyDef
>                            ,P.opLetter       = oneOf opLetters
>                            ,P.reservedOpNames= []
>                            ,P.reservedNames  = []
>                            ,P.caseSensitive  = False
>                            })

> opLetters :: String
> opLetters = ".:^*/%+-<>=|!"

================================================================================

= error message thing

enhanced show for errors which, in addition to the usual parsec error
message, displays the line containing the error with a little hat
pointing to the exact column below it, and the previous and next lines
for context (which works especially well for pretty printed sql which
outputs blank lines between everything). This additional text could
probably be added in the parse routines above to avoid all the clients
of this module having to do a load of work to get this information.

> showEr :: ParseError -> String -> String
> showEr er src =
>     let  pos  = errorPos er
>          lineNo = sourceLine pos
>          ls = lines src
>          line = safeGet ls(lineNo - 1)
>          prelines = map (safeGet ls) [(lineNo - 10) .. (lineNo - 2)]
>          postlines = map (safeGet ls) [lineNo .. (lineNo + 10)]
>          colNo = sourceColumn pos
>          highlightLine = (take (colNo -1) (repeat ' ')) ++ "^"
>     in "\n---------------------\n" ++ show er
>        ++ "\n------------\nCheck it out:\n" ++ unlines prelines ++ "\n"
>        ++ line ++ "\n" ++ highlightLine ++ "\nERROR HERE\n" ++ unlines postlines
>        ++ "\n-----------------\n"
>          where
>            safeGet a i = if i < 0
>                            then ""
>                            else if i >= length a
>                                   then ""
>                                   else a !! i
