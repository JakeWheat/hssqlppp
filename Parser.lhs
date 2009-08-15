Copyright 2009 Jake Wheat

The main file for parsing sql, uses parsec (badly). Only uses a lexer
in a few places which may both be wrong and a massive design flaw. Not
sure if parsec is the right choice either. Uses applicative parsing
style, see
http://book.realworldhaskell.org/read/using-parsec.html

For syntax reference see
http://savage.net.au/SQL/sql-2003-2.bnf.html
and
http://savage.net.au/SQL/sql-92.bnf.html
for some online sql grammar guides
and
http://www.postgresql.org/docs/8.4/interactive/sql-syntax.html
for some notes on postgresql syntax (the rest of that manual is also helpful)



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

> import Text.Parsec hiding(many, optional, (<|>))
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language
> import Text.Parsec.Expr
> import Text.Parsec.String
> import Text.Parsec.Error

> import Control.Applicative
> import Control.Monad.Identity

> import Data.Maybe



> import Tree

===============================================================================

= Top level parsing functions

parse fully formed sql

> parseSql :: String -> Either ParseError [Statement]
> parseSql = parse statements "(unknown)"

> parseSqlFile :: String -> IO (Either ParseError [Statement])
> parseSqlFile = parseFromFile statements

Parse expression fragment, used for testing purposes

> parseExpression :: String -> Either ParseError Expression
> parseExpression s = parse expr' "" s
>   where expr' = expr <* eof

================================================================================

= Parsing top level statements

> statements :: ParsecT String () Identity [Statement]
> statements = whitespace *> many statement <* eof

parse a statement

no attempt is made to reject plpgsql only statements outside of a
function or inside a sql function, this would probably be a pretty
easy fix by adding a flag or something.

> statement :: ParsecT String () Identity Statement
> statement = choice [
>              select
>             ,insert
>             ,update
>             ,delete
>             ,trykeyword "create" *>
>                         choice [createTable
>                                ,createType
>                                ,createFunction
>                                ,createView
>                                ,createDomain]
>             ,trykeyword "drop" *> dropFunction
>             ,execute
>             ,assignment
>             ,ifStatement
>             ,returnSt
>             ,raise
>             ,forStatement
>             ,whileStatement
>             ,perform
>             ,nullStatement]
>             <* semi
>    <|> copy

quick hack to support sql functions where the semicolon on the last
statement is optional. We only bother with sql statements

> statementOptionalSemi :: ParsecT String () Identity Statement
> statementOptionalSemi = choice [
>                          select
>                         ,insert
>                         ,update
>                         ,delete
>                         ,keyword "create" *> choice [
>                                       createTable
>                                      ,createType
>                                      ,createFunction
>                                      ,createView
>                                      ,createDomain]
>                         ,copy]
>     <* maybeP semi <* eof

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
>   trykeyword "select"
>   s1 <- selQuerySpec
>   choice [

don't know if this does associativity in the correct order for
statements with multiple excepts/ intersects and no parens

>     try $ CombineSelect Except s1 <$> (keyword "except" *> select)
>    ,try $ CombineSelect Intersect s1 <$> (keyword "intersect" *> select)
>    ,try $ CombineSelect Union s1 <$> (keyword "union" *> select)
>    ,return s1]

= insert, update and delete

insert statement: supports option column name list,
multiple rows to insert and insert from select statements

> insert :: ParsecT String () Identity Statement
> insert =
>   Insert <$> (trykeyword "insert"
>               *> keyword "into"
>               *> identifierString)
>          <*> maybeP columnNameList
>          <*> stuffToInsert
>          <*> maybeP returning
>       where
>         stuffToInsert = choice [
>                          InsertData
>                          <$> (keyword "values"
>                               *> (commaSep1 $ parens $ commaSep1 expr))
>                         ,InsertQuery <$> select]

> update :: ParsecT String () Identity Statement
> update = Update
>          <$> (trykeyword "update" *> identifierString <* keyword "set")
>          <*> commaSep1 setClause
>          <*> maybeP whereClause
>          <*> maybeP returning

> delete :: ParsecT String () Identity Statement
> delete = Delete
>          <$> (trykeyword "delete" *> keyword "from" *> identifierString)
>          <*> maybeP whereClause
>          <*> maybeP returning

= copy statement

copy: just reads the string in for now - read lines until we get to
one with just a \. in the first two columns

> copy :: ParsecT [Char] u Identity Statement
> copy = do
>   Copy <$> (trykeyword "copy" *> (lexeme $ getLinesTillMatches "\\.\n"))
>   where
>     getLinesTillMatches s = do
>                             x <- getALine
>                             if x == s
>                               then return x
>                               else (x++) <$> getLinesTillMatches s
>     getALine = do
>                x <- manyTill anyChar (try newline)
>                return $ x ++ "\n"

= ddl

> createTable :: ParsecT String () Identity Statement
> createTable =
>   uncurry
>   <$> (CreateTable <$> (trykeyword "table" *> identifierString))

parse our unordered list of attribute defs or constraints for each
line, want to try the constraint parser first, then the attribute
parser, so we need the swap to feed them in the right order into
createtable

>   <*> parens (swap <$> parseABsep1
>                        (try tableConstr)
>                        tableAtt
>                        (symbol ","))
>     where swap (a,b) = (b,a)

> createType :: ParsecT String () Identity Statement
> createType = CreateType
>              <$> (trykeyword "type" *> identifierString <* keyword "as")
>              <*> parens (commaSep1 typeAtt)


create function, support sql functions and
plpgsql functions. Actually parses the body in both cases
and provides a statement list for the body rather than just
a string

> createFunction :: GenParser Char () Statement
> createFunction = do
>   fnName <- trykeyword "function" *> identifierString
>   params <- parens $ commaSep param
>   retType <- keyword "returns" *> retTypeName
>   body <- keyword "as" *> stringLiteral
>   lang <- readLang
>   (q, b) <- parseBody lang body fnName
>   (CreateFunction lang fnName params retType q b) <$> pVol

if we have an error parsing the body, collect all the needed info
from that error and rethrow it

>     where
>         pVol = matchAKeyword [("volatile", Volatile)
>                              ,("stable", Stable)
>                              ,("immutable", Immutable)]
>         readLang = keyword "language" *> matchAKeyword [("plpgsql", Plpgsql)
>                                                        ,("sql",Sql)]
>         parseBody lang body fnName =
>             case parse
>               (functionBody lang)
>               ("function " ++ fnName)
>               (extrStr body) of
>                  Left e -> do
>                            sp <- getPosition
>                            error $ "in " ++ show sp
>                                      ++ ", " ++ showEr e (extrStr body)
>                  Right body' -> return (quoteOfString body, body')


> createView :: ParsecT String () Identity Statement
> createView = CreateView
>              <$> (trykeyword "view" *> identifierString)
>              <*> (keyword "as" *> select)

> createDomain :: ParsecT String () Identity Statement
> createDomain = CreateDomain
>                <$> (trykeyword "domain" *> identifierString)
>                <*> (maybeP (keyword "as") *> identifierString)
>                <*> maybeP (keyword "check" *> expr)

> dropFunction :: ParsecT String () Identity Statement
> dropFunction = DropFunction
>                <$> (trykeyword "function" *> identifierString)
>                <*> parens (many identifierString)

================================================================================

= component parsers for sql statements

select bits

> selQuerySpec :: ParsecT String () Identity Statement
> selQuerySpec = Select
>                <$> selectList
>                <*> maybeP from
>                <*> maybeP whereClause
>                <*> maybeP orderBy
>                <*> maybeP limit

> orderBy :: GenParser Char () [Expression]
> orderBy = keyword "order" *> keyword "by" *> commaSep1 expr

> from :: GenParser Char () From
> from = From <$> (keyword "from" *> tref)

> whereClause :: ParsecT String () Identity Where
> whereClause = Where <$> (keyword "where" *> expr)

> limit :: GenParser Char () Expression
> limit = keyword "limit" *> expr

== table refs
used in the from part of a select
have to cope with:
a simple tableref i.e just a name
an aliased table ref e.g. select a.b from tbl as a
a sub select e.g. select a from (select b from c)
 - these are handled in tref
then cope with joins recursively using joinpart below

> tref :: ParsecT String () Identity TableRef
> tref = parseOptionalSuffixThreaded getFirstTref joinPart
>          where
>            getFirstTref = choice [
>                            SubTref
>                            <$> parens select
>                            <*> (keyword "as" *> identifierString)
>                           ,parseOptionalSuffix
>                              TrefFun (try $ functionCall)
>                              TrefFunAlias (keyword "as" *> identifierString)
>                           ,parseOptionalSuffix
>                              Tref identifierString
>                              TrefAlias nonKeywordIdentifierString]
>            nonKeywordIdentifierString = do
>              x <- identifierString

avoid all these keywords as aliases since they can appear immediately
following a tableref as the next part of the statement, if we don't do
this then lots of things don't parse.


>              if x `elem` ["where"
>                          ,"except"
>                          ,"union"
>                          ,"intersect"
>                          ,"loop"
>                          ,"inner"
>                          ,"on"
>                          ,"left"
>                          ,"right"
>                          ,"full"
>                          ,"cross"
>                          ,"natural"
>                          ,"order"
>                          ,"limit"
>                          ,"using"]
>                then fail "not keyword"
>                else return x

joinpart: parse a join after the first part of the tableref
(which is a table name, aliased table name or subselect)
 - takes this tableref as an arg so it can recurse to
multiple joins

> joinPart :: TableRef -> GenParser Char () TableRef
> joinPart tr1 = do
>   parseOptionalSuffixThreaded readOneJoinPart joinPart
>     where
>       readOneJoinPart = (JoinedTref tr1)

look for the join flavour first

>          <$> (isJust <$> maybeP (keyword "natural"))
>          <*> choice [
>             Inner <$ keyword "inner"
>            ,LeftOuter <$ (keyword "left" *> keyword "outer")
>            ,RightOuter <$ (keyword "right" *> keyword "outer")
>            ,FullOuter <$ (keyword "full" >> keyword "outer")
>            ,Cross <$ keyword "cross"]

recurse back to tref to read the table

>          <*> (keyword "join" *> tref)

now try and read the join condition

>          <*> choice [
>              Just <$> (JoinOn <$> (keyword "on" *> expr))
>             ,Just <$> (JoinUsing <$> (keyword "using" *> columnNameList))
>             ,return Nothing]


selectlist and selectitem: the bit between select and from
check for into either before the whole list of select columns
or after the whole list

> selectList :: ParsecT String () Identity SelectList
> selectList =
>     choice [
>         (flip SelectList)
>         <$> (Just <$> try readInto) <*> itemList
>        ,SelectList <$> itemList <*> maybeP readInto]
>   where
>     readInto = (keyword "into" *> commaSep1 identifierStringMaybeDot)
>     itemList = commaSep1 selectItem


> selectItem :: ParsecT String () Identity SelectItem
> selectItem = parseOptionalSuffix
>                SelExp expr
>                SelectItem (keyword "as" *> identifierString)

> returning :: ParsecT String () Identity SelectList
> returning = keyword "returning" *> selectList

== update

> columnNameList :: ParsecT String () Identity [String]
> columnNameList = parens $ commaSep1 identifierString

set clause - the set a = 3, b=4 part of a update statement

> setClause :: ParsecT String () Identity SetClause
> setClause = SetClause <$> identifierString
>             <*> (symbol "=" *> expr)

== ddl

tableatt - an single attribute line in a create table

> tableAtt :: ParsecT String () Identity AttributeDef
> tableAtt = AttributeDef
>            <$> identifierString
>            <*> identifierString
>            <*> maybeP (keyword "default" *> expr)
>            <*> sepBy inlineConstraint whitespace

> tableConstr :: ParsecT String () Identity Constraint
> tableConstr = UniqueConstraint <$> try (keyword "unique" *> columnNameList)

> inlineConstraint :: ParsecT String () Identity InlineConstraint
> inlineConstraint =
>   choice [
>           InlineUniqueConstraint <$ keyword "unique"
>          ,InlineCheckConstraint <$> (keyword "check" *> parens expr)
>          ,NullConstraint <$ trykeyword "null"
>          ,NotNullConstraint <$ (keyword "not" *> keyword "null")
>          ]


typeatt: like a cut down version of tableatt, used in create type

> typeAtt :: ParsecT String () Identity TypeAttributeDef
> typeAtt = TypeAttDef <$> identifierString <*> identifierString

> retTypeName :: ParsecT String () Identity Expression
> retTypeName =
>   choice [
>      UnOpCall SetOf <$> (keyword "setof" *> parseBasicType)
>     ,parseBasicType]
>   where
>     parseBasicType = parseOptionalSuffix
>                        Identifier identifierString
>                        makeFunCall (IntegerL <$> parens integer)
>     makeFunCall a b = FunCall a [b]

================================================================================

= plpgsql statements

null statement is plpgsql nop, written 'null;'

> nullStatement :: ParsecT String u Identity Statement
> nullStatement = NullStatement <$ keyword "null"

> perform :: ParsecT String () Identity Statement
> perform = Perform <$> (trykeyword "perform" *> expr)

> execute :: ParsecT String () Identity Statement
> execute = Execute <$> (trykeyword "execute" *> expr)

> assignment :: ParsecT String () Identity Statement
> assignment = Assignment
>   -- put the := in the first try to attempt to get a better error if
>   -- the code looks like malformed assignment statement
>              <$> (try (identifierStringMaybeDot <* symbol ":="))
>              <*> expr

> returnSt :: ParsecT String () Identity Statement
> returnSt = trykeyword "return" *>
>            choice [
>             ReturnNext <$> (trykeyword "next" *> expr)
>            ,Return <$> maybeP expr]

> raise :: ParsecT String () Identity Statement
> raise = Raise
>         <$> (trykeyword "raise" *> raiseType)
>         <*> (extrStr <$> stringLiteral)
>         <*> option [] (symbol "," *> commaSep1 expr)
>         where
>           raiseType = matchAKeyword [("notice", RNotice)
>                                      ,("exception", RException)
>                                      ,("error", RError)]

for statement, only supports for x in [select statement]
flavour at the moment

> forStatement :: GenParser Char () Statement
> forStatement = prefixChoice
>                  (trykeyword "for" *> identifierString <* keyword "in")
>                  [(\i -> ForSelectStatement i <$> try select <*> theRest)
>                  ,(\i -> ForIntegerStatement i
>                          <$> expr
>                          <*> (symbol ".." *> expr)
>                          <*> theRest)]
>   where
>     theRest = keyword "loop" *> many statement
>               <* keyword "end" <* keyword "loop"

> whileStatement :: ParsecT String () Identity Statement
> whileStatement = WhileStatement
>                  <$> (trykeyword "while" *> expr <* keyword "loop")
>                  <*> many statement <* keyword "end" <* keyword "loop"

bit too clever coming up

> ifStatement :: ParsecT String () Identity Statement
> ifStatement =
>   If <$> (ifPart <:> elseifParts)
>      <*> (elsePart <* endIf)
>   where
>     ifPart = (ifk *> expr) <.> (thn *> many statement)
>     elseifParts = many ((elseif *> expr) <.> (thn *> many statement))
>     elsePart = maybeP (keyword "else" *> many statement)
>     endIf = keyword "end" <* keyword "if"
>     thn = keyword "then"
>     ifk = trykeyword "if"
>     elseif = trykeyword "elseif"
>     --might as well these in as well after all that
>     -- can't do <,> unfortunately, so use <.> instead
>     (<:>) a b = (:) <$> a <*> b
>     (<.>) a b = (,) <$> a <*> b


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
> table = [[singleDot "." (BinOpCall Qual) AssocLeft]
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
>         ,[lt "<" (BinOpCall Lt) AssocNone
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
>       binaryk s f
>          = Infix (try (keyword s >> return f))
>       prefixk s f
>          = Prefix (try (keyword s >> return f))
>       postfixk s f
>          = Postfix (try (keyword s >> return f))

some custom parsers

main problem is that .. in for can't be parsed properly since the
expression parser gets the . then barfs, so we put in a special
case to only parse as . if it isn't followed by another .

>       singleDot _ f
>          =  Infix (dontFollowWith '.' '.' >> return f)

fix problem parsing <> - don't parse as "<" if it is immediately
followed by ">"

>       lt _ f = Infix (dontFollowWith '<' '>' >> return f)

>       dontFollowWith c1 c2 = try $ do
>         char c1
>         notFollowedBy $ char c2
>         whitespace
>         return ()

the first argument to these is ignored, it is there so the symbol
can appear in the operator table above for readability purposes

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

shorthand to simplyfy parsers, helps because you can then avoid parens
or $

> trykeyword :: String -> ParsecT String u Identity ()
> trykeyword k = try $ keyword k

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
> maybeP p = (try $ optionMaybe p) <|> return Nothing

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

pass a list of pairs of strings and values
try each pair k,v in turn,
if keyword k matches then return v

> matchAKeyword :: [(String, a)] -> ParsecT String u Identity a
> matchAKeyword [] = fail "no matches"
> matchAKeyword ((k,v):kvs) = do
>   v <$ trykeyword k <|> matchAKeyword kvs

parseOptionalSuffix

can't think of a good name,
want to parse part a -> r1, then maybe parse part b -> r2
if r2 is nothing then return c1 r1
else return c2 r1 r2
This is to parse the something which has an optional bunch of stuff
on the end with one constructor which takes the mandatory first part
and another constructor which takes the mandatory first part
and the optional second part as args.

e.g.
parsing an identifier in a select list can be
a
or
a as b
so we can pass
* IdentifierCtor
* identifier (returns aval)
* AliasedIdentifierCtor
* parser for (as b) (returns bval)
as the four args,
and we get either
* IdentifierCtor aval
or
* AliasedIdentifierCtor aval bval
as the result depending on whether the "parser for (as b)"
succeeds or not.

probably this concept already exists under a better name in parsing
theory

> parseOptionalSuffix :: (r1 -> v)
>                     -> ParsecT [tok] u Identity r1
>                     -> (r1 -> r2 -> v)
>                     -> ParsecT [tok] u Identity r2
>                     -> ParsecT [tok] u Identity v
> parseOptionalSuffix c1 p1 c2 p2 = do
>   x <- p1
>   option (c1 x) (c2 x <$> try p2)

parseOptionalSuffixThreaded

variant on the previous version, this we parse something, get a parse
tree, then we pass this tree to the optional suffix parser, if it
fails we keep the original parse tree, else the suffix parser embeds
the original parse tree in the tree it returns which we use

parser1 -> tree1
(parser2 tree1) -> maybe tree2
tree2 isnothing ? tree1 : tree2

I'm pretty sure this is some standard monad operation but I don't know
what. It's a bit like the maybe monad but when you get nothing it
returns the previous result instead of nothing

> parseOptionalSuffixThreaded :: ParsecT [tok] st Identity a
>                             -> (a -> GenParser tok st a)
>                             -> ParsecT [tok] st Identity a
> parseOptionalSuffixThreaded p1 p2 = do
>   x <- p1
>   option x (try $ p2 x)

couldn't work how to to perms so just did this hack instead
e.g.
a1,a2,b1,b2,a2,b3,b4 parses to ([a1,a2,a3],[b1,b2,b3,b4])

> parseABsep1 :: (Stream s m t) =>
>                ParsecT s u m a1
>             -> ParsecT s u m a
>             -> ParsecT s u m sep
>             -> ParsecT s u m ([a1], [a])

> parseABsep1 p1 p2 sep = do
>   (r1, r2) <- unzip <$> sepBy1 parseAorB sep
>   return (catMaybes r1, catMaybes r2)
>   where
>     parseAorB = choice [
>                   (\x -> (Just x,Nothing)) <$> p1
>                  ,(\y -> (Nothing, Just y)) <$> p2]

prefix choice: run one parser (the prefix parser) then choice where
each of the choice parsers takes the result of the prefix as an
argument, not sure about this one, since you often end up having
to write lambda functions for the choices and it doesn't end up
any more concise

> prefixChoice :: (Stream s m t1) =>
>                 ParsecT s u m t
>              -> [t -> ParsecT s u m b]
>              -> ParsecT s u m b
> prefixChoice p1 p = do
>   x <- p1
>   choice (map (\q -> q x) p)

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
