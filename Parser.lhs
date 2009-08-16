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
>              --parse fully formed plpgsql statements from a string
>              ,parsePlpgsql
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
> parseSql = parse sqlStatements "(unknown)"

> parseSqlFile :: String -> IO (Either ParseError [Statement])
> parseSqlFile = parseFromFile sqlStatements

Parse expression fragment, used for testing purposes

> parseExpression :: String -> Either ParseError Expression
> parseExpression s = parse (expr <* eof) "" s

parse plpgsql statements, used for testing purposes

> parsePlpgsql :: String -> Either ParseError [Statement]
> parsePlpgsql s =  parse (whitespace
>                          *> many plPgsqlStatement
>                          <* eof) "(unknown)" s

================================================================================

= Parsing top level statements

> sqlStatements :: ParsecT String () Identity [Statement]
> sqlStatements = whitespace *> (many $ sqlStatement True) <* eof

parse a statement

> sqlStatement :: Bool -> ParsecT String () Identity Statement
> sqlStatement reqSemi = (choice [
>                         select
>                        ,values
>                        ,insert
>                        ,update
>                        ,delete
>                        ,trykeyword "create" *>
>                                    choice [
>                                       createTable
>                                      ,createType
>                                      ,createFunction
>                                      ,createView
>                                      ,createDomain]
>                        ,trykeyword "drop" *> dropFunction
>                        ]
>     <* (if reqSemi then semi >> return () else optional semi >> return ()))
>    --copy doesn't end with a semicolon
>    <|> copy

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

> select :: ParsecT String () Identity Statement
> select = do
>   s1 <- trykeyword "select" *> selQuerySpec
>   choice [
>     --don't know if this does associativity in the correct order for
>     --statements with multiple excepts/ intersects and no parens
>     try $ CombineSelect Except s1 <$> (keyword "except" *> select)
>    ,try $ CombineSelect Intersect s1 <$> (keyword "intersect" *> select)
>    ,try $ CombineSelect Union s1 <$> (keyword "union" *> select)
>    ,return s1]

> values :: ParsecT String () Identity Statement
> values = Values <$> (trykeyword "values"
>                      *> commaSep1 (parens $ commaSep1 expr))

= insert, update and delete

insert statement: supports option column name list,
multiple rows to insert and insert from select statements

> insert :: ParsecT String () Identity Statement
> insert =
>   Insert <$> (trykeyword "insert"
>               *> keyword "into"
>               *> identifierString)
>          <*> option [] (try columnNameList)
>          <*> (select <|> values)
>          <*> maybeP returning

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

> copy :: ParsecT String u Identity Statement
> copy = Copy <$> (trykeyword "copy" *> lexeme (getLinesTillMatches "\\.\n"))
>   where
>     getLinesTillMatches s = do
>                             x <- getALine
>                             if x == s
>                               then return x
>                               else (x++) <$> getLinesTillMatches s
>     getALine = (++"\n") <$> manyTill anyChar (try newline)

= ddl

> createTable :: ParsecT String () Identity Statement
> createTable =
>   uncurry
>   <$> (CreateTable <$> (trykeyword "table" *> identifierString))
>   --parse our unordered list of attribute defs or constraints for
>   --each line, want to try the constraint parser first, then the
>   --attribute parser, so we need the swap to feed them in the right
>   --order into createtable
>   <*> parens (swap <$> parseABsep1
>                          (try tableConstr)
>                          tableAtt
>                          (symbol ","))
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
>   retType <- keyword "returns" *> typeName
>   body <- keyword "as" *> stringLiteral
>   lang <- readLang
>   (q, b) <- parseBody lang body fnName
>   CreateFunction lang fnName params retType q b <$> pVol
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
>                            --if we have an error parsing the body,
>                            --collect all the needed info from that
>                            --error and rethrow it
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
>                <*> option [] groupBy
>                <*> option [] orderBy
>                <*> maybeP limit

> orderBy :: GenParser Char () [Expression]
> orderBy = trykeyword "order" *> keyword "by" *> commaSep1 expr

> groupBy :: GenParser Char () [Expression]
> groupBy = trykeyword "group" *> keyword "by" *> commaSep1 expr

> from :: GenParser Char () TableRef
> from = (keyword "from" *> tref)

> whereClause :: ParsecT String () Identity Expression
> whereClause = (keyword "where" *> expr)

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
>   where
>     getFirstTref = choice [
>                     SubTref
>                     <$> parens select
>                     <*> (keyword "as" *> identifierString)
>                    ,parseOptionalSuffix
>                       TrefFun (try functionCall)
>                       TrefFunAlias () (keyword "as" *> identifierString)
>                    ,parseOptionalSuffix
>                       Tref identifierString
>                       TrefAlias () nonKeywordIdentifierString]
>     nonKeywordIdentifierString = try $ do
>              x <- identifierString
>              --avoid all these keywords as aliases since they can
>              --appear immediately following a tableref as the next
>              --part of the statement, if we don't do this then lots
>              --of things don't parse.
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
>                          ,"group"
>                          ,"limit"
>                          ,"using"]
>                then fail "not keyword"
>                else return x

joinpart: parse a join after the first part of the tableref
(which is a table name, aliased table name or subselect)
; - takes this tableref as an arg so it can recurse to
multiple joins

> joinPart :: TableRef -> GenParser Char () TableRef
> joinPart tr1 = parseOptionalSuffixThreaded readOneJoinPart joinPart
>     where
>       readOneJoinPart = JoinedTref tr1
>          --look for the join flavour first
>          <$> (isJust <$> maybeP (keyword "natural"))
>          <*> choice [
>             Inner <$ keyword "inner"
>            ,LeftOuter <$ (keyword "left" *> keyword "outer")
>            ,RightOuter <$ (keyword "right" *> keyword "outer")
>            ,FullOuter <$ (keyword "full" >> keyword "outer")
>            ,Cross <$ keyword "cross"]
>          --recurse back to tref to read the table
>          <*> (keyword "join" *> tref)
>          --now try and read the join condition
>          <*> choice [
>              Just <$> (JoinOn <$> (trykeyword "on" *> expr))
>             ,Just <$> (JoinUsing <$> (trykeyword "using" *> columnNameList))
>             ,return Nothing]


selectlist and selectitem: the bit between select and from
check for into either before the whole list of select columns
or after the whole list

> selectList :: ParsecT String () Identity SelectList
> selectList =
>     choice [
>         flip SelectList <$> readInto <*> itemList
>        ,SelectList <$> itemList <*> option [] readInto]
>   where
>     readInto = trykeyword "into" *> commaSep1 identifierStringMaybeDot
>     itemList = commaSep1 selectItem


> selectItem :: ParsecT String () Identity SelectItem
> selectItem = parseOptionalSuffix
>                SelExp expr
>                SelectItem () (keyword "as" *> identifierString)

> returning :: ParsecT String () Identity SelectList
> returning = keyword "returning" *> selectList

== update

> columnNameList :: ParsecT String () Identity [String]
> columnNameList = parens $ commaSep1 identifierString

set clause - the set a = 3, b=4 part of a update statement

> setClause :: ParsecT String () Identity SetClause
> setClause = choice
>             [RowSetClause <$> parens (commaSep1 identifierString)
>                           <*> (symbol "=" *> parens (commaSep1 expr))
>             ,SetClause <$> identifierString
>                        <*> (symbol "=" *> expr)]

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

> typeName :: ParsecT String () Identity TypeName
> typeName = choice [
>             SetOfType <$> (trykeyword "setof" *> typeName)
>            ,do
>              s <- identifierString
>              choice [
>                PrecType s <$> parens integer
>               ,ArrayType (SimpleType s) <$ symbol "[]"
>               ,return $ SimpleType s]]

================================================================================

= plpgsql statements

> plPgsqlStatement :: ParsecT String () Identity Statement
> plPgsqlStatement = sqlStatement True
>                    <|> (choice [
>                          continue
>                         ,execute
>                         ,assignment
>                         ,ifStatement
>                         ,caseStatement
>                         ,returnSt
>                         ,raise
>                         ,forStatement
>                         ,whileStatement
>                         ,perform
>                         ,nullStatement]
>                         <* semi)

> nullStatement :: ParsecT String u Identity Statement
> nullStatement = NullStatement <$ keyword "null"

> continue :: ParsecT String u Identity Statement
> continue = ContinueStatement <$ trykeyword "continue"

> perform :: ParsecT String () Identity Statement
> perform = Perform <$> (trykeyword "perform" *> expr)

> execute :: ParsecT String () Identity Statement
> execute = Execute <$> (trykeyword "execute" *> expr)

> assignment :: ParsecT String () Identity Statement
> assignment = Assignment
>              -- put the := in the first try to attempt to get a
>              -- better error if the code looks like malformed
>              -- assignment statement
>              <$> try (identifierStringMaybeDot <* symbol ":=")
>              <*> expr

> returnSt :: ParsecT String () Identity Statement
> returnSt = trykeyword "return" *>
>            choice [
>             ReturnNext <$> (trykeyword "next" *> expr)
>            ,ReturnQuery <$> (trykeyword "query" *> select)
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

> forStatement :: GenParser Char () Statement
> forStatement = do
>                start <- (trykeyword "for" *> identifierString <* keyword "in")
>                choice [(ForSelectStatement start <$> try select <*> theRest)
>                       ,(ForIntegerStatement start
>                               <$> expr
>                               <*> (symbol ".." *> expr)
>                               <*> theRest)]
>   where
>     theRest = keyword "loop" *> many plPgsqlStatement
>               <* keyword "end" <* keyword "loop"

> whileStatement :: ParsecT String () Identity Statement
> whileStatement = WhileStatement
>                  <$> (trykeyword "while" *> expr <* keyword "loop")
>                  <*> many plPgsqlStatement <* keyword "end" <* keyword "loop"

bit too clever coming up

> ifStatement :: ParsecT String () Identity Statement
> ifStatement =
>   If <$> (ifPart <:> elseifParts)
>      <*> (elsePart <* endIf)
>   where
>     ifPart = (ifk *> expr) <.> (thn *> many plPgsqlStatement)
>     elseifParts = many ((elseif *> expr) <.> (thn *> many plPgsqlStatement))
>     elsePart = option [] (trykeyword "else" *> many plPgsqlStatement)
>     endIf = trykeyword "end" <* keyword "if"
>     thn = keyword "then"
>     ifk = trykeyword "if"
>     elseif = trykeyword "elseif"
>     --might as well these in as well after all that
>     -- can't do <,> unfortunately, so use <.> instead
>     (<.>) a b = (,) <$> a <*> b

> caseStatement :: ParsecT String () Identity Statement
> caseStatement =
>     CaseStatement <$> (trykeyword "case" *> expr)
>                   <*> (many whenSt)
>                   <*> (option [] (keyword "else" *> many plPgsqlStatement))
>                           <* keyword "end" <* keyword "case"
>     where
>       whenSt = (,) <$> (keyword "when" *> expr)
>                  <*> (keyword "then" *> many plPgsqlStatement)

===============================================================================

= statement components for plpgsql

> functionBody :: Language -> ParsecT String () Identity FnBody

sql function is just a list of statements, the last one has the
trailing semicolon optional

> functionBody Sql = do
>   a <- whitespace *> many (try $ sqlStatement True)
>   SqlFnBody <$> option a ((\b -> (a++[b])) <$> sqlStatement False)

plpgsql function has an optional declare section, plus the statements
are enclosed in begin ... end; (semi colon after end is optional

> functionBody Plpgsql =
>   whitespace *>
>   choice [
>      PlpgsqlFnBody <$> (keyword "declare" *> readVarDefs) <*> restOfIt
>     ,PlpgsqlFnBody [] <$> (keyword "begin" *> restOfIt)]
>   where
>     restOfIt = many plPgsqlStatement <* keyword "end" <* optional semi <* eof
>     readVarDefs = manyTill (try varDef) (try $ keyword "begin")

params to a function

> param :: ParsecT String () Identity ParamDef
> param = choice [
>          try (ParamDef <$> identifierString <*> typeName)
>         ,ParamDefTp <$> typeName]

variable declarations in a plpgsql function

> varDef :: ParsecT String () Identity VarDef
> varDef = VarDef
>          <$> identifierString
>          <*> typeName
>          <*> maybeP (symbol ":=" *> expr) <* semi

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

do array before array sub, since array parses an array selector which
looks exactly like an array subscript operator

>          ,try array
>          ,try arraySub

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

> table :: [[Operator String u Identity Expression]]
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
>       --use different parsers for symbols and keywords to get the
>       --right whitespace behaviour
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

>       dontFollowWith c1 c2 =
>         try $ char c1 *> notFollowedBy (char c2) *> whitespace

the first argument to these is ignored, it is there so the symbol
can appear in the operator table above for readability purposes

>

== factor parsers

> scalarSubQuery :: GenParser Char () Expression
> scalarSubQuery = liftM ScalarSubQuery $ parens select

in predicate - an identifier or row constructor followed by 'in'
then a list of expressions or a subselect

> inPredicate :: ParsecT String () Identity Expression
> inPredicate =
>   InPredicate
>   <$> (try rowCtor <|> Identifier <$> identifierString)
>   <*> option True (False <$ trykeyword "not")
>   <*> (keyword "in" *> parens ((InSelect <$> select)
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

> rowCtor :: ParsecT String () Identity Expression
> rowCtor = Row <$> choice [
>            keyword "row" *> parens (commaSep expr)
>           ,parens $ commaSep2 expr]

> positionalArg :: ParsecT String u Identity Expression
> positionalArg = PositionalArg <$> (char '$' *> (fromInteger <$> integer))

string parsing

> stringLiteral :: ParsecT String () Identity Expression
> stringLiteral = stringQuotes <|> stringLD
>   where
>     --parse a string delimited by single quotes
>     stringQuotes = StringL <$> stringPar
>     stringPar = char '\'' *> readQuoteEscape <* whitespace
>     --(readquoteescape reads the trailing ')

have to read two consecutive single quotes as a quote character
instead of the end of the string, probably an easier way to do this
other escapes (e.g. \n \t) are left unprocessed

>     readQuoteEscape = do
>                       x <- anyChar
>                       if x == '\''
>                         then try ((x:) <$> (char '\'' *> readQuoteEscape))
>                              <|> return ""
>                         else (x:) <$> readQuoteEscape

parse a dollar quoted string

>     stringLD = do
>                -- cope with $$ as well as $[identifier]$
>                tag <- char '$' *> ((char '$' *> return "")
>                                    <|> (identifierString <* char '$'))
>                s <- lexeme $ manyTill anyChar
>                       (try $ char '$' <* string tag <* char '$')
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
> integerLit = IntegerL <$> integer

case - only supports 'case when condition' flavour and not 'case
expression when value' currently

> caseParse :: ParsecT String () Identity Expression
> caseParse = Case <$> (keyword "case" *> many whenParse)
>                  <*> (maybeP ((keyword "else" *> expr)))
>                       <* keyword "end"
>   where
>     whenParse = (,) <$> (keyword "when" *> expr)
>                     <*> (keyword "then" *> expr)

> exists :: ParsecT String () Identity Expression
> exists = Exists <$> (keyword "exists" *> parens select)

> booleanLiteral :: ParsecT String u Identity Expression
> booleanLiteral = BooleanL <$> (=="true")
>                               <$> lexeme (string "true"
>                                           <|> string "false")

> nullL :: ParsecT String u Identity Expression
> nullL = NullL <$ keyword "null"

> array :: GenParser Char () Expression
> array = ArrayL <$> (keyword "array" *> squares (commaSep expr))

when you put expr instead of identifier in arraysub, it stack
overflows, not sure why.

> arraySub :: ParsecT [Char] () Identity Expression
> arraySub = ArraySub <$> identifier <*> squares (commaSep1 expr)

supports basic window functions of the form
fn() over ([partition bit]? [order bit]?)

> windowFn :: GenParser Char () Expression
> windowFn = WindowFn <$> (functionCall <* keyword "over")
>                     <*> (symbol "(" *> option [] partitionBy)
>                     <*> (option [] orderBy1 <* symbol ")")
>   where
>     orderBy1 = trykeyword "order" *> keyword "by" *> commaSep1 expr
>     partitionBy = trykeyword "partition" *> keyword "by" *> commaSep1 expr

> functionCall :: ParsecT String () Identity Expression
> functionCall = FunCall <$> identifierString <*> parens (commaSep expr)

> identifier :: ParsecT String () Identity Expression
> identifier = Identifier <$> identifierString


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
> operator = symbol

keyword has to not be immediately followed by letters or numbers
(symbols and whitespace are ok) so we know that we aren't reading an
identifier which happens to start with a complete keyword

> keyword :: String -> ParsecT String u Identity ()
> keyword k = lexeme (string k *> notFollowedBy alphaNum)
>             <?> k

shorthand to simplify parsers, helps because you can then avoid parens
or $ which in turn doesn't clutter up things and interfere with the
applicative operators

> trykeyword :: String -> ParsecT String u Identity ()
> trykeyword = try . keyword

> identifierString :: Parser String
> identifierString = lexeme (string "*" <|> letter <:> secondOnwards)
>   where
>     secondOnwards = many (alphaNum <|> char '_')

> identifierStringMaybeDot :: Parser String
> identifierStringMaybeDot = lexeme (string "*" <|> letter <:> secondOnwards)
>   where
>     secondOnwards = many (alphaNum <|> char '_' <|> char '.')

== combinatory things

> parens :: ParsecT String u Identity a
>           -> ParsecT String u Identity a
> parens = P.parens lexer

> squares :: ParsecT String u Identity a
>           -> ParsecT String u Identity a
> squares = P.squares lexer


> maybeP :: GenParser tok st a
>           -> ParsecT [tok] st Identity (Maybe a)
> maybeP p = try (optionMaybe p) <|> return Nothing

> commaSep2 :: ParsecT String u Identity t -> ParsecT String u Identity [t]
> commaSep2 p = sepBy2 p (symbol ",")

> sepBy2 :: (Stream s m t1) =>
>           ParsecT s u m t -> ParsecT s u m a -> ParsecT s u m [t]
> sepBy2 p sep = (p <* sep) <:> sepBy1 p sep

> commaSep :: ParsecT String u Identity a
>             -> ParsecT String u Identity [a]
> commaSep = P.commaSep lexer

> commaSep1 :: ParsecT String u Identity a
>             -> ParsecT String u Identity [a]
> commaSep1 = P.commaSep lexer

doesn't seem too gratuitous, comes up a few times

> (<:>) :: (Applicative f) =>
>          f a -> f [a] -> f [a]
> (<:>) a b = (:) <$> a <*> b


pass a list of pairs of strings and values
try each pair k,v in turn,
if keyword k matches then return v

> matchAKeyword :: [(String, a)] -> ParsecT String u Identity a
> matchAKeyword [] = fail "no matches"
> matchAKeyword ((k,v):kvs) = v <$ trykeyword k <|> matchAKeyword kvs

parseOptionalSuffix

parse the start of something -> parseResultA,
then parse an optional suffix -> parseResultB
if this second parser succeeds, call fn2 parseResultA parseResultB
else call fn1 parseResultA

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

> parseOptionalSuffix :: (Stream s m t2) =>
>                       (t1 -> b)
>                    -> ParsecT s u m t1
>                    -> (t1 -> a -> b)
>                    -> ()
>                    -> ParsecT s u m a
>                    -> ParsecT s u m b
> parseOptionalSuffix c1 p1 c2 _ p2 = do
>   x <- p1
>   option (c1 x) (c2 x <$> try p2)

parseOptionalSuffixThreaded

parse the start of something -> parseResultA,
then parse an optional suffix, passing parseResultA to this parser -> parseResultB
return parseResultB is it succeeds, else return parseResultA

sort of like a suffix operator parser where the suffixisable part
is parsed, then if the suffix is there it wraps the suffixisable
part in an enclosing tree node.

parser1 -> tree1
(parser2 tree1) -> maybe tree2
tree2 isnothing ? tree1 : tree2

> parseOptionalSuffixThreaded :: ParsecT [tok] st Identity a
>                             -> (a -> GenParser tok st a)
>                             -> ParsecT [tok] st Identity a
> parseOptionalSuffixThreaded p1 p2 = do
>   x <- p1
>   option x (try $ p2 x)

I'm pretty sure this is some standard monad operation but I don't know
what. It's a bit like the maybe monad but when you get nothing it
returns the previous result instead of nothing
(if you take the parsing stuff out you get:

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

== whitespacey things

> whitespace :: ParsecT String u Identity ()
> whitespace = skipMany ((space >> return ())
>                        <|> blockComment
>                        <|> lineComment)

not quite sure how comment parsing is suppose to work, but these in
the whitespace parser and in the lexer below seems to cover all the
bases

> blockComment :: ParsecT String st Identity ()
> blockComment = st >> manyTill anyChar en >> return ()
>     where
>       st = try $ string "/*"
>       en = try $ string "*/"

> lineComment :: ParsecT String st Identity ()
> lineComment = st >> manyTill anyChar en >> return ()
>   where
>     st = try $ string "--"
>     en = (char '\n' >> return ()) <|> eof

== lexerizer

bit piss poor at the moment, I think it only puts in real work when it
is used to allow the built expression parser to deal with comments
properly

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
pointing to the exact column below it, and some previous and next
lines for context. This additional text could probably be added in the
parse routines above to avoid all the clients of this module having to
do a load of work to get this information.

> showEr :: ParseError -> String -> String
> showEr er src =
>     let  pos  = errorPos er
>          lineNo = sourceLine pos
>          ls = lines src
>          line = safeGet ls(lineNo - 1)
>          prelines = map (safeGet ls) [(lineNo - 10) .. (lineNo - 2)]
>          postlines = map (safeGet ls) [lineNo .. (lineNo + 10)]
>          colNo = sourceColumn pos
>          highlightLine = replicate (colNo - 1) ' ' ++ "^"
>     in "\n---------------------\n" ++ show er
>        ++ "\n------------\nCheck it out:\n" ++ unlines prelines ++ "\n"
>        ++ line ++ "\n" ++ highlightLine ++ "\nERROR HERE\n" ++ unlines postlines
>        ++ "\n-----------------\n"
>          where
>            safeGet a i = if i < 0 || i >= length a
>                            then ""
>                            else a !! i
