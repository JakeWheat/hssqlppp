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

================================================================================

Notes on coding style if the code looks like gobbledygook (assumes you
are familiar with do notation). Mostly a crash course in using
Applicative, probably these docs will be uninteresting if you can
easily understand the first code snippet below.

Here is a version of the delete parser which parses stuff like:

delete from [tablename] [where]? [returning]?

This snippet will be used to illustrate some of the particular idioms
in the code:

 delete = trykeyword "delete" >> keyword "from" >>
          Delete
          <$> idString
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning

We transform this in several stages through to vanilla do notation,
this is equivalent to

  delete = trykeyword "delete" >> keyword "from" >>
           Delete
           `liftM` idString
           `ap` tryMaybeP whereClause
           `ap` tryMaybeP returning

(which in turn is equivalent to

 delete = trykeyword "delete" >> keyword "from" >>
          liftM3 Delete
                 idString
                 (tryMaybeP whereClause)
                 (tryMaybeP returning)

maybe this is a backward step)

finally, in do notation it is:

 delete = do
          trykeyword "delete"
          keyword "from"
          i <- idString
          w <- tryMaybeP whereClause
          r <- tryMaybeP returning
          return $ Delete i w r

Which is what you'd write if you didn't know/like liftM or Applicative
(or what I wrote before I learnt to use Applicative).

I think the top version is clearest as long as you can understand it
is a short hand for the bottom version.

Some of the other operators used in addition to <$> and <*> are:

<*

ignores the return of the parser on the right e.g.

p1 <* p2

is equivalent to
do
  x <- p1
  p2
  return x

*>

ignores the return of the parser on the left,i.e. its just like >>
except the precedence is different (which can be exploited to reduce
the number of () needed)

p1 *> p2

is equivalent to
do
  p1
  p2

(which is the same as
do
  p1
  y <- p2
  return y
to see the symmetry better)

<$
returns the (non monadic) value on the left if the parser on the right succeeds
e.g.

True <$ keyword "distinct"

is equivalent to
do
  keyword "distinct"
  return True

alternatively could be written

keyword "distinct" *> return True

which is also good but slightly longer so I think I prefer the first.

You can see how these are used in an attempt to make the code more
concise whilst not making it unreadable in the actual code below.

<:>
made this one up as an applicative version of (:),
to write an operator using applicative you do
(:) <$> p1 <$> p2
which is the same as:
do
  a <- p1
  b <- p2
  return (a:b)
but using <:>, instead of
(:) <$> p1 <$> p2
you can write
p1 <:> p2
don't know if the precedence of (<:>) is the same as (:) though

Returning to the delete parser:

 delete = trykeyword "delete" >> keyword "from" >>
          Delete
          <$> idString
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning

there are some other notable aspects, in the bit before the 'Delete'.

* the code here is before the delete to look like a kind of filter,
  many of the parsers look like this. The alternative code that this
  style replaced is this:

 delete = Delete
          <$> (trykeyword "delete" *> keyword "from" *> idString)
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning

  which I think looks less clear.

There are also a couple of tricks in this line which are connected to
parsing the haskell code and precedence:

if we use *> instead of >> at the top, we need to use (), this is why
 >> is chosen:

 delete = trykeyword "delete" *> keyword "from" *>
          (Delete
          <$> idString
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning)

(The code prefers *> in places where *> and >> are interchangeable
without adding ().)

trykeyword could be replaced:

 delete = try (keyword "delete") >> keyword "from" >>
          Delete
          <$> idString
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning

but it's extra brackets (trykeyword is only shorthand like this used
in the code, since it appears all over and loads of () and $ get added
without using trykeyword).

One last aspect:

 delete = trykeyword "delete" >> keyword "from" >>
          Delete
          <$> idString
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning

Why is try used for delete but not for from? This is to help improve
the error messages, see if you can work out why:

Hint: try parsing the following three lines with the current code
(errors in this code on purpose).

delte from x;

delete frm x;

delete from x x x x;

(Parse each line separately). You can use ghci and the function parseSql
from the prompt, (load this file, Parser.lhs into ghci first) e.g.

ghci> parseSql "delte from x;"

Then change the delete code in this file to read:

trykeyword "delete" *> trykeyword "from" *>
...
(try added on the keyword from parser)

and parse the three lines above again.

Finally, try this function

 delete = try (keyword "delete" >> keyword "from" >>
               Delete
               <$> idString
               <*> tryMaybeP whereClause
               <*> tryMaybeP returning)

And parse the three lines above. (Notice the difference between the
error messages. The trys are used to parse difficult sql syntax, but
with this sort of effect in mind for error messages.)

There are lots of little idioms in the code, which is an ongoing
effort, please comment if they are too obtuse or you think of some
better/additional ones.

One other tip is that when coding with <*> <$> *>, etc it can be a bit
confusing where parens are needed. One way of coping is to write code
using one of the existing parsers as a template, copying the layout of
indentation and () usage, adding plenty of extra () where you're not
sure.

After it compiles and tests ok, run hlint and it will tell you which
() are redundant and you can fix the code. After a while you get the
hang of it (and hopefully the type checking and tests catch
mistakes). (Don't just do what hlint says blindly because it gets
confused in some places and you'll end up with the code not compiling,
also run the tests after changing the code in this way, although it's
pretty rare to see this with the latest versions of hlint.)

Few of other notes:

The trys follow a pattern which attempts to lead to better error
messages.

Near the bottom of this file are two parser combinators:
parseOptionalSuffix, and parseOptionalSuffixThreaded (which were named
by a recovering java programmer), which have some long winded docs on
how they work if it isn't obvious where they're used.

One thing you might miss if you're unfamiliar with parsec is the
whitespace handling - this code uses lexeme style which is documented
in the parsec tutorial. Most of the whitespace handling is fully
implicit which aids readability but it helps if you're aware how this
is happening. Sometimes different parsers are used in order to not
skip whitespace e.g. using "char '.'" instead of "symbol '.'".

Some further reference/reading:

parsec tutorial:
http://legacy.cs.uu.nl/daan/download/parsec/parsec.html

parsec reference:
http://hackage.haskell.org/package/parsec-3.0.0

applicative parsing style taken from here:
http://book.realworldhaskell.org/read/using-parsec.html
(just over halfway down the page)

> module Parser (
>               --parse fully formed sql statements from a string
>               parseSql
>               --parse a file containing sql statements only
>              ,parseSqlFile
>               --parse an expression (one expression plus whitespace
>               --only allowed
>              ,parseExpression
>              --parse fully formed plpgsql statements from a string
>              ,parsePlpgsql
>              )
>     where

> import Text.Parsec hiding(many, optional, (<|>), string)
> --import qualified Text.Parsec.Token as P
> --import Text.Parsec.Language
> import Text.Parsec.Expr
> import Text.Parsec.String
> --import Text.Parsec.Error

> import Control.Applicative
> import Control.Monad.Identity

> import Data.Maybe
> import Data.Char

> import Lexer
> import ParseErrors
> import Tree

===============================================================================

= Top level parsing functions

parse fully formed sql

> parseSql :: String -> Either ExtendedError [Statement]
> parseSql s = parseIt (lexSqlText s) sqlStatements "" s

> parseSqlFile :: String -> IO (Either ExtendedError [Statement])
> parseSqlFile fn = do
>   sc <- readFile fn
>   x <- lexSqlFile fn
>   return $ parseIt x sqlStatements fn sc

Parse expression fragment, used for testing purposes

> parseExpression :: String -> Either ExtendedError Expression
> parseExpression s = parseIt (lexSqlText s) (expr <* eof) "" s


parse plpgsql statements, used for testing purposes

> parsePlpgsql :: String -> Either ExtendedError [Statement]
> parsePlpgsql s =  parseIt (lexSqlText s) (many plPgsqlStatement <* eof) "" s

utility function to do error handling in one place

 > parseIt :: (Stream s Identity t) =>
 >            Either ExtendedError s
 >         -> Parsec s () b
 >         -> SourceName
 >         -> String
 >         -> Either ExtendedError b

> parseIt lexed parser fn src =
>     case lexed of
>                Left er -> Left er
>                Right toks -> convertToExtendedError
>                                (parse parser fn toks) fn src

================================================================================

= Parsing top level statements

> sqlStatements :: ParsecT [Token] () Identity [Statement]
> sqlStatements = many (sqlStatement True) <* eof

parse a statement

> sqlStatement :: Bool -> ParsecT [Token] () Identity Statement
> sqlStatement reqSemi = (choice [
>                          select
>                         ,values
>                         ,insert
>                         ,update
>                         ,delete
>                         ,truncateSt
>                         ,copy
>                         ,trykeyword "create" *>
>                                    choice [
>                                       createTable
>                                      ,createType
>                                      ,createFunction
>                                      ,createView
>                                      ,createDomain]
>                         ,trykeyword "drop" *>
>                                    choice [
>                                       dropSomething
>                                      ,dropFunction]
>                         ]
>        <* (if reqSemi
>              then symbol ';' >> return ()
>              else optional (symbol ';') >> return ()))
>       <|> copyData

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

> select :: ParsecT [Token] () Identity Statement
> select = do
>   trykeyword "select"
>   s1 <- selQuerySpec
>   choice [
>     --don't know if this does associativity in the correct order for
>     --statements with multiple excepts/ intersects and no parens
>     CombineSelect Except s1 <$> (trykeyword "except" *> select)
>    ,CombineSelect Intersect s1 <$> (trykeyword "intersect" *> select)
>    ,CombineSelect UnionAll s1 <$> (try (keyword "union"
>                                          *> keyword "all") *> select)
>    ,CombineSelect Union s1 <$> (trykeyword "union" *> select)
>    ,return s1]
>   where
>     selQuerySpec = Select
>                <$> option Dupes (Distinct <$ trykeyword "distinct")
>                <*> selectList
>                <*> tryMaybeP from
>                <*> tryMaybeP whereClause
>                <*> option [] groupBy
>                <*> tryMaybeP having
>                <*> option [] orderBy
>                <*> option Asc (choice [
>                                 Asc <$ keyword "asc"
>                                ,Desc <$ keyword "desc"])
>                <*> tryMaybeP limit
>                <*> tryMaybeP offset
>     from = keyword "from" *> tref
>     groupBy = trykeyword "group" *> keyword "by"
>               *> commaSep1 expr
>     having = trykeyword "having" *> expr
>     orderBy = trykeyword "order" *> keyword "by"
>               *> commaSep1 expr
>     limit = keyword "limit" *> expr
>     offset = keyword "offset" *> expr

>     -- table refs
>     -- have to cope with:
>     -- a simple tableref i.e just a name
>     -- an aliased table ref e.g. select a.b from tbl as a
>     -- a sub select e.g. select a from (select b from c)
>     --  - these are handled in tref
>     -- then cope with joins recursively using joinpart below
>     tref = parseOptionalSuffixThreaded getFirstTref joinPart
>     getFirstTref = choice [
>                     SubTref
>                     <$> parens select
>                     <*> (keyword "as" *> idString)
>                    ,parseOptionalSuffix
>                       TrefFun (try functionCall)
>                       TrefFunAlias () (trykeyword "as" *> idString)
>                    ,parseOptionalSuffix
>                       Tref nkwid
>                       TrefAlias () (optional (trykeyword "as") *> nkwid)]
>     --joinpart: parse a join after the first part of the tableref
>     --(which is a table name, aliased table name or subselect) -
>     --takes this tableref as an arg so it can recurse to multiple
>     --joins
>     joinPart tr1 = parseOptionalSuffixThreaded (readOneJoinPart tr1) joinPart
>     readOneJoinPart tr1 = JoinedTref tr1
>          --look for the join flavour first
>          <$> option Unnatural (Natural <$ trykeyword "natural")
>          <*> choice [
>             Inner <$ trykeyword "inner"
>            ,LeftOuter <$ try (keyword "left" *> keyword "outer")
>            ,RightOuter <$ try (keyword "right" *> keyword "outer")
>            ,FullOuter <$ try (keyword "full" >> keyword "outer")
>            ,Cross <$ trykeyword "cross"]
>          --recurse back to tref to read the table
>          <*> (keyword "join" *> tref)
>          --now try and read the join condition
>          <*> choice [
>              Just <$> (JoinOn <$> (trykeyword "on" *> expr))
>             ,Just <$> (JoinUsing <$> (trykeyword "using" *> columnNameList))
>             ,return Nothing]
>     nkwid = try $ do
>              x <- idString
>              --avoid all these keywords as aliases since they can
>              --appear immediately following a tableref as the next
>              --part of the statement, if we don't do this then lots
>              --of things don't parse. Seems a bit inelegant but
>              --works for the tests and the test sql files don't know
>              --if these should be allowed as aliases without "" or
>              --[]
>              if (map toLower x) `elem` ["as"
>                          ,"where"
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

> values :: ParsecT [Token] () Identity Statement
> values = trykeyword "values" >>
>          Values <$> commaSep1 (parens $ commaSep1 $ expr)


= insert, update and delete

insert statement: supports option column name list,
multiple rows to insert and insert from select statements

> insert :: ParsecT [Token] () Identity Statement
> insert = trykeyword "insert" >> keyword "into" >>
>          Insert <$> idString
>                 <*> option [] (try columnNameList)
>                 <*> (select <|> values)
>                 <*> tryMaybeP returning

> update :: ParsecT [Token] () Identity Statement
> update = trykeyword "update" >>
>          Update
>          <$> idString
>          <*> (keyword "set" *> commaSep1 setClause)
>          <*> tryMaybeP whereClause
>          <*> tryMaybeP returning
>     where
>       setClause = choice
>             [RowSetClause <$> parens (commaSep1 idString)
>                           <*> (symbol '=' *> parens (commaSep1 expr))
>             ,SetClause <$> idString
>                        <*> (symbol '=' *> expr)]

> delete :: ParsecT [Token] () Identity Statement
> delete = trykeyword "delete" >> keyword "from" >>
>          Delete
>          <$> idString
>          <*> tryMaybeP whereClause
>          <*> tryMaybeP returning

> truncateSt :: ParsecT [Token] () Identity Statement
> truncateSt = trykeyword "truncate" >> optional (trykeyword "table") >>
>            Truncate
>            <$> commaSep1 idString
>            <*> option ContinueIdentity (choice [
>                                 ContinueIdentity <$ (keyword "continue"
>                                                      <* keyword "identity")
>                                ,RestartIdentity <$ (keyword "restart"
>                                                     <* keyword "identity")])
>            <*> cascade

> copy :: ParsecT [Token] () Identity Statement
> copy = do
>        trykeyword "copy"
>        tableName <- idString
>        cols <- option [] (parens $ commaSep1 idString)
>        keyword "from"
>        src <- choice [
>                CopyFilename <$> extrStr <$> stringVal
>               ,Stdin <$ keyword "stdin"]
>        return $ Copy tableName cols src

> copyData :: ParsecT [Token] () Identity Statement
> copyData = CopyData <$> (mytoken (\tok ->
>                                         case tok of
>                                                  CopyPayloadTok n -> Just n
>                                                  _ -> Nothing))

= ddl

> createTable :: ParsecT [Token] () Identity Statement
> createTable = do
>   trykeyword "table"
>   tname <- idString
>   choice [
>      CreateTableAs tname <$> (trykeyword "as" *> select)
>     ,uncurry (CreateTable tname) <$> readAttsAndCons]
>   where
>     --parse our unordered list of attribute defs or constraints for
>     --each line, want to try the constraint parser first, then the
>     --attribute parser, so we need the swap to feed them in the
>     --right order into createtable
>     readAttsAndCons = parens (swap <$> parseABsep1
>                                          (try tableConstr)
>                                          tableAtt
>                                          (symbol ','))
>                       where swap (a,b) = (b,a)
>     tableAtt = AttributeDef
>                <$> idString
>                <*> idString
>                <*> tryMaybeP (keyword "default" *> expr)
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
>                    <$> try (keyword "check" *> parens (expr))
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
>          ,NullConstraint <$ trykeyword "null"
>          ,NotNullConstraint <$ (keyword "not" *> keyword "null")
>          ,RowReferenceConstraint
>          <$> (trykeyword "references" *> idString)
>          <*> option [] (try $ parens $ many1 idString)
>          <*> onDelete
>          <*> onUpdate
>          ]
>     onDelete = onSomething "delete"
>     onUpdate = onSomething "update"
>     onSomething k = option Restrict $ try $ keyword "on"
>                    *> keyword k *> cascade


> createType :: ParsecT [Token] () Identity Statement
> createType = trykeyword "type" >>
>              CreateType
>              <$> idString
>              <*> (keyword "as" *> parens (commaSep1 typeAtt))
>   where
>     typeAtt = TypeAttDef <$> idString <*> idString


create function, support sql functions and
plpgsql functions. Actually parses the body in both cases
and provides a statement list for the body rather than just
a string

> createFunction :: ParsecT [Token] () Identity Statement
> createFunction = do
>   trykeyword "function"
>   fnName <- idString
>   params <- parens $ commaSep param
>   retType <- keyword "returns" *> typeName
>   body <- keyword "as" *> stringVal
>   lang <- readLang
>   (q, b) <- parseBody lang body fnName
>   CreateFunction lang fnName params retType q b <$> pVol
>     where
>         pVol = matchAKeyword [("volatile", Volatile)
>                              ,("stable", Stable)
>                              ,("immutable", Immutable)]
>         readLang = keyword "language" *> matchAKeyword [("plpgsql", Plpgsql)
>                                                        ,("sql",Sql)]
>         parseBody lang body fnName = do
>             case (parseIt
>                   (lexSqlText (extrStr body))
>                   (functionBody lang)
>                   ("function " ++ fnName)
>                   (extrStr body)) of
>                      Left e -> do
>                            --if we have an error parsing the body,
>                            --collect all the needed info from that
>                            --error and rethrow it
>                            sp <- getPosition
>                            error $ "in " ++ show sp
>                                      ++ ", " ++ show e

>                      Right body' -> return (quoteOfString body, body')

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
>                     <* keyword "end" <* optional (symbol ';') <* eof
>               declarePart = keyword "declare"
>                   *> manyTill (try varDef) (lookAhead $ trykeyword "begin")

params to a function

> param :: ParsecT [Token] () Identity ParamDef
> param = choice [
>          try (ParamDef <$> idString <*> typeName)
>         ,ParamDefTp <$> typeName]

variable declarations in a plpgsql function

> varDef :: ParsecT [Token] () Identity VarDef
> varDef = VarDef
>          <$> idString
>          <*> typeName
>          <*> tryMaybeP ((symbols ":=" <|> symbols "=")*> expr) <* symbol ';'


> createView :: ParsecT [Token] () Identity Statement
> createView = trykeyword "view" >>
>              CreateView
>              <$> idString
>              <*> (keyword "as" *> select)

> createDomain :: ParsecT [Token] () Identity Statement
> createDomain = trykeyword "domain" >>
>                CreateDomain
>                <$> idString
>                <*> (tryMaybeP (keyword "as") *> idString)
>                <*> tryMaybeP (keyword "check" *> parens (expr))

> dropSomething :: ParsecT [Token] () Identity Statement
> dropSomething = do
>   x <- try (choice [
>                  Domain <$ keyword "domain"
>                 ,Type <$ trykeyword "type"
>                 ,Table <$ keyword "table"
>                 ,View <$ keyword "view"
>             ])
>   (i,e,r) <- parseDrop idString
>   return $ DropSomething x i e r

> dropFunction :: ParsecT [Token] () Identity Statement
> dropFunction = do
>                trykeyword "function"
>                (i,e,r) <- parseDrop pFun
>                return $ DropFunction i e r
>                where
>                  pFun = (,) <$> idString
>                             <*> parens (many idString)

> parseDrop :: ParsecT [Token] () Identity a
>           -> ParsecT [Token] () Identity (IfExists, [a], Cascade)
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

> whereClause :: ParsecT [Token] () Identity Expression
> whereClause = keyword "where" *> expr

selectlist and selectitem: the bit between select and from
check for into either before the whole list of select columns
or after the whole list

> selectList :: ParsecT [Token] () Identity SelectList
> selectList =
>     choice [
>         flip SelectList <$> readInto <*> itemList
>        ,SelectList <$> itemList <*> option [] readInto]
>   where
>     readInto = trykeyword "into" *> commaSep1 idString
>     itemList = commaSep1 selectItem
>     selectItem = parseOptionalSuffix
>                    SelExp expr
>                    SelectItem () (trykeyword "as" *> idString)

> returning :: ParsecT [Token] () Identity SelectList
> returning = trykeyword "returning" *> selectList

> columnNameList :: ParsecT [Token] () Identity [String]
> columnNameList = parens $ commaSep1 idString

> typeName :: ParsecT [Token] () Identity TypeName
> typeName = choice [
>             SetOfType <$> (trykeyword "setof" *> typeName)
>            ,do
>              s <- idString
>              choice [
>                PrecType s <$> parens integer
>               ,ArrayType (SimpleType s) <$ symbol '[' <* symbol ']'
>               ,return $ SimpleType s]]

> cascade :: ParsecT [Token] () Identity Cascade
> cascade = option Restrict (choice [
>                             Restrict <$ keyword "restrict"
>                            ,Cascade <$ keyword "cascade"])

================================================================================

= plpgsql statements

> plPgsqlStatement :: ParsecT [Token] () Identity Statement
> plPgsqlStatement = sqlStatement True
>                    <|> (choice [
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
>                         <* symbol ';')

> nullStatement :: ParsecT [Token] () Identity Statement
> nullStatement = NullStatement <$ keyword "null"

> continue :: ParsecT [Token] () Identity Statement
> continue = ContinueStatement <$ trykeyword "continue"

> perform :: ParsecT [Token] () Identity Statement
> perform = trykeyword "perform" >>
>           Perform <$> expr

> execute :: ParsecT [Token] () Identity Statement
> execute = trykeyword "execute" >>
>           parseOptionalSuffix
>             Execute expr
>             ExecuteInto () readInto
>     where
>       readInto = trykeyword "into" *> commaSep1 idString

> assignment :: ParsecT [Token] () Identity Statement
> assignment = Assignment
>              -- put the := in the first try to attempt to get a
>              -- better error if the code looks like malformed
>              -- assignment statement
>              <$> try (idString <* (symbols ":=" <|> symbols "="))
>              <*> expr

> returnSt :: ParsecT [Token] () Identity Statement
> returnSt = trykeyword "return" >>
>            choice [
>             ReturnNext <$> (trykeyword "next" *> expr)
>            ,ReturnQuery <$> (trykeyword "query" *> select)
>            ,Return <$> tryMaybeP expr]

> raise :: ParsecT [Token] () Identity Statement
> raise = trykeyword "raise" >>
>         Raise
>         <$> raiseType
>         <*> (extrStr <$> stringVal)
>         <*> option [] (symbol ',' *> commaSep1 expr)
>         where
>           raiseType = matchAKeyword [("notice", RNotice)
>                                      ,("exception", RException)
>                                      ,("error", RError)]

> forStatement :: ParsecT [Token] () Identity Statement
> forStatement = do
>                trykeyword "for"
>                start <- idString
>                keyword "in"
>                choice [(ForSelectStatement start <$> try select <*> theRest)
>                       ,(ForIntegerStatement start
>                               <$> expr
>                               <*> (symbols ".." *> expr)
>                               <*> theRest)]
>   where
>     theRest = keyword "loop" *> many plPgsqlStatement
>               <* keyword "end" <* keyword "loop"

> whileStatement :: ParsecT [Token] () Identity Statement
> whileStatement = trykeyword "while" >>
>                  WhileStatement
>                  <$> (expr <* keyword "loop")
>                  <*> many plPgsqlStatement <* keyword "end" <* keyword "loop"

bit too clever coming up

> ifStatement :: ParsecT [Token] () Identity Statement
> ifStatement = trykeyword "if" >>
>               If
>               <$> (ifPart <:> elseifParts)
>               <*> (elsePart <* endIf)
>   where
>     ifPart = expr <.> (thn *> many plPgsqlStatement)
>     elseifParts = many ((elseif *> expr) <.> (thn *> many plPgsqlStatement))
>     elsePart = option [] (trykeyword "else" *> many plPgsqlStatement)
>     endIf = trykeyword "end" <* keyword "if"
>     thn = keyword "then"
>     elseif = trykeyword "elseif"
>     --might as well these in as well after all that
>     -- can't do <,> unfortunately, so use <.> instead
>     (<.>) a b = (,) <$> a <*> b

> caseStatement :: ParsecT [Token] () Identity Statement
> caseStatement = trykeyword "case" >>
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

> expr :: ParsecT [Token] () Identity Expression
> expr = buildExpressionParser table factor
>        <?> "expression"

> factor :: ParsecT [Token] () Identity Expression
> factor = choice [

order these so the ones which can be valid prefixes of others
appear further down the list

One little speed optimisation, to help with pretty printed code which
can contain a lot of parens - check for nested ((
This little addition speeds up ./ParseFile.lhs sqltestfiles/system.sql on my system
from ~4 minutes to ~4 seconds

>          try $ (lookAhead $ symbols "((") >> parens expr

start with the factors which start with parens - eliminate scalar
subquerys since they're easy to distinguish from the others then do in
predicate before row constructor, since an in predicate can start with
a row constructor, then finally vanilla parens

>          ,scalarSubQuery
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

>          ,stringVal

anything starting with a number has to be a number, so this
could probably appear anywhere in the list. Do float first
since the start of a float looks like an integer. Have to use
try not just because float starts like an integer, but also
to cope with .. operator

>          ,try floatLit
>          ,integerLit

put the factors which start with keywords before the ones which start
with a function, I think these all need try because functions can
start with the same letters as these keywords, and they have to be
tried after these. This claim might be wrong

>          ,caseParse
>          ,exists
>          ,try booleanLiteral
>          ,try nullL

do array before array sub, since array parses an array selector which
looks exactly like an array subscript operator

>          ,array
>          ,try arraySub

now the ones starting with a function name, since a function call
looks like the start of a window expression, try the window expression
first

>          ,try windowFn

try function call before identifier for same reason

>          ,castKeyword
>          ,substring
>          ,try betweenExp
>          ,try functionCall
>          ,try identifier]

== operator table

proper hacky, but sort of does the job
the 'missing' notes refer to pg operators which aren't yet supported
pg's operator table is on this page:
http://www.postgresql.org/docs/8.4/interactive/sql-syntax-lexical.html#SQL-SYNTAX-OPERATORS

will probably need something more custom to handle full range of sql
syntactical novelty

> table :: [[Operator [Token] () Identity Expression]]
> table = [[binary "::" (BinOpCall Cast) AssocLeft]
>          --missing [] for array element select
>         ,[prefix "-" (UnOpCall Neg)]
>         ,[binary "^" (BinOpCall Pow) AssocLeft]
>         ,[binary "*" (BinOpCall Mult) AssocLeft
>          ,binary "/" (BinOpCall Div) AssocLeft
>          ,binary "%" (BinOpCall Mod) AssocLeft]
>         ,[binary "+" (BinOpCall Plus) AssocLeft
>          ,binary "-" (BinOpCall Minus) AssocLeft]
>          --should be is isnull and notnull
>         ,[postfixks ["is", "not", "null"] (UnOpCall IsNotNull)
>          ,postfixks ["is", "null"] (UnOpCall IsNull)]
>          --other operators all added in this list according to the pg docs:
>         ,[binary "<->" (BinOpCall DistBetween) AssocNone
>          ,binary "<=" (BinOpCall Lte) AssocRight
>          ,binary ">=" (BinOpCall Gte) AssocRight
>          ,binary "||" (BinOpCall Conc) AssocLeft
>          ,prefix "@" (UnOpCall Abs)
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
>          ,binary "!=" (BinOpCall NotEql) AssocNone
>          ]
>         ,[prefixk "not" (UnOpCall Not)]
>         ,[binaryk "and" (BinOpCall And) AssocLeft
>          ,binaryk "or" (BinOpCall Or) AssocLeft]]
>     where
>       --use different parsers for symbols and keywords to get the
>       --right whitespace behaviour
>       binary s f
>          = Infix (try (symbols s >> return f))
>       prefix s f
>          = Prefix (try (symbols s >> return f))
>       binaryk s f
>          = Infix (try (keyword s >> return f))
>       prefixk s f
>          = Prefix (try (keyword s >> return f))
>       --postfixk s f
>       --   = Postfix (try (keyword s >> return f))
>       postfixks ss f
>          = Postfix (try ((mapM_ keyword ss) >> return f))

some custom parsers

fix problem parsing <> - don't parse as "<" if it is immediately
followed by ">"

>       lt _ f = Infix (dontFollowWith "<" ">" >> return f)

>       dontFollowWith c1 c2 =
>         try $ symbols c1 -- *> notFollowedBy (symbols c2)

the first argument to these twp above is ignored, it is there so the
symbol can appear in the operator table above for readability purposes

== factor parsers

> scalarSubQuery :: ParsecT [Token] () Identity Expression
> scalarSubQuery = try (symbol '(' *> lookAhead (keyword "select")) >>
>                  ScalarSubQuery
>                  <$> select <* symbol ')'

in predicate - an identifier or row constructor followed by 'in'
then a list of expressions or a subselect

> inPredicate :: ParsecT [Token] () Identity Expression
> inPredicate =
>   InPredicate
>   <$> (try rowCtor <|> Identifier <$> idString)
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

> rowCtor :: ParsecT [Token] () Identity Expression
> rowCtor = Row <$> choice [
>            keyword "row" *> parens (commaSep expr)
>           ,parens $ commaSep2 expr]

 > positionalArg :: ParsecT [Token] u Identity Expression
 > positionalArg = PositionalArg <$> (symbol '$' *> (fromInteger <$> integer))

> floatLit :: ParsecT [Token] () Identity Expression
> floatLit = FloatL <$> float

> integerLit :: ParsecT [Token] () Identity Expression
> integerLit = IntegerL <$> integer

case - only supports 'case when condition' flavour and not 'case
expression when value' currently

> caseParse :: ParsecT [Token] () Identity Expression
> caseParse = trykeyword "case" >>
>             Case <$> many whenParse
>                  <*> tryMaybeP (keyword "else" *> expr)
>                       <* keyword "end"
>   where
>     whenParse = (,) <$> (keyword "when" *> commaSep1 expr)
>                     <*> (keyword "then" *> expr)

> exists :: ParsecT [Token] () Identity Expression
> exists = trykeyword "exists" >>
>          Exists <$> parens select

> booleanLiteral :: ParsecT [Token] () Identity Expression
> booleanLiteral = BooleanL <$> (True <$ trykeyword "true"
>                                <|> False <$ keyword "false")

> nullL :: ParsecT [Token] () Identity Expression
> nullL = NullL <$ keyword "null"

> array :: ParsecT [Token] () Identity Expression
> array = trykeyword "array" >>
>         ArrayL <$> squares (commaSep expr)

when you put expr instead of identifier in arraysub, it stack
overflows, not sure why.

> arraySub :: ParsecT [Token] () Identity Expression
> arraySub = ArraySub <$> identifier <*> squares (commaSep1 expr)

supports basic window functions of the form
fn() over ([partition bit]? [order bit]?)

> windowFn :: ParsecT [Token] () Identity Expression
> windowFn = WindowFn <$> functionCall <* keyword "over"
>                     <*> (symbol '(' *> option [] partitionBy)
>                     <*> option [] orderBy1
>                     <*> option Asc (try $ choice [
>                                                Asc <$ keyword "asc"
>                                               ,Desc <$ keyword "desc"])
>                          <* symbol ')'
>   where
>     orderBy1 = trykeyword "order" *> keyword "by" *> commaSep1 expr
>     partitionBy = trykeyword "partition" *> keyword "by" *> commaSep1 expr

> betweenExp :: ParsecT [Token] () Identity Expression
> betweenExp = Between <$> identifier
>                      <*> (trykeyword "between" *> dodgyParseElement)
>                      <*> (keyword "and" *> dodgyParseElement)
>              --can't use the full expression parser at this time
>              --because of a conflict between the operator 'and' and
>              --the 'and' part of a between
>              --possible solution is to parse a between as binopcall
>              --and (between a) b then fix it up with a second pass
>              --just bodging it for now
>              where
>                dodgyParseElement =
>                    choice [
>                       functionCall
>                      ,identifier
>                      ,parens dodgyParseElement
>                      ,integerLit]

> functionCall :: ParsecT [Token] () Identity Expression
> functionCall = FunCall <$> idString <*> parens (commaSep expr)

> castKeyword :: ParsecT [Token] () Identity Expression
> castKeyword = trykeyword "cast" *> symbol '(' >>
>               CastKeyword <$> expr
>                           <*> (keyword "as" *> typeName <* symbol ')')

> substring :: ParsecT [Token] () Identity Expression
> substring = trykeyword "substring" >> symbol '(' >>
>             Substring
>             <$> expr
>             <*> (keyword "from" *> expr)
>             <*> (keyword "for" *> expr <* symbol ')')

> identifier :: ParsecT [Token] () Identity Expression
> identifier = Identifier <$> idString

================================================================================

= Utility parsers

== tokeny things

keyword has to not be immediately followed by letters or numbers
(symbols and whitespace are ok) so we know that we aren't reading an
identifier which happens to start with a complete keyword

> keyword :: String -> ParsecT [Token] () Identity String
> keyword k = try $ do
>   k1 <- idString
>   when (lcase k1 /= lcase k) $ fail $ "expected" ++ k
>   return k
>   where
>     lcase = map toLower

shorthand to simplify parsers, helps because you can then avoid parens
or $ which in turn doesn't clutter up things and interfere with the
applicative operators

> trykeyword :: String -> ParsecT [Token] () Identity String
> trykeyword = try . keyword


> idString :: MyParser String
> idString = mytoken (\tok -> case tok of
>                                      IdStringTok i -> Just i
>                                      _ -> Nothing)

> symbol :: Char -> MyParser Char
> symbol c = mytoken (\tok -> case tok of
>                                      SymbolTok s | c==s -> Just c
>                                      _           -> Nothing)

> symbols :: String -> ParsecT [Token] () Identity String
> symbols = mapM symbol

> integer :: MyParser Integer
> integer = mytoken (\tok -> case tok of
>                                     IntegerTok n -> Just n
>                                     _ -> Nothing)

> positionalArg :: ParsecT [Token] () Identity Expression
> positionalArg = PositionalArg <$> (mytoken (\tok -> case tok of
>                                     PositionalArgTok n -> Just n
>                                     _ -> Nothing))


> float :: MyParser Double
> float = mytoken (\tok -> case tok of
>                                     FloatTok n -> Just n
>                                     _ -> Nothing)


> stringVal :: MyParser Expression
> stringVal = mytoken (\tok ->
>                   case tok of
>                            StringTok d s | d == "'" -> Just $ StringL s
>                                          | otherwise -> Just $ StringLD d s
>                            _ -> Nothing)

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

== combinatory things

> parens :: ParsecT [Token] () Identity a
>        -> ParsecT [Token] () Identity a
> parens p  = between (symbol '(') (symbol ')') p

> squares :: ParsecT [Token] () Identity a
>        -> ParsecT [Token] () Identity a
> squares p  = between (symbol '[') (symbol ']') p

> tryMaybeP :: (Stream s m t) =>
>              ParsecT s u m a -> ParsecT s u m (Maybe a)
> tryMaybeP p = try (optionMaybe p) <|> return Nothing

> commaSep2 :: ParsecT [Token] () Identity a
>           -> ParsecT [Token] () Identity [a]
> commaSep2 p = sepBy2 p (symbol ',')

> sepBy2 :: (Stream s m t) =>
>           ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
> sepBy2 p sep = (p <* sep) <:> sepBy1 p sep

> commaSep :: ParsecT [Token] () Identity a
>          -> ParsecT [Token] () Identity [a]
> commaSep p = sepBy p (symbol ',')

> commaSep1 :: ParsecT [Token] () Identity a
>           -> ParsecT [Token] () Identity [a]
> commaSep1 p = sepBy1 p (symbol ',')

doesn't seem too gratuitous, comes up a few times

> (<:>) :: (Applicative f) =>
>          f a -> f [a] -> f [a]
> (<:>) a b = (:) <$> a <*> b

pass a list of pairs of strings and values
try each pair k,v in turn,
if keyword k matches then return v
doesn't really add a lot of value

> matchAKeyword :: [(String, a)] -> ParsecT [Token] () Identity a
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
then parse an optional suffix, passing parseResultA
  to this parser -> parseResultB
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

== lexer stuff

> type MyParser a   = GenParser Token () a

> mytoken :: (Tok -> Maybe a) -> MyParser a
> mytoken test
>   = token showToken posToken testToken
>   where
>   showToken (_,tok)   = show tok
>   posToken  (pos,_)   = pos
>   testToken (_,tok)   = test tok

