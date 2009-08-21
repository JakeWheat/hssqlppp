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
          <$> identifierString
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning

We transform this in several stages through to vanilla do notation,
this is equivalent to

  delete = trykeyword "delete" >> keyword "from" >>
           Delete
           `liftM` identifierString
           `ap` tryMaybeP whereClause
           `ap` tryMaybeP returning

(which in turn is equivalent to

 delete = trykeyword "delete" >> keyword "from" >>
          liftM3 Delete
                 identifierString
                 (tryMaybeP whereClause)
                 (tryMaybeP returning)

maybe this is a backward step)

finally, in do notation it is:

 delete = do
          trykeyword "delete"
          keyword "from"
          i <- identifierString
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
          <$> identifierString
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning

there are some other notable aspects, in the bit before the 'Delete'.

* the code here is before the delete to look like a kind of filter,
  many of the parsers look like this. The alternative code that this
  style replaced is this:

 delete = Delete
          <$> (trykeyword "delete" *> keyword "from" *> identifierString)
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning

  which I think looks less clear.

There are also a couple of tricks in this line which are connected to
parsing the haskell code and precedence:

if we use *> instead of >> at the top, we need to use (), this is why
 >> is chosen:

 delete = trykeyword "delete" *> keyword "from" *>
          (Delete
          <$> identifierString
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning)

(The code prefers *> in places where *> and >> are interchangeable
without adding ().)

trykeyword could be replaced:

 delete = try (keyword "delete") >> keyword "from" >>
          Delete
          <$> identifierString
          <*> tryMaybeP whereClause
          <*> tryMaybeP returning

but it's extra brackets (trykeyword is only shorthand like this used
in the code, since it appears all over and loads of () and $ get added
without using trykeyword).

One last aspect:

 delete = trykeyword "delete" >> keyword "from" >>
          Delete
          <$> identifierString
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
               <$> identifierString
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

> module ParseWithLexerTest (
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
> --import Text.Parsec.Expr
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
> parseSql s = case lexSqlText s of
>                Left er -> Left er
>                Right toks -> convertToExtendedError
>                                (parse sqlStatements "(unknown)" toks) "" s

> parseSqlFile :: String -> IO (Either ExtendedError [Statement])
> parseSqlFile fn = do
>   sc <- readFile fn
>   x <- lexSqlFile fn
>   return $ case x of
>                   Left er -> Left er
>                   Right toks -> convertToExtendedError
>                                   (parse sqlStatements fn toks) fn sc

Parse expression fragment, used for testing purposes

> parseExpression :: String -> Either ExtendedError Expression
> parseExpression s = case lexSqlText s of
>                       Left er -> Left er
>                       Right toks -> convertToExtendedError
>                                       (parse (expr <* eof) "" toks) "" s


parse plpgsql statements, used for testing purposes

> parsePlpgsql :: String -> Either ExtendedError [Statement]
> parsePlpgsql s =  case lexSqlText s of
>                       Left er -> Left er
>                       Right toks -> convertToExtendedError
>                                       (parse (many plPgsqlStatement <* eof)
>                                         "(unknown)" toks) "" s

================================================================================

= Parsing top level statements

> sqlStatements :: ParsecT [Token] () Identity [Statement]
> sqlStatements = many (sqlStatement True) <* eof

parse a statement

> sqlStatement :: Bool -> ParsecT [Token] () Identity Statement
> sqlStatement reqSemi = choice [
>                          insert
>                         ,select]
>                        <* symbol ';'

-- > sqlStatement :: Bool -> ParsecT String () Identity Statement
-- > sqlStatement reqSemi = (choice [
-- >                         select
-- >                        ,values
-- >                        ,insert
-- >                        ,update
-- >                        ,delete
-- >                        ,truncateSt
-- >                        ,trykeyword "create" *>
-- >                                    choice [
-- >                                       createTable
-- >                                      ,createType
-- >                                      ,createFunction
-- >                                      ,createView
-- >                                      ,createDomain]
-- >                        ,trykeyword "drop" *>
-- >                                    choice [
-- >                                       dropSomething
-- >                                      ,dropFunction]
-- >                        ]
-- >     <* (if reqSemi then semi >> return () else optional semi >> return ()))
-- >    --copy reads the semi colon itself to cope with possibly copying from stdin
-- >    <|> copy

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


================================================================================

= insert, update and delete

insert statement: supports option column name list,
multiple rows to insert and insert from select statements

> insert :: ParsecT [Token] () Identity Statement
> insert = trykeyword "insert" >> keyword "into" >>
>          Insert
>          <$> idString
>          <*> option [] (try columnNameList)
>          <*> values
>          <*> return Nothing

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

-- >                    <|> (choice [
-- >                          continue
-- >                         ,execute
-- >                         ,caseStatement
-- >                         ,assignment
-- >                         ,ifStatement
-- >                         ,returnSt
-- >                         ,raise
-- >                         ,forStatement
-- >                         ,whileStatement
-- >                         ,perform
-- >                         ,nullStatement]
-- >                         <* semi)






> identifierExp :: ParsecT [Token] () Identity Expression
> identifierExp = Identifier <$> idString



================================================================================

= Utility parsers

== tokeny things

keyword has to not be immediately followed by letters or numbers
(symbols and whitespace are ok) so we know that we aren't reading an
identifier which happens to start with a complete keyword

> keyword :: String -> ParsecT [Token] () Identity ()
> keyword k = do
>   k1 <- idString
>   when (k1 /=k) $ fail $ "expected" ++ k

shorthand to simplify parsers, helps because you can then avoid parens
or $ which in turn doesn't clutter up things and interfere with the
applicative operators

> trykeyword :: String -> ParsecT [Token] () Identity ()
> trykeyword = try . keyword


> idString :: MyParser String
> idString = mytoken (\tok -> case tok of
>                                      IdStringTok i -> Just i
>                                      _ -> Nothing)



doesn't seem too gratuitous, comes up a few times

 > (<:>) :: (Applicative f) =>
 >          f a -> f [a] -> f [a]
 > (<:>) a b = (:) <$> a <*> b


> symbol :: Char -> MyParser Char

> symbol c = mytoken (\tok -> case tok of
>                                      SymbolTok s | c==s -> Just c
>                                      _           -> Nothing)

> parens :: ParsecT [Token] () Identity a
>        -> ParsecT [Token] () Identity a
> parens p  = between (symbol '(') (symbol ')') p

> type MyParser a   = GenParser Token () a

> mytoken :: (Tok -> Maybe a) -> MyParser a
> mytoken test
>   = token showToken posToken testToken
>   where
>   showToken (_,tok)   = show tok
>   posToken  (pos,_)   = pos
>   testToken (_,tok)   = test tok

> integer :: MyParser Integer
> integer = mytoken (\tok -> case tok of
>                                     IntegerTok n -> Just n
>                                     _ -> Nothing)

> string :: MyParser Expression
> string = mytoken (\tok ->
>                   case tok of
>                            StringTok d s | d == "'" -> Just $ StringL s
>                                          | otherwise -> Just $ StringLD d s
>                            _ -> Nothing)

> commaSep1 :: ParsecT [Token] () Identity a
>           -> ParsecT [Token] () Identity [a]
> commaSep1 p = sepBy1 p (symbol ',')

> commaSep :: ParsecT [Token] () Identity a
>          -> ParsecT [Token] () Identity [a]
> commaSep p = sepBy p (symbol ',')


> tryMaybeP :: GenParser tok st a
>           -> ParsecT [tok] st Identity (Maybe a)
> tryMaybeP p = try (optionMaybe p) <|> return Nothing


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



> expr :: ParsecT [Token] () Identity Expression
> expr = (IntegerL <$> integer) <|> string <|> identifierExp


> functionCall :: ParsecT [Token] () Identity Expression
> functionCall = FunCall <$> idString <*> parens (commaSep expr)
