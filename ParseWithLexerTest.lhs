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
> sqlStatement reqSemi = insert <* symbol ';'

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

= insert, update and delete

insert statement: supports option column name list,
multiple rows to insert and insert from select statements

> insert :: ParsecT [Token] () Identity Statement
> insert = do
>   trykeyword "insert" >> keyword "into"
>   Insert <$> idString
>          <*> option [] (try columnNameList)
>          <*> values
>          <*> return Nothing

> columnNameList :: ParsecT [Token] () Identity [String]
> columnNameList = parens $ commaSep1 idString

> values :: ParsecT [Token] () Identity Statement
> values = trykeyword "values" >>
>          Values <$> commaSep1 (parens $ commaSep1 $ expr)

> expr :: ParsecT [Token] () Identity Expression
> expr = integer <|> string <|> identifierExp


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

> integer :: MyParser Expression
> integer = mytoken (\tok -> case tok of
>                                     IntegerTok n -> Just $ IntegerL n
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
