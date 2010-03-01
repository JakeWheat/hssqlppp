Copyright 2010 Jake Wheat

> {- | A quasiquoter for SQL. Antiquoting is a bit inconsistent. The splice variable
>    names must be all lower case because of a limitation in the parser.
>
> Example:
>
> >
> > {-# LANGUAGE QuasiQuotes #-}
> > import Database.HsSqlPpp.Ast
> > import Database.HsSqlPpp.SqlQuote
> > import Database.HsSqlPpp.Annotation
> >
> > test :: Statement
> > test = [$sqlStmt|
> >
> >   create table $(tablename) (
> >    $(varname) $(typename)
> >   );
> >
> >         |]
> >   where
> >     tablename = "my_table"
> >     varname = "my_field"
> >     typename = "text"
> >
>
> See <http://community.haskell.org/~JakeWheat/hssqlppp/QuasiQuoteTests.html>
> for more simple examples, and
> <http://community.haskell.org/~JakeWheat/hssqlppp/pandoc_source/examples/Database/HsSqlPpp/Examples/Extensions/
> for some example files which use quasiquotation to do ast
> transformations which implement syntax extensions to sql
>      -}
>
> {-# LANGUAGE TemplateHaskell,ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.SqlQuote
>     (sqlStmts,sqlStmt,pgsqlStmts,pgsqlStmt,sqlExpr) where

> import Language.Haskell.TH.Quote
> import Language.Haskell.TH
> import Data.Generics
> import Data.List
>
> import Database.HsSqlPpp.Parsing.ParserInternal
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.AstInternals.AstAnti
>
> -- | parses Statements
> sqlStmts :: QuasiQuoter
> sqlStmts = QuasiQuoter (parseExprExp parseAntiSql) (parseExprPat parseAntiSql)
>
> parseOneAntiSql :: Parser String Statement
> parseOneAntiSql f l c s =
>     case parseAntiSql f l c s of
>       Right [st] -> Right st
>       Right _ -> Left "got multiple statements"
>       Left e -> Left $ show e
>
> -- | parses a single Statement
> sqlStmt :: QuasiQuoter
> sqlStmt = QuasiQuoter (parseExprExp parseOneAntiSql) (parseExprPat parseOneAntiSql)
>
> -- | parses plpgsql Statements
> pgsqlStmts :: QuasiQuoter
> pgsqlStmts = QuasiQuoter (parseExprExp parseAntiPlpgsql) (parseExprPat parseAntiPlpgsql)
>
> -- | parses a plpgsql Statement
> pgsqlStmt :: QuasiQuoter
> pgsqlStmt = QuasiQuoter (parseExprExp parseOneAntiPlpgsql) (parseExprPat parseOneAntiPlpgsql)
>
> parseOneAntiPlpgsql :: Parser String Statement
> parseOneAntiPlpgsql f l c s =
>     case parseAntiPlpgsql f l c s of
>       Right [st] -> Right st
>       Right _ -> Left "got multiple statements"
>       Left e -> Left $ show e
>
> -- | parse an Expression
> sqlExpr :: QuasiQuoter
> sqlExpr = QuasiQuoter (parseExprExp parseAntiExpression) (parseExprPat parseAntiExpression)

> type Parser e a = (String
>                    -> Int
>                    -> Int
>                    -> String
>                    -> Either e a)

~~~~
these return asts of from the module
Database.HsSqlPpp.AstInternals.AstAnti, but when you expect the result
of a quasiquote to be from the module Database.HsSqlPpp.Ast, it
magically converts from one to the other ...
~~~~

>
> parseExprExp :: (Show e, Data a) =>
>                 (Parser e a) -> String -> Q Exp
> parseExprExp p s = (parseSql' p) s >>=  dataToExpQ (const Nothing
>                        `extQ` antiExpE
>                        `extQ` antiStrE
>                        `extQ` antiTriggerEventE
>                        `extQ` antiStatementE)
>
> parseExprPat ::(Show e, Data a) =>
>                (Parser e a) ->  String -> Q Pat
> parseExprPat p s = (parseSql' p) s >>=  dataToPatQ (const Nothing
>                        `extQ` antiExprP
>                        `extQ` antiStrP
>                        `extQ` annotToWildCard
>                        --`extQ` antiTriggerEventE
>                        --`extQ` antiStatementE
>                                   )
>

hack: replace annotations with wildcards, if we don't do this then
pattern matches generally don't work since the source position
annotations from the parser don't match up.

> annotToWildCard :: Annotation -> Maybe PatQ
> annotToWildCard (_::Annotation) = Just $ return WildP
>
> parseSql' :: (Data a, Show e) => Parser e a -> String -> Q a
> parseSql' p s = do
>     Loc fn _ _ (l,c) _ <- location
>     either (fail . show) return (p fn l c s)
>
> antiExpE :: Expression -> Maybe ExpQ
> antiExpE v = fmap varE (antiExp v)

> antiExprP :: Expression -> Maybe PatQ
> antiExprP v = fmap varP $ antiExp v

> antiExp :: Expression -> Maybe Name
> antiExp (AntiExpression v) = Just $ mkName v
> antiExp _ = Nothing

antistatements not working ...
trying to replace a single antistatement node with multiple statement
nodes and my generics skills aren't up to the task.

> antiStatementE :: [Statement] -> Maybe ExpQ
> antiStatementE (AntiStatement v : tl) =
>    Just (listE (vref : conArgs))
>    where
>      conArgs = gmapQ (dataToExpQ (const Nothing
>                        `extQ` antiExpE
>                        `extQ` antiStrE
>                        `extQ` antiTriggerEventE
>                        `extQ` antiStatementE)) tl
>      vref :: ExpQ
>      vref = varE $ mkName v
> antiStatementE _ = Nothing



> antiStrE :: String -> Maybe ExpQ
> antiStrE v = fmap varE $ antiStr v

> antiStrP :: String -> Maybe PatQ
> antiStrP v = fmap varP $ antiStr v

> antiStr :: [Char] -> Maybe Name
> antiStr v =
>   fmap mkName $ getSpliceName v
>   where
>     getSpliceName s | isPrefixOf "$(" s && last s == ')' =
>       Just $ drop 2 $ take (length s - 1) s
>                     | isPrefixOf "$s(" s && last s == ')' =
>       Just $ drop 3 $ take (length s - 1) s
>                     | isPrefixOf "$i(" s && last s == ')' =
>       Just $ drop 3 $ take (length s - 1) s
>                     | otherwise = Nothing

>
> antiTriggerEventE :: TriggerEvent -> Maybe ExpQ
> antiTriggerEventE (AntiTriggerEvent v) = Just $ varE $ mkName v
> antiTriggerEventE _ = Nothing

~~~~

 > antiExpP :: Expression -> Maybe PatQ
 > antiExpP (AntiExpression v ) = Just $ varP $ mkName v
 > antiExpP _ = Nothing

to add support for a new splice location, add the type name to the
list at the top of MakeAntiNodes, adjust the parser to parse splices
at that point, and add a new antiQ function in this file


new idea - to support antiquotation as much as possible, have more
than one splice syntax:

[$sqlExpr| $(x) |] - want to do a splice like this, sometimes it
should be

where x= "str" gives
Identifer [] "str" <- need $(x) to parse as an antiidentifier

and sometimes

where x = FunCall [] "fn" []
gives
FunCall [] "fn" [] <- need $(x) to parse as an antiexpression

need context which we don't have to make this decision (and would
probably be really hard even if the context was available)

so - use two different splice syntaxes.

to avoid doing string splices using [$sqlExpr| '$(sp)' |] which is
ugly and wrong (?), add another splice for strings?

...

work on tests to try to get some sort of design - want to minimise the
number of splice syntaxes at the same time not make it difficult to
work out which syntax to use in which spot.



what needs to be done to support _ in pattern quasiquotes? -> I think
it's just adding a wildcard ctor to the appropriate ast types using
makeantinodes, and adding in lexing and parsing support - actually
using wildcards is now working with the annotation approach above

~~~~

Some ideas about moving forward

after working on a number of extensions, it seems clear that the quasi
quotation is a bit limited, specifically the number of splice points
is too limiting and a lot of ast transforms end up using a lot of ast
types directly rather than via quotes which is crap.

some possible resolutions:

live with it

add lots more splice points, will need lots of different splice
syntaxes e.g. to cope with splicing a trigger event list or a single
trigger event. Will also need to have a load more quote functions to
parse sql fragments for different contexts.

try and use e.g. a haskell or lisp like syntax for sql, to reduce the
number of ast types drastically - this will allow much more regular
use of splice points, quotes, etc.

variant on this idea is instead to move to a tutorial d like syntax,
which is vastly more regular than sql syntax and should allow for a
much smaller range of quoters and splice syntaxes also.

maybe might as well switch to haskelldb? the haskell or lisp like
version of sql syntax keeps us writing the databases in sql, whereas
tutorial d. haskelldb mean sql really disappears into the
plumbing. These approaches might solve some problems with custom
syntax also.
