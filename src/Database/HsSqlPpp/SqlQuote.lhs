Copyright 2010 Jake Wheat

> {-# LANGUAGE TemplateHaskell #-}
>
> {- | A quasiquoter for SQL. Antiquoting is a bit inconsistent.
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
> for more examples.
>
>      -}
> module Database.HsSqlPpp.SqlQuote
>     (sqlStmts,sqlStmt,pgsqlStmts,pgsqlStmt,sqlExpr) where
>
> import Language.Haskell.TH.Quote
> import Language.Haskell.TH
> import Data.Generics
> import Data.List
> --import Control.Monad as M
> --import Debug.Trace
>
> import Database.HsSqlPpp.Parsing.ParserInternal
> --import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.AstInternals.AstAnti
>
> -- | parses Statements
> sqlStmts :: QuasiQuoter
> sqlStmts = QuasiQuoter (parseExprExp parseAntiSql) parseExprPat
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
> sqlStmt = QuasiQuoter (parseExprExp parseOneAntiSql) parseExprPat
>
> -- | parses plpgsql Statements
> pgsqlStmts :: QuasiQuoter
> pgsqlStmts = QuasiQuoter (parseExprExp parseAntiPlpgsql) parseExprPat
>
> -- | parses a plpgsql Statement
> pgsqlStmt :: QuasiQuoter
> pgsqlStmt = QuasiQuoter (parseExprExp parseOneAntiPlpgsql) parseExprPat
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
> sqlExpr = QuasiQuoter (parseExprExp parseAntiExpression) parseExprPat

~~~~
these badboys return asts of from the module
Database.HsSqlPpp.AstInternals.AstAnti, but when you expect the result
of a quasiquote to be from the module Database.HsSqlPpp.Ast, it
magically converts from one to the other ...
~~~~

> type Parser e a = (String
>                    -> Int
>                    -> Int
>                    -> String
>                    -> Either e a)
>
> parseExprExp :: (Show e, Data a) =>
>                 (Parser e a) -> String -> Q Exp
> parseExprExp p s = (parseSql' p) s >>=  dataToExpQ (const Nothing
>                        `extQ` antiExpE
>                        `extQ` antiStrE
>                        `extQ` antiTriggerEventE
>                        `extQ` antiStatementE)
>
> parseExprPat :: String -> Q Pat
> parseExprPat _ =  undefined
>
>
> parseSql' :: Show e => Parser e a -> String -> Q a
> parseSql' p s = do
>     Loc fn _ _ (l,c) _ <- location
>     either (fail . show) return (p fn l c s)
>
> antiExpE :: Expression -> Maybe ExpQ
> antiExpE (AntiExpression v) = Just $ varE $ mkName v
> antiExpE _ = Nothing

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
>
> antiStrE :: String -> Maybe ExpQ
> antiStrE v | isSpliceName v = Just $ varE $ mkName $ getSpliceName v
>                               where
>                                 isSpliceName x = isPrefixOf "$(" x
>                                                  && last x == ')'
>                                 getSpliceName x = drop 2 $ take (length x - 1) x
> antiStrE _ = Nothing
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
ugly and wrong, add another splice for strings?

...

work on tests to try to get some sort of design - want to minimise the
number of splice syntaxes at the same time not make it difficult to
work out which syntax to use in which spot.



what needs to be done to support _ in pattern quasiquotes?

~~~~