Copyright 2010 Jake Wheat

> {-# LANGUAGE TemplateHaskell #-}
>
> {- | A quasiquoter for SQL.
>
>    Example:
>
> >
> > {-# LANGUAGE QuasiQuotes #-}
> > import Database.HsSqlPpp.Ast
> > import Database.HsSqlPpp.SqlQuote
> > import Database.HsSqlPpp.Annotation
> >
> > test :: [Statement]
> > test = [$sqlQuote|
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

> -- | parses a 'Statement' list
> sqlStmts :: QuasiQuoter
> sqlStmts = QuasiQuoter (parseExprExp parseAntiSql) parseExprPat

> parseOneAntiSql :: Parser String Statement
> parseOneAntiSql f l c s =
>     case parseAntiSql f l c s of
>       Right [st] -> Right st
>       Right _ -> Left "got multiple statements"
>       Left e -> Left $ show e

> -- | parses a single Statement
> sqlStmt :: QuasiQuoter
> sqlStmt = QuasiQuoter (parseExprExp parseOneAntiSql) parseExprPat

> -- | parses plpgsql statements
> pgsqlStmts :: QuasiQuoter
> pgsqlStmts = QuasiQuoter (parseExprExp parseAntiPlpgsql) parseExprPat

> -- | parses a plpgsql statement
> pgsqlStmt :: QuasiQuoter
> pgsqlStmt = QuasiQuoter (parseExprExp parseOneAntiPlpgsql) parseExprPat

> parseOneAntiPlpgsql :: Parser String Statement
> parseOneAntiPlpgsql f l c s =
>     case parseAntiPlpgsql f l c s of
>       Right [st] -> Right st
>       Right _ -> Left "got multiple statements"
>       Left e -> Left $ show e


> -- | parse an 'Expression'
> sqlExpr :: QuasiQuoter
> sqlExpr = QuasiQuoter (parseExprExp parseAntiExpression) parseExprPat

these badboys return asts of from the module
Database.HsSqlPpp.AstInternals.AstAnti, but when you expect the result
of a quasiquote to be from the module Database.HsSqlPpp.Ast, it
magically converts from one to the other ...

> type Parser e a = (String
>                    -> Int
>                    -> Int
>                    -> String
>                    -> Either e a)

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

> parseSql' :: Show e => Parser e a -> String -> Q a
> parseSql' p s = do
>     Loc fn _ _ (l,c) _ <- location
>     either (fail . show) return (p fn l c s)

> antiExpE :: Expression -> Maybe ExpQ
> antiExpE (AntiExpression v) = Just $ varE $ mkName v
> antiExpE _ = Nothing

antistatements not working ...

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
> antiStrE v | isSpliceName v = Just $ varE $ mkName $ getSpliceName v
>                               where
>                                 isSpliceName x = isPrefixOf "$(" x
>                                                  && last x == ')'
>                                 getSpliceName x = drop 2 $ take (length x - 1) x
> antiStrE _ = Nothing

> antiTriggerEventE :: TriggerEvent -> Maybe ExpQ
> antiTriggerEventE (AntiTriggerEvent v) = Just $ varE $ mkName v
> antiTriggerEventE _ = Nothing

 > antiExpP :: Expression -> Maybe PatQ
 > antiExpP (AntiExpression v ) = Just $ varP $ mkName v
 > antiExpP _ = Nothing

to add support for a new splice location, add the type name to the
list at the top of MakeAntiNodes, adjust the parser to parse splices
at that point, and add a new antiQ function in this file