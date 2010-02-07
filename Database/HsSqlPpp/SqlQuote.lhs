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
>     (sqlQuote) where
>
> import Language.Haskell.TH.Quote
> import Language.Haskell.TH
> import Data.Generics
> import Data.List
>
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.AstInternals.AstAnti

> sqlQuote :: QuasiQuoter
> sqlQuote = QuasiQuoter parseExprExp parseExprPat

these badboys return asts of from the module
Database.HsSqlPpp.AstInternals.AstAnti, but when you expect the result
of a quasiquote to be from the module Database.HsSqlPpp.Ast, it
magically converts from one to the other ...

> parseExprExp :: String -> Q Exp
> parseExprExp s = parseSql' s >>= dataToExpQ (const Nothing `extQ` antiExpE
>                                                            `extQ` antiStrE)
>
> parseExprPat :: String -> Q Pat
> parseExprPat s =  parseSql' s >>= dataToPatQ (const Nothing `extQ` antiExpP)
>
> parseSql' :: String -> Q [Statement]
> parseSql' s = do
>     Loc fn _ _ (l,c) _ <- location
>     either (fail . show) return (parseSqlWithPositionAnti fn l c s)

> antiExpE :: Expression -> Maybe ExpQ
> antiExpE (AntiExpression v) = Just $ varE $ mkName v
> antiExpE _ = Nothing

> antiStrE :: String -> Maybe ExpQ
> antiStrE v | isSpliceName v = Just $ varE $ mkName $ getSpliceName v
>                               where
>                                 isSpliceName x = isPrefixOf "$(" x
>                                                  && last x == ')'
>                                 getSpliceName x = drop 2 $ take (length x - 1) x
> antiStrE _ = Nothing

> antiExpP :: Expression -> Maybe PatQ
> antiExpP (AntiExpression v ) = Just $ varP $ mkName v
> antiExpP _ = Nothing
