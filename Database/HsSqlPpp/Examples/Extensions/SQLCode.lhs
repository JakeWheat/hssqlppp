> {-# LANGUAGE TemplateHaskell #-}
>
> module Database.HsSqlPpp.Examples.Extensions.SQLCode
>     (sqlQuote) where
>
> import Language.Haskell.TH.Quote
> import Language.Haskell.TH
>
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Ast

quasiquoter for sql syntax

> sqlQuote :: QuasiQuoter
> sqlQuote = QuasiQuoter parseExprExp parseExprPat
>
> parseExprExp :: String -> Q Exp
> parseExprExp s = parseSql' s >>= dataToExpQ (const Nothing)
>
> parseExprPat :: String -> Q Pat
> parseExprPat s =  parseSql' s >>= dataToPatQ (const Nothing)
>
> parseSql' :: String -> Q [Statement]
> parseSql' s = do
>     Loc fn _ _ (l,c) _ <- location
>     case parseSqlWithPosition fn l c s of
>        Left err -> fail $ show err
>        Right e -> return e
