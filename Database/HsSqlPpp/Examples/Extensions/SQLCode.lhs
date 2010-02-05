
Multiline string support, mainly used to enter multiline sql strings

robbed from http://old.nabble.com/Multi-line-string-literals-are-both-easy--and--elegant-in-Haskell-td19959973.html

> {-# LANGUAGE TemplateHaskell #-}

> module Database.HsSqlPpp.Examples.Extensions.SQLCode
>     (sqlQuote) where

> import Language.Haskell.TH.Quote
> import Language.Haskell.TH

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Utils.Utils

> sqlQuote :: QuasiQuoter
> sqlQuote = QuasiQuoter parseExprExp parseExprPat

> parseExprExp :: String -> Q Exp
> parseExprExp sql = case parseSql "" sql of
>                           Right ast -> dataToExpQ (const Nothing) ast
>                           Left e -> error $ ppExpr e

> parseExprPat :: String -> Q Pat
> parseExprPat sql = case parseSql "" sql of
>                           Right ast -> dataToPatQ undefined ast
>                           Left e -> error $ ppExpr e