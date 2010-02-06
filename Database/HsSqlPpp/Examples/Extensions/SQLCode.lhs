> {-# LANGUAGE TemplateHaskell #-}
>
> module Database.HsSqlPpp.Examples.Extensions.SQLCode
>     (sqlQuote) where
>
> import Language.Haskell.TH.Quote
> import Language.Haskell.TH
> import Data.Generics
> --import Debug.Trace
> import Data.List
>
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Utils.Utils

quasiquoter for sql syntax

> sqlQuote :: QuasiQuoter
> sqlQuote = QuasiQuoter parseExprExp parseExprPat
>
> parseExprExp :: String -> Q Exp
> parseExprExp s = parseSql' s >>= dataToExpQ (const Nothing `extQ` antiExpE
>                                                        `extQ` antiStrE)
>
> parseExprPat :: String -> Q Pat
> parseExprPat s =  parseSql' s >>= dataToPatQ (const Nothing `extQ` antiExpP)
>
> parseSql' :: String -> Q [Statement]
> parseSql' s = do
>     Loc fn _ _ (l,c) _ <- location
>     either (fail . show) return (parseSqlWithPosition fn l c s)

attempt to support antiquotes

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
