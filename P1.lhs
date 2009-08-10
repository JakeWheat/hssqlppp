
> module P1 where

> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec as P (parse)
> import qualified Text.ParserCombinators.Parsec.Token as P
> import Text.ParserCombinators.Parsec.Language
> import qualified Text.Parsec.Prim
> import Control.Monad.Identity
> import Text.ParserCombinators.Parsec.Expr
> import Text.ParserCombinators.Parsec.Language (haskellStyle)


> import Grammar

> expr :: Parser Expression
> expr =
>   buildExpressionParser table factor
>   <?> "expression"

> parens :: Text.Parsec.Prim.ParsecT String u Identity a
>           -> Text.Parsec.Prim.ParsecT String u Identity a
> parens = P.parens lexer

> lexer :: P.GenTokenParser String u Identity
> lexer = P.makeTokenParser (haskellDef
>                            { reservedOpNames = ["*","/","+","-"]
>                            })

>   -- Recognizes a factor in an expression
> factor  = parens expr
>           <|> number
>           <?> "simple expression"

>   -- Recognizes a number
> number  :: Parser Expression
> number  = do{ ds <- many1 digit
>               ; return (IntegerL (read ds))
>               }
>           <?> "number"
>   -- Specifies operator, associativity, precendence, and constructor to execute
>   -- and built AST with.
> table =
>       [[prefix "-" (Exp Mult (IntegerL (-1)))]
>       ,[binary "^" (Exp Pow) AssocRight]
>       ,[binary "*" (Exp Mult) AssocLeft
>        ,binary "/" (Exp Div) AssocLeft
>        ,binary "%" (Exp Mod) AssocLeft]
>       ,[binary "+" (Exp Plus) AssocLeft
>        ,binary "-" (Exp Minus) AssocLeft]
>       ]
>     where
>       binary s f assoc
>          = Infix (do{ string s; return f}) assoc
>       prefix s f
>          = Prefix (do{ string s; return f})
>

> -- Parses a string into an AST, using the parser defined above
> parse s = case P.parse expr "" s of
>   Right ast -> ast
>   Left e -> error $ show e
>

