
> module ParseWithLexerTest (parseSql) where

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

> import Tree

===============================================================================

= Top level parsing functions

parse fully formed sql

> parseSql :: String -> Either ParseError Statement
> parseSql s = case lexSqlText s of
>                Left er -> Left er
>                Right toks -> parse insert "(unknown)" toks

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

= Utility parsers

== tokeny things


keyword has to not be immediately followed by letters or numbers
(symbols and whitespace are ok) so we know that we aren't reading an
identifier which happens to start with a complete keyword

> keyword :: String -> ParsecT [Token] () Identity ()
> keyword k = do
>   k1 <- idString
>   when (k1 /=k) $ fail $ "expected" ++ k

> idString :: MyParser String
> idString = mytoken (\tok -> case tok of
>                                      IdStringTok i -> Just i
>                                      _ -> Nothing)

> identifierExp :: ParsecT [Token] () Identity Expression
> identifierExp = Identifier <$> idString

shorthand to simplify parsers, helps because you can then avoid parens
or $ which in turn doesn't clutter up things and interfere with the
applicative operators

> trykeyword :: String -> ParsecT [Token] () Identity ()
> trykeyword = try . keyword

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
