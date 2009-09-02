Copyright 2009 Jake Wheat

> module ArgsParser where

> import Text.Parsec hiding(many, optional, (<|>), string)
> import Text.Parsec.Language
> import Text.Parsec.Char
> import qualified Text.Parsec.Token as P

> import Control.Applicative
> import Control.Monad.Identity
> import ParseErrors

> import Ast

> parseArgString :: String -> Either ExtendedError [Type]
> parseArgString s = convertToExtendedError (runParser (parser <* eof) () "" s) "" s

> parseArg :: String -> Either ExtendedError Type
> parseArg s = convertToExtendedError (runParser (arg <* eof) () "" s) "" s

> parser :: ParsecT String () Identity [Type]
> parser = commaSep arg

> tstring :: String -> ParsecT String () Identity String
> tstring s = try $ string s

> arg :: ParsecT String () Identity Type
> arg = choice [
>        try parseArgType
>       ,try namedType
>       ]
>       where
>         arg1 = choice [
>                 Pseudo Any <$ tstring "\"any\""
>                ,Pseudo AnyArray <$ tstring "anyarray"
>                ,Pseudo AnyElement <$ tstring "anyelement"
>                ,Pseudo AnyEnum <$ tstring "anyenum"
>                ,Pseudo AnyNonArray <$ tstring "anynonarray"
>                ,Pseudo Cstring <$ tstring "cstring"
>                ,Pseudo Record <$ tstring "record"
>                ,Pseudo Trigger <$ tstring "trigger"
>                ,Pseudo Void <$ tstring "void"
>                ,SetOfType <$> (tstring "SETOF " *> arg)
>                ,ScalarType <$> try (choice (map tstring typesWithSpaces))
>                ,ScalarType <$> try iden
>                ,ScalarType <$> try (quoted iden)
>                ]
>         parseArgType = threadOptionalSuffix arg1 makeArray
>         makeArray a = ArrayType a <$ string "[]"
>         quoted p = (\l -> "\"" ++ l ++ "\"") <$> (char '"' *> p <* char '"')
>         namedType = iden *> char ' ' *> parseArgType
>         iden = (:) <$> letter <*> secondOnwards
>         secondOnwards = many (alphaNum <|> char '_')



> lexer :: P.GenTokenParser String () Identity
> lexer = P.makeTokenParser emptyDef

> commaSep :: ParsecT String () Identity a -> ParsecT String () Identity [a]
> commaSep = P.commaSep lexer

> typesWithSpaces :: [String]
> typesWithSpaces = ["timestamp with time zone"
>                   ,"timestamp without time zone"
>                   ,"time without time zone"
>                   ,"time with time zone"
>                   ,"double precision"
>                   ,"character varying"
>                   ,"bit varying"]

> threadOptionalSuffix :: (Stream s m t) =>
>                         ParsecT s u m a
>                      -> (a -> ParsecT s u m a)
>                      -> ParsecT s u m a
> threadOptionalSuffix p1 p2 = do
>   x <- p1
>   option x (try $ p2 x)

