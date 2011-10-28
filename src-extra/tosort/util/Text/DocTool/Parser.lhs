
want to treat non literate source code in a similar way to lhs files -
the code is syntax highlighted, and the comments are converted as
markdown

specifically: convert the multiline comments into markdown, and the
rest into codeblocks

first parse the all the comments out
then run over the list to fix it up: a comment is converted to code if
is isn't the first element and the previous element is code and
doesn't end with \n >> spaces
it doesn't contain a \n
it isn't the last element and the next element doesn't start with spaces >> \n

also: option to not have nested comments
      ignore multiline comment symbols inside single line comments

> {-# LANGUAGE DeriveDataTypeable #-}
> module Text.DocTool.Parser
>     (Cc(..)
>     ,parseSource) where

> import Text.Parsec hiding(many, optional, (<|>), string, label)
> --import Text.Parsec.Expr
> import Text.Parsec.String
> ----import Text.Parsec.Perm
> import Text.Parsec.Char
>
> import Control.Applicative
> import Control.Monad.Identity
> --import Control.Monad.Error

> --import Debug.Trace

> data Cc = Code String
>         | Comments String
>           deriving Show

===========================================

fixup tree:

todo

===========================================

parsing

> type SParser =  GenParser Token ()

> parseSource :: String -> String -> String -> Either ParseError [Cc]
> parseSource cs ce s = do
>   l <- lexS cs ce s
>   --trace (show l) $ return ()
>   runParser (many item <* eof) () "" l



> item :: SParser Cc
> item = comment <|> code

> comment :: SParser Cc
> comment =
>   Comments <$> commentString
>   where
>     commentString = do
>       a <- commentStart
>       b <- commentCtd
>       return $ concat (a:b)
>     commentCtd = choice [
>                   do
>                   a <- commentString
>                   b <- commentCtd
>                   return (a:b)
>                  ,do
>                   a <- text
>                   b <- commentCtd
>                   return (a:b)
>                  ,do
>                    a <- commentEnd
>                    return [a]
>                  ]

> code :: SParser Cc
> code = Code <$> text

> commentStart :: SParser String
> commentStart = mytoken (\tok -> case tok of
>                                     CommentStart s -> Just s
>                                     _ -> Nothing)

> commentEnd :: SParser String
> commentEnd = mytoken (\tok -> case tok of
>                                     CommentEnd s -> Just s
>                                     _ -> Nothing)

> text :: SParser String
> text = mytoken (\tok -> case tok of
>                                     Text s -> Just s
>                                     _ -> Nothing)

> mytoken :: (Tok -> Maybe String) -> SParser String
> mytoken test
>   = token showToken posToken testToken
>   where
>   showToken (_,tok)   = show tok
>   posToken  (posi,_)  = posi
>   testToken (_,tok)   = test tok


===========================================

Lexing

> type Token = (SourcePos, Tok)

> data Tok = CommentStart String
>            | CommentEnd String
>            | Text String
>              deriving (Eq,Show)

> lexS :: String -> String -> String -> Either ParseError [Token]
> lexS cs ce = runParser (many $ tk cs ce) () ""

> type TParser =  GenParser Char ()

> tk :: String -> String -> TParser Token
> tk cst cet = do
>   sp <- getPosition
>   x <- choice [
>       CommentStart <$> cs
>      ,CommentEnd <$> ce
>      ,Text <$> (anyChar <:> manyTill anyChar (cse <|> eof))]
>   --trace ("got " ++ show x) $ return ()
>   return (sp,x)
>     where
>       cse = lookAhead (cs <|> ce) >> return ()
>       cs = try (string cst)
>       ce = try (string cet)

> (<:>) :: Monad m => m a1 -> m [a1] -> m [a1]
> (<:>) = liftM2 (:)

> --instance Error ParseError where
> --  noMsg = error "gone wrong"
> --    strMsg = error "gone wrong"