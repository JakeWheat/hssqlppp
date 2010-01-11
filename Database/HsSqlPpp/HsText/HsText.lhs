

> module Database.HsSqlPpp.HsText.HsText
>     (hsTextify
>     ,defaultCommands
>     ,makeFile
>     ,shell
>     ,wrapCommand
>     ) where

> import Text.Parsec hiding(many, optional, (<|>))

> import Data.Char
> import Data.List
> import System.Directory

> import Control.Applicative
> import Control.Monad.Identity
> import Control.Monad.Error
> import Control.Exception (bracket)

> import Database.HsSqlPpp.HsText.Commands

> data TextChunk = TextChunk String
>                | CommandInfo String String
>                  deriving Show

> type Command = String -> IO String

> hsTextify :: [(String, Command)] -> String -> String -> IO (Either String String)
> hsTextify cmds folder txt = do
>   cd <- getCurrentDirectory
>   setCurrentDirectory folder
>   bracket (return ())
>           (\_ -> setCurrentDirectory cd)
>    $ return $ runErrorT $
>        liftM concat
>          (either (throwError . show) return (parseText txt) >>=
>            mapM (liftIO . processCommand cmds))

> processCommand :: [(String, Command)] -> TextChunk -> IO String
> processCommand _ (TextChunk s) = return s
> processCommand cmds (CommandInfo c a) = do
>   let cr = lookup (map toLower c) cmds
>   case cr of
>     Nothing -> return $ "Command not found: " ++ c
>     Just f -> catchEx (f a)

> catchEx :: IO String -> IO String
> catchEx s = catch s (\i -> return $ "ERROR: " ++ show i)

================================================================================

parsing

> parseText :: String -> Either ParseError [TextChunk]
> parseText = runParser (many textChunk) () ""

> textChunk :: ParsecT String () Identity TextChunk
> textChunk = command <|> nonCommand

> command :: ParsecT String () Identity TextChunk
> command = CommandInfo
>           <$> (string "<%Command " *> many (noneOf " "))
>           <*> manyTill anyChar (try (string "%>"))

> nonCommand :: ParsecT String () Identity TextChunk
> nonCommand = TextChunk
>              <$> ((:) <$> anyChar <*>
>                   manyTill anyChar
>                      (try (lookAhead(string "<%Command ") >> return ())
>                       <|> eof))
