Copyright 2009 Jake Wheat

Wrappers used in the command line program

> module Database.HsSqlPpp.Commands.Commands where

> import Control.Monad.Error

> import Database.HsSqlPpp.Parsing.Lexer
> import Database.HsSqlPpp.Parsing.ParseErrors

> readInput :: (Error e, MonadIO m) => String -> ErrorT e m String
> readInput s = do
>   liftIO $ readFile s

parse

load into database

clear db

> lexSql :: Monad m => String -> ErrorT ExtendedError m [Token]
> lexSql  s = return (lexSqlText s) >>= throwEither


show

parseexpression

prettyprintast

annotate ast,env->ast,env

get type errors:
get annotations
filter

showemacsstyle annotations -> string

outputtofile

dbmsreadcatalog

checkbig stuff

================================================================================

Utilities

lifted print

> message :: MonadIO m => String -> m ()
> message x = liftIO (putStrLn x)

run in errort monad
should think of something better to do here than just rethrow as io error1

> wrapET :: (Show e, Monad m) => ErrorT e m a -> m a
> wrapET c = runErrorT c >>= \x ->
>          case x of
>            Left er -> error $ show er
>            Right l -> return l

> throwEither :: (MonadError t m) => Either t a -> m a
> throwEither (Left err) = throwError err
> throwEither (Right val) = return val

print a list, using newlines instead of commas, no outer []

> printList :: (MonadIO m, Show a) => [a] -> m ()
> printList = mapM_ (liftIO . print)
