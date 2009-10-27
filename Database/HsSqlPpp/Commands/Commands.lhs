Copyright 2009 Jake Wheat

Wrappers used in the command line program

> {-# LANGUAGE FlexibleContexts #-}

> module Database.HsSqlPpp.Commands.Commands where

> import Control.Monad.Error
> --import Control.Applicative

> import Text.Show.Pretty




> import Database.HsSqlPpp.Parsing.Lexer
> --import Database.HsSqlPpp.Parsing.ParseErrors

> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Parsing.Lexer

> import Database.HsSqlPpp.Ast.Annotator
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Ast.Environment
> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Ast.SqlTypes

> --import Database.HsSqlPpp.Utils

> import Database.HsSqlPpp.PrettyPrinter.PrettyPrinter

> import Database.HsSqlPpp.PrettyPrinter.AnnotateSource

> --import Database.HsSqlPpp.Dbms.DBAccess
> --import Database.HsSqlPpp.Dbms.DatabaseLoader

> --import Database.HsSqlPpp.Extensions.ChaosExtensions


> readInput :: (Error e, MonadIO m) => String -> ErrorT e m (String,String)
> readInput f =
>   liftIO $ readFile f >>= \l -> return (f,l)

parse

load into database

clear db

> lexSql :: Monad m => String -> String -> ErrorT AllErrors m [Token]
> lexSql f s = return (lexSqlText f s) >>= throwEEEither

> parseSql1 :: Monad m => String -> String -> ErrorT AllErrors m StatementList
> parseSql1 f s = return (parseSql f s) >>= throwEEEither

> stripAnn :: (Monad m, Error e) => StatementList -> ErrorT e m StatementList
> stripAnn s = return $ stripAnnotations s

> ppSh :: (Monad m, Error e, Show a) => a -> ErrorT e m String
> ppSh = return . ppShow

> ppSql :: (Monad m, Error e) => StatementList -> ErrorT e m String
> ppSql = return . printSql

> ppAnnOrig :: (Monad m, Error e) => Bool -> String -> StatementList -> ErrorT e m String
> ppAnnOrig doErrs fn ast = return $ annotateSource doErrs fn ast

> annotate :: (Monad m, Error e) => StatementList -> ErrorT e m StatementList
> annotate ast = return $ annotateAst ast

> data AllErrors = AEExtendedError ExtendedError
>                | AETypeErrors [TypeError]
>                | AEMisc String
>                  deriving (Show)

> instance Error AllErrors where
>   noMsg = AEMisc "Unknown error"
>   strMsg str = AEMisc str


> throwEEEither :: (MonadError AllErrors m) => Either ExtendedError a -> m a
> throwEEEither (Left err) = throwError $ AEExtendedError err
> throwEEEither (Right val) = return val

> throwTESEither :: (MonadError AllErrors m) => Either [TypeError] a -> m a
> throwTESEither (Left err) = throwError $ AETypeErrors err
> throwTESEither (Right val) = return val

> readCatalog :: MonadIO m => String -> ErrorT AllErrors m Environment
> readCatalog dbName =
>   liftIO (readEnvironmentFromDatabase dbName) >>=
>   throwTESEither . updateEnvironment defaultEnvironment

> getTEs :: (Monad m, Error e) =>
>           StatementList -> ErrorT e m [(Maybe AnnotationElement,[TypeError])]
> getTEs ast = return $ getTypeErrors ast

> lconcat :: (Monad m, Error e) => [[a]] -> ErrorT e m [a]
> lconcat as = return $ concat as


> annotateWithCatalog :: (Monad m, Error e) => Environment -> StatementList -> ErrorT e m StatementList
> annotateWithCatalog cat ast = return $ annotateAstEnv cat ast

> ppTypeErrors :: Monad m =>
>                 [(Maybe AnnotationElement, [TypeError])] -> m [String]
> ppTypeErrors tes =
>   return $ map showSpTe tes
>   where
>     showSpTe (Just (SourcePos fn l c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>     showSpTe (_,e) = "unknown:0:0:\n" ++ show e

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

> putStrLnList :: MonadIO m => [String]-> m ()
> putStrLnList = mapM_ (liftIO . putStrLn)
