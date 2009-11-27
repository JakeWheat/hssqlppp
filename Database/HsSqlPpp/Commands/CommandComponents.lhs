Copyright 2009 Jake Wheat

Wrappers used in the command line program

> {-# LANGUAGE FlexibleContexts #-}


> {- | This module contains all the functions used in the hssqlsystem
>      exe. Mainly a set of wrappers to lift other functions into an
>      ErrorT monad. See HsSqlSystem.lhs for example use.
> -}
> module Database.HsSqlPpp.Commands.CommandComponents
>     (
>      -- * errort wrapper
>      wrapET
>      -- * parsing
>     ,lexSql
>     ,parseSql1
>     ,parseExpression1
>      -- * show and pretty print
>     ,printList
>     ,ppSh
>     ,ppSql
>     ,ppAnnOrig
>     ,ppTypeErrors
>      -- * annotations
>     ,stripAnn
>     ,typeCheckC
>     ,typeCheckExpressionC
>     ,getTEs
>     ,Database.HsSqlPpp.Commands.CommandComponents.getTopLevelTypes
>      -- * dbms access
>     ,readCatalog
>     ,clearDB
>     ,loadAst
>     ,loadSqlUsingPsqlFromFile
>     ,loadSqlUsingPsql
>     ,pgDump
>      -- catalog diffs
>     ,compareCatalogs
>     ,ppCatDiff
>      -- extensions
>     ,runExtensions
>     -- docs
>     ,pandoc
>     ,hsTextize
>      -- * utils
>     ,message
>     ,putStrLnList
>     ,readInput
>     ,Database.HsSqlPpp.Commands.CommandComponents.writeFile
>     ,lconcat
>     ,lfst
>     ,lsnd
>     ) where

> import Control.Monad.Error
> import System
> import Data.List
> import System.IO
> import Data.Generics

> import Text.Show.Pretty
> import System.Process.Pipe
> import Text.Pandoc

> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Parsing.Lexer

> import Database.HsSqlPpp.Ast.Annotator as A
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Ast.Environment
> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Ast.SqlTypes

> import Database.HsSqlPpp.PrettyPrinter.PrettyPrinter
> import Database.HsSqlPpp.PrettyPrinter.AnnotateSource

> import Database.HsSqlPpp.Dbms.DBAccess
> import Database.HsSqlPpp.Dbms.DatabaseLoader

> import Database.HsSqlPpp.Extensions.ChaosExtensions

> import Database.HsSqlPpp.Utils
> import Database.HsSqlPpp.HsText.HsText

===============================================================================

parsing

> -- | Lex a string to a list of tokens.
> lexSql :: Monad m => String -> String -> ErrorT AllErrors m [Token]
> lexSql f s = return (lexSqlText f s) >>= throwEEEither

> -- | Parse a string to an ast.
> parseSql1 :: Monad m => String -> String -> ErrorT AllErrors m StatementList
> parseSql1 f s = return (parseSql f s) >>= throwEEEither


> -- | Parse an expression to an ast.
> parseExpression1 :: Monad m => String -> String -> ErrorT AllErrors m Expression
> parseExpression1 f s = return (parseExpression f s) >>= throwEEEither

================================================================================

> -- | Transform an ast using the chaos syntax extensions.
> runExtensions :: (Monad m, Error e) => StatementList -> ErrorT e m StatementList
> runExtensions = return . extensionize

================================================================================

annotation ish

> -- | Take an ast and remove all the annotations. Can be used to view
> --   an ast without all the source position annotations cluttering
> --   it up.
> stripAnn :: (Monad m, Error e, Data a) => a -> ErrorT e m a
> stripAnn s = return $ stripAnnotations s

> -- | Type check an ast against a catalog, return the annotated ast
> --   and the updated catalog.
> typeCheckC :: (Monad m, Error e) => Environment -> StatementList
>            -> ErrorT e m (Environment, StatementList)
> typeCheckC cat ast = return $ typeCheck cat ast

> -- | Type check an expression ast against a catalog
> typeCheckExpressionC :: (Monad m, Error e) => Environment -> Expression
>                      -> ErrorT e m Expression
> typeCheckExpressionC cat ast = return $ typeCheckExpression cat ast

could probably make this more general, so can run an arbitrary filter
on annotations and then get a list of them with source positions

> getTEs :: (Monad m, Error e, Data d) =>
>           d -> ErrorT e m [(Maybe AnnotationElement,[TypeError])]
> getTEs ast = return $ getTypeErrors ast

> ppTypeErrors :: Monad m =>
>                 [(Maybe AnnotationElement, [TypeError])] -> m [String]
> ppTypeErrors tes =
>   return $ map showSpTe tes
>   where
>     showSpTe (Just (SourcePos fn l c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>     showSpTe (_,e) = "unknown:0:0:\n" ++ show e

> getTopLevelTypes :: (Monad m, Error e, Data d) =>
>           d -> ErrorT e m [Type]
> getTopLevelTypes ast = return $ A.getTopLevelTypes [ast]


================================================================================

pretty printing

todo: change the naming convention, so fns which produce haskell
syntax start with show, human readable stuff starts with pp, not sure
where printsql comes in system though

> ppSh :: (Monad m, Error e, Show a) => a -> ErrorT e m String
> ppSh = return . ppShow

> ppSql :: (Monad m, Error e) => StatementList -> ErrorT e m String
> ppSql = return . printSql

> ppAnnOrig :: (Monad m, Error e) => Bool -> String -> StatementList -> ErrorT e m String
> ppAnnOrig doErrs src ast = return $ annotateSource doErrs src ast

================================================================================

dbms utilities

> readCatalog :: MonadIO m => String -> ErrorT AllErrors m Environment
> readCatalog dbName =
>   liftIO (readEnvironmentFromDatabase dbName) >>=
>   throwTESEither . updateEnvironment defaultEnvironment

> loadSqlUsingPsql :: MonadIO m  => String -> String -> ErrorT AllErrors m String
> loadSqlUsingPsql dbName script = do
>   liftIO $ pipeString [("psql", [dbName
>                                 ,"-q"
>                                 ,"--set"
>                                 ,"ON_ERROR_STOP=on"
>                                 ,"--file=-"])] script

> loadSqlUsingPsqlFromFile :: MonadIO m  => String -> String -> ErrorT AllErrors m String
> loadSqlUsingPsqlFromFile dbName fn = do
>   ex <- liftIO $ system ("psql " ++ dbName ++
>                 " -q --set ON_ERROR_STOP=on" ++
>                 " --file=" ++ fn)
>   case ex of
>     ExitFailure e -> throwError $ AEMisc $ "psql failed with " ++ show e
>     ExitSuccess -> return ""

> loadAst :: (MonadIO m, Error e) => String -> String -> StatementList -> ErrorT e m ()
> loadAst db fn ast = liftIO $ loadIntoDatabase db fn ast

> clearDB :: MonadIO m => String -> ErrorT AllErrors m ()
> clearDB db =
>   liftIO $ withConn ("dbname=" ++ db) $ \conn ->
>     runSqlCommand conn "drop owned by jake cascade;"

> pgDump :: MonadIO m => String -> ErrorT AllErrors m String
> pgDump db = liftIO $ pipeString [("pg_dump", [db
>                                              ,"--schema-only"
>                                              ,"--no-owner"
>                                              ,"--no-privileges"])] ""

================================================================================

catalog stuff - just a diff to compare two catalogs

> data CatDiff = CatDiff [EnvironmentUpdate] [EnvironmentUpdate]
>                deriving Show

> compareCatalogs :: (Monad m, Error e) => Environment -> Environment -> Environment -> ErrorT e m CatDiff
> compareCatalogs base start end =
>         let baseEnvBits = deconstructEnvironment base
>             startEnvBits = deconstructEnvironment start \\ baseEnvBits
>             endEnvBits = deconstructEnvironment end \\ baseEnvBits
>             missing = sort $ endEnvBits \\ startEnvBits
>             extras = sort $ startEnvBits \\ endEnvBits
>         in return $ CatDiff missing extras

> ppCatDiff :: (Monad m, Error e) => CatDiff -> ErrorT e m String
> ppCatDiff (CatDiff missing extra) =
>           return $ "\nmissing:\n"
>                    ++ intercalate "\n" (map ppEnvUpdate missing)
>                    ++ "\nextra:\n"
>                    ++ intercalate "\n" (map ppEnvUpdate extra)

================================================================================

> pandoc :: MonadIO m => String -> ErrorT AllErrors m String
> pandoc txt =
>   hsTextize txt >>=
>   return . writeHtmlString wopt . readMarkdown defaultParserState
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               ,writerTitlePrefix = "HsSqlPpp documentation"
>               ,writerTableOfContents = True
>               ,writerHeader = "<style>\n\
>                               \pre {\n\
>                               \    border: 1px dotted gray;\n\
>                               \    background-color: #ececec;\n\
>                               \    color: #1111111;\n\
>                               \    padding: 0.5em;\n\
>                               \}\n\
>                               \</style>"
>              }


writerStandalone :: Bool	Include header and footer
writerHeader :: String	Header for the document
writerTitlePrefix :: String	Prefix for HTML titles
writerTabStop :: Int	Tabstop for conversion btw spaces and tabs
writerTableOfContents :: Bool	Include table of contents
writerS5 :: Bool	We're writing S5
writerHTMLMathMethod :: HTMLMathMethod	How to print math in HTML
writerIgnoreNotes :: Bool	Ignore footnotes (used in making toc)
writerIncremental :: Bool	Incremental S5 lists
writerNumberSections :: Bool	Number sections in LaTeX
writerIncludeBefore :: String	String to include before the body
writerIncludeAfter :: String	String to include after the body
writerStrictMarkdown :: Bool	Use strict markdown syntax
writerReferenceLinks :: Bool	Use reference links in writing markdown, rst
writerWrapText :: Bool	Wrap text to line length
writerLiterateHaskell :: Bool	Write as literate haskell
writerEmailObfuscation :: ObfuscationMethod	How to obfu

>   {-ex <- liftIO $ system ("pandoc -s -f markdown -t html "
>                          ++ src ++ " -o " ++ tgt)
>   case ex of
>     ExitFailure e -> throwError $ AEMisc $ "psql failed with " ++ show e
>     ExitSuccess -> return ()-}

================================================================================

process doc commands

> hsTextize :: MonadIO m => String -> ErrorT AllErrors m String
> hsTextize s =
>     liftIO (hsTextify
>              (("hssqlsystem", hsSqlSystemCommand):defaultCommands)
>              "docs/build"
>              s) >>= throwEither . mapLeft AEMisc

> hsSqlSystemCommand :: String -> IO String
> hsSqlSystemCommand s =  shell ("HsSqlSystem " ++ s) >>= \m ->
>                         return $ "$ HsSqlSystem " ++ s
>                                  ++ "\n\n~~~~~~~~~~\n"
>                                  ++ m
>                                  ++ "\n~~~~~~~~~~\n\n"


================================================================================

errort stuff

wrap all our errors in an algebraic data type, not sure if there is a
more elegant way of doing this but it does the job for now

> data AllErrors = AEExtendedError ExtendedError
>                | AETypeErrors [TypeError]
>                | AEMisc String
>                  deriving (Show)

> instance Error AllErrors where
>   noMsg = AEMisc "Unknown error"
>   strMsg str = AEMisc str

> throwEEEither :: (MonadError AllErrors m) => Either ExtendedError a -> m a
> throwEEEither = throwEither . mapLeft AEExtendedError

> throwTESEither :: (MonadError AllErrors m) => Either [TypeError] a -> m a
> throwTESEither = throwEither . mapLeft AETypeErrors

> throwEither :: (MonadError t m) => Either t a -> m a
> throwEither (Left err) = throwError err
> throwEither (Right val) = return val

================================================================================

read file as string - issues are:

want to support reading from stdin, and reading from a string passed
as an argument to the exe

> readInput :: (Error e, MonadIO m) => String -> ErrorT e m String
> readInput f =
>   liftIO $ case f of
>              "-" -> hGetContents stdin
>              _ | length f >= 2 &&
>                  head f == '"' && last f == '"'
>                    -> return $ drop 1 $ take (length f - 1) f
>                | otherwise -> readFile f

================================================================================

> writeFile :: (Error e, MonadIO m) => String -> String -> ErrorT e m ()
> writeFile fn src =
>     liftIO $ System.IO.writeFile fn src

================================================================================

Utilities

> message :: MonadIO m => String -> m ()
> message x = liftIO (putStrLn x)

run in errort monad
should think of something better to do here than just rethrow as io error1

> wrapET :: (Show e, Monad m) => ErrorT e m a -> m a
> wrapET c = runErrorT c >>= \x ->
>          case x of
>            Left er -> error $ show er
>            Right l -> return l

print a list, using newlines instead of commas, no outer []

> printList :: (MonadIO m, Show a) => [a] -> m ()
> printList = mapM_ (liftIO . print)

> putStrLnList :: MonadIO m => [String]-> m ()
> putStrLnList = mapM_ (liftIO . putStrLn)

> lfst :: (Monad m, Error e) => (a,b) -> ErrorT e m a
> lfst = return . fst

> lsnd :: (Monad m, Error e) => (a,b) -> ErrorT e m b
> lsnd = return . snd

> lconcat :: (Monad m, Error e) => [[a]] -> ErrorT e m [a]
> lconcat as = return $ concat as
