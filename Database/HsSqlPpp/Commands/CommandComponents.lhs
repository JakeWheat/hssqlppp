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
>     ,CatDiff(..)
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
>     ,AllErrors(..)
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

> import Database.HsSqlPpp.Ast.TypeChecker as A
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
> lexSql f = throwEEEither . lexSqlText f

> -- | Parse a string to an ast.
> parseSql1 :: Monad m => String -> String -> ErrorT AllErrors m StatementList
> parseSql1 f = throwEEEither . parseSql f

> -- | Parse an expression to an ast.
> parseExpression1 :: Monad m => String -> String -> ErrorT AllErrors m Expression
> parseExpression1 f = throwEEEither . parseExpression f

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
> stripAnn = return . stripAnnotations

> -- | Type check an ast against a catalog, return the annotated ast
> --   and the updated catalog.
> typeCheckC :: (Monad m, Error e) => Environment -> StatementList
>            -> ErrorT e m (Environment, StatementList)
> typeCheckC cat = return . typeCheck cat

> -- | Type check an expression ast against a catalog
> typeCheckExpressionC :: (Monad m, Error e) => Environment -> Expression
>                      -> ErrorT e m Expression
> typeCheckExpressionC cat = return . typeCheckExpression cat

could probably make this more general, so can run an arbitrary filter
on annotations and then get a list of them with source positions

> -- | Take an ast and return a list of type errors with source position
> --   if available.
> getTEs :: (Monad m, Error e, Data d) =>
>           d -> ErrorT e m [(Maybe AnnotationElement,[TypeError])]
> getTEs = return . getTypeErrors

> -- | Pretty print list of type errors with optional source position
> --   in emacs readable format.
> ppTypeErrors :: Monad m =>
>                 [(Maybe AnnotationElement, [TypeError])] -> m [String]
> ppTypeErrors tes =
>   return $ map showSpTe tes
>   where
>     showSpTe (Just (SourcePos fn l c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>     showSpTe (_,e) = "unknown:0:0:\n" ++ show e

> -- | Get the top level type annotation from the ast passed.
> getTopLevelTypes :: (Monad m, Error e, Data d) =>
>           d -> ErrorT e m [Type]
> getTopLevelTypes ast = return $ A.getTopLevelTypes [ast]


================================================================================

pretty printing

todo: change the naming convention, so fns which produce haskell
syntax start with show, human readable stuff starts with pp, not sure
where printsql comes in system though

> -- | use ppshow to pretty print a value.
> ppSh :: (Monad m, Error e, Show a) => a -> ErrorT e m String
> ppSh = return . ppShow

> -- | pretty print an ast.
> ppSql :: (Monad m, Error e) => StatementList -> ErrorT e m String
> ppSql = return . printSql

> -- | take a source text and annotated ast and interpolate annotations into the source
> --   as comments
> ppAnnOrig :: (Monad m, Error e) => Bool -> String -> StatementList -> ErrorT e m String
> ppAnnOrig doErrs src = return . annotateSource doErrs src

================================================================================

dbms utilities

> -- | get the catalog from the database
> readCatalog :: MonadIO m => String -> ErrorT AllErrors m Environment
> readCatalog dbName =
>   liftIO (readEnvironmentFromDatabase dbName) >>=
>   throwTESEither . updateEnvironment defaultEnvironment

> -- | run psql to load the sql text into a database.
> loadSqlUsingPsql :: MonadIO m  => String -> String -> ErrorT AllErrors m String
> loadSqlUsingPsql dbName =
>   liftIO . pipeString [("psql", [dbName
>                                 ,"-q"
>                                 ,"--set"
>                                 ,"ON_ERROR_STOP=on"
>                                 ,"--file=-"])]

> -- | run psql to load sql from the filename given into a database.
> loadSqlUsingPsqlFromFile :: MonadIO m  => String -> FilePath -> ErrorT AllErrors m String
> loadSqlUsingPsqlFromFile dbName fn = do
>   ex <- liftIO $ system ("psql " ++ dbName ++
>                 " -q --set ON_ERROR_STOP=on" ++
>                 " --file=" ++ fn)
>   case ex of
>     ExitFailure e -> throwError $ AEMisc $ "psql failed with " ++ show e
>     ExitSuccess -> return ""

> -- | use the hssqlppp code to load the sql into a database directly
> --   (this parses and pretty prints the sql to load it)
> loadAst :: (MonadIO m, Error e) => String -> String -> StatementList -> ErrorT e m ()
> loadAst db fn = liftIO . loadIntoDatabase db fn

> -- | use a dodgy hack to clear the database given
> clearDB :: MonadIO m => String -> ErrorT AllErrors m ()
> clearDB db =
>   liftIO $ withConn ("dbname=" ++ db) $ \conn ->
>     runSqlCommand conn "drop owned by jake cascade;"

> -- | dump the given database to sql source using pg_dump
> pgDump :: MonadIO m => String -> ErrorT AllErrors m String
> pgDump db = liftIO $ pipeString [("pg_dump", [db
>                                              ,"--schema-only"
>                                              ,"--no-owner"
>                                              ,"--no-privileges"])] ""

================================================================================

catalog stuff - just a diff to compare two catalogs

> -- | items in first catalog and not second, items in second and not first.
> data CatDiff = CatDiff [EnvironmentUpdate] [EnvironmentUpdate]
>                deriving Show

> -- | find differences between two catalogs
> compareCatalogs :: (Monad m, Error e) => Environment -> Environment -> Environment -> ErrorT e m CatDiff
> compareCatalogs base start end =
>         let baseEnvBits = deconstructEnvironment base
>             startEnvBits = deconstructEnvironment start \\ baseEnvBits
>             endEnvBits = deconstructEnvironment end \\ baseEnvBits
>             missing = sort $ endEnvBits \\ startEnvBits
>             extras = sort $ startEnvBits \\ endEnvBits
>         in return $ CatDiff missing extras

> -- | print a catdiff in a more human readable way than show.
> ppCatDiff :: (Monad m, Error e) => CatDiff -> ErrorT e m String
> ppCatDiff (CatDiff missing extra) =
>           return $ "\nmissing:\n"
>                    ++ intercalate "\n" (map ppEnvUpdate missing)
>                    ++ "\nextra:\n"
>                    ++ intercalate "\n" (map ppEnvUpdate extra)

================================================================================

> -- | Documentation command to produce some hssqlppp docs, takes a
> --   pandoc source file and converts to html, can run and insert
> --   commands embedded in the source
> pandoc :: MonadIO m => String -> ErrorT AllErrors m String
> pandoc txt =
>   liftM (writeHtmlString wopt . readMarkdown defaultParserState)
>     (hsTextize txt)
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

> -- | read a text file, and pull out the commands, run them and insert
> --   the results into the text
> hsTextize :: MonadIO m => String -> ErrorT AllErrors m String
> hsTextize s =
>     liftIO (hsTextify
>              (("hssqlsystem", hsSqlSystemCommand):defaultCommands)
>              "docs/build"
>              s) >>= throwEither . mapLeft AEMisc

> -- | run hssqlsystem using shell
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

> data AllErrors = AEExtendedError ParseErrorExtra
>                | AETypeErrors [TypeError]
>                | AEMisc String
>                  deriving (Show)

> instance Error AllErrors where
>   noMsg = AEMisc "Unknown error"
>   strMsg = AEMisc

> throwEEEither :: (MonadError AllErrors m) => Either ParseErrorExtra a -> m a
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

> -- | read a file as text, will read from stdin if filename is '-'.
> readInput :: (Error e, MonadIO m) => FilePath -> ErrorT e m String
> readInput f =
>   liftIO $ case f of
>              "-" -> getContents
>              _ | length f >= 2 &&
>                  head f == '"' && last f == '"'
>                    -> return $ drop 1 $ take (length f - 1) f
>                | otherwise -> readFile f

================================================================================

> -- | write text to a file
> writeFile :: (Error e, MonadIO m) => FilePath -> String -> ErrorT e m ()
> writeFile fn =
>     liftIO . System.IO.writeFile fn

================================================================================

Utilities

> -- | wrapper for putstrln
> message :: MonadIO m => String -> m ()
> message = liftIO . putStrLn

run in errort monad
should think of something better to do here than just rethrow as io error1

> -- wrapper to run in errorT monad and return right or error on left
> wrapET :: (Show e, Monad m) => ErrorT e m a -> m a
> wrapET c = runErrorT c >>= \x ->
>          case x of
>            Left er -> error $ show er
>            Right l -> return l

> -- | print a list, using newlines instead of commas, no outer []
> printList :: (MonadIO m, Show a) => [a] -> m ()
> printList = mapM_ (liftIO . print)

> -- | run putstrln over each element of a list
> putStrLnList :: MonadIO m => [String]-> m ()
> putStrLnList = mapM_ (liftIO . putStrLn)

> -- | lifted fst
> lfst :: (Monad m, Error e) => (a,b) -> ErrorT e m a
> lfst = return . fst

> -- | lifted snd
> lsnd :: (Monad m, Error e) => (a,b) -> ErrorT e m b
> lsnd = return . snd

> -- | lifted concat
> lconcat :: (Monad m, Error e) => [[a]] -> ErrorT e m [a]
> lconcat = return . concat
