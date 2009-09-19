Copyright 2009 Jake Wheat

This module mainly contains the function to take a statement list and
load it into the database.

The loading system works by parsing a sql file then passing the
[Statement] to this module which pretty prints each Statement. (There
is no fundamental reason why it loads each statement one at a time
instead of pretty printing the whole lot, just that the run command
from hdbc only supports one command at a time. This might be a
postgresql limitation.)

The next todo to help this code move along is to start adding token
position information to the ast, Then when error (and notice, etc.)
messages come back from the database when loading the sql, can show
the pretty printed sql with the error pos highlighted, as well as
providing the original position from the sql file.

This seems like a whole lot of effort for nothing, but will then allow
using transforming the ast so it no longer directly corresponds to the
source sql, and loading the transformed sql into the database. For
generated code, some thought will be needed on how to link any errors
back to the original source text.

One option for some of the code is to use the original source code
when it hasn't been changed for a given statement, instead of the
pretty printed stuff, don't know how much help this will be.

This code is currently on the backburner, and is a massive mess.

> {- | Routine load SQL into a database. Should work alright, but if
>  you get any errors from PostGreSQL it won't be easy to track them
>   down in the original source.-}
> module Database.HsSqlPpp.Dbms.DatabaseLoader
>     (
>      loadIntoDatabase
>     ) where

> import System.IO
> import System.Directory
> import Control.Monad
> import Control.Exception
> import Text.Regex.Posix
> import Data.List
> import Data.Maybe

> import Database.HsSqlPpp.PrettyPrinter.PrettyPrinter
> import Database.HsSqlPpp.TypeChecking.Ast as Ast
> import Database.HsSqlPpp.Dbms.DBAccess

> loadIntoDatabase :: String -- ^ database name
>                  -> FilePath -- ^ filename to include in error messages
>                              -- (this file is not accessed)
>                  -> StatementList -- ^ ast to load into database
>                  -> IO ()
> loadIntoDatabase dbName fn ast =
>   withConn ("dbname=" ++ dbName) $ \conn -> do
>          loadPlpgsqlIntoDatabase conn
>          mapM_ (\st ->
>                 loadStatement conn st
>                 >> putStr ".") (filterStatements ast)
>   where
>     loadStatement conn st = case st of
>                                          Skipit -> return ()
>                                          VanillaStatement vs ->
>                                              handleError fn ("",0::Int,0::Int) vs (runSqlCommand conn
>                                                       (printSql [vs]))
>                                          CopyStdin a b -> runCopy conn a b ("",0::Int,0::Int)
>     --hack cos we don't have support in hdbc for copy from stdin
>     --(which libpq does support - adding this properly should be a todo)
>     --we need the copy from stdin and the copydata to be processed in one go,
>     --so filter the list to replace instances of these with a replacement
>     --and a dummy statement following. Well dodgy, don't really know
>     --how it manages to work correctly.
>     filterStatements sts =
>        map (\(x,y) ->
>                 case (x,y) of
>                        (a@(Copy _ _ _ Stdin), b@(CopyData _ _)) -> (CopyStdin a b)
>                        (CopyData _ _, _) -> Skipit
>                        (vs,_) -> VanillaStatement vs)
>            statementWithNextStatement
>            where
>              statementWithNextStatement =
>                  zip sts (tail sts ++ [NullStatement []])
>     runCopy conn a b _ = case (a,b) of
>                          (Copy _ tb cl Stdin, CopyData _ s) ->
>                            withTemporaryFile (\tfn -> do
>                                writeFile tfn s
>                                tfn1 <- canonicalizePath tfn
>                                loadStatement conn
>                                  (VanillaStatement (Copy [] tb cl
>                                                     (CopyFilename tfn1))))
>                          _ -> error "internal error: pattern match fail in runCopy in loadIntoDatabase"
>     loadPlpgsqlIntoDatabase conn = do
>          -- first, check plpgsql is in the database
>          x <- selectValue conn
>                 "select count(1) from pg_language where lanname='plpgsql';"
>          when (readInt x == 0) $
>               runSqlCommand conn "create procedural language plpgsql;"
>     readInt x = read x :: Int

> data Wrapper = CopyStdin Ast.Statement Ast.Statement
>              | Skipit
>              | VanillaStatement Ast.Statement

================================================================================

= Error reporting

The goal is to give a position in the original source when an error
occurs as best as possible.

For now, all statements in the ast correspond 1:1 with statement text
in the source code, so each line,col pair coming from pg should
correspond to onne line,col pair in the original source text.

The lines load in one at a time, so there needs to be a fair bit of
translation to ultimately output the original line and source number.

(For code with no source information, e.g. stuff which is generated
programmatically from scratch, plan is to give a index (the statement
position in the [Statement] arg, and then pretty print this statement
and provide a line and column just for this statement text.

Later on, when working with generated code may need to make guesses as
to which line, and possibly supply multiple references (e.g. for a
template you might want to see the part of the template which is
causing the error, plus the arguments being passed to that template
instantion call (if nested, you want to see all of them back up to the
top level).

So, for now: want to get the line number and column number from the
error message text

If there is a line and column number in the error coming from
postgresql, they should appear something like this:

execute: PGRES_FATAL_ERROR: ERROR:  column "object_name" of relation "system_implementation_objects" does not exist
LINE 1: insert into system_implementation_objects (object_name,objec...
                                                   ^

So we are looking at the LINE x: part, and the line with a bunch of
blanks preceding the ^

Parse the line number out : easy
Parse the number of spaces before the ^ easy

Problem: the text following the LINE 1: is a arbitrary snippet of the
source so the column number of the ^ doesn't relate to the column
number in the source in any straightforward way at all...

Probably a better way of doing this, might need alterations to HDBC
though

> getLineAndColumnFromErrorText :: String -> (Int, Int)
> getLineAndColumnFromErrorText errorText =
>   let ls = lines errorText
>       lineLine = find isLineLine ls
>       lineNo = case lineLine of
>                              Nothing -> 0
>                              Just ll -> getLineNo ll
>       column = 0 --(case find isHatLine ls of
>                  --                     Nothing -> 0
>                  --                     Just ll -> getHatPos ll 0)
>       prestuff = 0 --case lineLine of
>       --             Nothing -> 0
>       --             Just l -> getLineStuffLength l
>   in (column - prestuff,lineNo)
>   -- wish I knew how to get haskell regexes working...
>   -- can't find a way to get just the () part out
>   where
>     isLineLine l = l =~ "^LINE ([0-9]+):" :: Bool
>     getLineNo l = read (stripCrap (l =~ "^LINE ([0-9]+):" :: String)) :: Int
>     stripCrap = reverse . drop 1 . reverse . drop 5

 >     isHatLine l = l =~ "^[ ]*\\^" :: Bool
 >     getHatPos l i = case l of
 >                            (x:xs) | x == ' ' -> getHatPos xs (i + 1)
 >                                   | otherwise -> i
 >                            _ -> 0
 >     getLineStuffLength l = length (l =~ "^LINE ([0-9]+):" :: String) + 1

> handleError :: String -> (String,Int,Int) -> Ast.Statement -> IO () -> IO ()
> handleError fn (_, sl,sc) _ f = catchSql f
>                    (\e -> do
>                      --s <- readFile fn
>                      let (l,c) = getLineAndColumnFromErrorText (seErrorMsg e)
>                      --let (sl,sc) = getStatementPos st
>                      error $ "ERROR!!!!\n"
>                                ++ show l ++ ":" ++ show c ++ ":\n"
>                                ++ "from here:\n"
>                                ++ fn ++ ":" ++ show sl ++ ":" ++ show sc ++ ":\n"
>                                ++ seErrorMsg e)

 > getStatementPos (Insert (Just (SourcePos x y)) _ _ _ _) = (x,y)

 > getStatementPos :: t -> (Integer, Integer)
 > getStatementPos _ = (0::Integer,0::Integer)

================================================================================

withtemporaryfile, part of the copy from stdin hack

can't use opentempfile since it gets locked and then pg program can't
open the file for reading

proper dodgy, needs fixing:

> withTemporaryFile :: (String -> IO c) -> IO c
> withTemporaryFile f = bracket (return ())
>                               (\_ -> removeFile "hssqlppptemp.temp")
>                               (\_ -> f "hssqlppptemp.temp")
