
Command line access to a bunch of utility functions.

run
HsSqlUtil.lhs -?
to get a list of commands and purpose and usage info

> {-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables,FlexibleContexts,QuasiQuotes #-}
>
> import System.Console.CmdArgs
> import Control.Monad.Error

> --import Debug.Trace
> import Data.Maybe
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Utils.Utils
>
> import Database.HsSqlPpp.Ast
> import qualified Database.HsSqlPpp.TypeChecker as A
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.SqlTypes
>
> import qualified Database.HsSqlPpp.Parser as P
> import Database.HsSqlPpp.Parsing.Lexer
>
> import Database.HsSqlPpp.PrettyPrinter
>
> import Database.HsSqlPpp.Utils.DBUtils
> import Database.HsSqlPpp.Utils.PPExpr

-------------------------------------------------------------------------------

command defs
============

> data HsSqlUtil = Lex {files :: [String]}
>                  | Parse {files :: [String]}
>                  | ParseExpression {files :: [String]}
>                  | Ppp {files :: [String]}
>                  | Pppp {files :: [String]}
>
>                  | TypeCheck {database :: String
>                              ,files :: [String]}
>                  | TypeCheckExpression {database :: String
>                                        ,files :: [String]}
>                  | AllAnnotations {database :: String
>                                   ,files :: [String]}
>                    deriving (Show, Data, Typeable)

-------------------------------------------------------------------------------

hacky thing: to run an ast transform on all sql asts in commands, put it in here

> astTransformer :: Data a => a -> a
> astTransformer = id

-------------------------------------------------------------------------------

Lex
===

lex the files given and output the tokens on separate lines, useful
for testing and debugging only

example
-------

test2.sql:

~~~~~~~~~~~~ {.SqlPostgresql}
create table s (
       s_no int primary key,
       sname text not null,
       status int not null,
       city text not null
);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.sh}
$ HsSqlUtil lex test2.sql
lexing test2.sql
("test2.sql" (line 1, column 1),IdStringTok "create")
("test2.sql" (line 1, column 8),IdStringTok "table")
("test2.sql" (line 1, column 14),IdStringTok "s")
("test2.sql" (line 1, column 16),SymbolTok "(")
("test2.sql" (line 2, column 8),IdStringTok "s_no")
("test2.sql" (line 2, column 13),IdStringTok "int")
("test2.sql" (line 2, column 17),IdStringTok "primary")
("test2.sql" (line 2, column 25),IdStringTok "key")
("test2.sql" (line 2, column 28),SymbolTok ",")
("test2.sql" (line 3, column 8),IdStringTok "sname")
("test2.sql" (line 3, column 14),IdStringTok "text")
("test2.sql" (line 3, column 19),IdStringTok "not")
("test2.sql" (line 3, column 23),IdStringTok "null")
("test2.sql" (line 3, column 27),SymbolTok ",")
("test2.sql" (line 4, column 8),IdStringTok "status")
("test2.sql" (line 4, column 15),IdStringTok "int")
("test2.sql" (line 4, column 19),IdStringTok "not")
("test2.sql" (line 4, column 23),IdStringTok "null")
("test2.sql" (line 4, column 27),SymbolTok ",")
("test2.sql" (line 5, column 8),IdStringTok "city")
("test2.sql" (line 5, column 13),IdStringTok "text")
("test2.sql" (line 5, column 18),IdStringTok "not")
("test2.sql" (line 5, column 22),IdStringTok "null")
("test2.sql" (line 6, column 1),SymbolTok ")")
("test2.sql" (line 6, column 2),SymbolTok ";")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> lexA = mode Lex {files = def &= typ "FILES" & args}
>        &= text "lex the files given and output the tokens on separate lines"
>
> lexFiles :: [FilePath] -> IO ()
> lexFiles fns = wrapETs $
>                forM_ fns $ \f ->
>                  (liftIO . putStrLn) ("lexing " ++ f) >>
>                  (liftIO . readInput) f >>=
>                  tsl . lexSqlText f >>=
>                  mapM_ (liftIO . print)

-------------------------------------------------------------------------------

Parse
=====

parse some sql and output the ast

examples
--------

test2.sql:

~~~~~~~~~~~~ {.SqlPostgresql}
create table s (
       s_no int primary key,
       sname text not null,
       status int not null,
       city text not null
);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ HsSqlUtil parse test2.sql
-- ast of test2.sql
[CreateTable [] "s"
   [AttributeDef [] "s_no" (SimpleTypeName [] "int") Nothing
      [RowPrimaryKeyConstraint [] ""],
    AttributeDef [] "sname" (SimpleTypeName [] "text") Nothing
      [NotNullConstraint [] ""],
    AttributeDef [] "status" (SimpleTypeName [] "int") Nothing
      [NotNullConstraint [] ""],
    AttributeDef [] "city" (SimpleTypeName [] "text") Nothing
      [NotNullConstraint [] ""]]
   []]

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test3.sql:

~~~~~~~~~~~~ {.SqlPostgresql}
select * from s natural inner join p natural inner join sp;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ HsSqlUtil parse test3.sql
-- ast of test3.sql
[SelectStatement []
   (Select [] Dupes (SelectList [] [SelExp [] (Identifier [] "*")] [])
      [JoinedTref []
         (JoinedTref [] (Tref [] "s" NoAlias) Natural Inner
            (Tref [] "p" NoAlias)
            Nothing
            NoAlias)
         Natural
         Inner
         (Tref [] "sp" NoAlias)
         Nothing
         NoAlias]
      Nothing
      []
      Nothing
      []
      Nothing
      Nothing)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test3a.sql

~~~~~~~~~~~~ {.SqlPostgresql}
select * from s natural inner join p natural inners join sp;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~ {.haskell}
$ ./HsSqlUtil parse test3a.sql
-- ast of test3a.sql
HsSqlUtil: "test3a.sql" (line 1, column 46):
unexpected IdStringTok "inners"
test3a.sql:1:46:

Context:
select * from s natural inner join p natural inners join sp;
                                             ^
ERROR HERE


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> parseA = mode $ Parse {files = def &= typ "FILES" & args}
>          &= text "Parse files and output the asts"
>
> showAst :: [String] -> IO ()
> showAst = wrapETs . mapM_ (\f ->
>                (liftIO . putStrLn) ("-- ast of " ++ f) >>
>                (liftIO . readInput) f >>=
>                tsl . P.parseSql f >>=
>                return . (astTransformer |>
>                          --stripAnnotations |>
>                          ppExpr) >>=
>                liftIO . putStrLn)

-------------------------------------------------------------------------------

ParseExpression
===============

Parse an expression, useful for trying things out.

examples
--------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "test(a,v)" | HsSqlUtil parseexpression -
-- ast of -
FunCall [] "test" [Identifier [] "a", Identifier [] "v"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "1+1+1" | HsSqlUtil parseexpression -
-- ast of -
FunCall [] "+"
  [FunCall [] "+" [IntegerLit [] 1, IntegerLit [] 1],
   IntegerLit [] 1]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "case 1 when 2 then 3 else 4 end" | HsSqlUtil parseexpression -
-- ast of -
CaseSimple [] (IntegerLit [] 1)
  [([IntegerLit [] 2], IntegerLit [] 3)]
  (Just (IntegerLit [] 4))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "3 = any (array[1,2])" | HsSqlUtil parseexpression -
-- ast of -
LiftOperator [] "=" LiftAny
  [IntegerLit [] 3,
   FunCall [] "!arrayctor" [IntegerLit [] 1, IntegerLit [] 2]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> parseExpressionA = mode $ ParseExpression {files = def &= typ "FILES" & args}
>                    &= text "Parse files each containing one expression \
>                            \and output the asts"
>
> parseExpression :: [String] -> IO ()
> parseExpression = wrapETs . mapM_ (\f ->
>                (liftIO . putStrLn) ("-- ast of " ++ f) >>
>                (liftIO . readInput) f >>=
>                tsl . P.parseExpression f >>=
>                return . (astTransformer |>
>                          resetAnnotations |>
>                          ppExpr) >>=
>                liftIO . putStrLn)

-------------------------------------------------------------------------------

ppp
===
Parse then pretty print

example
-------

test4.sql:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.SqlPostgresql}
create table s (
       s_no int primary key,
       sname text not null,
       status int not null,
       city text not null
);

create table p (
       p_po int primary key,
       pname text not null,
       color text not null,
       weight int not null,
       city text not null
);

create table sp (
       s_no int not null references s,
       p_no int not null references p,
       qty int not null,
       primary key (s_no,p_no)
);

select * from s inner join p inner join sp;

insert into s (s_no, sname, status, city) values (1, 'name', 'good', 'london');
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ HsSqlUtil ppp test4.sql
--ppp test4.sql
create table s (
  s_no int primary key,
  sname text not null,
  status int not null,
  city text not null
);

create table p (
  p_po int primary key,
  pname text not null,
  color text not null,
  weight int not null,
  city text not null
);

create table sp (
  s_no int not null references s on delete restrict on update restrict,
  p_no int not null references p on delete restrict on update restrict,
  qty int not null,
  primary key (s_no,p_no)
);

select *
  from ((s
         inner join p)
        inner join sp);

insert into s (s_no,sname,status,city)
values
  (1,'name','good','london')
;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> pppA = mode $ Ppp {files = def &= typ "FILES" & args}
>        &= text "Parse then pretty print some sql so you can check the result \
>                \hasn't mangled the sql, or see the result of the ast \
>                \transformer if you've added one."
>
> ppp :: [String] -> IO()
> ppp fs = wrapETs $ forM_ fs (\f ->
>             (liftIO . putStrLn) ("--ppp " ++ f) >>
>             (liftIO . readInput) f >>=
>             tsl . P.parseSql f >>=
>             return . (astTransformer |>
>                       printSql) >>=
>             liftIO . putStrLn)

-------------------------------------------------------------------------------

Pppp
====
Parse, pretty print and parse, then check two parse trees are same

example
-------

~~~~~~~~~~~~~~~~{.sh}
$ HsSqlUtil pppp test4.sql
success
~~~~~~~~~~~~~~~~

> ppppA = mode $ Pppp {files = def &= typ "FILES" & args}
>         &= text "Routine to parse sql from a file, pretty print it \
>                 \then parse it again and check the post pretty \
>                 \printing ast is the same as the initial ast"
>
> testPppp :: [String] -> IO ()
> testPppp = wrapETs . mapM_ (\f -> do
>             ast1 <- (liftIO . readInput) f >>=
>                     tsl . P.parseSql f >>=
>                     return . resetAnnotations
>             ast2 <- (return . printSql) ast1 >>=
>                     tsl . P.parseSql "" >>=
>                     return . resetAnnotations
>             if ast1 /= ast2
>                then liftIO $ do
>                       putStrLn "asts are different\n-- original"
>                       putStrLn $ ppExpr ast1
>                       putStrLn "-- ppp'd"
>                       putStrLn $ ppExpr ast2
>                else (liftIO . putStrLn) "success")

-------------------------------------------------------------------------------

typeCheck
=========

examples
--------

test4.sql:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.SqlPostgresql}
create table s (
       s_no int primary key,
       sname text not null,
       status int not null,
       city text not null
);

create table p (
       p_po int primary key,
       pname text not null,
       color text not null,
       weight int not null,
       city text not null
);

create table sp (
       s_no int not null references s,
       p_no int not null references p,
       qty int not null,
       primary key (s_no,p_no)
);

select * from s inner join p inner join sp;

insert into s (s_no, sname, status, city) values (1, 'name', 'good', 'london');
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(no output for success)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ HsSqlUtil typecheck test4.sql
$
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test5.sql:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.SqlPostgresql}
create table s (
       s_no int primary key,
       sname text not null,
       status int not null,
       city text not null
);

create table p (
       p_po int primary key,
       pname text not null,
       color text not null,
       weight int not null,
       city text not null
);

create table sp (
       s_no int not null references s,
       p_no int not null references p,
       qty int not null,
       primary key (s_no,p_no)
);

select * from s1 inner join p inner join sp;

insert into s (s_no, sname, status, cityc) values (1, 'name', 'good', 'london');
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ HsSqlUtil typecheck test5.sql
test5.sql:23:15:
[UnrecognisedRelation "s1"]
test5.sql:25:1:
[UnrecognisedIdentifier "cityc"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


> typeCheckA = mode $ TypeCheck {database = def
>                               ,files = def &= typ "FILES" & args}
>              &= text "reads each file, parses, type checks, then outputs \
>                      \any type errors"
>
> typeCheck2 :: String -> [FilePath] -> IO ()
> typeCheck2 db fns = wrapETs $ do
>   cat <- liftIO (readCatalog db) >>= tsl
>   mapM (\f -> (liftIO . readInput) f >>=
>               tsl . P.parseSql f) fns >>=
>     return . (concat |>
>               astTransformer |>
>               A.typeCheck cat |>
>               snd |>
>               getTypeErrors |>
>               ppTypeErrors) >>=
>     mapM_ (liftIO . putStrLn)

-------------------------------------------------------------------------------

typeCheckExpression
===================

examples
--------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "3 = any (array[1,2])" | ./HsSqlUtil typecheckexpression -
ScalarType "bool"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ echo "test(3,'stuff'::what)" | ./HsSqlUtil typecheckexpression -
-:1:17:
[UnknownTypeName "what"]
TypeCheckFailed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ echo "test(3,'stuff')" | ./HsSqlUtil typecheckexpression -
-:1:5:
[NoMatchingOperator "test" [ScalarType "int4",UnknownType]]
TypeCheckFailed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type checks strings as unknown types, doesn't parse the contents of
these yet, works just like postgresql which catches this error only at
runtime.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ echo "array[3,'stuff']" | ./HsSqlUtil typecheckexpression -
ArrayType (ScalarType "int4")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ echo "array[3,'stuff'::text]" | ./HsSqlUtil typecheckexpression -
-:1:1:
[NoMatchingOperator "!arrayctor" [ScalarType "int4",ScalarType "text"]]
TypeCheckFailed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> typeCheckExpressionA = mode $ TypeCheckExpression {database = def
>                               ,files = def &= typ "FILES" & args}
>      &= text "reads each file, parses as expression, \
>              \ type checks, then outputs the type or any type errors"
>
> typeCheckExpression :: String -> [FilePath] -> IO ()
> typeCheckExpression db fns = wrapETs $ do
>   aasts <- liftIO (readCatalog db) >>= tsl >>= \cat ->
>            forM fns (\f -> (liftIO . readInput) f >>=
>                            tsl . P.parseExpression f >>=
>                            return . (astTransformer |>
>                                      A.typeCheckExpression cat))
>   tes <- mapM (return . getTypeErrors) aasts
>   mapM_ (\x -> (mapM_ (liftIO . putStrLn) (ppTypeErrors x))) $
>         filter (not . null) tes
>   mapM_ (\a -> liftM show
>                (return $ atype $ getAnnotation a) >>=
>                liftIO . putStrLn) aasts

-------------------------------------------------------------------------------

allAnnotations
==============

example
-------

test6.sql:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.SqlPostgresql}
create table s (
       s_no int primary key,
       sname text not null,
       status int not null,
       city text not null
);

select * from s;

insert into s (s_no, sname, status, city) values (1, 'name', 'good', 'london');
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ ./HsSqlUtil allannotations test6.sql
[CreateTable
   [TypeAnnotation (Pseudo Void),
    CatUpdates
      [CatCreateTable "s"
         [("s_no", ScalarType "int4"), ("sname", ScalarType "text"),
          ("status", ScalarType "int4"), ("city", ScalarType "text")]
         [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
          ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
          ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")]],
    SourcePos "test6.sql" 1 8]
   "s"
   [AttributeDef [SourcePos "test6.sql" 2 8] "s_no"
      (SimpleTypeName [SourcePos "test6.sql" 2 13] "int")
      Nothing
      [RowPrimaryKeyConstraint [SourcePos "test6.sql" 2 17] ""],
    AttributeDef [SourcePos "test6.sql" 3 8] "sname"
      (SimpleTypeName [SourcePos "test6.sql" 3 14] "text")
      Nothing
      [NotNullConstraint [SourcePos "test6.sql" 3 19] ""],
    AttributeDef [SourcePos "test6.sql" 4 8] "status"
      (SimpleTypeName [SourcePos "test6.sql" 4 15] "int")
      Nothing
      [NotNullConstraint [SourcePos "test6.sql" 4 19] ""],
    AttributeDef [SourcePos "test6.sql" 5 8] "city"
      (SimpleTypeName [SourcePos "test6.sql" 5 13] "text")
      Nothing
      [NotNullConstraint [SourcePos "test6.sql" 5 18] ""]]
   [],
 SelectStatement
   [TypeAnnotation (Pseudo Void),
    StatementTypeA
      (StatementType []
         [("s_no", ScalarType "int4"), ("sname", ScalarType "text"),
          ("status", ScalarType "int4"), ("city", ScalarType "text")]),
    CatUpdates [], SourcePos "test6.sql" 8 1]
   (Select
      [TypeAnnotation
         (SetOfType
            (CompositeType
               [("s_no", ScalarType "int4"), ("sname", ScalarType "text"),
                ("status", ScalarType "int4"), ("city", ScalarType "text")])),
       SourcePos "test6.sql" 8 1]
      Dupes
      (SelectList [SourcePos "test6.sql" 8 8]
         [SelExp [SourcePos "test6.sql" 8 8]
            (Identifier [SourcePos "test6.sql" 8 8] "*")]
         [])
      [Tref [SourcePos "test6.sql" 8 15] "s" NoAlias]
      Nothing
      []
      Nothing
      []
      Nothing
      Nothing),
 Insert
   [TypeAnnotation (Pseudo Void),
    StatementTypeA (StatementType [] []), CatUpdates [],
    SourcePos "test6.sql" 10 1]
   "s"
   ["s_no", "sname", "status", "city"]
   (Values
      [TypeAnnotation
         (SetOfType
            (CompositeType
               [("column1", ScalarType "int4"), ("column2", UnknownType),
                ("column3", UnknownType), ("column4", UnknownType)])),
       SourcePos "test6.sql" 10 43]
      [[IntegerLit
          [TypeAnnotation (ScalarType "int4"), SourcePos "test6.sql" 10 51,
           InferredType (ScalarType "int4")]
          1,
        StringLit
          [TypeAnnotation UnknownType, InferredType (ScalarType "text")]
          "'"
          "name",
        StringLit
          [TypeAnnotation UnknownType, InferredType (ScalarType "int4")]
          "'"
          "good",
        StringLit
          [TypeAnnotation UnknownType, InferredType (ScalarType "text")]
          "'"
          "london"]])
   Nothing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> allAnnotationsA = mode $ AllAnnotations {database = def
>                                        ,files = def &= typ "FILES" & args}
>                   &= text "reads each file, parses, type checks, then pretty \
>                           \prints the ast with all annotations except the \
>                           \source positions"
>
> allAnnotations :: String -> [FilePath] -> IO ()
> allAnnotations db fns = wrapETs $ do
>   cat <- liftIO (readCatalog db) >>= tsl
>   mapM (\f -> (liftIO . readInput) f >>=
>                             tsl . P.parseSql f) fns >>=
>     return . (concat |>
>               astTransformer |>
>               A.typeCheck cat |>
>               snd |>
>               ppExpr) >>=
>     liftIO . putStrLn

-------------------------------------------------------------------------------

> -- | Pretty print list of type errors with optional source position
> --   in emacs readable format.
> ppTypeErrors :: [(Maybe (String,Int,Int), [TypeError])] -> [String]
> ppTypeErrors tes =
>   map showSpTe tes
>   where
>     showSpTe (Just (fn,l,c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>     showSpTe (_,e) = "unknown:0:0:\n" ++ show e

> getTypeErrors :: Data a => a -> [(Maybe (String,Int,Int), [TypeError])]
> getTypeErrors es =
>   let as = [(a::Annotation) | a <- universeBi es]
>   in mapMaybe getTes as
>   where
>     getTes as = let tes = errs as
>                 in if null tes
>                    then Nothing
>                    else Just (asrc as, tes)

-------------------------------------------------------------------------------

read file as string - issues are:

want to support reading from stdin, and reading from a string passed
as an argument to the exe

> -- | read a file as text, will read from stdin if filename is '-'.
> readInput :: FilePath -> IO String
> readInput f =
>   case f of
>              "-" -> getContents
>              _ | length f >= 2 &&
>                  head f == '"' && last f == '"'
>                    -> return $ drop 1 $ take (length f - 1) f
>                | otherwise -> readFile f

-------------------------------------------------------------------------------

 > readParseTransform :: (MonadIO m, MonadError String m) =>
 >                       [FilePath] -> m [Statement]

> {-readParseTransform fns =
>      mapM (\f -> (liftIO . readInput) f >>=
>                  tsl . P.parseSql f) fns >>=
>      return . (concat |>
>                astTransformer)-}

-------------------------------------------------------------------------------

> readParseTransformList :: (MonadIO m, MonadError String m) =>
>                       [FilePath] -> m [[Statement]]
> readParseTransformList =
>      mapM (\f -> (liftIO . readInput) f >>=
>                  tsl . P.parseSql f >>=
>                  return . astTransformer)

-------------------------------------------------------------------------------

> {-readInto act fns =
>                forM_ fns $ \f ->
>                  (liftIO . readInput) f >>=
>                  act-}

 >                  tsl . lexSqlText f >>=
 >                  mapM_ (liftIO . print)

-------------------------------------------------------------------------------

main
====

>
> main :: IO ()
> main = do
>        cmd <- cmdArgs "HsSqlUtil, Copyright Jake Wheat 2010"
>                       [lexA, parseA, ppppA, pppA,
>                        parseExpressionA, typeCheckExpressionA,
>                        typeCheckA,allAnnotationsA]
>
>        case cmd of
>          Lex fns -> lexFiles fns
>          Parse fns -> showAst fns
>          Pppp fns -> testPppp fns
>          Ppp fns -> ppp fns
>          ParseExpression fns -> parseExpression fns
>          TypeCheck db fns -> typeCheck2 db fns
>          TypeCheckExpression db fns -> typeCheckExpression db fns
>          AllAnnotations db fns -> allAnnotations db fns
>
> lexA, parseA, ppppA, pppA,
>   typeCheckA, parseExpressionA, typeCheckExpressionA,
>   allAnnotationsA :: Mode HsSqlUtil

-------------------------------------------------------------------------------

TODOS

command to display all annotation inline in sql source
better catalog diff, e.g. if two tables differ in one column just show
that difference
see if can improve ppshow to use slightly less newlines
command to show which bits of preexisting catalog are used
command to list which views/functions/etc. aren't used anywhere
replace a bunch of commands with composible set:
parse(expression), ppp, typecheck(expression), allannots
->
parse (expression option)
type check
annotation filter + options
output: ast, sql, annotations + options




want better options system:
first set of options get passed around in reader? monad:
  database name, username, password for pg
  useextensions
  annotation control? - when showing values, which annotations to also output?
these have defaults, can change defaults in ~/.config/hssqlutil or something,
and can be overridden using --dbname=...,etc. command line args (make
these work like psql? then have environment variables also)
second set of options is for input and output:
read from file(s), stdin, or from string literal command line arg, e.g.
HsSqlUtil expressionType --src="3 + 5"
output to file(s) or stdout

review command names and arguments:
find a better naming convention: some commands produce haskell values as text,
some produce non haskell compatible text e.g. lexfile
some run tests and produce a success/fail result, maybe a list of issues

run multiple commands in one invocation?

check errors passed to user are understandable

command line commands to add:

showAast
ppCatalog - read from db and print in human readable rather
            than as haskell value
showexpressionast
showexpressionaast
typecheckexpression
pppexpression
showCatalogUpdates - run over some sql files, outputs the catalog changes
                     made by this sql
ppCatalogUpdates

graphviz stuff: dependency graph: pass a view name or function and it
draws a graph of all the views, tables and functions which are used by
that view or function

run an extension by name over some sql source to view differences: add
integration with external diff viewers, so can see before and after,
maybe option to either view pp'd sql, annotated pp'd sql, ast, aast,
etc.  - can also use this for pppsql and pppexpression to view how the
pretty printer mangles things, and for testing, etc.

logging/verbosity:

want a way to log to stderr/stdout/ files with different verbosity
settings

command line arguments:
options:
--database=xxx
?
--extensions
--username
--password
?other connection info
annotation control?

command name
other command arguments:
- on its own is read from stdin
--input='xxx' add literal input rather than from file/stdin


plan for changing how this is written

problem 1: cmdargs is non local, want to create a complete description
of each command, then maybe use template haskell or haskell-src-exts
to create a main function

problem 2: everything relies on everything else, so if one file in one
place is broken, then can't run any of the commands since this file
won't compile - this is a problem when generating things like
defaulttemplate1catalog and astanti. solution - package each command
in separate file, then can easily create a exe which only brings one
command in for instance.

problem 3: want to say e.g. that the database setting is common, and
share it's definition amongst multiple commands, and provide docs/
help which don't repeat the database setting help unneccessarily -
this might be because I don't know how to use cmdargs
