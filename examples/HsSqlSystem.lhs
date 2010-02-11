! /usr/bin/env runghc

Copyright 2009 Jake Wheat

Command line access to a bunch of utility functions.

run
HsSqlSystem.lhs -?
to get a list of commands and purpose and usage info

> {-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables,FlexibleContexts,QuasiQuotes #-}
>
> import System.Console.CmdArgs
> import System.IO
> import Control.Monad.Error
> --import Debug.Trace
> import Test.Framework (defaultMainWithArgs)
>
> import Database.HsSqlPpp.Tests.Tests
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Catalog
> import qualified Database.HsSqlPpp.TypeChecker as A
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.SqlTypes
>
> import qualified Database.HsSqlPpp.Parser as P
> import Database.HsSqlPpp.Parsing.Lexer
>
> import Database.HsSqlPpp.PrettyPrinter
>
> import Database.HsSqlPpp.Examples.AnnotateSource
>
> import Database.HsSqlPpp.Examples.DatabaseLoader
> --import Database.HsSqlPpp.Examples.WrapperGen
> import Database.HsSqlPpp.Examples.DBUtils
>
> import Database.HsSqlPpp.DevelTools.MakeWebsite
> import Database.HsSqlPpp.DevelTools.MakeAntiNodes
> import Database.HsSqlPpp.Examples.Extensions.TransitionConstraints
> import Database.HsSqlPpp.Examples.Extensions.ChaosExtensions

-------------------------------------------------------------------------------

command defs
============

> data HsSqlSystem = Lex {files :: [String]}
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
>                  | AnnotateSource {database :: String
>                                   ,file :: String}
>                  | PPCatalog {database :: String
>                              ,files :: [String]}
>
>                  | Load {database :: String
>                         ,files :: [String]}
>                  | LoadPsql {database :: String
>                             ,files :: [String]}
>                  | PgDump {database :: String}
>                  | Clear {database :: String}
>                  | ClearLoad {database :: String
>                              ,files :: [String]}
>                  | DBCatalog {database :: String}
>
>                  | TestBattery {database :: String
>                                ,files :: [String]}
>
>                  | Test {extra :: [String]}
>
>                  | GenWrap {database :: String
>                            ,file :: String}
>
>                  | MakeWebsite
>                  | MakeAntiNodes
>                  | ResetChaos
>
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ HsSqlSystem lex test2.sql
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
$ HsSqlSystem parse test2.sql
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
$ HsSqlSystem parse test3.sql
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
$ ./HsSqlSystem parse test3a.sql
-- ast of test3a.sql
HsSqlSystem: "test3a.sql" (line 1, column 46):
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
>                          stripAnnotations |>
>                          ppExpr) >>=
>                liftIO . putStrLn)

-------------------------------------------------------------------------------

ParseExpression
===============

Parse an expression, useful for trying things out.

examples
--------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "test(a,v)" | HsSqlSystem parseexpression -
-- ast of -
FunCall [] "test" [Identifier [] "a", Identifier [] "v"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "1+1+1" | HsSqlSystem parseexpression -
-- ast of -
FunCall [] "+"
  [FunCall [] "+" [IntegerLit [] 1, IntegerLit [] 1],
   IntegerLit [] 1]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "case 1 when 2 then 3 else 4 end" | HsSqlSystem parseexpression -
-- ast of -
CaseSimple [] (IntegerLit [] 1)
  [([IntegerLit [] 2], IntegerLit [] 3)]
  (Just (IntegerLit [] 4))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "3 = any (array[1,2])" | HsSqlSystem parseexpression -
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
>                          stripAnnotations |>
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
$ HsSqlSystem ppp test4.sql
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

~~~~~~~~~~~~~~~~
$ HsSqlSystem pppp test4.sql
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
>                     return . stripAnnotations
>             ast2 <- (return . printSql) ast1 >>=
>                     tsl . P.parseSql "" >>=
>                     return . stripAnnotations
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ HsSqlSystem typecheck test4.sql
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
$ HsSqlSystem typecheck test5.sql
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
>               A.getTypeErrors |>
>               ppTypeErrors) >>=
>     mapM_ (liftIO . putStrLn)

-------------------------------------------------------------------------------

typeCheckExpression
===================

examples
--------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "3 = any (array[1,2])" | ./HsSqlSystem typecheckexpression -
ScalarType "bool"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ echo "test(3,'stuff'::what)" | ./HsSqlSystem typecheckexpression -
-:1:17:
[UnknownTypeName "what"]
TypeCheckFailed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ echo "test(3,'stuff')" | ./HsSqlSystem typecheckexpression -
-:1:5:
[NoMatchingOperator "test" [ScalarType "int4",UnknownType]]
TypeCheckFailed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type checks strings as unknown types, doesn't parse the contents of
these yet, works just like postgresql which catches this error only at
runtime.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ echo "array[3,'stuff']" | ./HsSqlSystem typecheckexpression -
ArrayType (ScalarType "int4")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ echo "array[3,'stuff'::text]" | ./HsSqlSystem typecheckexpression -
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
>   tes <- mapM (return . A.getTypeErrors) aasts
>   mapM_ (\x -> (mapM_ (liftIO . putStrLn) (ppTypeErrors x))) $
>         filter (not . null) tes
>   mapM_ (\a -> liftM (show . head)
>                (return $ A.getTopLevelTypes [a]) >>=
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ ./HsSqlSystem allannotations test6.sql
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

annotateSource
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.SqlPostgresql}
$ ./HsSqlSystem annotatesource --file=test6.sql
--annotated source of test6.sql

/*[CatUpdates
   [CatCreateTable "s"
      [("s_no", ScalarType "int4"), ("sname", ScalarType "text"),
       ("status", ScalarType "int4"), ("city", ScalarType "text")]
      [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
       ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
       ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")]]]*/
create table s (
       s_no int primary key,
       sname text not null,
       status int not null,
       city text not null
);


/*[StatementTypeA
   (StatementType []
      [("s_no", ScalarType "int4"), ("sname", ScalarType "text"),
       ("status", ScalarType "int4"), ("city", ScalarType "text")])]*/
select * from s;


/*[StatementTypeA (StatementType [] [])]*/
insert into s (s_no, sname, status, city) values (1, 'name', 'good', 'london');
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> annotateSourceA = mode $ AnnotateSource {database = def
>                                         ,file = def &= typ "FILE"}
>                   &= text "reads a file, parses, type checks, then \
>                           \outputs info on each statement \
>                           \interspersed with the original source \
>                           \code"
>
> annotateSourceF :: String -> FilePath -> IO ()
> annotateSourceF db f = do
>   putStrLn $ "--annotated source of " ++ f
>   s <- readInput f
>   s1 <- annotateSource (Just astTransformer) Nothing db f s
>   putStrLn s1

-------------------------------------------------------------------------------

ppCatalog
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ ./HsSqlSystem ppcatalog test4.sql

missing:
CatCreateTable p(
    "p_po" ScalarType "int4"
    "pname" ScalarType "text"
    "color" ScalarType "text"
    "weight" ScalarType "int4"
    "city" ScalarType "text"
)(
    "tableoid" ScalarType "oid"
    "cmax" ScalarType "cid"
    "xmax" ScalarType "xid"
    "cmin" ScalarType "cid"
    "xmin" ScalarType "xid"
    "ctid" ScalarType "tid"
)
CatCreateTable s(
    "s_no" ScalarType "int4"
    "sname" ScalarType "text"
    "status" ScalarType "int4"
    "city" ScalarType "text"
)(
    "tableoid" ScalarType "oid"
    "cmax" ScalarType "cid"
    "xmax" ScalarType "xid"
    "cmin" ScalarType "cid"
    "xmin" ScalarType "xid"
    "ctid" ScalarType "tid"
)
CatCreateTable sp(
    "s_no" ScalarType "int4"
    "p_no" ScalarType "int4"
    "qty" ScalarType "int4"
)(
    "tableoid" ScalarType "oid"
    "cmax" ScalarType "cid"
    "xmax" ScalarType "xid"
    "cmin" ScalarType "cid"
    "xmin" ScalarType "xid"
    "ctid" ScalarType "tid"
)
extra:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> ppCatalogA = mode $ PPCatalog {database = def
>                               ,files = def &= typ "FILES" & args}
>              &= text "reads each file, parses, type checks, then outputs the \
>                      \changes to the catalog that the sql makes"
>
> ppCatalog :: String -> [FilePath] -> IO ()
> ppCatalog db fns = wrapETs $ do
>   scat <- liftIO (readCatalog db) >>= tsl
>   (ncat, _) <- mapM (\f -> (liftIO . readInput) f >>=
>                            tsl . P.parseSql f) fns >>=
>                  return . (concat |>
>                            astTransformer |>
>                            A.typeCheck scat)
>   liftIO $ putStrLn $ ppCatDiff $ compareCatalogs scat emptyCatalog ncat

-------------------------------------------------------------------------------

load
====

load sql files into a database via parsing and pretty printing them

> loadA = mode $ Load {database = def
>                     ,files = def &= typ "FILES" & args}
>         &= text "This takes one or more files with sql source text, \
>            \parses them then loads them into the database given."
>
> loadSql :: String -> [String] -> IO ()
> loadSql db fns = wrapETs $
>      liftIO (hSetBuffering stdout NoBuffering) >>
>      mapM (\f -> (liftIO . readInput) f >>=
>                  tsl . P.parseSql f) fns >>=
>      return . (concat |>
>                astTransformer) >>=
>      liftIO . loadAst db

-------------------------------------------------------------------------------

loadPsql
========

load sql files into a database via psql

> loadPsqlA = mode $ LoadPsql {database = def
>                             ,files = def &= typ "FILES" & args}
>             &= text "loads sql into a database using psql."
>
> loadSqlPsql :: String -> [String] -> IO ()
> loadSqlPsql db = wrapETs .
>   mapM_ (\s -> liftIO (loadSqlUsingPsqlFromFile db s) >>=
>                tsl >>=
>                liftIO . putStrLn)

-------------------------------------------------------------------------------

pgDump
======

run pg dump

> pgDumpA = mode $ PgDump {database = def}
>           &= text "run pg dump, used for testing."
>
> pgDump1 :: String -> IO ()
> pgDump1 db = pgDump db >>= putStrLn

-------------------------------------------------------------------------------

clear
=====

small hack utility to help with testing, clears the database given (as
long as your user name is jake!)

TODO: use the correct username in this command
TODO: do something more correct

> clearA = mode $ Clear {database = def}
>          &= text "hacky util to clear a database"
>
> cleardb :: String -> IO ()
> cleardb = clearDB

-------------------------------------------------------------------------------

clearLoad
=========

like load above, but runs the clear command first

might try to work out a way of running multiple commands in one invoc
of this exe, then this command will disappear

> clearLoadA = mode $ ClearLoad {database = def
>                               ,files = def &= typ "FILES" & args}
>              &= text "cleardb then loadsql"
>
> clearAndLoadSql :: String -> [String] -> IO ()
> clearAndLoadSql db fns = cleardb db >> loadSql db fns

-------------------------------------------------------------------------------

dbCatalog
=========

This reads an catalog from a database and writes it out using
show. Warning: these things are big since they contain all the default
PostgreSQL catalog contents.

> catalogA = mode $ DBCatalog {database = def}
>            &= text "read the catalog for the given db and dumps it in source \
>                    \format, used to create the catalog value for template1"
>
> readCat :: String -> IO ()
> readCat dbName = do
>   cat <- readCatalogFromDatabase dbName
>   putStrLn preamble
>   putStrLn $ prefixLines $ ppExpr cat
>   where
>     preamble = [$here|

\begin{code}

This file is auto generated, to regenerate run
example/HsSqlSystem dbcatalog --database=template1 > src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs

from the project root (i.e. where the cabal file is located).

> module Database.HsSqlPpp.AstInternals.Catalog.DefaultTemplate1Catalog
>      (defaultTemplate1Catalog) where
>
> import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
> import Database.HsSqlPpp.AstInternals.TypeType
>
> defaultTemplate1Catalog :: Catalog
> defaultTemplate1Catalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      updateCatalog defaultCatalog
\end{code}
>|]
>     prefixLines = unlines . map (">        " ++) . lines

-------------------------------------------------------------------------------

testBattery
===========

run test battery: run a bunch of tests including consistency on the
database and sql files given

The idea is to typecheck the sql, load it into pg and dump it via psql
and via database loader, can then compare asts, catalogs, etc. in a
lot of different ways

currently:
parse and type check sql, save the catalog
load the sql into the db using psql, compare the catalog read from pg
with the catalog from typechecking
dump the sql and typecheck the dump, compare the catalog from this
check with the catalog from the original typecheck

todo: compare asts from initial parse with parsed dump - this is going
to be a lot of work to get passing since the statements are
re-ordered, and sometimes changed/ split up by pg

also: load the sql using the extension system and database loader,
then compare pg catalog with initial catalog, and dump and compare ast
with original ast

want to run a similar set of tests starting with the dump sql:
get ast,cat from dump sql, load using psql and using databaseloader
and check cats and subsequent dump asts.

getting the dump ast comparing with the original ast:

step one: convert tests in parser test to also roundtrip through
database, see parsertests for details

step two: write an ast conversion routine: assume that the pgdump ast
is like the ast fed into pg but with a few statements split into
components (e.g. create table with serial is split into create
sequence and create table), and then the statements are reordered, so
write a routine to mirror this - will then have
(anyast -> rearrange and reorder) == (anyast -> pg->pgdump)

> testBatteryA = mode $ TestBattery {database = def
>                                   ,files = def &= typ "FILES" & args}
>                &= text "runs a load of consistency tests on the sql passed"
>
> runTestBattery :: String -> [FilePath] -> IO ()
> runTestBattery dbName fns = wrapETs $ do
>     liftIO $ hSetBuffering stdout NoBuffering
>     liftIO $ hSetBuffering stderr NoBuffering
>     liftIO $ clearDB dbName
>     startingCat <- liftIO (readCatalog dbName) >>= tsl
>     (originalCat :: Catalog,
>      originalAast :: StatementList) <-
>        mapM (\f -> (liftIO . readInput) f >>= tsl . P.parseSql f) fns >>=
>        return . (concat |>
>                  astTransformer |>
>                  A.typeCheck startingCat)
>
>     headerMessage "type errors from initial parse:\n"
>     (return . A.getTypeErrors) originalAast >>=
>        return . ppTypeErrors >>=
>        mapM_ (liftIO . putStrLn)
>
>     mapM_ (\s -> liftIO (loadSqlUsingPsqlFromFile dbName s) >>= tsl >>=
>                  liftIO . putStrLn) fns
>     properCat <- liftIO (readCatalog dbName) >>= tsl
>
>     headerMessage "catalog differences from initial parse and vanilla load:\n"
>     liftIO $ putStrLn $ ppCatDiff (compareCatalogs startingCat
>                                                    originalCat
>                                                    properCat)
>
>     headerMessage "dump and check\n"
>     (dumpCat,dumpAast) <-
>       liftIO (pgDump dbName) >>=
>       tsl . P.parseSql "dump" >>=
>       return . A.typeCheck startingCat
>
>     headerMessage "type errors from dump:\n"
>     (return . A.getTypeErrors) dumpAast >>=
>        return . ppTypeErrors >>=
>        mapM_ (liftIO . putStrLn)
>
>     headerMessage "catalog differences from initial parse and rechecked \
>                   \pg dump:\n"
>     liftIO $ putStrLn $ ppCatDiff $ compareCatalogs startingCat
>                                                     originalCat
>                                                     dumpCat
>
>     (liftIO . putStrLn) "complete!"
>     where
>       headerMessage = liftIO . putStrLn .
>                       ("-----------------------------\n" ++)

-------------------------------------------------------------------------------

test
====

run the test suite

> testA = mode $ Test {extra = def &= typ "ANY" & args & unknownFlags}
>         &= text "run automated tests, uses test.framework can pass arguments \
>                 \to this e.g. HsSqlSystem test -t parserTests"
>
> runTests :: [String] -> IO ()
> runTests = defaultMainWithArgs allTests

-------------------------------------------------------------------------------

genWrap
=======

> genWrapA = mode $ GenWrap {database = def
>                           ,file = def &= typ "FILE"}
>            &= text "experimental code to generate typesafe haskell wrapper \
>                    \for db access"
> genWrap :: String -> String -> IO ()
> genWrap db f = undefined
>   {-wrapETs doit
>     where
>       doit :: (MonadIO m) => ErrorT String m ()
>       doit = liftIO (wrapperGen db f >>= putStrLn)-}

-------------------------------------------------------------------------------

> -- | Pretty print list of type errors with optional source position
> --   in emacs readable format.
> ppTypeErrors :: [(Maybe AnnotationElement, [TypeError])] -> [String]
> ppTypeErrors tes =
>   map showSpTe tes
>   where
>     showSpTe (Just (SourcePos fn l c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>     showSpTe (_,e) = "unknown:0:0:\n" ++ show e

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

reset chaos
===========

routine to reset the chaos2010 database from the sql files, for
testing the extensions used for chaos.

clearLoad
=========

like load above, but runs the clear command first

might try to work out a way of running multiple commands in one invoc
of this exe, then this command will disappear

> resetChaosA = mode $ ResetChaos
>              &= text "reset the chaos database"
>
> resetChaos :: IO ()
> resetChaos = wrapETs $ do
>   let db = "chaos"
>   liftIO $ do
>     hSetBuffering stdout NoBuffering
>     cleardb db
>   ast <- mapM (\f -> (liftIO . readInput) f >>=
>                  tsl . P.parseSql f) files >>=
>      return . (concat |>
>                stripAnnotations |> -- figure out why some exts only work with this - need the sourceposes to stay
>                chaosExtensions)
>   {-mapM_ (liftIO . putStrLn) $
>             (A.typeCheck defaultTemplate1Catalog |>
>              snd |>
>              A.getTypeErrors |>
>              ppTypeErrors) ast-}
>   liftIO $ putStrLn $ printSql ast
>   --liftIO $ loadAst db ast
>   where
>     files =
>         ["testfiles/chaos2010sql/chaos/server/Metadata.sql"
>         ,"testfiles/chaos2010sql/chaos/server/PiecePrototypes.sql"
>         ,"testfiles/chaos2010sql/chaos/server/Spells.sql"
>         ,"testfiles/chaos2010sql/chaos/server/GlobalData.sql"
>         ,"testfiles/chaos2010sql/chaos/server/Wizards.sql"
>         ,"testfiles/chaos2010sql/chaos/server/Pieces.sql"
>         ,"testfiles/chaos2010sql/chaos/server/TurnSequence.sql"
>         ,"testfiles/chaos2010sql/chaos/server/ActionTestSupport.sql"
>         ,"testfiles/chaos2010sql/chaos/server/SquaresValid.sql"
>         ,"testfiles/chaos2010sql/chaos/server/Actions.sql"
>         ,"testfiles/chaos2010sql/chaos/server/ActionHistory.sql"
>         ,"testfiles/chaos2010sql/chaos/server/NewGame.sql"
>         ,"testfiles/chaos2010sql/chaos/server/AI.sql"
>         ,"testfiles/chaos2010sql/chaos/client/WindowManagement.sql"
>         ,"testfiles/chaos2010sql/chaos/client/Sprites.sql"
>         ,"testfiles/chaos2010sql/chaos/client/WizardDisplayInfo.sql"
>         ,"testfiles/chaos2010sql/chaos/client/BoardWidget.sql"
>         ,"testfiles/chaos2010sql/chaos/client/SpellBookWidget.sql"
>         ,"testfiles/chaos2010sql/chaos/client/NewGameWidget.sql"
>         ,"testfiles/chaos2010sql/chaos/client/ClientActions.sql"
>         ,"testfiles/chaos2010sql/chaos/client/ClientNewGame.sql"
>         ]

-------------------------------------------------------------------------------

MakeWebsite
===========

code to build the website for hssqlppp

> makeWebsiteA = mode $ MakeWebsite
>                &= text "build the website"

-------------------------------------------------------------------------------

MakeAntinodes
=============

code to generate the anti ast source, for supporting anti quoting in
sql quasiquotes

> makeAntiNodesA = mode $ MakeAntiNodes
>                &= text "development tool"
> makeAntiNodesF :: IO ()
> makeAntiNodesF = do
>   s <- makeAntiNodes
>   writeFile "src/Database/HsSqlPpp/AstInternals/AstAnti.hs" s

-------------------------------------------------------------------------------

 > readParseTransform :: (MonadIO m, MonadError String m) =>
 >                       [FilePath] -> m [Statement]

> readParseTransform fns =
>      mapM (\f -> (liftIO . readInput) f >>=
>                  tsl . P.parseSql f) fns >>=
>      return . (concat |>
>                astTransformer)

-------------------------------------------------------------------------------

> readParseTransformList :: (MonadIO m, MonadError String m) =>
>                       [FilePath] -> m [[Statement]]
> readParseTransformList =
>      mapM (\f -> (liftIO . readInput) f >>=
>                  tsl . P.parseSql f >>=
>                  return . astTransformer)

-------------------------------------------------------------------------------

> readInto act fns =
>                forM_ fns $ \f ->
>                  (liftIO . readInput) f >>=
>                  act

 >                  tsl . lexSqlText f >>=
 >                  mapM_ (liftIO . print)

-------------------------------------------------------------------------------

main
====

>
> main :: IO ()
> main = do
>        cmd <- cmdArgs "HsSqlSystem, Copyright Jake Wheat 2010"
>                       [lexA, parseA, ppppA, pppA,
>                        parseExpressionA, typeCheckExpressionA,
>                        typeCheckA,allAnnotationsA,
>                        annotateSourceA, ppCatalogA,
>                        clearA, loadA, clearLoadA, catalogA, loadPsqlA,
>                        pgDumpA, testBatteryA,
>                        testA, genWrapA, makeWebsiteA, makeAntiNodesA
>                       ,resetChaosA]
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
>          AnnotateSource db fn -> annotateSourceF db fn
>          PPCatalog db fns -> ppCatalog db fns
>          Clear db -> cleardb db
>          Load db fns -> loadSql db fns
>          ClearLoad db fns -> clearAndLoadSql db fns
>          DBCatalog db -> readCat db
>          LoadPsql db fns -> loadSqlPsql db fns
>          PgDump db -> pgDump1 db
>          TestBattery db fns -> runTestBattery db fns
>          Test as -> runTests as
>          GenWrap db f -> genWrap db f
>          MakeWebsite -> makeWebsite
>          MakeAntiNodes -> makeAntiNodesF
>          ResetChaos -> resetChaos
>
> lexA, parseA, ppppA, pppA, annotateSourceA, clearA, loadA,
>   clearLoadA, catalogA, loadPsqlA, pgDumpA, testBatteryA,
>   typeCheckA, testA, parseExpressionA, typeCheckExpressionA,
>   allAnnotationsA, ppCatalogA, genWrapA, makeWebsiteA,
>   makeAntiNodesA, resetChaosA :: Mode HsSqlSystem

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




think of a better name for this command than hssqlsystem

want better options system:
first set of options get passed around in reader? monad:
  database name, username, password for pg
  useextensions
  annotation control? - when showing values, which annotations to also output?
these have defaults, can change defaults in ~/.config/hssqlsystem or something,
and can be overridden using --dbname=...,etc. command line args (make
these work like psql? then have environment variables also)
second set of options is for input and output:
read from file(s), stdin, or from string literal command line arg, e.g.
HsSqlSystem expressionType --src="3 + 5"
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
