
Command line access to a bunch of utility functions.

ghc --make -threaded -XScopedTypeVariables -XDeriveDataTypeable -DPOSTGRES -cpp -pgmPcpphs -optP--cpp -idevel:src/lib:src/qq:src/postgresql:examples/chaos:examples/extensions/:examples/util/:tests/ --make examples/util/HsSqlPppUtil.lhs

run
HsSqlPppUtil.lhs -?
to get a list of commands and purpose and usage info

> {-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables,FlexibleContexts #-}
>
> import System.Console.CmdArgs
> import Control.Monad.Error

> --import Debug.Trace
> import Data.Maybe
> import Data.Generics.Uniplate.Data

> import qualified Language.Haskell.Exts as Exts
>
> import qualified Database.HsSqlPpp.TypeChecker as A
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.SqlTypes
>
> import qualified Database.HsSqlPpp.Parser as P
> import Database.HsSqlPpp.Parsing.Lexer
>
> import Database.HsSqlPpp.PrettyPrinter
>
> import Database.HsSqlPpp.Utils.DBUtils
> import Database.HsSqlPpp.Utils.PPExpr
> import Database.HsSqlPpp.Utils.RoundTripTester

-------------------------------------------------------------------------------

command defs
============

> data HsSqlPppUtil = Lex {files :: [String]}
>                   | Parse {files :: [String]}
>                   | ParseExpression {files :: [String]}
>                   | Ppp {files :: [String]}
>                   | Pppp {files :: [String]}
>                   | TypeCheck {database :: String
>                               ,files :: [String]}
>                   | TypeCheckExpression {database :: String
>                                         ,files :: [String]}
>                   | AllAnnotations {database :: String
>                                    ,files :: [String]}
>                   | Rtt {database :: String
>                         ,files :: [String]}
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
$ examples/util/HsSqlPppUtil lex test2.sql
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
> lexFiles fns = mapM_ doF fns
>   where
>     doF fn = runES $ do
>                liftIO $ putStrLn ("lexing " ++ fn)
>                s <- liftIO $ readInput fn
>                tokens <- etsr $ lexSqlText fn s
>                liftIO $ mapM_ print tokens

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
$ HsSqlPppUtil parse test2.sql
-- ast of test2.sql
[CreateTable Ann "s"
   [AttributeDef Ann "s_no" (SimpleTypeName Ann "int") Nothing
      [RowPrimaryKeyConstraint Ann ""],
    AttributeDef Ann "sname" (SimpleTypeName Ann "text") Nothing
      [NotNullConstraint Ann ""],
    AttributeDef Ann "status" (SimpleTypeName Ann "int") Nothing
      [NotNullConstraint Ann ""],
    AttributeDef Ann "city" (SimpleTypeName Ann "text") Nothing
      [NotNullConstraint Ann ""]]
   [] Nothing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test3.sql:

~~~~~~~~~~~~ {.SqlPostgresql}
select * from s natural inner join p natural inner join sp;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ HsSqlPppUtil parse test3.sql
-- ast of test3.sql
[SelectStatement Ann
   (Select Ann Dupes
      (SelectList Ann [SelExp Ann (Identifier Ann "*")] [])
      [JoinedTref Ann
         (JoinedTref Ann (Tref Ann (Identifier Ann "s") NoAlias) Natural
            Inner
            (Tref Ann (Identifier Ann "p") NoAlias)
            Nothing
            NoAlias)
         Natural
         Inner
         (Tref Ann (Identifier Ann "sp") NoAlias)
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
$ ./HsSqlPppUtil parse test3a.sql
-- ast of test3a.sql
"test3a.sql" (line 1, column 46):
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
> showAst =
>   mapM_ sa
>   where
>     sa :: String -> IO ()
>     sa fn = runES $ do
>               liftIO $ putStrLn ("-- ast of " ++ fn)
>               s <- liftIO $ readInput fn
>               ast <- etsr $ P.parseSql fn s
>               liftIO $ putStrLn $ ppExprNoAnns $ astTransformer ast

> ppExprNoAnns :: Show a => a -> String
> ppExprNoAnns = ppExprT stripA
>   where
>     stripA :: Exts.Exp -> Exts.Exp
>     stripA = transformBi $ \x ->
>                case x of
>                  (Exts.Paren (Exts.RecConstr (Exts.UnQual (Exts.Ident "Annotation")) _)) ->
>                           Exts.Con $ Exts.UnQual $ Exts.Ident "Ann"
>                  x1 -> x1

-------------------------------------------------------------------------------

ParseExpression
===============

Parse an expression, useful for trying things out.

examples
--------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "test(a,v)" | HsSqlPppUtil parseexpression -
-- ast of -
FunCall Ann "test" [Identifier Ann "a", Identifier Ann "v"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "1+1+1" | HsSqlPppUtil parseexpression -
-- ast of -
FunCall Ann "+"
  [FunCall Ann "+" [IntegerLit Ann 1, IntegerLit Ann 1],
   IntegerLit Ann 1]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "case 1 when 2 then 3 else 4 end" | HsSqlPppUtil parseexpression -
-- ast of -
CaseSimple Ann (IntegerLit Ann 1)
  [([IntegerLit Ann 2], IntegerLit Ann 3)]
  (Just (IntegerLit Ann 4))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "3 = any (array[1,2])" | HsSqlPppUtil parseexpression -
-- ast of -
LiftOperator Ann "=" LiftAny
  [IntegerLit Ann 3,
   FunCall Ann "arrayctor" [IntegerLit Ann 1, IntegerLit Ann 2]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> parseExpressionA = mode $ ParseExpression {files = def &= typ "FILES" & args}
>                    &= text "Parse files each containing one expression \
>                            \and output the asts"
>
> parseExpression :: [String] -> IO ()
> parseExpression = mapM_ pe
>   where
>     pe :: String -> IO ()
>     pe fn = runES $ do
>               liftIO $ putStrLn ("-- ast of " ++ fn)
>               s <- liftIO $ readInput fn
>               a <- etsr $ P.parseExpression fn s
>               liftIO $ putStrLn $ ppExprNoAnns $ astTransformer a

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
$ HsSqlPppUtil ppp test4.sql
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
> ppp = mapM_ f 
>   where
>     f :: String -> IO ()
>     f fn = runES $ do
>              liftIO $ putStrLn $ "--ppp " ++ fn
>              s <- liftIO $ readInput fn
>              a <- etsr $ P.parseSql fn s
>              liftIO $ putStrLn $ printSql $ astTransformer a

-------------------------------------------------------------------------------

Pppp
====
Parse, pretty print and parse, then check two parse trees are same

example
-------

~~~~~~~~~~~~~~~~{.sh}
$ HsSqlPppUtil pppp test4.sql
success
~~~~~~~~~~~~~~~~

> ppppA = mode $ Pppp {files = def &= typ "FILES" & args}
>         &= text "Routine to parse sql from a file, pretty print it \
>                 \then parse it again and check the post pretty \
>                 \printing ast is the same as the initial ast"
>
> testPppp :: [String] -> IO ()
> testPppp = mapM_ tp
>   where
>     tp fn = runES $ do
>               ast1 <- ps fn =<< liftIO (readInput fn)
>               ast2 <- ps "" $ printSql ast1
>               liftIO $ if ast1 /= ast2
>                        then do
>                             putStrLn "asts are different\n-- original"
>                             putStrLn $ ppExprNoAnns ast1
>                             putStrLn "-- ppp'd"
>                             putStrLn $ ppExprNoAnns ast2
>                          else putStrLn "success"
>     ps :: String -> String -> ErrorT String IO [Statement]
>     ps fn s = resetAnnotations `fmap` etsr (P.parseSql fn s)

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
$ HsSqlPppUtil typecheck test4.sql
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
$ HsSqlPppUtil typecheck test5.sql
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
> typeCheck2 db fns = do
>   cat <- either (error . show) return =<< readCatalog db
>   a1 <- concat `fmap` parseInputs P.parseSql fns
>   mapM_ putStrLn $ ppTypeErrors
>                  $ getTypeErrors
>                  $ snd
>                  $ A.typeCheck cat
>                  $ astTransformer a1

-------------------------------------------------------------------------------

typeCheckExpression
===================

examples
--------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
$ echo "3 = any (array[1,2])" | ./HsSqlPppUtil typecheckexpression -
ScalarType "bool"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ echo "test(3,'stuff'::what)" | ./HsSqlPppUtil typecheckexpression -
-:1:17:
[UnknownTypeName "what"]
TypeCheckFailed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ echo "test(3,'stuff')" | ./HsSqlPppUtil typecheckexpression -
-:1:5:
[NoMatchingOperator "test" [ScalarType "int4",UnknownType]]
TypeCheckFailed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type checks strings as unknown types, doesn't parse the contents of
these yet, works just like postgresql which catches this error only at
runtime.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ echo "array[3,'stuff']" | ./HsSqlPppUtil typecheckexpression -
ArrayType (ScalarType "int4")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ echo "array[3,'stuff'::text]" | ./HsSqlPppUtil typecheckexpression -
-:1:1:
[NoMatchingOperator "arrayctor" [ScalarType "int4",ScalarType "text"]]
TypeCheckFailed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> typeCheckExpressionA = mode $ TypeCheckExpression {database = def
>                               ,files = def &= typ "FILES" & args}
>      &= text "reads each file, parses as expression, \
>              \ type checks, then outputs the type or any type errors"
>
> typeCheckExpression :: String -> [FilePath] -> IO ()
> typeCheckExpression db fns = do
>   cat <- either (error . show) return =<< readCatalog db
>   a1 <- parseInputs P.parseExpression fns
>   let aa1 = map (A.typeCheckExpression cat . astTransformer) a1
>       tes = concatMap getTypeErrors aa1
>   mapM_ putStrLn $ ppTypeErrors tes

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
$ HsSqlPppUtil allannotations test6.sql
[CreateTable
   (Annotation{asrc = Just ("test6.sql", 1, 8),
               atype = Just (Pseudo Void), errs = [], stType = Nothing,
               catUpd =
                 [CatCreateTable "s"
                    [("s_no", ScalarType "int4"), ("sname", ScalarType "text"),
                     ("status", ScalarType "int4"), ("city", ScalarType "text")]
                    [("tableoid", ScalarType "oid"), ("cmax", ScalarType "cid"),
                     ("xmax", ScalarType "xid"), ("cmin", ScalarType "cid"),
                     ("xmin", ScalarType "xid"), ("ctid", ScalarType "tid")]],
               fnProt = Nothing, infType = Nothing})
   "s"
   [AttributeDef
      (Annotation{asrc = Just ("test6.sql", 2, 8), atype = Nothing,
                  errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                  infType = Nothing})
      "s_no"
      (SimpleTypeName
         (Annotation{asrc = Just ("test6.sql", 2, 13), atype = Nothing,
                     errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                     infType = Nothing})
         "int")
      Nothing
      [RowPrimaryKeyConstraint
         (Annotation{asrc = Just ("test6.sql", 2, 17), atype = Nothing,
                     errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                     infType = Nothing})
         ""],
    AttributeDef
      (Annotation{asrc = Just ("test6.sql", 3, 8), atype = Nothing,
                  errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                  infType = Nothing})
      "sname"
      (SimpleTypeName
         (Annotation{asrc = Just ("test6.sql", 3, 14), atype = Nothing,
                     errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                     infType = Nothing})
         "text")
      Nothing
      [NotNullConstraint
         (Annotation{asrc = Just ("test6.sql", 3, 19), atype = Nothing,
                     errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                     infType = Nothing})
         ""],
    AttributeDef
      (Annotation{asrc = Just ("test6.sql", 4, 8), atype = Nothing,
                  errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                  infType = Nothing})
      "status"
      (SimpleTypeName
         (Annotation{asrc = Just ("test6.sql", 4, 15), atype = Nothing,
                     errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                     infType = Nothing})
         "int")
      Nothing
      [NotNullConstraint
         (Annotation{asrc = Just ("test6.sql", 4, 19), atype = Nothing,
                     errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                     infType = Nothing})
         ""],
    AttributeDef
      (Annotation{asrc = Just ("test6.sql", 5, 8), atype = Nothing,
                  errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                  infType = Nothing})
      "city"
      (SimpleTypeName
         (Annotation{asrc = Just ("test6.sql", 5, 13), atype = Nothing,
                     errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                     infType = Nothing})
         "text")
      Nothing
      [NotNullConstraint
         (Annotation{asrc = Just ("test6.sql", 5, 18), atype = Nothing,
                     errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                     infType = Nothing})
         ""]]
   [],
 SelectStatement
   (Annotation{asrc = Just ("test6.sql", 8, 1),
               atype = Just (Pseudo Void), errs = [],
               stType =
                 Just
                   ([],
                    [("s_no", ScalarType "int4"), ("sname", ScalarType "text"),
                     ("status", ScalarType "int4"), ("city", ScalarType "text")]),
               catUpd = [], fnProt = Nothing, infType = Nothing})
   (Select
      (Annotation{asrc = Just ("test6.sql", 8, 1),
                  atype =
                    Just
                      (SetOfType
                         (CompositeType
                            [("s_no", ScalarType "int4"), ("sname", ScalarType "text"),
                             ("status", ScalarType "int4"), ("city", ScalarType "text")])),
                  errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                  infType = Nothing})
      Dupes
      (SelectList
         (Annotation{asrc = Just ("test6.sql", 8, 8), atype = Nothing,
                     errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                     infType = Nothing})
         [SelExp
            (Annotation{asrc = Just ("test6.sql", 8, 8), atype = Nothing,
                        errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                        infType = Nothing})
            (Identifier
               (Annotation{asrc = Just ("test6.sql", 8, 8),
                           atype =
                             Just
                               (CompositeType
                                  [("s_no", ScalarType "int4"), ("sname", ScalarType "text"),
                                   ("status", ScalarType "int4"), ("city", ScalarType "text")]),
                           errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                           infType = Nothing})
               "*")]
         [])
      [Tref
         (Annotation{asrc = Just ("test6.sql", 8, 15), atype = Nothing,
                     errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                     infType = Nothing})
         (Identifier
            (Annotation{asrc = Just ("test6.sql", 8, 15), atype = Nothing,
                        errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                        infType = Nothing})
            "s")
         NoAlias]
      Nothing
      []
      Nothing
      []
      Nothing
      Nothing),
 Insert
   (Annotation{asrc = Just ("test6.sql", 10, 1),
               atype = Just (Pseudo Void), errs = [], stType = Just ([], []),
               catUpd = [], fnProt = Nothing, infType = Nothing})
   (Identifier
      (Annotation{asrc = Just ("test6.sql", 10, 13), atype = Nothing,
                  errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                  infType = Nothing})
      "s")
   ["s_no", "sname", "status", "city"]
   (Values
      (Annotation{asrc = Just ("test6.sql", 10, 43),
                  atype =
                    Just
                      (SetOfType
                         (CompositeType
                            [("column1", ScalarType "int4"), ("column2", UnknownType),
                             ("column3", UnknownType), ("column4", UnknownType)])),
                  errs = [], stType = Nothing, catUpd = [], fnProt = Nothing,
                  infType = Nothing})
      [[IntegerLit
          (Annotation{asrc = Just ("test6.sql", 10, 51),
                      atype = Just (ScalarType "int4"), errs = [], stType = Nothing,
                      catUpd = [], fnProt = Nothing, infType = Just (ScalarType "int4")})
          1,
        StringLit
          (Annotation{asrc = Just ("test6.sql", 10, 54),
                      atype = Just UnknownType, errs = [], stType = Nothing, catUpd = [],
                      fnProt = Nothing, infType = Just (ScalarType "text")})
          "name",
        StringLit
          (Annotation{asrc = Just ("test6.sql", 10, 62),
                      atype = Just UnknownType, errs = [], stType = Nothing, catUpd = [],
                      fnProt = Nothing, infType = Just (ScalarType "int4")})
          "good",
        StringLit
          (Annotation{asrc = Just ("test6.sql", 10, 70),
                      atype = Just UnknownType, errs = [], stType = Nothing, catUpd = [],
                      fnProt = Nothing, infType = Just (ScalarType "text")})
          "london"]])
   Nothing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> allAnnotationsA = mode $ AllAnnotations {database = def
>                                        ,files = def &= typ "FILES" & args}
>                   &= text "reads each file, parses, type checks, then pretty \
>                           \prints the ast with all annotations "
>
> allAnnotations :: String -> [FilePath] -> IO ()
> allAnnotations db fns = do
>   cat <- either (error . show) return =<< readCatalog db
>   a1 <- concat `fmap` parseInputs P.parseSql fns
>   putStrLn $ ppExpr $ snd $ A.typeCheck cat a1

-------------------------------------------------------------------------------

> rttA = mode $ Rtt {database = def
>                   ,files = def &= typ "FILES" & args}
>                   &= text "run tests on sql"
>
> rtt :: String -> [FilePath] -> IO ()
> rtt db fns = do
>   rt <- roundTripTest astTransformer db fns
>   putStrLn $ rtShowBrief rt

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


> type ErrorIO e a = ErrorT e IO a

> runES :: ErrorIO String () -> IO ()
> runES f = runErrorT f >>= either putStrLn return

> etsr :: Show e => Either e a -> ErrorIO String a
> etsr = either (throwError . show) return

> parseInputs :: (Show a1, Error a1) =>
>                (FilePath -> String -> Either a1 a)
>             -> [FilePath]
>             -> IO [a]
> parseInputs p fns = do
>   as <- mapM pin fns
>   return $ either (error . show) id $ sequence as
>   where
>     pin fn = fmap (p fn) $ readInput fn

-------------------------------------------------------------------------------

main
====

>
> main :: IO ()
> main = do
>        cmd <- cmdArgs "HsSqlPppUtil, Copyright Jake Wheat 2010"
>                       [lexA, parseA, ppppA, pppA,
>                        parseExpressionA, typeCheckExpressionA,
>                        typeCheckA,allAnnotationsA, rttA]
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
>          Rtt db fns -> rtt db fns
>
> lexA, parseA, ppppA, pppA,
>   typeCheckA, parseExpressionA, typeCheckExpressionA,
>   allAnnotationsA, rttA :: Mode HsSqlPppUtil

