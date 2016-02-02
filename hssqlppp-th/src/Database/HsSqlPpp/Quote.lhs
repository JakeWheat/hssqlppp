> {- | A quasiquoter for SQL. Antiquoting is a bit inconsistent.
>
> Example:
>
> >
> > {-# LANGUAGE QuasiQuotes #-}
> > import Database.HsSqlPpp.Ast
> > import Database.HsSqlPpp.Quote
> > import Database.HsSqlPpp.Annotation
> >
> > test :: Statement
> > test = [$sqlStmt|
> >
> >   create table $n(tablename) (
> >    $m(varname) $n(typename)
> >   );
> >
> >         |]
> >   where
> >     tablename = [sqlName| my_table |]
> >     varname = [sqlNameComponent| my_field |]
> >     typename = [sqlName| text |]
> >
>
> See <http://jakewheat.github.com/hssqlppp/QuasiQuoteTests.html>
> for more simple examples
>
> The splices are:
>
> * $e(scalarexpression)
>
> * $s(string)
>
> * $t(triggerevent)
>
> * $s(statement)
>
> * $n(name)
>
> * $m(namecomponent)
>
> You can use $m() in a name context, and $n() or $m() in a scalar
> expression context. You can only use a single variable name in a
> splice atm.
>      -}

, and
<http://jakewheat.github.com/hssqlppp/source/examples/Database/HsSqlPpp/Examples/Extensions/>
for some example files which use quasiquotation to do ast
transformations which implement syntax extensions to sql
these files need fixing up before ready for public consumption


improvements to qq:
quasi quotes and antiquotes for?:
tablerefs
select lists
multiple statements

use haskell syntax inside antiquotes

>
> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Quote
>     (sqlStmts,sqlStmt,pgsqlStmts,pgsqlStmt,sqlExpr,sqlName,sqlNameComponent) where

> import Language.Haskell.TH.Quote
> import Language.Haskell.TH
> import Data.Generics
> import Data.List
>
> import qualified Database.HsSqlPpp.Parse as P
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Syntax hiding (Name)
> import qualified  Database.HsSqlPpp.Syntax as A
> --import qualified Data.Text as T
> import qualified Data.Text.Lazy as L
> --import Data.Data

> import Data.Text ()

> -- Control.Monad.Identity
> --import Text.Parsec

public api: the quasiquote functions

> -- | quotes Statements
> sqlStmts :: QuasiQuoter
> sqlStmts = makeQQ $ parseStatements P.defaultParseFlags
>
> -- | quotes a single Statement
> sqlStmt :: QuasiQuoter
> sqlStmt = makeQQ parseOneStatement
>
> -- | quotes plpgsql Statements
> pgsqlStmts :: QuasiQuoter
> pgsqlStmts = makeQQ $ parsePlpgsql P.defaultParseFlags
>
> -- | quotes a plpgsql Statement
> pgsqlStmt :: QuasiQuoter
> pgsqlStmt = makeQQ parseOnePlpgsql

> -- | quotes a ScalarExpr
> sqlExpr :: QuasiQuoter
> sqlExpr = makeQQ $ parseScalarExpr P.defaultParseFlags
>      {-QuasiQuoter {quoteExp = prs}
>   where
>     prs :: String -> Q Exp
>     prs s = either (fail . show) return (pse s)
>             >>= dataToExpQ (const Nothing)-}

> --pse :: String -> Either P.ParseErrorExtra ScalarExpr
> --pse = parseScalarExpr P.defaultParseFlags "" Nothing

ghc -Wall -threaded -rtsopts  -isrc:src-extra/catalogReader:src-extra/chaos:src-extra/devel-util:src-extra/docutil:src-extra/examples:src-extra/extensions:src-extra/h7c:src-extra/tests:src-extra/chaos/extensions:src-extra/utils temp.lhs

> -- | quotes a Name
> sqlName :: QuasiQuoter
> sqlName = makeQQ $ parseName P.defaultParseFlags

> -- | quotes a Name
> sqlNameComponent :: QuasiQuoter
> sqlNameComponent = makeQQ $ parseNameComponent P.defaultParseFlags


boilerplate utils to hook everything together

> type Parser e a = (String
>                    -> Maybe (Int,Int)
>                    -> String
>                    -> Either e a)
>
> makeQQ :: (Show e, Data a) =>
>           Parser e a -> QuasiQuoter
> makeQQ p = QuasiQuoter {quoteExp = parseExprExp p
>                        ,quotePat = parseExprPat p
>                        ,quoteType = error "quasi-quoter doesn't work for types"
>                        ,quoteDec = error "quasi-quoter doesn't work for declarations"}

hack for the text issue:

create parallel ast with text replaced with strings automatically
create conversion function to convert tree with text to tree with
strings automatically
pass this tree into dataToExpQ
then get the result, and convert the Exp type back to using text
not sure what needs to be done about which package the Exp refers to
maybe it will work?

> parseExprExp :: (Show e, Data a) =>
>                 Parser e a -> String -> Q Exp
> parseExprExp p s = parseSql' p s
>                    >>= dataToExpQ (const Nothing
>                                     `extQ` antiExpE
>                                     `extQ` antiStrE
>                                     `extQ` antiTriggerEventE
>                                     `extQ` antiStatementE
>                                     `extQ` antiNameE
>                                     `extQ` antiNameComponentE)

> parseExprPat :: (Show e, Data a) =>
>                 Parser e a ->  String -> Q Pat
> parseExprPat p s = parseSql' p s
>                    >>=  dataToPatQ (const Nothing
>                                     `extQ` antiExpP
>                                     `extQ` antiStrP
>                                     `extQ` antiTriggerEventP
>                                     `extQ` antiStatementP
>                                     `extQ` antiNameP
>                                     `extQ` antiNameComponentP
>                                     `extQ` annotToWildCard)
>

wrapper for all the different parsers which sets the source location
and converts left to fail

todo: error messages not coming out nicely from ghc when doing
fail.show.

> parseSql' :: (Data a, Show e) => Parser e a -> String -> Q a
> parseSql' p s = do
>     Loc fn _ _ (l,c) _ <- location
>     either (fail . show) return (p fn (Just (l,c)) s)

wrappers - the Parser module doesn't expose methods which parse
exactly one statement

> parseOnePlpgsql :: Parser String Statement
> parseOnePlpgsql f sp s =
>     case parsePlpgsql P.defaultParseFlags f sp s of
>       Right [st] -> Right st
>       Right _ -> Left "got multiple statements"
>       Left e -> Left $ show e
>
> parseOneStatement :: Parser String Statement
> parseOneStatement f sp s =
>     case parseStatements P.defaultParseFlags f sp s of
>       Right [st] -> Right st
>       Right _ -> Left "got multiple statements"
>       Left e -> Left $ show e

hack: replace the annotations in asts produced by parsing with
wildcards, if you don't do this then pattern matches generally don't
work since the source position annotations from the parser don't match
up. The source position annotations are still available so that e.g. a
function can pattern match against a statement then get the source
position from the matched statements.

> annotToWildCard :: Annotation -> Maybe PatQ
> annotToWildCard _ = Just $ return WildP

= individual antinode lookup functions

> antiExpE :: ScalarExpr -> Maybe ExpQ
> antiExpE v = fmap varE (antiExp v)
>
> antiExpP :: ScalarExpr -> Maybe PatQ
> antiExpP v = fmap varP $ antiExp v
>
> antiExp :: ScalarExpr -> Maybe Name
> antiExp (AntiScalarExpr v) = Just $ mkName v
> antiExp _ = Nothing


> antiNameE :: A.Name -> Maybe ExpQ
> antiNameE v = fmap varE (antiName v)
>
> antiNameP :: A.Name -> Maybe PatQ
> antiNameP v = fmap varP $ antiName v
>
> antiName :: A.Name -> Maybe Name
> antiName (AntiName v) = Just $ mkName v
> antiName _ = Nothing

> antiNameComponentE :: NameComponent -> Maybe ExpQ
> antiNameComponentE v = fmap varE (antiNameComponent v)
>
> antiNameComponentP :: NameComponent -> Maybe PatQ
> antiNameComponentP v = fmap varP $ antiNameComponent v
>
> antiNameComponent :: NameComponent -> Maybe Name
> antiNameComponent (AntiNameComponent v) = Just $ mkName v
> antiNameComponent _ = Nothing



> antiStatementE :: Statement -> Maybe ExpQ
> antiStatementE v = fmap varE (antiStatement v)
>
> antiStatementP :: Statement -> Maybe PatQ
> antiStatementP v = fmap varP $ antiStatement v
>
> antiStatement :: Statement -> Maybe Name
> antiStatement (AntiStatement v) = Just $ mkName v
> antiStatement _ = Nothing



antistatements not working ...
trying to replace a single antistatement node with multiple statement
nodes and my generics skills aren't up to the task.


> {-antiStatementE :: [Statement] -> Maybe ExpQ
> antiStatementE (AntiStatement v : tl) =
>    Just (listE (vref : conArgs))
>    where
>      conArgs = gmapQ (dataToExpQ (const Nothing
>                                     `extQ` antiExpE
>                                     -- `extQ` antiStrE
>                                     `extQ` antiTriggerEventE
>                                     `extQ` antiStatementE
>                                     `extQ` antiNameE
>                                     `extQ` antiNameComponentE)) tl
>      vref :: ExpQ
>      vref = varE $ mkName v
> antiStatementE _ = Nothing-}


> antiStrE :: String -> Maybe ExpQ
> antiStrE v = fmap varE $ antiStr v

> antiStrP :: String -> Maybe PatQ
> antiStrP v = fmap varP $ antiStr v

> antiStr :: String -> Maybe Name
> antiStr v =
>   fmap mkName $ getSpliceName v
>   where
>     getSpliceName s
>       | isPrefixOf "$s(" s && last s == ')' =
>           Just $ drop 3 $ init s
>       | otherwise = Nothing

>
> antiTriggerEventE :: TriggerEvent -> Maybe ExpQ
> antiTriggerEventE (AntiTriggerEvent v) = Just $ varE $ mkName v
> antiTriggerEventE _ = Nothing

> antiTriggerEventP :: TriggerEvent -> Maybe PatQ
> antiTriggerEventP (AntiTriggerEvent v) = Just $ varP $ mkName v
> antiTriggerEventP _ = Nothing


what needs to be done to support _ in pattern quasiquotes? -> I think
it's just adding a wildcard ctor to the appropriate ast types using
makeantinodes, and adding in lexing and parsing support - actually
using wildcards is now working with the annotation approach above

also, how to use haskell syntax in splices

----------------------------------

> parseStatements :: P.ParseFlags -- ^ parse options
>                 -> FilePath -- ^ filename to use in errors
>                 -> Maybe (Int,Int) -- ^ set the line number and column number
>                                    -- of the first char in the source (used in annotation)
>                 -> String -- ^ a string containing the sql to parse
>                 -> Either P.ParseErrorExtra [Statement]
> parseStatements p f s src = P.parseStatements p f s (L.pack src)

> {-parseQueryExpr :: P.ParseFlags -- ^ parse options
>                -> FilePath -- ^ filename to use in errors
>                -> Maybe (Int,Int) -- ^ set the line number and column number
>                -> String -- ^ a string containing the sql to parse
>                -> Either P.ParseErrorExtra QueryExpr
> parseQueryExpr p f s src = P.parseQueryExpr p f s (L.pack src)-}

> parseScalarExpr :: P.ParseFlags -- ^ parse options
>                 -> FilePath -- ^ filename to use in errors
>                 -> Maybe (Int,Int) -- ^ set the line number and column number
>                 -> String -- ^ a string containing the sql to parse
>                 -> Either P.ParseErrorExtra ScalarExpr
> parseScalarExpr p f s src = P.parseScalarExpr p f s (L.pack src)

> parseName :: P.ParseFlags
>           -> FilePath
>           -> Maybe (Int,Int)
>           -> String
>           -> Either P.ParseErrorExtra A.Name
> parseName p f s src = P.parseName p f s (L.pack src)

> parseNameComponent :: P.ParseFlags
>                    -> FilePath
>                    -> Maybe (Int,Int)
>                    -> String
>                    -> Either P.ParseErrorExtra NameComponent
> parseNameComponent p f s src = P.parseNameComponent p f s (L.pack src)

> parsePlpgsql :: P.ParseFlags -- ^ parse options
>              -> FilePath -- ^ filename to use in errors
>              -> Maybe (Int,Int) -- ^ set the line number and column number
>              -> String
>              -> Either P.ParseErrorExtra [Statement]
> parsePlpgsql p f s src = P.parseProcSQL p f s (L.pack src)
