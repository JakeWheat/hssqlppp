
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
> import Database.HsSqlPpp.Parsing.ParserInternal
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast hiding (Name)
> import qualified  Database.HsSqlPpp.Ast as A

public api: the quasiquote functions

> -- | quotes Statements
> sqlStmts :: QuasiQuoter
> sqlStmts = makeQQ $ parseStatements defaultParseFlags
>
> -- | quotes a single Statement
> sqlStmt :: QuasiQuoter
> sqlStmt = makeQQ parseOneStatement
>
> -- | quotes plpgsql Statements
> pgsqlStmts :: QuasiQuoter
> pgsqlStmts = makeQQ $ parsePlpgsql defaultParseFlags
>
> -- | quotes a plpgsql Statement
> pgsqlStmt :: QuasiQuoter
> pgsqlStmt = makeQQ parseOnePlpgsql

> -- | quotes a ScalarExpr
> sqlExpr :: QuasiQuoter
> sqlExpr = makeQQ $ parseScalarExpr defaultParseFlags

> -- | quotes a Name
> sqlName :: QuasiQuoter
> sqlName = makeQQ $ parseName defaultParseFlags

> -- | quotes a Name
> sqlNameComponent :: QuasiQuoter
> sqlNameComponent = makeQQ $ parseNameComponent defaultParseFlags


boilerplate utils to hook everything together

> type Parser e a = (String
>                    -> Maybe (Int,Int)
>                    -> String
>                    -> Either e a)
>
> makeQQ :: (Show e, Data a) =>
>           Parser e a -> QuasiQuoter
> makeQQ p = QuasiQuoter {quoteExp = parseExprExp p
>                        ,quotePat = parseExprPat p}

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
>     case parsePlpgsql defaultParseFlags f sp s of
>       Right [st] -> Right st
>       Right _ -> Left "got multiple statements"
>       Left e -> Left $ show e
>
> parseOneStatement :: Parser String Statement
> parseOneStatement f sp s =
>     case parseStatements defaultParseFlags f sp s of
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
