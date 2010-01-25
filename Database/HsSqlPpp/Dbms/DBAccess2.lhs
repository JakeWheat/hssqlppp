Copyright 2010 Jake Wheat

More refined example wrapper generator. The idea is to start with
$(runStmt connStr sqlStr)
and return a fn :: (IConnection conn) => conn -> Arg1 -> Arg2 -> ...
                   -> IO hlist<fieldname, fieldtype>

at some point the type checker will be able to determine whether the
output fields are maybes or not, we output them all as maybes at the
moment. For input args, this may not be possible, but we assume all
input args can never be null for now.

Can't work out how to get the hlist aspect working, so just outputs
lists of tuples at the moment, so currenly does

fn :: (IConnection conn) => conn -> Arg1 -> Arg2 -> ...
                   -> IO [(r1,r2,...)]

> {-# LANGUAGE TemplateHaskell,QuasiQuotes,EmptyDataDecls,ScopedTypeVariables,RankNTypes,FlexibleContexts #-}

> module Database.HsSqlPpp.Dbms.DBAccess2 (withConn, sqlStmt,IConnection) where

> import Language.Haskell.TH

> import Data.Maybe

> import Control.Applicative
> import Control.Monad.Error
> import Control.Monad
> import Control.Exception



> import Database.HDBC
> import qualified Database.HDBC.PostgreSQL as Pg

> import Database.HsSqlPpp.Dbms.WrapLib
> import qualified Database.HsSqlPpp.Ast.SqlTypes as Sql
> import Database.HsSqlPpp.Ast.Catalog
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Utils

> -- | Simple wrapper so that all client code needs to do is import this file
> -- and use withConn and sqlStmt without importing HDBC, etc.

> withConn :: String -> (Pg.Connection -> IO c) -> IO c
> withConn cs = bracket (Pg.connectPostgreSQL cs) disconnect


> -- | template haskell fn to roughly do typesafe database access, pretty experimental atm
> --
> -- sketch is:
> --
> -- > sqlStmt connStr sqlStr ->
> -- >  \conn a_0 a_0 ... ->
> -- >         selectRelation conn sqlStr [toSql (a_0::Ti0)
> -- >                                    ,toSql (a_1::Ti1), ... ] >>=
> -- >         return . map (\ [r_0, r_1, ...] ->
> -- >           (fromSql r_0::To0
> -- >           ,fromSql r_1::To1
> -- >           ,...)
> --
> -- example usage:
> --
> -- > pieces_at_pos = $(sqlStmt connStr "select * from pieces where x = ? and y = ?;")
> --
> -- will come close enough to inferring the type:
> --
> -- > pieces_at_pos :: IConnection conn =>
> -- >                 conn
> -- >              -> Maybe Int
> -- >              -> Maybe Int
> -- >              -> IO [(Maybe String, Maybe String, Maybe Int, Maybe Int, Maybe Int)]
> --
> sqlStmt :: String -> String -> Q Exp
> sqlStmt dbName sqlStr = do
>   (StatementHaskellType inA outA) <- liftStType
>   let cnName = mkName "cn"
>   argNames <- getNNewNames "a" $ length inA
>   lamE (map varP (cnName : argNames))
>     [| selectRelation $(varE cnName) sqlStr
>                       $(ListE <$> zipWithM toSqlIt argNames inA) >>=
>        return . map $(mapTupleFromSql $ map snd outA)|]
>
>   where
>     liftStType :: Q StatementHaskellType
>     liftStType = runIO stType >>= (either (error . show) toH)
>
>     stType :: IO (Either String StatementType)
>     stType = runErrorT $ do
>       -- this is very slow, rereads the catalog from the database for
>       -- each call to sqlStmt
>       cat <- liftIO (readCatalogFromDatabase dbName) >>=
>              (tsl . updateCatalog defaultCatalog)
>       tsl (getStatementType cat sqlStr)
>     -- mapper is the lambda which takes a list of sqlvalues
>     -- and produces a tuple using fromSql n :: t
>     mapTupleFromSql :: [Type] -> Q Exp
>     mapTupleFromSql outT = do
>       retNames <- getNNewNames "r" $ length outT
>       lamE [listP (map varP retNames)]
>         (tupE $ zipWith fromSqlIt retNames outT)
>
>
>     toSqlIt :: Name -> Type -> Q Exp
>     toSqlIt n t = [| toSql $(castName n t)|]
>
>     fromSqlIt :: Name -> Type -> Q Exp
>     fromSqlIt n t = do
>       n1 <- [| fromSql $(varE n) |]
>       cast n1 t
>
>     cast :: Exp -> Type -> Q Exp
>     cast e = return . SigE e
>
>     castName :: Name -> Type -> Q Exp
>     castName = cast . VarE
>
>     getNNewNames :: String -> Int -> Q [Name]
>     getNNewNames i n = forM [1..n] $ const $ newName i

================================================================================

parsing and typechecking

get the input and output types for a parameterized sql statement:

> getStatementType :: Catalog -> String -> Either String StatementType
> getStatementType cat sql = do
>     ast <- tsl $ parseSql "" sql
>     let (_,aast) = typeCheck cat ast
>     let a = getTopLevelInfos aast
>     return $ fromJust $ head a

convert sql statement type to equivalent with sql types replaced with
haskell equivalents - HDBC knows how to convert the actual values using
toSql and fromSql as long as we add in the appropriate casts

> data StatementHaskellType = StatementHaskellType [Type] [(String,Type)]

> toH :: StatementType -> Q StatementHaskellType
> toH (StatementType i o) = do
>   ih <- mapM sqlTypeToHaskell i
>   oht <- mapM (sqlTypeToHaskell . snd) o
>   return $ StatementHaskellType ih $ zip (map fst o) oht


return the equivalent haskell type for a sql type as a string

> sqlTypeToHaskell :: Sql.Type -> TypeQ
> sqlTypeToHaskell t =
>     case t of
>        Sql.ScalarType "text" -> [t| Maybe String |]
>        Sql.ScalarType "int4" -> [t| Maybe Int |]
>        Sql.ScalarType "int8" -> [t| Maybe Int |]
>        Sql.ScalarType "bool" -> [t| Maybe Bool |]
>        Sql.DomainType _ -> [t| Maybe String |]
>        x -> error $ show x

================================================================================

TODO:
work out how to use hlist
cache the database catalog once per compile?
get error reporting at compile time working nicely:
can't connect to database
problem getting catalog
problem getting statement type: parse and type check issues

================================================================================

This is the kind of stuff that want to produce with hlist. Problem is,
how to create the proxies for the field names:

> {-
> data Ptype;   ptype :: Proxy Ptype ; ptype    = proxy::Proxy Ptype
> data Allegiance; allegiance :: Proxy Allegiance ; allegiance = proxy::Proxy Allegiance
> data Tag; tag :: Proxy Tag ; tag = proxy::Proxy Tag
> data X; x :: Proxy X ; x = proxy::Proxy X
> data Y; y :: Proxy Y ; y = proxy::Proxy Y


> sqlStmt :: String -> String -> Q Exp
> sqlStmt connStr sqlStr = do
>   runQ [| \conn ->
>           selectRelation conn sqlStr [] >>=
>           return . map (\ [a0, a1, a2, a3, a4] ->
>               ptype .=. fromSql a0 .*.
>               allegiance .=. fromSql a1 .*.
>               tag .=. fromSql a2 .*.
>               x .=. fromSql a3 .*.
>               y .=. fromSql a4 .*.
>               emptyRecord)
>         |]
> -}