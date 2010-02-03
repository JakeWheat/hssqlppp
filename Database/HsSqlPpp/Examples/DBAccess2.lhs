Copyright 2010 Jake Wheat

More refined example wrapper generator, uses template haskell. See the
docs attached to sqlStmt function below

> {-# LANGUAGE TemplateHaskell #-}

> module Database.HsSqlPpp.Dbms.DBAccess2
>     (withConn
>     ,sqlStmt
>     ,IConnection) where

> import Language.Haskell.TH

> import Data.Maybe
> import Control.Applicative
> import Control.Monad.Error
> import Control.Monad
> import Control.Exception

> import Database.HDBC
> import qualified Database.HDBC.PostgreSQL as Pg

> import System.IO.Unsafe
> import Data.IORef

> import Database.HsSqlPpp.Dbms.WrapLib
> import qualified Database.HsSqlPpp.Ast.SqlTypes as Sql
> import Database.HsSqlPpp.Ast.Catalog
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Utils

> -- | template haskell fn to roughly do typesafe database access, pretty experimental atm
> --
> -- sketch is:
> --
> -- > $(sqlStmt connStr sqlStr)
> -- >
> -- > -- is transformed into
> -- >
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
> -- (as well as producing a working function which accesses a database)
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
>       cat <- getCat
>       tsl (getStatementType cat sqlStr)
>
>     getCat :: ErrorT String IO Catalog
>     getCat = do
>       -- bad code to avoid reading the catalog multiple times
>       c1 <- liftIO $ readIORef globalCachedCatalog
>       case c1 of
>         Just c -> return c
>         Nothing -> do
>                    c <- liftIO (readCatalogFromDatabase dbName) >>=
>                           (tsl . updateCatalog defaultCatalog)
>                    liftIO $ writeIORef globalCachedCatalog (Just c)
>                    return c
>
>     -- lambda which does [SqlValue] -> (T1, T2, ...)
>     mapTupleFromSql :: [Type] -> Q Exp
>     mapTupleFromSql outT = do
>       retNames <- getNNewNames "r" $ length outT
>       lamE [listP (map varP retNames)]
>         (tupE $ zipWith fromSqlIt retNames outT)
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

> -- | Simple wrapper so that all client code needs to do is import this file
> -- and use withConn and sqlStmt without importing HDBC, etc.

> withConn :: String -> (Pg.Connection -> IO c) -> IO c
> withConn cs = bracket (Pg.connectPostgreSQL cs) disconnect

================================================================================

evil hack to avoid reading the catalog from the database for each call
to sqlStmt. Atm this means that you can only read the catalog from one
database at compile time, but this should be an easy fix if too
limiting. TODO: make this change, in case the catalog ends up being
cached in ghci meaning if you change the database whilst developing in
emacs it will go wrong

> globalCachedCatalog :: IORef (Maybe Catalog)
> {-# NOINLINE globalCachedCatalog #-}
> globalCachedCatalog = unsafePerformIO (newIORef Nothing)

================================================================================

sql parsing and typechecking

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
>   where
>     sqlTypeToHaskell :: Sql.Type -> TypeQ
>     sqlTypeToHaskell t =
>       case t of
>         Sql.ScalarType "text" -> [t| Maybe String |]
>         Sql.ScalarType "int4" -> [t| Maybe Int |]
>         Sql.ScalarType "int8" -> [t| Maybe Int |]
>         Sql.ScalarType "bool" -> [t| Maybe Bool |]
>         Sql.DomainType _ -> [t| Maybe String |]
>         x -> error $ show x

================================================================================

TODO:
get error reporting at compile time working nicely:
can't connect to database
problem getting catalog -> report connection string used and source
  position
problem getting statement type: parse and type check issues, report
  source position
