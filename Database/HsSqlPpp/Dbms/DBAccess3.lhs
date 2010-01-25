Copyright 2010 Jake Wheat

Attempt to use hlists in template haskell db access. The idea is that
you copy this file into your project and add the hlist proxies by hand
to your copy of this file. This is slightly annoying, but if you miss
any the compiler will complain nicely, and once you set of queries
settles down you won't need to keep editing. Crucially, the
compile-time type safety of sqlstmt themselves aren't affected by this
editing, the only manual part is adding the proxy definitions.

> {-# LANGUAGE TemplateHaskell,EmptyDataDecls #-}

> module Database.HsSqlPpp.Dbms.DBAccess3
>     (withConn
>     ,sqlStmt
>     ,IConnection) where

> import Language.Haskell.TH

> import Data.Maybe
> import Control.Applicative
> import Control.Monad.Error
> --import Control.Monad
> import Control.Exception

> import Database.HDBC
> import qualified Database.HDBC.PostgreSQL as Pg

> import Data.HList
> import Data.HList.Label4 ()
> import Data.HList.TypeEqGeneric1 ()
> import Data.HList.TypeCastGeneric1 ()

> import System.IO.Unsafe
> import Data.IORef

> import Database.HsSqlPpp.Dbms.WrapLib
> import qualified Database.HsSqlPpp.Ast.SqlTypes as Sql
> import Database.HsSqlPpp.Ast.Catalog
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Utils


================================================================================

> data Ptype;   ptype :: Proxy Ptype ; ptype    = proxy::Proxy Ptype
> data Allegiance; allegiance :: Proxy Allegiance ; allegiance = proxy::Proxy Allegiance
> data Tag; tag :: Proxy Tag ; tag = proxy::Proxy Tag
> data X; x :: Proxy X ; x = proxy::Proxy X
> data Y; y :: Proxy Y ; y = proxy::Proxy Y

================================================================================






> -- | template haskell fn to roughly do typesafe database access, pretty experimental atm
> --
> -- sketch is:
> --
> -- > $(sqlStmt connStr sqlStr)
> -- >
> -- > -- is transformed into
> -- >
> -- >  \conn a_0 a_1 ... ->
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

> test = do
>   runQ [| \conn ->
>           selectRelation conn "x" [] >>=
>           return . map (\ [a0, a1, a2, a3, a4] ->
>               ptype .=. fromSql a0 .*.
>               allegiance .=. fromSql a1 .*.
>               tag .=. fromSql a2 .*.
>               x .=. fromSql a3 .*.
>               y .=. fromSql a4 .*.
>               emptyRecord)
>         |] >>= print
>   let list = [("ptype", [t| Int |], mkName "a0")
>              ,("allegiance", [t| String |], mkName "a0")]
>   l2 <- runQ [| $(do
>                   l1 <- mapM (\(a,b,c) -> toHlistField a b c) list
>                   er1 <- er
>                   let l2 = l1 ++ [er1]
>                   mt1 <- mt
>                   foldM (\a b -> [| $(return a) .*. $(return b) |]) mt1 l2)
>               |]
>   print l2
>   putStrLn $ pprint l2
>   --runQ l2 >>= print
>   --mapM_ (\l -> runQ l >>= print) l1
>   return ()

>   where
>     mt :: ExpQ
>     mt = [| [] |]

>     er :: ExpQ
>     er = [| emptyRecord |]

>     toHlistField :: String -> TypeQ -> Name -> Q Exp
>     toHlistField f t v = do
>       t' <- t
>       [| $(varE $ mkName f) .=. $(fromSqlIt v t') |]

>     fromSqlIt :: Name -> Type -> Q Exp
>     fromSqlIt n t = do
>       n1 <- [| fromSql $(varE n) |]
>       casti n1 t
>
>     casti :: Exp -> Type -> Q Exp
>     casti e = return . SigE e
>
>     castName :: Name -> Type -> Q Exp
>     castName = casti . VarE


> sqlStmt :: String -> String -> Q Exp
> sqlStmt dbName sqlStr = do
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
>   (StatementHaskellType inA outA) <- liftStType
>   let cnName = mkName "cn"
>   argNames <- getNNewNames "a" $ length inA
>   lamE (map varP (cnName : argNames))
>     [| selectRelation $(varE cnName) sqlStr
>                       $(ListE <$> zipWithM toSqlIt argNames inA) >>=
>        return . map $(mapHlistFromSql outA)|]
>
>   where

>     toHlistField :: String -> Type -> Name -> Q Exp
>     toHlistField f t v = [| $(varE $ mkName f) .=. $(fromSqlIt v t) |]

>     mapHlistFromSql :: [(String,Type)] -> Q Exp
>     mapHlistFromSql outA = do
>       let outT = map snd outA
>       retNames <- getNNewNames "r" $ length outT
>       lamE [listP (map varP retNames)]
>         (tupE $ zipWith fromSqlIt retNames outT)
 
>     mapTupleFromSql :: [Type] -> Q Exp
>     mapTupleFromSql outT = do
>       retNames <- getNNewNames "r" $ length outT
>       lamE [listP (map varP retNames)]
>         (tupE $ zipWith fromSqlIt retNames outT)


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
>     {-mapTupleFromSql :: [Type] -> Q Exp
>     mapTupleFromSql outT = do
>       retNames <- getNNewNames "r" $ length outT
>       lamE [listP (map varP retNames)]
>         (tupE $ zipWith fromSqlIt retNames outT)-}
>
>     toSqlIt :: Name -> Type -> Q Exp
>     toSqlIt n t = [| toSql $(castName n t)|]
>
>     fromSqlIt :: Name -> Type -> Q Exp
>     fromSqlIt n t = do
>       n1 <- [| fromSql $(varE n) |]
>       casti n1 t
>
>     casti :: Exp -> Type -> Q Exp
>     casti e = return . SigE e
>
>     castName :: Name -> Type -> Q Exp
>     castName = casti . VarE
>
>     getNNewNames :: String -> Int -> Q [Name]
>     getNNewNames i n = forM [1..n] $ const $ newName i


> sqlStmt1 :: String -> String -> Q Exp
> sqlStmt1 dbName sqlStr = do
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
>       casti n1 t
>
>     casti :: Exp -> Type -> Q Exp
>     casti e = return . SigE e
>
>     castName :: Name -> Type -> Q Exp
>     castName = casti . VarE
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
>         z -> error $ show z

================================================================================

TODO:
work out how to use hlist
cache the database catalog once per compile?
get error reporting at compile time working nicely:
can't connect to database
problem getting catalog -> report connection string used and source
  position
problem getting statement type: parse and type check issues, report
  source position

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