Copyright 2010 Jake Wheat

Do type safe database access using template haskell and hlists. Limitation
is that you have to edit this file to add the field definitions and possibly
exports. Suggested use is to copy this file into your own project

> {-# LANGUAGE TemplateHaskell,EmptyDataDecls,DeriveDataTypeable #-}

> module Database.HsSqlPpp.Dbms.DBAccess3
>     (withConn
>     ,sqlStmt
>     ,IConnection

If you are using this then export the proxy types and values here,
useful if you need to write down type signatures, exporting them isn't
neccessarily neccessary otherwise.

>     ,ptype,allegiance,tag,x,y
>     ,Ptype,Allegiance,Tag,X,Y
>     ,get_turn_number,current_wizard,colour,sprite
>     ,Get_Turn_Number,Current_Wizard,Colour,Sprite


>     ) where

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

If you are using this file, this is the bit where you add your own
fields, just follow the pattern. deriving Typeable seems to allow
showing the returned hlists, don't know if there's a better way.
TODO: add some template code to remove the boilerplate, apparently
there is some in the hlist darcs repo.

> data Ptype deriving Typeable
> ptype :: Proxy Ptype
> ptype = proxy::Proxy Ptype
> data Allegiance deriving Typeable
> allegiance :: Proxy Allegiance
> allegiance = proxy::Proxy Allegiance
> data Tag deriving Typeable
> tag :: Proxy Tag
> tag = proxy::Proxy Tag
> data X deriving Typeable
> x :: Proxy X
> x = proxy::Proxy X
> data Y deriving Typeable
> y :: Proxy Y
> y = proxy::Proxy Y

> data Get_Turn_Number deriving Typeable
> get_turn_number :: Proxy Get_Turn_Number
> get_turn_number = proxy::Proxy Get_Turn_Number
> data Current_Wizard deriving Typeable
> current_wizard :: Proxy Current_Wizard
> current_wizard = proxy::Proxy Current_Wizard
> data Colour deriving Typeable
> colour :: Proxy Colour
> colour = proxy::Proxy Colour
> data Sprite deriving Typeable
> sprite :: Proxy Sprite
> sprite    = proxy::Proxy Sprite

================================================================================

> -- | template haskell fn to roughly do typesafe database access with
> -- hlists, pretty experimental atm
> --
> -- sketch is:
> --
> -- > $(sqlStmt connStr sqlStr)
> -- >
> -- > -- is transformed into
> -- >
> -- >
> -- >  \conn a_0 a_1 ... ->
> -- >      selectRelation conn sqlStr [toSql (a_0::Ti0)
> -- >                                 ,toSql (a_1::Ti1), ... ] >>=
> -- >      return . map (\ [r_0, r_1, ...] ->
> -- >        f1 .=. fromSql (r_0::To0) .*.
> -- >        f2 .=. fromSql (r_1::To1) .*.
> -- >        ... .*.
> -- >        emptyRecord)
> -- >
> -- where the names f1, f2 are the attribute names from the database,
> -- the types Ti[n] are the types of the placeholders in the sql
> -- string, and the types To[n] are the types of the attributes in
> -- the returned relation. To work around a limitation in the
> -- implementation, these names must be in scope in this file, so to
> -- use this in your own projects you need to copy the source and
> -- then add the field defitions in as needed.
> --
> -- example usage:
> --
> -- > pieces_at_pos = $(sqlStmt connStr "select * from pieces where x = ? and y = ?;")
> --
> -- might infer the type:
> --
> -- >
> -- >   pieces_at_pos :: IConnection conn =>
> -- >                    conn
> -- >                 -> Maybe Int
> -- >                 -> Maybe Int
> -- >                 -> IO [Record (HCons (LVPair (Proxy Ptype)
> -- >                                              (Maybe String))
> -- >                               (HCons (LVPair (Proxy Allegiance)
> -- >                                              (Maybe String))
> -- >                               (HCons (LVPair (Proxy Tag)
> -- >                                              (Maybe Int))
> -- >                               (HCons (LVPair (Proxy X)
> -- >                                              (Maybe Int))
> -- >                               (HCons (LVPair (Proxy Y)
> -- >                                              (Maybe Int))
> -- >                                HNil)))))]
> --
> -- (as well as producing a working function which accesses a database). Currently, I get
> --
> -- >
> -- > Test3.lhs:16:12:
> -- >     Ambiguous type variable `conn' in the constraint:
> -- >       `IConnection conn'
> -- >         arising from a use of `pieces' at Test3.lhs:16:12-22
> -- >     Probable fix: add a type signature that fixes these type variable(s)
> -- >
> -- which can be worked around by adding a type signature like
> -- >
> -- > pieces_at_pos :: IConnection conn =>
> -- >                  conn
> -- >               -> a
> -- >               -> b
> -- >               -> IO c
> -- >
> -- and then ghc will complain and tell you what a,b,c should be (make
> -- sure you match the number of arguments after conn to the number
> -- of ? placeholders in the sql string).

> sqlStmt :: String -> String -> Q Exp
> sqlStmt dbName sqlStr = do
>   (StatementHaskellType inA outA) <- liftStType
>   let cnName = mkName "cn"
>   argNames <- getNNewNames "a" $ length inA
>   lamE (map varP (cnName : argNames))
>     [| selectRelation $(varE cnName) sqlStr
>                       $(ListE <$> zipWithM toSqlIt argNames inA) >>=
>        return . map $(mapHlistFromSql outA)|]
>
>   where
>     -- th code gen utils
>     mapHlistFromSql :: [(String,Type)] -> Q Exp
>     mapHlistFromSql outA = do
>       retNames <- getNNewNames "r" $ length outA
>       l1 <- mapM (\(a,b,c) -> toHlistField a b c) $ zipWith (\(a,b) c -> (a,b,c)) outA retNames
>       lamE [listP (map varP retNames)] $ foldHlist l1
>
>     toHlistField :: String -> Type -> Name -> Q Exp
>     toHlistField f t v = [| $(varE $ mkName f) .=. $(fromSqlIt v t) |]
>
>     foldHlist :: [Exp] -> Q Exp
>     foldHlist (e:e1) = [| $(return e) .*. $(foldHlist e1) |]
>     foldHlist [] = [| emptyRecord |]
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
>
>     -- statement type stuff
>     liftStType :: Q StatementHaskellType
>     liftStType = runIO stType >>= either (error . show) toH
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