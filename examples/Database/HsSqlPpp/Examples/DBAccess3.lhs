Copyright 2010 Jake Wheat

Do type safe database access using template haskell and
hlists. Limitation is that you have to edit this file to add the field
definitions and possibly exports. Suggested use is to copy this file
into your own project and edit it there.

> {-# LANGUAGE TemplateHaskell,EmptyDataDecls,DeriveDataTypeable,
>   TupleSections #-}

> module Database.HsSqlPpp.Examples.DBAccess3
>     (withConn
>     ,sqlStmt
>     ,IConnection

Export the field proxies for your project here.  If you want to write
down type signatures of the result hlists then export the proxy types
here also, exporting them isn't neccessarily neccessary otherwise.

>     ,ptype,allegiance,tag,x,y
>     ,get_turn_number,current_wizard,colour,sprite
>     ,Ptype,Allegiance,Tag,X,Y
>     ,Get_turn_number,Current_wizard,Colour,Sprite


>     ) where

> import Language.Haskell.TH

> import Control.Applicative
> import Control.Monad.Error
> import Control.Exception

> import Database.HDBC
> import qualified Database.HDBC.PostgreSQL as Pg

> import Data.HList
> import Data.HList.Label1 ()
> import Data.HList.TypeEqGeneric1 ()
> import Data.HList.TypeCastGeneric1 ()
> import Database.HsSqlPpp.Dbms.MakeLabels

> import System.IO.Unsafe
> --import Data.IORef

> import Database.HsSqlPpp.Dbms.WrapLib
> import qualified Database.HsSqlPpp.Ast.SqlTypes as Sql
> import Database.HsSqlPpp.Ast.Catalog
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Utils


================================================================================

If you are using this file, this is the bit where you add your own
fields.

> $(makeLabels ["ptype"
>              ,"allegiance"
>              ,"tag"
>              ,"x"
>              ,"y"
>              ,"get_turn_number"
>              ,"current_wizard"
>              ,"colour"
>              ,"sprite"])

================================================================================

> -- | template haskell fn to roughly do typesafe database access with
> -- hlists, pretty experimental atm
> --
> -- sketch is:
> --
> -- >
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
> --
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
> -- >
> -- > pieces_at_pos = $(sqlStmt connStr "select * from pieces where x = ? and y = ?;")
> -- >
> --
> -- might (!) infer the type:
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
> -- >
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
> --
> -- which can be worked around by adding a type signature like
> --
> -- >
> -- > pieces_at_pos :: IConnection conn =>
> -- >                  conn
> -- >               -> a
> -- >               -> b
> -- >               -> IO c
> -- >
> --
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
>     getNNewNames i n = replicateM n $ newName i
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
>     [st] <- tsl $ parseSql "" sql
>     ast <- typeCheckPS cat st
>     let [Just a] = getTopLevelInfos [ast]
>     return a

convert sql statement type to equivalent with sql types replaced with
haskell equivalents - HDBC knows how to convert the actual values using
toSql and fromSql as long as we add in the appropriate casts

> data StatementHaskellType = StatementHaskellType [Type] [(String,Type)]

> toH :: StatementType -> Q StatementHaskellType
> toH (StatementType i o) =
>   StatementHaskellType
>     <$> mapM sqlTypeToHaskell i
>     <*> forM o (\(a,b) -> (a,) <$> sqlTypeToHaskell b)
>   where
>     sqlTypeToHaskell :: Sql.Type -> TypeQ
>     sqlTypeToHaskell t = do
>       let t' = case t of
>                       Sql.ScalarType "text" -> [t| String |]
>                       Sql.ScalarType "int4" -> [t| Int |]
>                       Sql.ScalarType "int8" -> [t| Int |]
>                       Sql.ScalarType "bool" -> [t| Bool |]
>                       Sql.DomainType _ -> [t| String |]
>                       z -> error $ show z
>       [t| Maybe $t' |]

> instance Applicative Q where
>     pure = return
>     (<*>) = ap

> {-

attempt to create shorthand for hlist type signatures, hit a bug in ghc

-- > makeHListType :: [(TypeQ,TypeQ)] -> Q Type

AppT (AppT (AppT (AppT (AppT (TupleT 5) (AppT (AppT (TupleT 2) (ConT Database.HsSqlPpp.Dbms.DBAccess3.Ptype)) (ConT GHC.Base.String))) (AppT (AppT (TupleT 2) (ConT Database.HsSqlPpp.Dbms.DBAccess3.Allegiance)) (ConT GHC.Base.String))) (AppT (AppT (TupleT 2) (ConT Database.HsSqlPpp.Dbms.DBAccess3.Tag)) (ConT GHC.Types.Int))) (AppT (AppT (TupleT 2) (ConT Database.HsSqlPpp.Dbms.DBAccess3.X)) (ConT GHC.Types.Int))) (AppT (AppT (TupleT 2) (ConT Database.HsSqlPpp.Dbms.DBAccess3.Y)) (ConT GHC.Types.Int))

> hlt :: Q Type -> Q Type
> hlt a = do
>   z <- a
>   runIO $ print z
>   pt <- [t|Ptype|]
>   al <- [t|Allegiance|]
>   tg <- [t|Tag|]
>   xt <- [t|X|]
>   yt <- [t|Y|]
>   str <- [t|String|]
>   int <- [t|Int|]
>   makeHListType [(pt,str)
>                 ,(al,str)
>                 ,(tg,int)
>                 ,(xt,int)
>                 ,(yt,int)]
>   {-[t| Record (HCons (LVPair (Proxy Ptype)
>                                              (Maybe String))
>                               (HCons (LVPair (Proxy Allegiance)
>                                              (Maybe String))
>                               (HCons (LVPair (Proxy Tag)
>                                              (Maybe Int))
>                               (HCons (LVPair (Proxy X)
>                                              (Maybe Int))
>                               (HCons (LVPair (Proxy Y)
>                                              (Maybe Int))
>                                HNil)))))
>                  |]-}

> makeHListType :: [(Type,Type)] -> Q Type
> makeHListType x =
>   [t| Record $(foldTi x)|]
>   where
>     foldTi ((f,t):xs) = [t|HCons (LVPair (Proxy $f) $t) $(foldTi xs)|]
>     foldTi [] = [t|HNil|]

>                   {-[t| Record (HCons (LVPair (Proxy Ptype)
>                                              (Maybe String))
>                               (HCons (LVPair (Proxy Allegiance)
>                                              (Maybe String))
>                               (HCons (LVPair (Proxy Tag)
>                                              (Maybe Int))
>                               (HCons (LVPair (Proxy X)
>                                              (Maybe Int))
>                               (HCons (LVPair (Proxy Y)
>                                              (Maybe Int))
>                                HNil)))))
>                  |]-}
> -}

================================================================================

TODO:
get error reporting at compile time working nicely:
can't connect to database
problem getting catalog -> report connection string used and source
  position
problem getting statement type: parse and type check issues, report
  source position

turn this file into a toolkit of bits, which can import so can use
without having to copy then edit this file

figure out a better way of handling the proxies
