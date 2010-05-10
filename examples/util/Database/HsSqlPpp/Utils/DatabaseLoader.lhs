
This code is an example to demonstrate loading a sql file, parsing it,
running a transform on the ast, then loading the result straight into
PostgreSQL.

> module Database.HsSqlPpp.Utils.DatabaseLoader
>     (loadAst
>     ,loadAstN
>     ) where
>
> import Debug.Trace
> import qualified Database.HDBC.PostgreSQL as Pg
>
> import Database.HsSqlPpp.PrettyPrinter
> import Database.HsSqlPpp.Ast as Ast
> import Database.HsSqlPpp.Utils.DbmsCommon
> import Database.HsSqlPpp.Utils.PQH

Small hack to use pg lib to do copy from stdin

> data HackStatement = Regular Statement
>                    | CopyHack Statement Statement
>                      deriving Show
>
> hackStatements :: [Statement] -> [HackStatement]
> hackStatements (st1@(Copy _ _ _ Stdin) :
>                 st2@(CopyData _ _) :
>                 sts) =
>   CopyHack st1 st2 : hackStatements sts
> hackStatements (st : sts) =
>   Regular st : hackStatements sts
> hackStatements [] = []

The main routine, which takes an AST and loads it into a database using HDBC.

> loadAst :: Pg.Connection -> [Statement] -> IO ()
> loadAst c ast =
>   {-withConn ("dbname=" ++ dbName) $ \conn ->
>     withTransaction conn $ \c1 -> do-}
>        mapM_ (loadStatement c) $ hackStatements ast
>   where
>     loadStatement conn (Regular st) =
>       runSqlCommand conn $ printSql [st]
>                            {-let a = printSql [st]
>                            in trace a $ a -}
>     loadStatement conn (CopyHack cpSt (CopyData _ s)) = do
>       let c1 = connToPqConn conn
>       r <- exec c1 $ printSql [cpSt]
>       r2 <- putCopyStringData c1 (either (error . show) id r) s
>       either (error . show) (const $ return ()) r2
>     loadStatement _ x = error $ "got bad copy hack: " ++ show x

> loadAstN :: String -> [Statement] -> IO ()
> loadAstN dbName ast =
>   withConn ("dbname=" ++ dbName)
>     $ flip withTransaction
>       $ flip loadAst ast

todo: change the hack so that we can create a hdbc-postgresql
connection out of a PGconn, this will be much less invasive to
hdbc-postgresql

> connToPqConn :: Pg.Connection -> PGconn
> connToPqConn = makePGConn . Pg.pgConn

diff for hdbc-postgresql-2.2.3.1 to add the ability to get the
PGconn ptr out of a hdbc-postgresql connection

diff -ru HDBC-postgresql-2.2.3.1/Database/HDBC/PostgreSQL/Connection.hsc HDBC-postgresql-2.2.3.1-mangled//Database/HDBC/PostgreSQL/Connection.hsc
--- HDBC-postgresql-2.2.3.1/Database/HDBC/PostgreSQL/Connection.hsc	2010-05-06 20:14:54.000000000 +0100
+++ HDBC-postgresql-2.2.3.1-mangled//Database/HDBC/PostgreSQL/Connection.hsc	2010-05-06 20:03:09.000000000 +0100
@@ -20,7 +20,7 @@
 -}
 
 module Database.HDBC.PostgreSQL.Connection
-	(connectPostgreSQL, Impl.Connection())
+	(connectPostgreSQL, Impl.Connection(), Impl.pgConn)
  where
 
 import Database.HDBC
@@ -55,13 +55,14 @@
             wrappedptr <- wrapconn ptr nullPtr
             fptr <- newForeignPtr pqfinishptr wrappedptr
             case status of
-                     #{const CONNECTION_OK} -> mkConn args fptr
+                     #{const CONNECTION_OK} -> mkConn args fptr (castPtr ptr)
+
                      _ -> raiseError "connectPostgreSQL" status ptr
 
 -- FIXME: environment vars may have changed, should use pgsql enquiries
 -- for clone.
-mkConn :: String -> Conn -> IO Impl.Connection
-mkConn args conn = withConn conn $
+mkConn :: String -> Conn -> Ptr () -> IO Impl.Connection
+mkConn args conn c1 = withConn conn $
   \cconn -> 
     do children <- newMVar []
        begin_transaction conn children
@@ -83,7 +84,9 @@
                             Impl.dbServerVer = show serverver,
                             Impl.dbTransactionSupport = True,
                             Impl.getTables = fgetTables conn children,
-                            Impl.describeTable = fdescribeTable conn children}
+                            Impl.describeTable = fdescribeTable conn children,
+                            Impl.pgConn = c1
+                   }
        quickQuery rconn "SET client_encoding TO utf8;" []
        return rconn
 
diff -ru HDBC-postgresql-2.2.3.1/Database/HDBC/PostgreSQL/ConnectionImpl.hs HDBC-postgresql-2.2.3.1-mangled//Database/HDBC/PostgreSQL/ConnectionImpl.hs
--- HDBC-postgresql-2.2.3.1/Database/HDBC/PostgreSQL/ConnectionImpl.hs	2010-05-06 20:14:54.000000000 +0100
+++ HDBC-postgresql-2.2.3.1-mangled//Database/HDBC/PostgreSQL/ConnectionImpl.hs	2010-05-06 20:01:40.000000000 +0100
@@ -20,6 +20,7 @@
 
 import qualified Database.HDBC.Types as Types
 import Database.HDBC.ColTypes as ColTypes
+import Foreign
 
 data Connection = 
     Connection {
@@ -37,7 +38,8 @@
                 dbServerVer :: String,
                 dbTransactionSupport :: Bool,
                 getTables :: IO [String],
-                describeTable :: String -> IO [(String, ColTypes.SqlColDesc)]
+                describeTable :: String -> IO [(String, ColTypes.SqlColDesc)],
+                pgConn :: Ptr ()
                }
 
 instance Types.IConnection Connection where
diff -ru HDBC-postgresql-2.2.3.1/Database/HDBC/PostgreSQL.hs HDBC-postgresql-2.2.3.1-mangled//Database/HDBC/PostgreSQL.hs
--- HDBC-postgresql-2.2.3.1/Database/HDBC/PostgreSQL.hs	2010-05-06 20:14:54.000000000 +0100
+++ HDBC-postgresql-2.2.3.1-mangled//Database/HDBC/PostgreSQL.hs	2010-05-06 20:02:45.000000000 +0100
@@ -63,7 +63,7 @@
 module Database.HDBC.PostgreSQL
     (
      -- * Connecting to Databases
-     connectPostgreSQL, Connection,
+     connectPostgreSQL, Connection, pgConn,
      -- * PostgreSQL Error Codes
      --
      -- |When an @SqlError@ is thrown, the field @seState@ is set to one of the following
@@ -73,5 +73,5 @@
 
 where
 
-import Database.HDBC.PostgreSQL.Connection(connectPostgreSQL, Connection())
+import Database.HDBC.PostgreSQL.Connection(connectPostgreSQL, Connection(), pgConn)
 import Database.HDBC.PostgreSQL.ErrorCodes
