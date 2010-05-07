
hacked up slighly higher level access to copy from stdin/ to
stdout. Uses strings for the copy data which is probably wrong and
slow except for very small payloads.

> {-# LANGUAGE ScopedTypeVariables #-}
> module Database.HsSqlPpp.Utils.PQH
>     (PGconn
>     ,connect
>     ,disconnect
>     ,exec
>     ,withConnection
>     ,putCopyStringData
>     ,getCopyStringData
>     ,makePGConn
>     ) where

> import Control.Exception

> import Foreign
> import Foreign.C.Types
> import Foreign.C.String

> import Database.HsSqlPpp.Utils.PQ

> connect :: String -> IO (Either String PGconn)
> connect cs = do
>   conn <- pqConnectdb cs
>   st <- pqStatus conn
>   if st == CONNECTION_OK
>     then return $ Right conn
>     else do
>       stem <- pqErrorMessage conn
>       pqFinish conn
>       return $ Left stem

> exec :: PGconn -> String -> IO (Either String ExecStatusType)
> exec conn str = do
>   res <- pqExec conn str
>   err <- pqResultErrorMessage res
>   st <- pqResultStatus res
>   pqClear res
>   return $ if err == ""
>            then Right st
>            else Left err


> disconnect :: PGconn -> IO ()
> disconnect conn = pqFinish conn

> withConnection :: String -> (PGconn -> IO c) -> IO (Either String c)
> withConnection s f =
>   bracket (connect s) (\l -> case l of
>                                     Left _ -> return ()
>                                     Right c -> disconnect c)
>           (\c -> case c of
>                    Left e -> return $ Left e
>                    Right cn -> f cn >>= return . Right)

> putCopyStringData :: PGconn -> ExecStatusType -> String -> IO (Either String ())
> putCopyStringData conn cst cd =
>   if cst /= PGRES_COPY_IN
>     then return $ Left $ "expected Right PGRES_COPY_IN, got " ++ show cst
>     else do
>          pcd <- pqPutCopyData conn cd (length cd)
>          if pcd == -1
>            then do
>                 err <- pqErrorMessage conn
>                 return $ Left err
>            else do
>                 pce <- pqPutCopyEnd conn Nothing
>                 if pce == -1
>                   then do
>                        err <- pqErrorMessage conn
>                        return $ Left err
>                   else return $ Right ()

> getCopyStringData :: PGconn -> ExecStatusType -> IO (Either String String)
> getCopyStringData conn cst =
>   if cst /= PGRES_COPY_OUT
>     then return $ Left $ "expected Right PGRES_COPY_OUT, got " ++ show cst
>     else alloca $ \b -> do
>       cd <- readSomeData b []
>       checkRet
>       return $ Right cd
>   where
>     readSomeData :: Ptr (Ptr CChar) -> [String] -> IO String
>     readSomeData b acc = do
>       r <- pqGetCopyData conn b 0
>       if r /= -1
>         then do
>              b1 <- peek b
>              s <- peekCStringLen (b1,r)
>              pqFreemem $ castPtr b1
>              readSomeData b (s:acc)
>         else return $ concat $ reverse acc
>     checkRet = do
>       cr <- pqGetResult conn
>       case cr of
>               Just cr1 -> do
>                           cr2 <- pqResultStatus cr1
>                           print cr2
>                           checkRet
>               Nothing -> return ()
