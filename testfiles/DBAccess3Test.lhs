
> {-# LANGUAGE TemplateHaskell,QuasiQuotes,RelaxedPolyRec,NoMonomorphismRestriction #-}

> import Data.HList
> import Data.HList.Label5 ()
> import Data.HList.TypeEqGeneric1 ()
> import Data.HList.TypeCastGeneric1 ()

> import Database.HsSqlPpp.Dbms.DBAccess3

> import CS -- import the connection string from another file so we
>           -- can use it in splices

> main :: IO ()
> main = do
>   withConn ("dbname=" ++ connStr) $ \conn -> do
>     x1 <- pieces conn
>     print x1
>     mapM (putStrLn . xToStr) x1
>     return ()
>     where
>       xToStr x1 = "ptype: " ++ show (x1 # ptype) ++
>                   "allegiance: " ++ show (x1 # allegiance) ++ ", " ++
>                   "tag: " ++ show (x1 # tag) ++ ", " ++
>                   "x: " ++ show (x1 # x) ++ ", " ++
>                   "y: " ++ show (x1 # y) ++ ", "

hopefully can use
some type template haskell to write
this more succintly
in ghc 6.12, e.g. $(record [(Ptype,String)
                           ,(Allegiance,String)
                           ...])

> pieces :: IConnection conn =>
>                  conn
>               -> IO [Record (HCons (LVPair (Proxy Ptype) (Maybe String))
>                              (HCons (LVPair (Proxy Allegiance) (Maybe String))
>                               (HCons (LVPair (Proxy Tag) (Maybe Int))
>                                (HCons (LVPair (Proxy X) (Maybe Int))
>                                 (HCons (LVPair (Proxy Y) (Maybe Int))
>                                  HNil)))))]

> pieces = $(sqlStmt connStr "select * from pieces;")

> pieces_at_pos :: IConnection conn =>
>                  conn
>               -> Maybe Int
>               -> Maybe Int
>               -> IO [Record (HCons (LVPair (Proxy Ptype) (Maybe String))
>                              (HCons (LVPair (Proxy Allegiance) (Maybe String))
>                               (HCons (LVPair (Proxy Tag) (Maybe Int))
>                                (HCons (LVPair (Proxy X) (Maybe Int))
>                                 (HCons (LVPair (Proxy Y) (Maybe Int))
>                                  HNil)))))]
> pieces_at_pos = $(sqlStmt connStr "select * from pieces where x = ? and y = ?;")

> turn_number = $(sqlStmt connStr "select get_turn_number();")

> wizard_info = $(sqlStmt connStr "select current_wizard,colour,allegiance,sprite \n\
>                                  \  from current_wizard_table\n\
>                                  \  inner join allegiance_colours\n\
>                                  \  on current_wizard = allegiance\n\
>                                  \  natural inner join wizard_sprites;")

> key_pressed = $(sqlStmt connStr "select action_key_pressed(?);")
