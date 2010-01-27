
> {-# LANGUAGE TemplateHaskell,QuasiQuotes,RelaxedPolyRec #-}

> import Database.HsSqlPpp.Dbms.DBAccess2

> import CS -- import the connection string from another file so we
>           -- can use it in splices

> main :: IO ()
> main = do
>   withConn ("dbname=" ++ connStr) $ \conn -> do
>     x <- pieces conn
>     print x

> pieces :: IConnection conn => conn
>                                      -> IO
>                                           [(Maybe String,
>                                             Maybe String,
>                                             Maybe Int,
>                                             Maybe Int,
>                                             Maybe Int)]
> pieces = $(sqlStmt connStr "select * from pieces;")

> pieces_at_pos :: IConnection conn =>
>                  conn
>               -> Maybe Int
>               -> Maybe Int
>               -> IO
>                  [(Maybe String, Maybe String, Maybe Int, Maybe Int, Maybe Int)]
> pieces_at_pos = $(sqlStmt connStr "select * from pieces where x = ? and y = ?;")

> turn_number :: IConnection conn => conn -> IO [Maybe Int]
> turn_number = $(sqlStmt connStr "select get_turn_number();")

> wizard_info :: IConnection conn => conn -> IO [(Maybe String, Maybe String, Maybe String, Maybe String)]
> wizard_info = $(sqlStmt connStr "select current_wizard,colour,allegiance,sprite \n\
>                                  \  from current_wizard_table\n\
>                                  \  inner join allegiance_colours\n\
>                                  \  on current_wizard = allegiance\n\
>                                  \  natural inner join wizard_sprites;")

> key_pressed :: IConnection conn => conn -> Maybe String -> IO [()]
> key_pressed = $(sqlStmt connStr "select action_key_pressed(?);")
