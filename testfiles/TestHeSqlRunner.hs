
-- temp code to load the processed hesql file and run it

import Database.HsSqlPpp.Dbms.DBAccess

import Testhesql1


main :: IO ()
main = do
  withConn ("dbname=chaos") $ \conn -> do
    x <- pieces conn
    print x

