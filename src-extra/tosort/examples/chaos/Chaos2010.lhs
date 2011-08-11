
Small hack transform and load the chaos sql into the database.

ghc --make -threaded -XScopedTypeVariables -XDeriveDataTypeable -DPOSTGRES -cpp -pgmPcpphs -optP--cpp -idevel:src/lib:src/qq:src/postgresql:examples/chaos:examples/extensions/:examples/util/:tests/ --make examples/chaos/Chaos2010.lhs

time examples/chaos/Chaos2010 sql > chaos1.sql && time examples/chaos/Chaos2010 clear && time psql chaos -q --set ON_ERROR_STOP=on --file=chaos1.sql


> import System.Environment
> import System.Console.CmdArgs
> import System.IO
> import Control.Monad.Error

> import Data.Maybe
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Utils.Utils
>
> import Database.HsSqlPpp.Catalog
> import qualified Database.HsSqlPpp.TypeChecker as A
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.SqlTypes
>
> import qualified Database.HsSqlPpp.Parser as P
>
> import Database.HsSqlPpp.PrettyPrinter
>
> import Database.HsSqlPpp.Utils.DatabaseLoader
> import Database.HsSqlPpp.Utils.DBUtils
> import Database.HsSqlPpp.Utils.DbmsCommon
>
> import Database.HsSqlPpp.Chaos.ChaosExtensions
> import Database.HsSqlPpp.Chaos.ChaosFiles
> import Database.HsSqlPpp.Utils.RoundTripTester

> databaseName :: String
> databaseName = "chaos"


> main :: IO()
> main = do
>   argv <- getArgs
>   case argv of
>     ["reset"] -> reset
>     ["sql"] -> sql
>     ["check"] -> check
>     ["clear"] -> withConn ("dbname=" ++ databaseName) clearDB
>     ["test"] -> rtt
>     x -> putStrLn $ "don't understand " ++ show x

-------------------------------------------------------------------------------

> reset :: IO ()
> reset =
>   withConn ("dbname=" ++ databaseName) $
>   flip withTransaction $ \conn -> wrapETs $ do
>   --clear the db and get the transformed ast

>   liftIO $ do
>     hSetBuffering stdout NoBuffering
>     clearDB conn
>   ast <- fmap (concat |> chaosExtensions)
>            $ mapM (\ f -> (liftIO . readInput) f
>                           >>= tsl . P.parseSql f)
>                   chaosFiles
>   liftIO $ loadAst conn ast
>   return ()


> sql :: IO ()
> sql = wrapETs $ do
>   ast <- fmap (concat |> chaosExtensions)
>            $ mapM (\ f -> (liftIO . readInput) f
>                           >>= tsl . P.parseSql f)
>                   chaosFiles
>   liftIO $ putStrLn $ printSql ast
>   return ()

> check :: IO ()
> check = wrapETs $ do
>   --clear the db and get the transformed ast
>   ast <- fmap (concat |> chaosExtensions)
>            $ mapM (\ f -> (liftIO . readInput) f
>                           >>= tsl . P.parseSql f)
>                   chaosFiles
>   mapM_ (liftIO . putStrLn) $
>             (A.typeCheck defaultTemplate1Catalog |>
>              snd |>
>              getTypeErrors |>
>              ppTypeErrors) ast
>   return ()



> -- | read a file as text, will read from stdin if filename is '-'.
> readInput :: FilePath -> IO String
> readInput f =
>   case f of
>              "-" -> getContents
>              _ | length f >= 2 &&
>                  head f == '"' && last f == '"'
>                    -> return $ drop 1 $ take (length f - 1) f
>                | otherwise -> readFile f

> -- | Pretty print list of type errors with optional source position
> --   in emacs readable format.
> ppTypeErrors :: [(Maybe (String,Int,Int), [TypeError])] -> [String]
> ppTypeErrors tes =
>   map showSpTe tes
>   where
>     showSpTe (Just (fn,l,c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>     showSpTe (_,e) = "unknown:0:0:\n" ++ show e

> getTypeErrors :: Data a => a -> [(Maybe (String,Int,Int), [TypeError])]
> getTypeErrors es =
>   let as = [a::Annotation | a <- universeBi es]
>   in mapMaybe getTes as
>   where
>     getTes as = let tes = errs as
>                 in if null tes
>                    then Nothing
>                    else Just (asrc as, tes)

> rtt :: IO()
> rtt = do
>   rt <- roundTripTest chaosExtensions databaseName chaosFiles
>   putStrLn $ rtShowBrief rt
