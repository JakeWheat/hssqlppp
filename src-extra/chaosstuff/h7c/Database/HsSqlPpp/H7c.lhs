
> module Database.HsSqlPpp.H7c
>     (h7c
>     ,H7cConfig(..)
>     ,defaultConfig
>     ) where

> import System.FilePath
> --import System.Environment
> import System.Exit

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Pretty


> data H7cConfig = H7cConfig
>     {connectionString :: String
>     ,sqlSourceFiles :: [FilePath]
>     ,sqlSourceDir :: FilePath
>     ,extensions :: [Statement] -> [Statement]}

> defaultConfig :: H7cConfig
> defaultConfig = H7cConfig
>                 {connectionString = ""
>                 ,sqlSourceFiles = []
>                 ,sqlSourceDir = "."
>                 ,extensions = id}

> h7c :: H7cConfig -> IO ()
> h7c o = do
>   let fns = map (sqlSourceDir o </>) $ sqlSourceFiles o
>   eas <- mapM parseStatementsFromFile fns
>   let east :: Either ParseErrorExtra [Statement]
>       east = do
>              as <- sequence eas
>              return $ concat as
>   ast <- either (\e -> print e >> exitFailure)
>                 return
>                 east
>   let tast = extensions o ast
>   putStrLn $ printStatements tast
