
> module Database.HsSqlPpp.HsText.Commands
>     (defaultCommands
>     ,makeFile
>     ,shell
>     ,wrapCommand
>     ) where

> import Data.Char
> import Data.List
> import System.Exit
> import System.Process (readProcessWithExitCode)
> import System.FilePath

> import Control.Monad.Error


> defaultCommands :: [(String,String -> IO String)]
> defaultCommands = [("file", makeFile)
>                   ,("shell", shell)]

> makeFile :: String -> IO String
> makeFile s =
>   let (fn,content) = break ws $ dropWhile space s
>   in do
>     writeFile fn content
>     return $
>      "File " ++ fn ++ "\n\n"
>      ++ "~~~~~~~~~~~~{" ++ takeExtension fn ++ "}"
>      ++ content ++ "\n~~~~~~~~~~~~\n\n"
>   where
>     ws = (`elem` " \n\t")
>     space = (==' ')

> shell :: String -> IO String
> shell s = {-let (c:as) = words s
>           in -} do
>              (ex,o,e) <- readProcessWithExitCode "bash" ("-c":("PATH=$PATH:~/wd/hssqlppp/trunk " ++ s):[]) ""
>              return $ o ++ e ++ case ex of
>                                         ExitSuccess -> ""
>                                         ExitFailure i -> "\nErrorCode: " ++ show i ++ "\n"

PATH=$PATH:~/wd/hssqlppp/trunk && HsSqlSystem

> wrapCommand :: String -> String -> (String -> IO String) -> (String -> IO String)
> wrapCommand pre post c l = c l >>= \m -> return $ pre ++ m ++ post