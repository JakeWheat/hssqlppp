just chucked together

use runghc Build.lhs from the src/ folder to regenerate the html files
in the root from these txt files written in pandoc flavour markdown

> import System.Cmd
> import System.FilePath

> main :: IO ()
> main = do
>   let files = [("index.txt", "HsSqlPpp developer notes")
>               ,("background_reading.txt", "Background reading")
>               ,("applicative_parsing.txt", "Applicative Parsing intro")
>               ,("compiling_and_running_tests.txt", "Compiling and running the tests")
>               ,("adding_syntax.txt", "Adding new syntax")]
>   mapM_ pandocerize files

> pandocerize :: (String,String) -> IO ()
> pandocerize (pdnm, title) = do
>   _ <- system $ "pandoc -s " ++ pdnm ++ " -o " ++ "../" ++ htmlname ++ " -c main.css -T \"" ++ title ++ "\""
>   return ()
>   where
>     htmlname = dropExtension pdnm ++ ".html"

