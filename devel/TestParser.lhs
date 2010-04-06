

> {-# LANGUAGE QuasiQuotes #-}

> import Data.List

> import Text.DocTool.Parser
> import Text.DocTool.Here


> main :: IO()
> main = do
>   flip mapM_ [t1,t2,t3,t4,t5,t6] $ \t -> do
>     putStrLn "-------------"
>     putStrLn t
>     let p = parseSource "{-" "-}" t
>     putStrLn $ either show (intercalate "\n" . map show) p
>   flip mapM_ [s1] $ \t -> do
>     putStrLn "-------------"
>     putStrLn t
>     let p = parseSource "/*" "*/" t
>     putStrLn $ either show (intercalate "\n" . map show) p


embedded comment ok

> t1 :: String
> t1 = [$here|this is a test
>{-
>some comments
>here
>-}
>and stuff|]

starts with comment

> t2 :: String
> t2 = [$here|{-
>some comments
>here
>-}
>and stuff|]

ends with comment

> t3 :: String
> t3 = [$here|this is a test
>{-some comments
>here
>-}|]

no good - no embedded newline

> t4 :: String
> t4 = [$here|this is a test
>{-some comments here-}
>and stuff|]

nested

> t5 :: String
> t5 = [$here|this is a test
>{-\nsome
>{-
>comments
>comments
>-}
>here
>-}
>and stuff|]

embedded with newline - no good

> t6 :: String
> t6 = [$here|
>this is some code{-embedded stuff
>                   blah -}more code|]

> s1 :: String
> s1 = [$here|this is a test
>/*
>some comments
>here
>*/
>and stuff|]

no comments
starts with comment
ends with comment
comment in middle
start + middle
middle + end
2 middles
no new line
doesn't start at start of line
doesn't end at end of line
