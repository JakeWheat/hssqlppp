#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

Mainly test error messages from check failures from bad sql

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char

> import Ast
> import Parser

> main :: IO ()
> main =
>   defaultMain [
>     testGroup "test test"
>     (mapAttr [
>       p "select 1;" []
>      ])
>    ,testGroup "loop tests"
>     (mapAttr [
>       p "create function cont_test1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \begin\n\
>         \  for r in select a from b loop\n\
>         \    null;\n\
>         \    continue;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;\n" []
>      ,p "create function cont_test2() returns void as $$\n\
>         \begin\n\
>         \    continue;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;\n" [Error ("",3,5) ContinueNotInLoop]
>      ])
>    ]
>         where
>           mapAttr = map $ uncurry checkAttrs
>           p a b = (a,b)
> checkAttrs :: String -> [Message] -> Test.Framework.Test
> checkAttrs src msgs = testCase ("check " ++ src) $ do
>   let ast = case parseSql src of
>                Left er -> error $ show er
>                Right l -> l
>       msgs1 = runAtts ast
>   assertEqual ("check " ++ src) msgs msgs1
