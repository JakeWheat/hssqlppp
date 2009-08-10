#!/usr/bin/env runghc

> import Test.HUnit
> import Test.QuickCheck
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char
> import Control.Monad

> import Grammar
> import P1
> -- import PrettyPrinter

> main :: IO ()
> main = do
>   defaultMain [
>         checkParseExpression "1" (IntegerL 1)
>        ,checkParseExpression "1+1" (Exp Plus (IntegerL 1) (IntegerL 1))
>        ,checkParseExpression "1+1+1" (Exp Plus (Exp Plus (IntegerL 1) (IntegerL 1)) (IntegerL 1))

 >        ,checkParseExpression "'test'" (StringL "test")
 >        ,checkParseExpression "fn()" (FunctionCall "fn" [])
 >        ,checkParseExpression "fn(1)" (FunctionCall "fn" [IntegerL 1])
 >        ,checkParseExpression "fn('test')" (FunctionCall "fn" [StringL "test"])
 >        ,checkParseExpression "fn(1,'test')" (FunctionCall "fn" [IntegerL 1, StringL "test"])

>        ,checkParseExpression "true" (BooleanL True)
>        ,checkParseExpression "false" (BooleanL False)

>        ]
>   
> checkParseExpression :: String -> Expression -> Test.Framework.Test
> checkParseExpression src ast = testCase ("parse " ++ src) $ do
>   assertEqual ("parse " ++ src) ast (parse src)

 >   -- pretty print then parse to check
 >   let pp = printSql ast
 >   let ast'' = case parseSql pp of
 >               Left er -> error $ "reparse " ++ show er ++ "\n" ++ pp ++ "\n"
 >               Right l -> l
 >   assertEqual ("reparse " ++ pp) ast ast''

