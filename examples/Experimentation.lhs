
Here is some source for use with ghci/ haskell-mode in emacs.

You can try out a bunch of things easily in the ghci window in emacs,
and edit the source here to experiment.

open this file in emacs with haskell-mode installed and set up

type C-c C-z to load ghci

When the ghci prompt has loaded, enter this into it:

:set -hide-package hssqlppp -isrc:src-extra/examples:src-extra/tests:src-extra/utils -XOverloadedStrings

change back to the window with Experimentation.lhs (this file) and
type C-c C-l again

now you can try the functions out, e g.

*Main> testParseStatements "select a from t;"


> {-# LANGUAGE OverloadedStrings #-}
> import System.Environment


> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Utils.GroomUtils
> import Text.Groom
> import qualified Data.Text.Lazy as T

> testParseStatements src = do
>   let ast = parseStatements defaultParseFlags "unknown" Nothing src
>   putStrLn $ either groom groomNoAnns ast



TODO:

add more example functions:

parsing using the different parser functions
pretty printing
groom variations for showing the ast
sql dialects

simple type checking example against default cat
create fresh cat and type check against

example type checking against create table sql syntax
