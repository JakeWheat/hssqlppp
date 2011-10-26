test files todo:

create new test hierarchy, move test code here, add new exe to run
  them

disable failing tests

get website generating again

start new test pages + renderer


extensions tests

local bindings tests

parameterized statement tests
parsertests
quasiquote tests
roundtrip tests
type check tests
type inference tests

> import Database.HsSqlPpp.Tests.Tests
> import Test.Framework

> main :: IO ()
> main = defaultMain allTests

cd /home/jake/wd/hssqlppp/trunk/ && ghc -Wall -XTupleSections -XScopedTypeVariables -XDeriveDataTypeable -isrc:src-extra/util:src-extra/tests/ src-extra/tests/Tests.lhs