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

> main :: IO()
> main = defaultMain allTests


ghc -XTupleSections -XScopedTypeVariables -XDeriveDataTypeable -i/home/jake/wd/hssqlppp/trunk/src:/home/jake/wd/hssqlppp/trunk/src-extra/examples:/home/jake/wd/hssqlppp/trunk/src-extra/postgresql:/home/jake/wd/hssqlppp/trunk/src-extra/qq:/home/jake/wd/hssqlppp/trunk/src-extra/tests:/home/jake/wd/hssqlppp/trunk/src-extra/util /home/jake/wd/hssqlppp/trunk/src-extra/tests/Tests.lhs