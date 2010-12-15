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


PATH=$PATH:/home/jake/.cabal/bin/ ghc -cpp -pgmPcpphs -optP--cpp --make -i/home/jake/wd/hssqlppp/trunk/src:/home/jake/wd/hssqlppp/trunk/tests:/home/jake/wd/hssqlppp/trunk/devel:/home/jake/wd/hssqlppp/trunk/examples/chaos/:/home/jake/wd/hssqlppp/trunk/examples/extensions/:/home/jake/wd/hssqlppp/trunk/examples/util/:/home/jake/wd/hssqlppp/trunk/examples/wrappers/ /home/jake/wd/hssqlppp/trunk/tests/Tests.lhs