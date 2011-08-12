
# most of the time just using cabal build will be enough
# if you change the ast types then you will need a full build:

# generate a new hs from the ag files

echo uuagc

uuagc  -dcfwsp -P src/Database/HsSqlPpp/Internals/ src/Database/HsSqlPpp/Internals/AstInternal.ag || exit $?

# generate a new astanti file

echo compile MakeAntiNodes

ghc -isrc-extra/util:src-extra/devel-util src-extra/devel-util/MakeAntiNodesRunner.lhs || exit $?

echo run MakeAntiNodes

src-extra/devel-util/MakeAntiNodesRunner || exit $?

echo cabal build

cabal build || exit $?
