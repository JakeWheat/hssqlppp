
# regenerate the file
# src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs

# you need postgresql installed for this to work

echo compile

ghc -isrc:src-extra/util:src-extra/devel-util src-extra/devel-util/MakeDefaultTemplate1Catalog.lhs || exit $?

echo generate

src-extra/devel-util/MakeDefaultTemplate1Catalog > src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs_new || exit $?

echo move

mv src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs_new src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs || exit $?
