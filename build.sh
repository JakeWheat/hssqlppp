
# most of the time just using cabal build will be enough
# if you change the ast types then you will need a full build:

#todo: get rid of this sh file and put everything in the make file

make || exit $?
cabal build || exit $?
