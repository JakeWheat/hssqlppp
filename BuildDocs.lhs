Copyright 2010 Jake Wheat

This file creates the documentation for the project, to be uploaded to
the website.

do

cabal haddock --hyperlink-source
& copy files across

README -> pandoc -> index.html
the readme file is written in markdown

hssqlsystem -> filter out documentation sections -> pandoc ->
hssqlsystem.html
the idea with hsssqlsystem is to add a markdown chunk
next to each command with some sort of delimiters (all of this outside
the birdfeet),
then to build the docs: filter hsssqlsystem to leave just these
chunks, which gives us a regular markdown file

parsertests.lhs,typechecktests.lhs -> hs src exts -> some sort of uniplate thing -> simple ast
 -> markdown -> pandoc -> html
idea is to get a parse tree of the source code, the transform this into a simple
data structure representing sections, and test data, drop all the
code. then these are put into a table in markdown syntax, the sql and
hs marked with the appropriate stuff to get them syntax highlighting,
then pandoc to html
