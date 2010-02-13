Copyright 2010 Jake Wheat

Idea is to add a load of information to the source code for some sort
of presentation.

1) fix extensions to copy the source positions on each transformed
statement. might need to add more than one source position, e.g.  for
modules, we have the module declaration, and the statement which both
produce an additional insert statement, this insert statement's
primary source position is that of the ddl statement it is for, but
has a secondary source position which is the module declaration.

2) write method to split source files by statement:

[FilePath] -> [(String,Statement)]

The text is split at the source position of each statement, so any
comments before the first token in the statement get attached to the
previous statement. TODO: add source position of the ';' at the end of
each statement to the parsed ast in the parser.

3) add annotations (not sql ast annotations):

(a -> b) -> [a] -> [(a,b)]
annotate -> list -> annotated list

for each statement we want:

the transformed ast iff it is different from the original -> need to
run the whole tree through the extensions needed, and then link back
using the source position annotation on the statements as a key/ fk.
 - put all the transformed tree statements in first, and then can
filter using ==

annotate :: [Statement] -> [Statement] -> [(Statement,[Statement])]
annotate sourceAst transformedAst = ...

all the annotations from type checking the transformed tree, so pass
the typechecked transformed tree into the previous function

- provide this result as one public function:

[FilePath] -> [(String,(Statement,[Statement]))]
   (originaltext,(parse tree of original text, [transformed and annotated trees]))

then have a second function to render this:

  source statement in original formatting up to the ';'
  ast of source statement
  if statement isn't transformed (i.e. the annotation-stripped asts are identical)

  dostatement = ast of statement annotations, ast of statement without
  annotations, full ast with all annotations, separated out highlighted
  list of type error annotations

  if the statement is transformed: then for each statement in the transformed list,
    show the sql source of that statement, then run the dostatement above.

  finally, the trailing comments

so we do (string,Statement,[Statement]) -> (string,string,string)

then concat the lot together, and can then render with pandoc


> module Database.HsSqlPpp.Examples.AnnotateSource2
>     (annotateSource2) where
>
> {-import Data.List
> import Data.Char
> import Control.Monad.Error
>
> import Database.HsSqlPpp.Utils.Utils hiding (split)
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Examples.DBUtils-}

> annotateSource2 :: [FilePath] -> String
>
> annotateSource2 = undefined
