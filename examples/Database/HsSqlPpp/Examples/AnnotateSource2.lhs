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
> import Database.HsSqlPpp.Parser -}
> import Control.Monad.Error
> --import System.FilePath
> import Data.Maybe
> import Data.List
>
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Examples.Extensions.ChaosExtensions
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Ast hiding (Sql)
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.PrettyPrinter


> annotateSource2 :: [FilePath] -> IO [(String,String)]
>
> annotateSource2 fs = do
>   origAst <- readSourceFiles fs
>   let transformedAst = chaosExtensions origAst
>       lump :: [(Maybe (FilePath,Int), [Statement], [Statement])]
>       lump = joinLists getSp getSp origAst transformedAst

The idea is to take the list of files passed to this function ('source
filenames'), then take the entries from the lump and match each entry
with its file.

type Entry = (Maybe (FilePath,Int), [Statement], [Statement])

[Entry] -> [(FilePath, [Entry])]

But, we want to keep the entries in order they're already in, and some
of the entries may have missing source files or the files might not
match one of the input files (this happens with some of the
extensions), or they might be out of order (not sure if this happens
with the current set of extensions, but it's definitely possible.

Quick and dirty solution

we keep adding the entries against the first source file until we hit
a statement with its source position pointing to the second source
file. This means all the prepended sql goes against the first file
given. If there are no entries against the first source file, or if
there isn't an entry against the second source file after one against
the first source file, give up. (Deal with this if/when it starts
happening).

Once we hit an entry against the second source file, switch to that
one, we keep adding entries against it until we find one against the
third source file. Repeat until we get to the last source file, which
gets all the remaining code. This way, both the first set of
statements, and the second set of statements should be in the right
orders still.

I think it still has problem with orphan statements in the transformed
list appearing in the wrong place

>   let lumpByFile :: [(String, [(Maybe (FilePath,Int), [Statement], [Statement])])]
>       lumpByFile = appendList (\(p, _,_) -> case p of
>                                               Just (f,_) -> f
>                                               Nothing -> head fs)
>                               fs
>                               lump
>                               []
>   let newFiles :: [(String,String)]
>       newFiles = flip map lumpByFile $ \(f,es)
>                    -> (f, concatMap showEntry $ reverse es)
>                  where
>                    showEntry (_, sts1, sts2) =
>                        let s1 = stripAnnotations sts1
>                            s2 = stripAnnotations sts2
>                        in case () of
>                             _ | s1 == s2 -> printSql sts1
>                               | isPrefixOf s1 s2 -> printBoth s1 (drop (length s1) s2)
>                               | otherwise -> printBoth s1 s2
>                    printBoth s1 s2 =
>                        printSql s1
>                        ++ "\n-- start generated code ------------------------\n"
>                        ++ trim (printSql s2)
>                        ++ "\n-- end generated code --------------------------\n"
>   return $ reverse newFiles

> appendList :: Eq k => (v -> k) -> [k] -> [v] -> [(k,[v])] -> [(k,[v])]
> -- last key: shove the rest of the list against it
> appendList _ (lastKey:[]) vs acc = (lastKey, vs) : acc
> -- more keys, more values
> appendList gk
>            kx@(k1 : ks)
>            (v : vs)
>            acc@((k,ms) : as) | gk v == k1 = -- found the next key, so move to the next entry
>                                             appendList gk ks vs ((k1,[v]):acc)
>                              | otherwise = -- not found next key, so keep appending to this entry
>                                            appendList gk kx vs ((k,v:ms):as)
> appendList gk (k1 : ks) (v : vs) [] = appendList gk ks vs [(k1, [v])]
> -- no more values - we're done, need to reverse the [v]s
> appendList _ _ [] acc = acc -- map (\(a,b) -> (a, reverse b)) acc
> appendList _ [] _ _ = error "no keys in annotatesource2.appendlist"




> readSourceFiles :: [String] -> IO [Statement]
> readSourceFiles fns = wrapETs $
>   mapM (\f -> liftIO (parseSqlFile f) >>= tsl) fns >>=
>   concat |> return

> getSp :: Statement -> Maybe (String,Int)
> getSp st =
>   getSP (getAnnotation st)
>   where
>     getSP (SourcePos f l _ : _ ) = Just (f,l)
>     getSP (_ : xs) = getSP xs
>     getSP [] = Nothing
