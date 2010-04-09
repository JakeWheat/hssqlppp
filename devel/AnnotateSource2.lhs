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

> {-# LANGUAGE ScopedTypeVariables #-}
> module AnnotateSource2
>     (annotateSource2) where
>
> import Control.Monad.Error
> import Data.List
> import Data.Maybe
> import Control.Applicative
> --import Debug.Trace
>
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Ast hiding (Sql)
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.PrettyPrinter
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Utils.DBUtils

> annotateSource2 :: (Maybe ([Statement] -> [Statement]))
>                 -> (Maybe (Annotation -> String))
>                 -> String
>                 -> [FilePath] -> IO [(String,String)]
>
> annotateSource2 astTransform annotPrinter dbName fs = do
>   origAst <- readSourceFiles fs
>   cat <- either (\l -> error $ show l) id <$> readCatalog dbName
>   let transformedAst = maybe origAst (\t -> t origAst) astTransform
>       (_,transformedAast) = typeCheck cat transformedAst
>   let sps :: [Maybe (String,Int,Int)]
>       sps = map (asrc . getAnnotation) transformedAst
>   when (any (==Nothing) sps)
>        $ error "statements without source positions"
>   let afns = map (\(f,_,_) -> f) $ catMaybes sps
>   when (any (`notElem` fs) afns)
>        $ error "statements with unrecognised source file"

TODO:
check order of statements

partition transformed ast by source position filename
then for each file we have the filename and the list of transformed statements
we want to print each of these transformed statements in order to a new file
interspersed we want chunks of the original source

>   let p = npartition (fromMaybe (head fs) . fmap (\(a,_,_) -> a) . asrc . getAnnotation) transformedAast
>   (f1 :: [(String,[(String,[Statement])])]) <- forM p (\(f, sts) -> do
>              src <- readFile f
>              return (f, intersperseSource f src sts))
>   let annPr = maybe defaultAnnotationPrinter id annotPrinter
>   let (f2 :: [(String,String)]) =
>           flip map f1 $ \(f,e) -> (f, unlines $ map (showEntry annPr) e)

>   return f2
>   where
>     showEntry :: (Annotation -> String) -> (String, [Statement]) -> String
>     showEntry a (s, sts) =
>         let sto = either (const []) id $ parseSql "" s -- bit crap, we could reuse the already parsed statements
>             s1 = resetAnnotations sto
>             s2 = resetAnnotations sts
>         in case () of
>              _ | trim s == "" && sts == [] -> ""
>                | --hack
>                  trim s == "/*" && sts == [] -> ""
>                | s1 == s2 -> concatMap (annStr a) s2
>                              ++ printStatement "" "" "" s
>                | isPrefixOf s1 s2 -> printStatement "" "" (concatMap (annStr a) (take (length s1) sts)) s
>                                      ++ printStatement "GeneratedSql" "generated sql" ""
>                                         (prSql a $ drop (length s1) sts)
>                | otherwise -> printStatement "UnusedSql" "replaced sql" "" s
>                               ++ printStatement "GeneratedSql" "generated sql" "" (prSql a sts)
>     prSql :: (Annotation -> String) -> [Statement] -> String
>     prSql a sts = concatMap (\st -> annStr a st ++ printSql [st]) sts
>     annStr :: (Annotation -> String) -> Statement -> String
>     annStr a st = case a (getAnnotation st) of
>                      y | trim y == "" -> ""
>                      x -> "/*" ++ x ++ "*/\n"
>     printStatement :: String -> String -> String -> String -> String
>     printStatement cssClass comment pre st =
>        if cssClass == ""
>        then p1
>        else "<div class='" ++ cssClass ++ "'>" ++ p1 ++ "</div>"
>        where
>          p1 = "\n\n~~~~{.SqlPostgresql}\n"
>               ++ (if comment /= ""
>                   then "-- " ++ comment ++ " ------------------------\n"
>                   else "")
>               ++ pre ++ trim st
>               ++ "\n~~~~\n\n"

> intersperseSource :: FilePath -> String -> [Statement] -> [(String,[Statement])]
> intersperseSource fileName src statements =
>   let firstLine = fromJust $ msum $ map (\s -> case (asrc . getAnnotation) s of
>                                                  Just (f,l,_) | f == fileName -> {-trace ("sp: " ++ (ppExpr s)) $-} Just l
>                                                  _ -> Nothing) statements
>   in {-trace ("firstLine: " ++ show firstLine) $ -}
>      (unlines $ take firstLine fl, []) :
>      reverse (map (\(a,b) -> (a,reverse b)) $ addSource [] firstLine [] statements)
>   where
>     addSource :: [(String,[Statement])] -> Int -> [Statement] -> [Statement] -> [(String,[Statement])]
>     addSource acc lno sts1 (st:sts) =
>       case (asrc . getAnnotation) st of
>            Nothing -> addSource acc lno (st:sts1) sts
>            Just (f,l,_) | f /= fileName || l == lno -> addSource acc lno (st:sts1) sts
>                         | otherwise -> addSource ((fileLines lno l, sts1):acc)
>                                                l [st] sts
>     addSource acc lno sts1 [] = ((fileLines lno (length fl)), sts1):acc
>     fileLines :: Int -> Int -> String
>     fileLines s e = unlines $ take (e - s) (drop (s - 1) fl)
>     fl = lines src

> readSourceFiles :: [String] -> IO [Statement]
> readSourceFiles fns = wrapETs $
>   mapM (\f -> liftIO (parseSqlFile f) >>= tsl) fns >>=
>   concat |> return

> {-getSp :: Statement -> Maybe (String,Int)
> getSp st =
>   getSP (getAnnotation st)
>   where
>     getSP (SourcePos f l _ : _ ) = Just (f,l)
>     getSP (_ : xs) = getSP xs
>     getSP [] = Nothing-}

> defaultAnnotationPrinter :: Annotation -> String
> defaultAnnotationPrinter a =
>   intercalate "\n" $ catMaybes [fmap show $ stType a
>                                ,fmap show $ nl $ catUpd a
>                                ,fmap show $ nl $ errs a]
>   where
>     nl l = if null l
>            then Nothing
>            else Just l

