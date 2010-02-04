Copyright 2009 Jake Wheat

A possible goal of this code is to add some annotations in comments to
SQL source code, to provide additional information that isn't usually
easily found out, e.g. the exact type of a view.

To make this work well, want to be able to rerun the code on a file
and have the comments updated rather than mangled or appended to.

Code doesn't do this atm, just sticks annotations into the source code
whilst preserving formatting, comments, etc. as a starting point.

Approach:

Get all the annotations indexed by source position. Split the original
text on these points, then zip 'em together and output.


> module Database.HsSqlPpp.Examples.AnnotateSource
>     (annotateSource) where
>
> import Data.List
> import Data.Char
> import Control.Monad.Error
>
> import Database.HsSqlPpp.Utils.Utils hiding (split)
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Examples.DBUtils
>
> annotateSource :: (Maybe ([Statement] -> [Statement]))
>                -> (Maybe (Annotation -> String))
>                -> String
>                -> String
>                -> String
>                -> IO String
> annotateSource astTransform annotationPrinter dbName fileName sqlText = wrap $ do
>   ast <- tsl $ parseSql fileName sqlText
>   let ast1 = maybe ast (\t -> t ast) astTransform
>   cat <- liftIO (readCatalog dbName) >>= tsl
>   let (_,aast) = typeCheck cat ast1
>   let anns = getPrintAnnotations (maybe defaultAnnotationPrinter id annotationPrinter) aast
>       splitsSrc = splitAts sqlText $ map fst anns
>       annSrcPairs = zip splitsSrc $ map snd anns
>   return $ concatMap (uncurry (++)) annSrcPairs
>            -- make sure we get the last bit of the source code
>            ++ last splitsSrc
>   where
>     getPrintAnnotations :: (Annotation -> String)
>                         -> [Statement] -> [(Int -- line number
>                                           ,String)]
>     getPrintAnnotations annPr ast =
>       let rawAnnotations = getAnnotations ast
>           pairs = map getSp rawAnnotations -- pair the annotations off with line number
>           pr = map (\(a,b) -> (a, annPr b)) pairs -- convert the annotations to string using the supplied fn
>           fpr = filter (\(_,b) -> any (not . isSpace) b) pr -- filter out the empty string entries
>           cfpr = map (\(a,b) -> (a,"/*" ++ b ++ "*/\n")) fpr -- add comment delimiters around the remaining
>       in cfpr
>
>     getSp :: Annotation -> (Int,Annotation)
>     getSp a = (findSp a, a)
>             where
>               findSp ((SourcePos _ l _) : _) = l - 1
>               findSp (_ : xs) = findSp xs
>               findSp [] = 0
>     splitAts :: String -> [Int] -> [String]
>
>     splitAts s splits =
>          let slines = lines s
>              --make sure we get from the last split to the end of the file
>              splits1 = splits ++ [length slines]
>              pairs :: [(Int,Int)]
>              pairs = zip (0:splits) splits1
>          in map (\(st,en) -> unlines $ take (en - st) $ drop st slines) pairs
>
>     wrap :: Monad m => ErrorT String m a -> m a
>     wrap c = runErrorT c >>= \x ->
>              case x of
>                     Left er -> error er
>                     Right l -> return l
>
> defaultAnnotationPrinter :: Annotation -> String
> defaultAnnotationPrinter a =
>   intercalate "\n" $ map ppA a
>   where
>     ppA x = case x of
>               StatementTypeA st@(StatementType i o) | not (null i) && not (null o) -> ppExpr st
>               CatUpdates u | not (null u) -> ppExpr u
>               _ -> ""
