Copyright 2009 Jake Wheat

The purpose of this module is to add annotations in comments to the
original source code, so that we can preserve the original formatting
and comments.

A second goal will be to update these comments if they are already
present, so we can run this process repeatedly on a file and not fill
it with junk, or can e.g. make a few changes to the sql, run this
process, then use source control diff to view how the types, etc. have
changed.


Algorithm design

Get all the annotations ordered by source position. Split the original
text on these points, then zip it and output it.


> {- | Function to pretty print annotation information interspersed
>      with original source file, so e.g. you can view types,
>      etc. inline in the source whilst preserving the original
>      formatting and comments.  -}
> module Database.HsSqlPpp.PrettyPrinter.AnnotateSource
>     (annotateSource) where

> import Data.List
> import Data.Maybe
> import Debug.Trace

> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Ast.Annotator

> annotateSource :: Bool -> String -> StatementList -> String
> annotateSource doErrs src aast =

Details:

First need better syb so we can get two separate lists of annotations,
one for statements and one for non-statements. This will allow us to
output full information for each statement, but output reduced
information for other nodes - just want to output type error
annotations for now. (This could be made more general by allowing a
different kind of annotation pretty printer depending on the node
type, value or context?)

filter these two lists, mainly to strip all the annotations from the non-statement annotation list except the source positions and the type errors.

merge these two lists and sort by source position, then map to get
[(sourceposition, annotation without sourceposition)]

Now we have a list of sourcepositions that we can split the original source with:
0->firstsp, firstsp->secondsp, ... second last sp-> last sp, last sp -> eof
-> this produces a [string] from the original source
use a zip:
zip splitSource $ ([]:map snd mungedAnnotationlist)
to get [(string,annotation)]
then do map second prettyPrint over this
to gives us a [(string,string)] which we can concatenate to produce the output text.

To replace existing comments rather than repeatedly add them:
1) make sure the pretty printed comments have some marker
   strip all the comments with this marker out after splitting the string on the annotation source positions, i.e. when we get to [(string,annotation)] or [(string,string)] stage.


>    let allAnn = sortBy ordSps $ getStatementPosStringPairs ++ getTypeErrorPosPairs
>        splitPoints = map ((\(SourcePos _ l _) -> l - 1) . fst) allAnn
>        splitsSrc = splitAts src splitPoints
>        anSrcPairs = zip splitsSrc $ map snd allAnn
>    in concatMap (uncurry (++)) anSrcPairs
>           -- make sure we get the last bit of the source code
>           ++ last splitsSrc
>    where
>      ordSps :: (AnnotationElement,String) -> (AnnotationElement,String) -> Ordering
>      ordSps a b = case (a,b) of
>                     ((SourcePos _ l c, _),(SourcePos _ l1 c1, _)) -> compare (l,c) (l1,c1)
>                     _ -> EQ
>      getTypeErrorPosPairs :: [(AnnotationElement, String)]
>      getTypeErrorPosPairs =
>          if doErrs
>            then map (\(a,b) -> (a,"\n/*ERROR:" ++ show b ++ "*/\n")) typeErrorsWithPositions
>            else []
>          where
>            typeErrorsWithPositions = mapMaybe (\(a,b) -> case a of
>                                                                 Nothing -> Nothing
>                                                                 Just a1 -> Just (a1,b)) typeErrors
>            typeErrors = getTypeErrors aast
>      getStatementPosStringPairs :: [(AnnotationElement, String)]
>      getStatementPosStringPairs =
>          let statementAnnotations = map interestingAnn $ getStatementAnnotations aast
>              split = mapMaybe (\l -> let notSp = filter (not.isSp) l
>                                      in if notSp == []
>                                           then Nothing
>                                           else Just (find isSp l, notSp)) statementAnnotations
>              splitsWithSps = mapMaybe (\(a,b) -> case a of
>                                                         Nothing -> Nothing
>                                                         Just a1 -> Just (a1,b)) split
>          in map (\(a,b) -> (a, "\n/*" ++ show b ++ "*/\n")) splitsWithSps
>          where
>            interestingAnn anns =
>              flip filter anns (\a ->
>                                case a of
>                                       TypeAnnotation _ -> False
>                                       EnvUpdates [] -> False
>                                       _ -> True)

>      isSp t = case t of
>                      SourcePos _ _ _ -> True
>                      _ -> False
>      splitAts :: String -> [Int] -> [String]
>      splitAts s splits =
>          let slines = lines s
>              --make sure we get from the last split to the end of the file
>              splits1 = splits ++ [length slines]
>              pairs :: [(Int,Int)]
>              pairs = zip (0:splits) splits1
>          in map (\(st,en) -> unlines $ take (en - st) $ drop st slines) pairs

