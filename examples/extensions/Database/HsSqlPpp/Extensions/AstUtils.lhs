Copyright 2010 Jake Wheat

This is the start of some code to support ast transforms where we need
some information that might come from the type checker, only we want
to support sql which doesn't type check until after the
transformations have been made.

At the moment, it supports a function to list the create function,
view and table statements from an ast, and another funtion to take an
arbitrary ast fragment and list the tables it references, either
directly, or via views and functions. This is to support adding a
constraint of an arbitrary expression, we need to know which tables to
attach the implementation trigger to. Will be expanded and more docs
added.

TODO: add test for recursive function, to make sure this code catches
it and quits.

> module Database.HsSqlPpp.Extensions.AstUtils
>     (getAstInfo
>     ,getReferencedTableList
>     ,replaceSourcePos
>     ,replaceSourcePos1) where
>
> import Data.Generics.Uniplate.Data
> import Data.Generics
> import Data.List
> --import Debug.Trace
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
>
> data AstInfo = AstInfo {
>      functions :: [(String,Statement)]
>     ,views :: [(String,Statement)]
>     ,tables :: [(String,Statement)]
>     }
>
> getAstInfo :: [Statement] -> AstInfo
> getAstInfo ast = AstInfo (listFunctions ast)
>                          (listViews ast)
>                          (listTables ast)
>
> -- won't need this wrapper when the recursion loop detection logic
> -- is in place
> getReferencedTableList :: Data a => AstInfo -> a -> [String]
> getReferencedTableList asti a =
>     nub $ getReferencedTableListI asti a
>
> getReferencedTableListI :: Data a => AstInfo -> a -> [String]
> getReferencedTableListI asti a =
>         doTables
>         ++ doViews
>         ++ doFunctions
>     where
>       doTables = intersect trefs (map fst $ tables asti)
>       doViews = let vdefs = filter ((`elem` trefs) . fst) $ views asti
>                 in concatMap (getReferencedTableList asti . snd) vdefs
>       doFunctions = let fdefs = filter ((`elem` funrefs) . fst) $ functions asti
>                     in concatMap (getReferencedTableList asti . snd) fdefs
>       trefs = getTrefs a
>       funrefs = getFunctionRefs a
>
> getTrefs :: Data a => a -> [String]
> getTrefs ast = [getName tbl| Tref _ tbl _ <- universeBi ast]
>
> getName :: Expression -> String
> getName (Identifier _ i) = i
> getName (FunCall _ "." [_,Identifier _ i]) = i
> getName (FunCall _ "." [_,a]) = getName a
> getName x = error $ "internal error getName called on: " ++ show x
>
> -- this is wrong because we don't take into account function overloading
> getFunctionRefs :: Data a => a -> [String]
> getFunctionRefs ast = [fn| FunCall _ fn _ <- universeBi ast]
>
> listFunctions :: Data a => a -> [(String,Statement)]
> listFunctions ast =
>   [(fn,f) | f@(CreateFunction _ fn _ _ _ _ _ _) <- universeBi ast]
>
> listViews :: Data a => a -> [(String,Statement)]
> listViews ast =
>   [(vn,v) | v@(CreateView _ vn _) <- universeBi ast]
>
> listTables :: Data a => a  -> [(String,Statement)]
> listTables ast =
>   [(tn,t) | t@(CreateTable  _ tn _ _ ) <- universeBi ast]

> replaceSourcePos1 :: Statement -> Statement -> Statement
> replaceSourcePos1 st st1 = head $ replaceSourcePos st [st1]

> replaceSourcePos :: Statement -> [Statement] -> [Statement]
> replaceSourcePos st =
>     map (adjSp gsp)
>     where
>       gsp :: SourcePosition
>       gsp = maybe ("unknown",1,1) id $ asrc $ getAnnotation st
>       adjSp sp1 = updateAnnotation (\a -> a {asrc = Just sp1})
