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

> module Database.HsSqlPpp.Examples.Extensions.AstUtils
>     (getAstInfo
>     ,getReferencedTableList) where
>
> import Data.Generics.Uniplate.Data
> import Data.Generics
> import Data.List
>
> import Database.HsSqlPpp.Ast
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
> getReferencedTableList :: Data a => AstInfo -> a -> [String]
> getReferencedTableList asti a =
>     doTables
>     ++ doViews
>     ++ doFunctions
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
> getTrefs ast = [tbl| Tref _ tbl _ <- universeBi ast]
>
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
