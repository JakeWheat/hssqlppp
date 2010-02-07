Copyright 2010 Jake Wheat

> module Database.HsSqlPpp.Examples.Extensions.AstUtils where
>
> import Data.Generics.Uniplate.Data
> import Data.Generics
> import Data.List
>
> import Database.HsSqlPpp.Ast

> data AstInfo = AstInfo {
>      functions :: [(String,Statement)]
>     ,views :: [(String,Statement)]
>     ,tables :: [(String,Statement)]
>     }

> getAstInfo :: [Statement] -> AstInfo
> getAstInfo ast = AstInfo (listFunctions ast)
>                          (listViews ast)
>                          (listTables ast)

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


gettrefs -> is view -> get referenced tables for each view
            is in table list -> append
            otherwise -> not interested
getfunctions -> getreferenced table for each function

> getTrefs :: Data a => a -> [String]
> getTrefs ast = [tbl| Tref _ tbl _ <- universeBi ast]

> getFunctionRefs :: Data a => a -> [String]
> getFunctionRefs ast = [fn| FunCall _ fn _ <- universeBi ast]

> listFunctions :: Data a => a -> [(String,Statement)]
> listFunctions ast =
>   [(fn,f) | f@(CreateFunction _ fn _ _ _ _ _ _) <- universeBi ast]

> listViews :: Data a => a -> [(String,Statement)]
> listViews ast =
>   [(vn,v) | v@(CreateView _ vn _) <- universeBi ast]

> listTables :: Data a => a  -> [(String,Statement)]
> listTables ast =
>   [(tn,t) | t@(CreateTable  _ tn _ _ ) <- universeBi ast]
