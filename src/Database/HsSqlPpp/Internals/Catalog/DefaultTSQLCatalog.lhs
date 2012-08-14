
> -- | Hacky start on a separate catalog for tsql. At the moment, reuses the
> -- postgresql default template1 catalog and adds a few things.
> {-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
> module Database.HsSqlPpp.Internals.Catalog.DefaultTSQLCatalog
>      (defaultTSQLCatalog) where
>
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Internals.Catalog.DefaultTemplate1Catalog
> import Data.List
> import Data.Generics.Uniplate.Data
> import Debug.Trace
> import Text.Groom

> defaultTSQLCatalog :: Catalog
> defaultTSQLCatalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      flip updateCatalog defaultTemplate1Catalog $

>     --trace ("fns: " ++ groom (int1fns ++ int12fns))
>     int1fns ++
>     int12fns ++
>     [CatCreateScalarType "nvarchar"
>     ,CatCreateTypeCategoryEntry "nvarchar" ("S", False)
>     ,CatCreateBinaryOp "+" "varchar" "varchar" "varchar"
>     ,CatCreateFunction "getdate" [] False "date"
>     ,CatCreateFunction "isnumeric" ["anyelement"] False "int4"
>     ,CatCreateFunction "grt_lengthconv" ["int4"] False "int4"
>     ,CatCreateFunction "isnull" ["anyelement","anyelement"] False "anyelement"
>     -- put these in to stop use the text only version and a bunch of casts
>     ,CatCreateFunction "replace" ["char", "char", "char"] False "char"
>     ,CatCreateFunction "replace" ["varchar", "varchar", "varchar"] False "varchar"
>     ,CatCreateFunction "replace" ["nvarchar", "nvarchar", "nvarchar"] False "nvarchar"
>     ,CatCreateFunction "patindex" ["char","char"] False "int4"
>     ,CatCreateFunction "patindex" ["varchar","varchar"] False "int4"
>     ,CatCreateFunction "patindex" ["nvarchar","nvarchar"] False "int4"
>     ,CatCreateFunction "isdate" ["varchar"] False "bool"
>     ,CatCreateFunction "isdate" ["char"] False "int4"
>     ,CatCreateFunction "isdate" ["nvarchar"] False "int4"
>     ,CatCreateFunction "len" ["nvarchar"] False "int4"
>     ]
>   where
>     -- find all the functions on int2 and replace int2 with int1
>     -- then find all the functions with int2 and int4, and
>     -- replace int2 with int1 and int4 with int2
>     -- really hacky
>     int1fns = let s = filter (\x -> replaceItp x && hasInt2 x)
>                              (deconstructCatalog defaultTemplate1Catalog)
>               in flip transformBi s $ \x -> case (x :: CatName) of
>                                        "int2" -> "int1"
>                                        _ -> x
>     int12fns = let s = filter (\x -> replaceItp x && hasInt2Int4 x)
>                               (deconstructCatalog defaultTemplate1Catalog)
>                in flip transformBi s $ \x -> case (x :: CatName) of
>                                        "int2" -> "int1"
>                                        "int4" -> "int2"
>                                        _ -> x
>     hasInt2 x = not $ null [() | ("int2" :: CatName) <- universeBi x]
>     hasInt2Int4 x = not $ null [() | ("int2" :: CatName) <- universeBi x
>                                    , ("int4" :: CatName) <- universeBi x]
>     replaceItp x = case x of
>                      CatCreateScalarType {} -> True
>                      CatCreateArrayType {} -> True
>                      CatCreatePrefixOp {} -> True
>                      CatCreateBinaryOp {} -> True
>                      CatCreateFunction f _ _ _ | f `elem` ["abs","float4","float8","int2","int4","mod","numeric"] -> True
>                      CatCreateAggregate f _ _ | f `elem` ["avg","max","min","sum"] -> True
>                      CatCreateCast a b c | a == "int2" || b == "int2" -> True
>                      CatCreateTypeCategoryEntry {} -> True
>                      _ -> False

comparisons with all ints
abs
cast functions
avg to numeric max min sum
catcreatecast: float4,float8, int4,int8,numeric, from

