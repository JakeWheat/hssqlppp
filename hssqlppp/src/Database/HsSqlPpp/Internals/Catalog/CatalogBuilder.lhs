
> {-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Internals.Catalog.CatalogBuilder
>     (updateCatalog
>     ,deconstructCatalog
>      -- todo: temporary export before the basecatalog is fixed not to use
>      -- the catalog internals
>     ,insertOperators
>     ) where

>
> import Control.Monad
> --import Data.List
> --import Data.Data
> --import Data.Char
> --import Data.Maybe

> import qualified Data.Map as M
> import qualified Data.Set as S
> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Utils.Utils
> --import Data.Text (Text)
> import qualified Data.Text as T
> --import qualified Data.Text.Lazy as LT

> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> --import Database.HsSqlPpp.Dialects.BaseCatalog
> import Database.HsSqlPpp.Internals.Catalog.CatalogUtils

> -- | Applies a list of 'CatalogUpdate's to an 'Catalog' value
> -- to produce a new Catalog value. TODO: there will be a split
> -- between the individual low level updates which just update
> -- one 'row' in the catalog type, and the high level updates
> -- which correspond to ddl (e.g. create type will also add the
> -- array type, create table will add a table, supply the
> -- private columns automatically, and add the composite type)
> -- highlevel not implemented yet. You must use the correct case and
> -- the canonical names for identifiers/types

> updateCatalog :: [CatalogUpdate]
>               -> Catalog
>               -> Either [TypeError] Catalog
> updateCatalog eus cat' =
>   foldM updateCat' (cat' {catUpdates = catUpdates cat' ++ eus}) eus
>   where
>     updateCat' cat u = case u of
>       CatCreateSchema n ->
>         if S.member n (catSchemas cat)
>         then Left [SchemaAlreadyExists n]
>         else Right $ cat {catSchemas = S.insert n (catSchemas cat)}
>       CatCreateScalarType n ->
>         if S.member n (catScalarTypeNames cat)
>         -- todo: need to check all the type lists
>         -- and maybe need to check the name doesn't conflict with pseudo names or something?
>         -- this should happen with other cases as well
>         -- also: needs to take into account alias, so int and int4 are
>         -- both disallowed for new types, and lookup of either finds int4
>         then Left [InternalError $ "type already exists: " ++ show n]
>         else Right $ cat {catScalarTypeNames = S.insert n (catScalarTypeNames cat)}
>       CatCreateDomainType n b ->
>         Right $ cat {catDomainTypes = M.insert n b (catDomainTypes cat)}
>       CatCreateArrayType n b ->
>         Right $ cat {catArrayTypes = M.insert n b (catArrayTypes cat)}
>       -- todo: check the uniqueness of operator names (can overload by type)
>       -- also check the name of the operator is a valid operator name
>       -- and that the op has the correct number of args (1 or 2 resp.)
>       CatCreatePrefixOp n lt ret -> do
>         ltt <- catLookupType cat [QNmc $ T.unpack lt]
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catPrefixOps = insertOperators
>                                     [(n,(n,[ltt],rett,False))]
>                                     (catPrefixOps cat)}
>       CatCreatePostfixOp n rt ret -> do
>         rtt <- catLookupType cat [QNmc $ T.unpack rt]
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catPostfixOps = insertOperators
>                                      [(n,(n,[rtt],rett,False))]
>                                      (catPostfixOps cat)}
>       CatCreateBinaryOp n lt rt ret -> do
>         ltt <- catLookupType cat [QNmc $ T.unpack lt]
>         rtt <- catLookupType cat [QNmc $ T.unpack rt]
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catBinaryOps = insertOperators
>                                     [(n,(n,[ltt,rtt],rett,False))]
>                                     (catBinaryOps cat)}

>       CatCreateSpecialOp n ps rs ret -> do
>         pst <- mapM (\nc -> catLookupType cat [QNmc $ T.unpack nc]) ps
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         let rett' = if rs
>                     then Pseudo $ SetOfType rett
>                     else rett
>         -- thrown into the binary ops atm, todo: add a new namespace
>         Right $ cat {catBinaryOps = insertOperators
>                                     [(n,(n,pst,rett',False))]
>                                     (catBinaryOps cat)}
>       CatCreateFunction n ps rs ret -> do
>         pst <- mapM (\nc -> catLookupType cat [QNmc $ T.unpack nc]) ps
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         let rett' = if rs
>                     then Pseudo $ SetOfType rett
>                     else rett
>         Right $ cat {catFunctions = insertOperators
>                                     [(n,(n,pst,rett',False))]
>                                     (catFunctions cat)}
>       -- this wraps the last parameter in the array type for now
>       CatCreateVariadicFunction n ps rs ret -> do
>         let promoteLastType [] = []
>             promoteLastType [a] = [ArrayType a]
>             promoteLastType (a:as) = a : promoteLastType as
>         pst <- promoteLastType `fmap` mapM (\nc -> catLookupType cat [QNmc $ T.unpack nc]) ps
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         let rett' = if rs
>                     then Pseudo $ SetOfType rett
>                     else rett
>         Right $ cat {catFunctions = insertOperators
>                                     [(n,(n,pst,rett',True))]
>                                     (catFunctions cat)}
>       CatCreateAggregate n ps ret -> do
>         pst <- mapM (\nc -> catLookupType cat [QNmc $ T.unpack nc]) ps
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catAggregateFunctions = insertOperators
>                                     [(n,(n,pst,rett,False))]
>                                     (catAggregateFunctions cat)}
>       CatCreateTable n cs -> do
>         cts <- mapM (\(cn,te) -> do
>                        t' <- catLookupType cat [QNmc $ T.unpack $ catName te]
>                        -- for composite types, the information added here (about precision
>                        --   and nullability) is redundant
>                        let te' = TypeExtra t' (catPrecision te) (catScale te) (catNullable te)
>                        return (cn,te')) cs
>         Right $ cat {catTables = M.insert n (cts,[]) (catTables cat)}
>       CatCreateCast n0 n1 ctx -> do
>         t0 <- catLookupType cat [QNmc $ T.unpack n0]
>         t1 <- catLookupType cat [QNmc $ T.unpack n1]
>         Right $ cat {catCasts = S.insert (t0,t1,ctx) (catCasts cat)}
>       CatCreateTypeCategoryEntry n (c,p) -> do
>         t <- catLookupType cat [QNmc $ T.unpack n]
>         Right $ cat {catTypeCategories = M.insert t (c,p) $ catTypeCategories cat}

> deconstructCatalog :: Catalog -> [CatalogUpdate]
> deconstructCatalog = catUpdates

> insertOperators :: [(CatName,OperatorPrototype)]
>                 -> M.Map CatName [OperatorPrototype]
>                 -> M.Map CatName [OperatorPrototype]
> insertOperators vs m =
>   foldr i m vs
>   where
>     i (k,v) = M.insertWith (++) k [v]
