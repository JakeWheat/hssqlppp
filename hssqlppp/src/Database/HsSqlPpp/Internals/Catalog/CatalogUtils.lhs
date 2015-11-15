
some utilities which are used by catalog builder and by catalog
internals

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.Catalog.CatalogUtils
>        (catLookupType, getCatName) where

> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> --import Control.Monad
> --import Data.List
> --import Data.Data
> --import Data.Char
> --import Data.Maybe

> import qualified Data.Map as M
> import qualified Data.Set as S
> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Utils.Utils
> --import Data.Text (Text)
> --import qualified Data.Text as T
> --import qualified Data.Text.Lazy as LT
> --import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> --import Database.HsSqlPpp.Dialects.BaseCatalog

names to refer to the pseudo types. This is very postgresql
specific. Some of these should be deleted since hssqlppp has nothing
to do with them. Some of them will become postgresql dialect specific
and not appear here, and some we will repurpose to implement features
for non-postgresql dialects as well.

> pseudoTypes :: M.Map CatName Type
> pseudoTypes = M.fromList
>     [("any",Pseudo Any)
>     ,("anyarray",Pseudo AnyArray)
>     ,("anyelement",Pseudo AnyElement)
>     ,("anyenum",Pseudo AnyEnum)
>     ,("anyrange",Pseudo AnyRange)
>     ,("anynonarray",Pseudo AnyNonArray)
>     --,("cstring",Pseudo Cstring)
>     ,("record",Pseudo (Record Nothing))
>     --,("trigger",Pseudo Trigger)
>      -- todo: fix this?
>     --,("event_trigger",Pseudo Trigger)
>     ,("void",Pseudo Void)
>     --("_cstring",ArrayType $ Pseudo Cstring)
>     ,("_record",ArrayType $ Pseudo (Record Nothing))
>     --,("internal",Pseudo Internal)
>     --,("language_handler", Pseudo LanguageHandler)
>     --,("opaque", Pseudo Opaque)
>     --,("fdw_handler", Pseudo FdwHandler)
>     ]

> -- | takes a [NameComponent] and returns the type for that name
> -- will return a type not recognised if the type isn't in the catalog
> catLookupType :: Catalog -> [NameComponent] -> Either [TypeError] Type
> catLookupType cat ncs =
>   case getCatName ncs of
>     -- check if is a pseudo type
>     cn | Just p <- M.lookup cn pseudoTypes -> Right p
>     -- check for base, domain, enum or composite, and array
>        | S.member cn (catScalarTypeNames cat) -> Right $ ScalarType cn
>        | M.member cn (catDomainTypes cat) -> Right $ DomainType cn
>        | M.member cn (catCompositeTypes cat) -> Right $ NamedCompositeType cn
>        | Just t <- M.lookup cn (catArrayTypes cat) -> Right $ ArrayType $ ScalarType t
>        | otherwise -> Left [UnknownTypeName cn]

> getCatName :: [NameComponent] -> CatName
> getCatName [] = error "empty name component in catalog code"
> getCatName [x] = ncStrT x
> getCatName (_:xs) = getCatName xs
