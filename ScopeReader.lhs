
> module ScopeReader where

> import qualified Data.Map as M
> import Data.Maybe

> import DBAccess
> import Scope
> import TypeType


> readScope :: String -> IO Scope
> readScope dbName = withConn ("dbname=" ++ dbName) $ \conn -> do
>    typeInfo <- selectRelation conn
>                  "select t.oid as oid,\n\
>                  \       t.typtype,\n\
>                  \       t.typname,\n\
>                  \       t.typarray,\n\
>                  \       coalesce(e.typtype,'0') as atyptype,\n\
>                  \       e.oid as aoid,\n\
>                  \       e.typname as atypname\n\
>                  \  from pg_catalog.pg_type t\n\
>                  \  left outer join pg_type e\n\
>                  \    on t.typarray = e.oid\n\
>                  \  where pg_catalog.pg_type_is_visible(t.oid)\n\
>                  \   and not exists(select 1 from pg_catalog.pg_type el\n\
>                  \                       where el.typarray = t.oid)\n\
>                  \  order by t.typname;" []
>    let types = concatMap convTypeInfoRow typeInfo
>        typeMap = M.fromList $ zip (map head typeInfo) types
>    castInfo <- selectRelation conn
>                  "select castsource,casttarget,castcontext from pg_cast;" []
>    let jlt k = fromJust $ M.lookup k typeMap
>    let casts = flip map castInfo
>                  (\l -> (jlt (l!!0), jlt (l!!1),
>                          case (l!!2) of
>                                      "a" -> AssignmentCastContext
>                                      "i" -> ImplicitCastContext
>                                      "e" -> ExplicitCastContext
>                                      _ -> error $ "unknown cast context " ++ (l!!2)))
>    return $ Scope types casts M.empty
>    where
>      convTypeInfoRow l =
>        let name = (l!!2)
>            ctor = case (l!!1) of
>                     "b" -> ScalarType
>                     "c" -> CompositeType
>                     "d" -> DomainType
>                     "e" -> EnumType
>                     "p" -> (\t -> Pseudo (case t of
>                                                  "any" -> Any
>                                                  "anyarray" -> AnyArray
>                                                  "anyelement" -> AnyElement
>                                                  "anyenum" -> AnyEnum
>                                                  "anynonarray" -> AnyNonArray
>                                                  "cstring" -> Cstring
>                                                  "internal" -> Internal
>                                                  "language_handler" -> LanguageHandler
>                                                  "opaque" -> Opaque
>                                                  "record" -> Record
>                                                  "trigger" -> Trigger
>                                                  "void" -> Void
>                                                  _ -> error $ "unknown pseudo " ++ t))
>                     _ -> error $ "unknown type type: " ++ (l !! 1)
>        in if (l!!4) /= "0"
>           then [ArrayType $ ctor name, ctor name]
>           else [ctor name]
