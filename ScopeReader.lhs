Copyright 2009 Jake Wheat

This file contains the code to read all the scope data from a
postgresql database catalog. This is used to generate the default
scope, and to type check against a database schema in a live database.

It basically runs through each field in the Scope data type and reads
the values for that field out of the database, and turns it into the
appropriate data types.

Maps are use to hold oids during this process to e.g. hook up the
types of table columns (which are read as oids which refer to the
pg_type table) to the haskell values represent those types. These maps
are discarded after the Scope value is created.

> module ScopeReader (readScope) where

> import qualified Data.Map as M
> import Data.Maybe

> import Debug.Trace

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
>    let typeStuff = concatMap convTypeInfoRow typeInfo
>        typeAssoc = map (\(a,b,_) -> (a,b)) typeStuff
>        typeMap = M.fromList typeAssoc
>        types = map snd typeAssoc
>        typeNames = map (\(_,a,b) -> (b,a)) typeStuff

>    domainDefInfo <- selectRelation conn
>                       "select oid, typbasetype\n\
>                       \from pg_type where typtype = 'd'\n\
>                       \     and  pg_catalog.pg_type_is_visible(oid);" []
>    let jlt k = fromJust $ M.lookup k typeMap
>    let domainDefs = map (\l -> (jlt (l!!0),  jlt (l!!1))) domainDefInfo
>    let domainCasts = map (\(t,b) ->(t,b,ImplicitCastContext)) domainDefs
>    castInfo <- selectRelation conn
>                  "select castsource,casttarget,castcontext from pg_cast;" []
>    let casts = domainCasts ++ flip map castInfo
>                  (\l -> (jlt (l!!0), jlt (l!!1),
>                          case (l!!2) of
>                                      "a" -> AssignmentCastContext
>                                      "i" -> ImplicitCastContext
>                                      "e" -> ExplicitCastContext
>                                      _ -> error $ "internal error: unknown cast context " ++ (l!!2)))
>    typeCatInfo <- selectRelation conn
>                        "select pg_type.oid, typcategory, typispreferred from pg_type\n\
>                        \where pg_catalog.pg_type_is_visible(pg_type.oid);" []
>    let typeCats = flip map typeCatInfo
>                     (\l -> (jlt (l!!0), l!!1, read (l!!2)::Bool))
>    operatorInfo <- selectRelation conn
>                        "select oprname,\n\
>                        \       oprleft,\n\
>                        \       oprright,\n\
>                        \       oprresult\n\
>                        \from pg_operator\n\
>                        \      where not (oprleft <> 0 and oprright <> 0\n\
>                        \         and oprname = '@') --hack for now\n\
>                        \      order by oprname;" []
>    let getOps a b c [] = (a,b,c)
>        getOps pref post bin (l:ls) =
>          let bit = (\a -> (l!!0, a, jlt(l!!3)))
>          in case () of
>                   _ | l!!1 == "0" -> getOps (bit [jlt (l!!2)]:pref) post bin ls
>                     | l!!2 == "0" -> getOps pref (bit [jlt (l!!1)]:post) bin ls
>                     | otherwise -> getOps pref post (bit [jlt (l!!1), jlt (l!!2)]:bin) ls
>    let (prefixOps, postfixOps, binaryOps) = getOps [] [] [] operatorInfo
>    functionInfo <- selectRelation conn
>                       "select proname,\n\
>                       \       array_to_string(proargtypes,','),\n\
>                       \       proretset,\n\
>                       \       prorettype\n\
>                       \from pg_proc\n\
>                       \where pg_catalog.pg_function_is_visible(pg_proc.oid)\n\
>                       \      and provariadic = 0\n\
>                       \      and not proisagg\n\
>                       \      and not proiswindow\n\
>                       \order by proname,proargtypes;" []
>    let fnProts = map (convFnRow jlt) functionInfo

>    aggregateInfo <- selectRelation conn
>                       "select proname,\n\
>                       \       array_to_string(proargtypes,','),\n\
>                       \       proretset,\n\
>                       \       prorettype\n\
>                       \from pg_proc\n\
>                       \where pg_catalog.pg_function_is_visible(pg_proc.oid)\n\
>                       \      and provariadic = 0\n\
>                       \      and proisagg\n\
>                       \order by proname,proargtypes;" []
>    let aggProts = map (convFnRow jlt) aggregateInfo


>    attrInfo <- selectRelation conn
>                   "select distinct\n\
>                   \   cls.relkind,\n\
>                   \   cls.relname,\n\
>                   \     array_to_string(\n\
>                   \       array_agg(attname || ';' || atttypid)\n\
>                   \          over (partition by relname order by attnum\n\
>                   \               range between unbounded preceding\n\
>                   \               and unbounded following)\n\
>                   \      ,',')\n\
>                   \ from pg_attribute att\n\
>                   \ inner join pg_class cls\n\
>                   \   on cls.oid = attrelid\n\
>                   \ where\n\
>                   \   pg_catalog.pg_table_is_visible(cls.oid)\n\
>                   \   and cls.relkind in ('r','v','c')\n\
>                   \   and not attisdropped\n\
>                   \   and attnum > 0\n\
>                   \ order by relkind, relname;" []
>    let attrs = map (convAttrRow jlt) attrInfo

>    systemAttrInfo <- selectRelation conn
>                   "select distinct\n\
>                   \   cls.relkind,\n\
>                   \   cls.relname,\n\
>                   \     array_to_string(\n\
>                   \       array_agg(attname || ';' || atttypid)\n\
>                   \          over (partition by relname order by attnum\n\
>                   \               range between unbounded preceding\n\
>                   \               and unbounded following)\n\
>                   \      ,',')\n\
>                   \ from pg_attribute att\n\
>                   \ inner join pg_class cls\n\
>                   \   on cls.oid = attrelid\n\
>                   \ where\n\
>                   \   pg_catalog.pg_table_is_visible(cls.oid)\n\
>                   \   and cls.relkind in ('r','v','c')\n\
>                   \   and not attisdropped\n\
>                   \   and attnum < 0\n\
>                   \ order by relkind, relname;" []
>    let systemAttrs = map (convAttrRow jlt) systemAttrInfo

>    return $ Scope {scopeTypes = types
>                   ,scopeTypeNames = typeNames
>                   ,scopeDomainDefs = domainDefs
>                   ,scopeCasts = casts
>                   ,scopeTypeCategories = typeCats
>                   ,scopePrefixOperators = prefixOps
>                   ,scopePostfixOperators = postfixOps
>                   ,scopeBinaryOperators =  binaryOps
>                   ,scopeFunctions = fnProts
>                   ,scopeAggregates =  aggProts
>                   ,scopeAllFns = (prefixOps ++ postfixOps ++
>                                   binaryOps ++ fnProts ++ aggProts)
>                   ,scopeAttrDefs = attrs
>                   ,scopeAttrSystemColumns = systemAttrs
>                   ,scopeIdentifierTypes = []
>                   ,scopeJoinIdentifiers = []}

>    where
>      convAttrRow jlt l =
>         (l!!1, ty, atts)
>          where
>            ty = case l!!0 of
>                   "r" -> TableComposite
>                   "v" -> ViewComposite
>                   "c" -> Composite
>                   x -> error $ "internal error: unknown composite type: " ++ x
>            atts = let ps = split ',' (l!!2)
>                       ps1 = map (split ';') ps
>                   in UnnamedCompositeType $ map (\pl -> (head pl, jlt (pl!!1))) ps1
>      convFnRow jlt l =
>         (head l,fnArgs,fnRet)
>         where
>           fnRet = let rt1 = jlt (l!!3)
>                   in if read (l!!2)::Bool
>                        then SetOfType rt1
>                        else rt1
>           fnArgs = if (l!!1) == ""
>                      then []
>                      else let a = split ',' (l!!1)
>                           in map jlt a
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
>                                                  _ -> error $ "internal error: unknown pseudo " ++ t))
>                     _ -> error $ "internal error: unknown type type: " ++ (l !! 1)
>            scType = (head l, ctor name, name)
>        in if (l!!4) /= "0"
>           then [(l!!5,ArrayType $ ctor name, '_':name), scType]
>           else [scType]


> split :: Char -> String -> [String]
> split _ ""                =  []
> split c s                 =  let (l, s') = break (== c) s
>                            in  l : case s' of
>                                            [] -> []
>                                            (_:s'') -> split c s''
