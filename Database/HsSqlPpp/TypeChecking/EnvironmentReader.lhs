Copyright 2009 Jake Wheat

This module contains the code to read a set of environment updates
from a database.

> {-# OPTIONS_HADDOCK hide  #-}



> module Database.HsSqlPpp.TypeChecking.EnvironmentReader
>     (readEnvironmentFromDatabase) where

> import qualified Data.Map as M
> import Data.Maybe
> import Control.Applicative

> import Database.HsSqlPpp.Dbms.DBAccess
> import Database.HsSqlPpp.TypeChecking.TypeType
> import Database.HsSqlPpp.Utils
> import Database.HsSqlPpp.TypeChecking.EnvironmentInternal

> -- | Creates an 'EnvironmentUpdate' list by reading the database given.
> -- To create an Environment value from this, use
> --
> -- @
> -- env <- readEnvironmentFromDatabase 'something'
> -- let newEnv = updateEnvironment defaultEnvironment env
> -- @
> readEnvironmentFromDatabase :: String -- ^ name of the database to read
>                             -> IO [EnvironmentUpdate]
> readEnvironmentFromDatabase dbName = withConn ("dbname=" ++ dbName) $ \conn -> do
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
>        --types = map snd typeAssoc
>        --typeNames = map (\(_,a,b) -> (b,a)) typeStuff
>    cts <- map (\(nm:cat:pref:[]) ->
>                EnvCreateScalar (ScalarType nm) cat ((read pref)::Bool)) <$>
>           selectRelation conn
>                        "select t.typname,typcategory,typispreferred\n\
>                        \from pg_type t\n\
>                        \where t.typarray<>0 and\n\
>                        \    typtype='b' and\n\
>                        \    pg_catalog.pg_type_is_visible(t.oid);" []
>    domainDefInfo <- selectRelation conn
>                       "select oid, typbasetype\n\
>                       \from pg_type where typtype = 'd'\n\
>                       \     and  pg_catalog.pg_type_is_visible(oid);" []
>    let jlt k = fromJust $ M.lookup k typeMap
>    let domainDefs = map (\l -> (jlt (l!!0),  jlt (l!!1))) domainDefInfo
>    --let domainCasts = map (\(t,b) ->(t,b,ImplicitCastContext)) domainDefs
>    castInfo <- selectRelation conn
>                  "select castsource,casttarget,castcontext from pg_cast;" []
>    let casts = {- domainCasts ++ -}  flip map castInfo
>                  (\l -> (jlt (l!!0), jlt (l!!1),
>                          case (l!!2) of
>                                      "a" -> AssignmentCastContext
>                                      "i" -> ImplicitCastContext
>                                      "e" -> ExplicitCastContext
>                                      _ -> error $ "internal error: unknown cast context " ++ (l!!2)))
>    --typeCatInfo <- selectRelation conn
>    --                    "select pg_type.oid, typcategory, typispreferred from pg_type\n\
>    --                    \where pg_catalog.pg_type_is_visible(pg_type.oid);" []
>    --let typeCats = flip map typeCatInfo
>    --                 (\l -> (jlt (l!!0), l!!1, read (l!!2)::Bool))
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


-- >    attrInfo <- selectRelation conn
-- >                   "select distinct\n\
-- >                   \   cls.relkind,\n\
-- >                   \   cls.relname,\n\
-- >                   \     array_to_string(\n\
-- >                   \       array_agg(attname || ';' || atttypid)\n\
-- >                   \          over (partition by relname order by attnum\n\
-- >                   \               range between unbounded preceding\n\
-- >                   \               and unbounded following)\n\
-- >                   \      ,',')\n\
-- >                   \ from pg_attribute att\n\
-- >                   \ inner join pg_class cls\n\
-- >                   \   on cls.oid = attrelid\n\
-- >                   \ where\n\
-- >                   \   pg_catalog.pg_table_is_visible(cls.oid)\n\
-- >                   \   and cls.relkind in ('r','v','c')\n\
-- >                   \   and not attisdropped\n\
-- >                   \   and attnum > 0\n\
-- >                   \ order by relkind, relname;" []
-- >    --let attrs = map (convAttrRow jlt) attrInfo

-- >    systemAttrInfo <- selectRelation conn
-- >                   "select distinct\n\
-- >                   \   cls.relkind,\n\
-- >                   \   cls.relname,\n\
-- >                   \     array_to_string(\n\
-- >                   \       array_agg(attname || ';' || atttypid)\n\
-- >                   \          over (partition by relname order by attnum\n\
-- >                   \               range between unbounded preceding\n\
-- >                   \               and unbounded following)\n\
-- >                   \      ,',')\n\
-- >                   \ from pg_attribute att\n\
-- >                   \ inner join pg_class cls\n\
-- >                   \   on cls.oid = attrelid\n\
-- >                   \ where\n\
-- >                   \   pg_catalog.pg_table_is_visible(cls.oid)\n\
-- >                   \   and cls.relkind in ('r','v','c')\n\
-- >                   \   and not attisdropped\n\
-- >                   \   and attnum < 0\n\
-- >                   \ order by relkind, relname;" []
-- >    --let systemAttrs = map (convAttrRow jlt) systemAttrInfo

>    comps <- map (\(kind:nm:atts:sysatts:[]) ->
>              case kind of
>                "c" -> EnvCreateComposite nm (convertAttString jlt atts)
>                "v" -> EnvCreateTable nm (convertAttString jlt atts) (convertAttString jlt sysatts)
>                "r" -> EnvCreateView nm (convertAttString jlt atts)
>                _ -> error $ "unrecognised relkind: " ++ kind) <$>
>                 selectRelation conn
>                   "with att1 as (\n\
>                   \ select\n\
>                   \     attrelid,\n\
>                   \     attname,\n\
>                   \     attnum,\n\
>                   \     atttypid\n\
>                   \   from pg_attribute\n\
>                   \   inner join pg_class cls\n\
>                   \      on cls.oid = attrelid\n\
>                   \   where pg_catalog.pg_table_is_visible(cls.oid)\n\
>                   \      and cls.relkind in ('r','v','c')\n\
>                   \      and not attisdropped),\n\
>                   \ sysAtt as (\n\
>                   \ select attrelid,\n\
>                   \     array_to_string(\n\
>                   \       array_agg(attname || ';' || atttypid)\n\
>                   \         over (partition by attrelid order by attnum\n\
>                   \               range between unbounded preceding\n\
>                   \               and unbounded following)\n\
>                   \       ,',') as sysAtts\n\
>                   \   from att1\n\
>                   \   where attnum < 0),\n\
>                   \ att as (\n\
>                   \ select attrelid,\n\
>                   \     array_to_string(\n\
>                   \       array_agg(attname || ';' || atttypid)\n\
>                   \          over (partition by attrelid order by attnum\n\
>                   \                range between unbounded preceding\n\
>                   \                and unbounded following)\n\
>                   \       ,',') as atts\n\
>                   \   from att1\n\
>                   \   where attnum > 0)\n\
>                   \ select distinct\n\
>                   \     cls.relkind,\n\
>                   \     cls.relname,\n\
>                   \     atts,\n\
>                   \     sysAtts\n\
>                   \   from att left outer join sysAtt using (attrelid)\n\
>                   \   inner join pg_class cls\n\
>                   \     on cls.oid = attrelid\n\
>                   \   order by relkind,relname;" []


>    return $ concat [
>                cts
>               ,map (uncurry EnvCreateDomain) domainDefs
>               ,map (\(a,b,c) -> EnvCreateCast a b c) casts
>               ,map (\(a,b,c) -> EnvCreateFunction FunPrefix a b c) prefixOps
>               ,map (\(a,b,c) -> EnvCreateFunction FunPostfix a b c) postfixOps
>               ,map (\(a,b,c) -> EnvCreateFunction FunBinary a b c) binaryOps
>               ,map (\(a,b,c) -> EnvCreateFunction FunName a b c) fnProts
>               ,map (\(a,b,c) -> EnvCreateFunction FunAgg a b c) aggProts
>               ,comps]
>    where
>      convertAttString jlt s =
>          let ps = split ',' s
>              ps1 = map (split ';') ps
>          in map (\pl -> (head pl, jlt (pl!!1))) ps1
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

