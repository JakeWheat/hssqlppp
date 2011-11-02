
This module contains the code to read a set of catalog updates from a
database. This can be applied to the default catalog to be able to
typecheck against that database.


> {-# LANGUAGE QuasiQuotes #-}
>
> module Database.HsSqlPpp.Utils.CatalogReader
>     (readCatalogFromDatabase) where
>
> import qualified Data.Map as M
> import Data.Maybe
> import Control.Applicative
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Utils.PgUtils
> --import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Data.List.Split
>
> -- | Creates an 'CatalogUpdate' list by reading the database given.
> -- To create an Catalog value from this, use
> --
> -- @
> -- cat <- readCatalogFromDatabase 'something'
> -- let newCat = updateCatalog defaultCatalog cat
> -- @
> readCatalogFromDatabase :: String -- ^ connection string of the database to read
>                             -> IO [CatalogUpdate]
> readCatalogFromDatabase cs = withConn cs $ \conn -> do


>   scalarTypeNames <-
>     map (CatCreateScalarType . head) `fmap`
>         selectRelation conn [here|
\begin{code}

select  case nspname
         when 'public' then t.typname
         when 'pg_catalog' then t.typname
         else nspname || '.' || t.typname
       end as typname
from pg_catalog.pg_type t
  inner join pg_namespace ns
      on t.typnamespace = ns.oid
where typtype = 'b'
  and ns.nspname in ('pg_catalog'
                    ,'public'
                    ,'information_schema')
  and not exists(select 1 from pg_catalog.pg_type el
                    where el.typarray = t.oid)
  order by t.typname;

\end{code}
>                                     |] []



>   domainTypes <-
>     map (\[d,b] -> CatCreateDomainType d b) `fmap`
>         selectRelation conn [here|
\begin{code}

select case ns.nspname
         when 'public' then t.typname
         when 'pg_catalog' then t.typname
         else ns.nspname || '.' || t.typname
       end as typname,
       case bns.nspname
         when 'public' then bt.typname
         when 'pg_catalog' then bt.typname
         else bns.nspname || '.' || bt.typname
       end as basename
from pg_catalog.pg_type t
  inner join pg_namespace ns
      on t.typnamespace = ns.oid
  inner join pg_catalog.pg_type bt
      on t.typbasetype = bt.oid
  inner join pg_namespace bns
      on bt.typnamespace = bns.oid
where t.typtype = 'd'
  and ns.nspname in ('pg_catalog'
                    ,'public'
                    ,'information_schema')
  and not exists(select 1 from pg_catalog.pg_type el
                    where el.typarray = t.oid)
  order by t.typname;

\end{code}
>                                     |] []

>   arrayTypes <-
>     map ( \[nm,bs] -> CatCreateArrayType nm bs) `fmap`
>         selectRelation conn [here|
\begin{code}

select e.typname as arraytype,
       t.typname as basetype
  from pg_catalog.pg_type t
  inner join pg_type e
    on t.typarray = e.oid
   inner join pg_namespace ns
      on t.typnamespace = ns.oid
         and ns.nspname in ('pg_catalog'
                           ,'public'
                           ,'information_schema')
  order by t.typname;

\end{code}
>                                     |] []

>   prefixOps <-
>     map ( \[nm,rt,res] -> CatCreatePrefixOp nm rt res) `fmap`
>         selectRelation conn [here|
\begin{code}
select oprname,
       rt.typname,
       res.typname
from pg_operator
inner join pg_type rt
  on oprright = rt.oid
inner join pg_type res
  on oprresult = res.oid
order by oprname;
\end{code}
>                                     |] []

>   postfixOps <-
>     map ( \[nm,lt,res] -> CatCreatePostfixOp nm lt res) `fmap`
>         selectRelation conn [here|
\begin{code}
select oprname,
       lt.typname,
       res.typname
from pg_operator
inner join pg_type lt
  on oprleft = lt.oid
inner join pg_type res
  on oprresult = res.oid
order by oprname;
\end{code}
>                                     |] []

>   binaryOps <-
>     map ( \[nm,lt,rt,res] -> CatCreateBinaryOp nm lt rt res) `fmap`
>         selectRelation conn [here|
\begin{code}
select oprname,
       lt.typname,
       rt.typname,
       res.typname
from pg_operator
inner join pg_type lt
  on oprleft = lt.oid
inner join pg_type rt
  on oprright = rt.oid
inner join pg_type res
  on oprresult = res.oid
where not oprname = '@' --hack for now
order by oprname;
\end{code}
>                                     |] []

>   fns <-
>     map ( \[nm,ts,pr,res] -> CatCreateFunction nm (splitOn "," ts) (pr == "t") res) `fmap`
>         selectRelation conn [here|
\begin{code}
-- maybe the args will come out in the right order?
with typenames as (
select pg_type.oid as toid,typname from pg_type
inner join pg_namespace ns
      on typnamespace = ns.oid
where
  ns.nspname in ('pg_catalog'
                ,'public'
                ,'information_schema')
),
unnestargs as (
select oid as prooid,
       proname,
       unnest(proargtypes) as arg,
       proretset,
       prorettype
from pg_proc
where pg_catalog.pg_function_is_visible(pg_proc.oid)
      and provariadic = 0
      and not proisagg
      and not proiswindow
),
namedtypes as (
select prooid,
       proname,
       arg.typname as argname,
       proretset,
       ret.typname as retname
from unnestargs
  inner join typenames arg
    on arg = arg.toid
  inner join typenames ret
    on prorettype = ret.toid)
select proname,
       array_to_string(array_agg(argname),','),
       proretset,
       retname
from namedtypes
group by prooid,proname,proretset,retname;
\end{code}
>                                     |] []


>   return $ concat [scalarTypeNames
>                   ,domainTypes
>                   ,arrayTypes
>                   ,prefixOps
>                   ,postfixOps
>                   ,binaryOps
>                   ,fns]







>    {-typeInfo <- selectRelation conn [here|
\begin{code}

select t.oid as oid,
       t.typtype,
       case nspname
         when 'public' then t.typname
         when 'pg_catalog' then t.typname
         else nspname || '.' || t.typname
       end as typname,
       t.typarray,
       coalesce(e.typtype,'0') as atyptype,
       e.oid as aoid,
       e.typname as atypname
  from pg_catalog.pg_type t
  left outer join pg_type e
    on t.typarray = e.oid
   inner join pg_namespace ns
      on t.typnamespace = ns.oid
         and ns.nspname in ('pg_catalog'
                           ,'public'
                           ,'information_schema')
  where /*pg_catalog.pg_type_is_visible(t.oid)
   and */not exists(select 1 from pg_catalog.pg_type el
                       where el.typarray = t.oid)
  order by t.typname;

\end{code}
>                |] []
>    let typeStuff = concatMap convTypeInfoRow typeInfo
>        typeAssoc = map (\(a,b,_) -> (a,b)) typeStuff
>        typeMap = M.fromList typeAssoc
>    cts <- map (\(nm:cat:pref:[]) ->
>                CatCreateScalar (ScalarType nm) cat ( read pref :: Bool)) <$>
>           selectRelation conn [here|
\begin{code}

select t.typname,typcategory,typispreferred
from pg_type t
   inner join pg_namespace ns
      on t.typnamespace = ns.oid
         and ns.nspname in ('pg_catalog', 'public', 'information_schema')
where t.typarray<>0 and
    typtype='b' /*and
    pg_catalog.pg_type_is_visible(t.oid)*/;

\end{code}
>                |] []
>    domainDefInfo <- selectRelation conn [here|
\begin{code}

select pg_type.oid, typbasetype
  from pg_type
  inner join pg_namespace ns
      on pg_type.typnamespace = ns.oid
         and ns.nspname in ('pg_catalog', 'public', 'information_schema')
 where typtype = 'd'
     /*and  pg_catalog.pg_type_is_visible(oid)*/;

\end{code}
>                |] []
>    let jlt k = fromJust $ M.lookup k typeMap
>    let domainDefs = map (\l -> (jlt (l!!0),  jlt (l!!1))) domainDefInfo
>    --let domainCasts = map (\(t,b) ->(t,b,ImplicitCastContext)) domainDefs
>    castInfo <- selectRelation conn
>                  "select castsource,casttarget,castcontext from pg_cast;" []
>    let casts =
>      {- domainCasts ++ -}
>          flip map castInfo
>               (\l -> (jlt (l!!0)
>                      ,jlt (l!!1)
>                      ,case (l!!2) of
>                                   "a" -> AssignmentCastContext
>                                   "i" -> ImplicitCastContext
>                                   "e" -> ExplicitCastContext
>                                   _ -> error $ "internal error: unknown \
>                                                \cast context " ++ (l!!2)))
>    operatorInfo <- selectRelation conn [here|
\begin{code}

select oprname,
       oprleft,
       oprright,
       oprresult
from pg_operator
      where not (oprleft <> 0 and oprright <> 0
         and oprname = '@') --hack for now
      order by oprname;

\end{code}
>                |] []
>    let getOps a b c [] = (a,b,c)
>        getOps pref post bin (l:ls) =
>          let bit = (\a -> (l!!0, a, jlt(l!!3)))
>          in case () of
>                   _ | l!!1 == "0"
>                         -> getOps (bit [jlt (l!!2)]:pref) post bin ls
>                     | l!!2 == "0"
>                         -> getOps pref (bit [jlt (l!!1)]:post) bin ls
>                     | otherwise -> getOps pref post (bit [jlt (l!!1)
>                                                          ,jlt (l!!2)]:bin) ls
>    let (prefixOps, postfixOps, binaryOps) = getOps [] [] [] operatorInfo
>    functionInfo <- selectRelation conn [here|
\begin{code}

select proname,
       array_to_string(proargtypes,','),
       proretset,
       prorettype
from pg_proc
where pg_catalog.pg_function_is_visible(pg_proc.oid)
      and provariadic = 0
      and not proisagg
      and not proiswindow
order by proname,proargtypes;
\end{code}
>                |] []
>    let fnProts = map (convFnRow jlt) functionInfo
>    aggregateInfo <- selectRelation conn [here|
\begin{code}

select proname,
       array_to_string(proargtypes,','),
       proretset,
       prorettype
from pg_proc
where pg_catalog.pg_function_is_visible(pg_proc.oid)
      and provariadic = 0
      and proisagg
order by proname,proargtypes;
\end{code}
>                |] []
>    let aggProts = map (convFnRow jlt) aggregateInfo
>    windowInfo <- selectRelation conn [here|
\begin{code}

select proname,
       array_to_string(proargtypes,','),
       proretset,
       prorettype
from pg_proc
where pg_catalog.pg_function_is_visible(pg_proc.oid)
      and provariadic = 0
      and proiswindow
order by proname,proargtypes;

\end{code}
>                |] []
>    let winProts = map (convFnRow jlt) windowInfo
>    comps <- map (\(kind:nm:atts:sysatts:nsp:[]) ->
>              let nm1 = case nsp of
>                                 "pg_catalog" -> nm
>                                 "public" -> nm
>                                 n -> n ++ "." ++ nm
>              in case kind of
>                     "c" -> CatCreateComposite nm1 (convertAttString jlt atts)
>                     "r" -> CatCreateTable nm1 (convertAttString jlt atts)
>                                               (convertAttString jlt sysatts)
>                     "v" -> CatCreateView nm1 (convertAttString jlt atts)
>                     _ -> error $ "unrecognised relkind: " ++ kind) <$>
>                 selectRelation conn [here|
\begin{code}

with att1 as (
 select
     attrelid,
     attname,
     attnum,
     atttypid
   from pg_attribute
   inner join pg_class cls
      on cls.oid = attrelid
   inner join pg_namespace ns
      on cls.relnamespace = ns.oid
         and ns.nspname in ('pg_catalog', 'public', 'information_schema')
   where /*pg_catalog.pg_table_is_visible(cls.oid)
      and*/ cls.relkind in ('r','v','c')
      and not attisdropped),
 sysAtt as (
 select attrelid,
     array_to_string(
       array_agg(attname || ';' || atttypid)
         over (partition by attrelid order by attnum
               range between unbounded preceding
               and unbounded following)
       ,',') as sysAtts
   from att1
   where attnum < 0),
 att as (
 select attrelid,
     array_to_string(
       array_agg(attname || ';' || atttypid)
          over (partition by attrelid order by attnum
                range between unbounded preceding
                and unbounded following)
       ,',') as atts
   from att1
   where attnum > 0)
 select distinct
     cls.relkind,
     cls.relname,
     atts,
     coalesce(sysAtts,''),
     nspname
   from att left outer join sysAtt using (attrelid)
   inner join pg_class cls
     on cls.oid = attrelid
   inner join pg_namespace ns
      on cls.relnamespace = ns.oid
   order by relkind,relname;

\end{code}
>                |] []
>    return
>      $ concat [cts
>               ,map (uncurry CatCreateDomain) domainDefs
>               ,map (\(a,b,c) -> CatCreateCast a b c) casts
>               ,map (\(a,b,c) ->
>                     CatCreateFunction FunPrefix a b c False) prefixOps
>               ,map (\(a,b,c) ->
>                     CatCreateFunction FunPostfix a b c False) postfixOps
>               ,map (\(a,b,c) ->
>                     CatCreateFunction FunBinary a b c False) binaryOps
>               ,map (\(a,b,c) ->
>                     CatCreateFunction FunName a b c False) fnProts
>               ,map (\(a,b,c) ->
>                     CatCreateFunction FunAgg a b c False) aggProts
>               ,map (\(a,b,c) ->
>                     CatCreateFunction FunWindow a b c False) winProts
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
>                        then Pseudo (SetOfType rt1)
>                        else rt1
>           fnArgs = if (l!!1) == ""
>                      then []
>                      else let a = split ',' (l!!1)
>                           in map jlt a
>      convTypeInfoRow l =
>        let name = canonicalizeTypeName (l!!2)
>            ctor = case (l!!1) of
>                     "b" -> ScalarType
>                     "c" -> NamedCompositeType
>                     "d" -> DomainType
>                     "e" -> EnumType
>                     "p" -> Pseudo . pn
>                     _ -> error $ "internal error: unknown type type: "
>                          ++ (l !! 1)
>            scType = (head l, ctor name, name)
>        in if (l!!4) /= "0"
>           then [(l!!5,ArrayType $ ctor name, '_':name), scType]
>           else [scType]
>      pn t = case t of
>                    "any" -> Any
>                    "anyarray" -> AnyArray
>                    "anyelement" -> AnyElement
>                    "anyenum" -> AnyEnum
>                    "anynonarray" -> AnyNonArray
>                    "cstring" -> Cstring
>                    "internal" -> Internal
>                    "language_handler" -> LanguageHandler
>                    "opaque" -> Opaque
>                    "record" -> Record
>                    "trigger" -> Trigger
>                    "void" -> Void
>                    "fdw_handler" -> FdwHandler
>                    _ -> error $ "internal error: unknown pseudo " ++ t -}

> {-split :: Char -> String -> [String]
> split _ ""                =  []
> split c s                 =  let (l, s') = break (== c) s
>                            in  l : case s' of
>                                            [] -> []
>                                            (_:s'') -> split c s''-}


select pg_type.oid as toid,typname from pg_type
inner join pg_namespace ns
      on typnamespace = ns.oid
where
  ns.nspname in ('pg_catalog'
                ,'public'
                ,'information_schema')
  and typname = 'internal';
