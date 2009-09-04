#! /usr/bin/env runhaskell

Copyright 2009 Jake Wheat

command line is
./HsSqlSystem.lhs [commandName] [commandArgs ...]

commands are:
loadsql
cleardb
clearandloadsql
lexfile
showfileatts
checkppp
roundtrip

command args:

loadsql [databasename] [filename]*
database must already exist, loads sql from files into database, via
parsing, checking and pretty printing

cleardb [databasename]
attempts to reset the database to empty, using a hack

clearandloadsql [databasename] [filename]*
runs cleardb then loadsql

lexfile [filename]
lexes the file given then displays each token on a separate line

showfileatts [filename]
parses then runs the attribute grammar processor over the ast,
displays all the values produced

checkppp [filename]
parses then pretty prints then parses the pretty printed output. Used
to check a file can parse, and that pretty printing then parsing gives
you the same ast.

roundtrip [filename] [targetfilename]

getfntables [databasename]
output function and operator tables for the parser and type checker
from the given database


TODO 1: add options to specify username and password (keep optional though)
TODO 2: think of a name for this command

> import System
> import System.IO
> import Control.Monad
> import System.Directory
> import Data.List

> import Parser
> import DatabaseLoader
> import Lexer
> import Ast
> import PrettyPrinter
> import DBAccess

================================================================================

= main

> main :: IO ()
> main = do
>   --do this to avoid having to put flushes everywhere when we
>   --provide "..." progress thingys, etc.
>   hSetBuffering stdout NoBuffering
>   args <- getArgs
>   when (length args == 0) $ error "no command given"
>   case () of
>     _ | (length args == 2 && head args == "cleardb") -> cleardb (args !! 1)
>       | (length args >= 3 && head args == "loadsql") -> loadsqlfiles args
>       | (length args >= 3 && head args == "clearandloadsql") ->
>            cleardb (args !! 1) >> loadsqlfiles args
>       | (length args == 2 && head args == "lexfile") -> lexFile (args !! 1)
>       | (length args >= 2 && head args == "showfileatts") -> showfileatts (tail args)
>       | (length args >= 2 && head args == "parsefile") -> parseFile (tail args)
>       | (length args == 3 && head args == "roundtrip") -> roundTripFile (tail args)
>       | (length args == 2 && head args == "getfntables") -> getFnTables (args !! 1)
>       | (length args == 2 && head args == "gettypestuff") -> getTypeStuff (args !! 1)
>       | (length args == 1 && head args == "checkfntypes") -> checkFnTypes
>       | otherwise -> error "couldn't parse command line"
>   where
>     loadsqlfiles args = mapM_ (loadSqlfile (args !! 1)) (tail $ tail args)


================================================================================

= load sql file

This takes a file full of sql from the disk and loads it into the
database given.

> loadSqlfile :: String -> String -> IO ()
> loadSqlfile db fn = do
>   res <- parseSqlFileWithState fn
>   case res of
>     Left er -> error $ show er
>     Right ast -> putStrLn ("loading " ++ fn)
>                  >> loadIntoDatabase db fn ast

================================================================================

= small hack utility to help with testing

TODO: use the correct username in this command
TODO: do something more correct

> cleardb :: String -> IO ()
> cleardb db = do
>   withConn ("dbname=" ++ db) $ \conn -> do
>     runSqlCommand conn "drop owned by jake cascade;"
>   putStrLn $ "database " ++ db ++ " cleared."


================================================================================

> lexFile :: FilePath -> IO ()
> lexFile f = do
>   putStrLn $ "lexing " ++ show f
>   x <- lexSqlFile f
>   return ()
>   case x of
>        Left er -> print er
>        Right l -> mapM_ print l

================================================================================

> showfileatts :: [String] -> IO ()
> showfileatts = mapM_ pf
>   where
>     pf f = do
>       putStrLn $ "parsing " ++ show f
>       x <- parseSqlFileWithState f
>       case x of
>            Left er -> print er
>            Right st -> do
>                mapM_ print st
>                putStrLn "\nchecking ast"
>                let y = checkAst st
>                print y
>       return ()

================================================================================

Routine to parse sql from a file, check that it appears to parse ok,
that pretty printing it and parsing that text gives the same ast,
and then displays the pretty printed version so you can see how well it's
done (maybe it could interpolate each original statement with its
parsed, pretty printed version so you can more easily check how
authentic the sql is and how much has been silently dropped on the floor

> parseFile :: [String] -> IO ()
> parseFile = mapM_ pf
>   where
>     pf f = do
>       putStrLn $ "parsing " ++ show f
>       x <- parseSqlFileWithState f
>       case x of
>            Left er -> print er
>            Right st -> do
>                --print l
>                --putStrLn "END OF AST END OF AST END OF AST END OF AST END OF AST END OF AST"
>                putStrLn "parse ok"
>                print st
>                let pp = printSql st
>                --putStrLn pp
>                --check roundtrip
>                case parseSql pp of
>                  Left er -> error $ "roundtrip failed: " ++ show er
>                  Right st' -> if resetSps' st == resetSps' st'
>                                then putStrLn "roundtrip ok"
>                                else putStrLn "roundtrip failed: different ast"
>       return ()

================================================================================

Used to test the parsing and pretty printing round trip. Takes two
arguments, a source filename and a target filename. If the target file
exists, it quits. Parses the source file then pretty prints it to the
target filename.

> roundTripFile :: [FilePath] -> IO ()
> roundTripFile args = do
>   when (length args /= 2) $
>          error "Please pass exactly two filenames, source and target."
>   let (source:target:[]) = args
>   targetExists <- doesFileExist target
>   when targetExists $
>          error "the target file name exists already, please delete it or choose a new filename"
>   x <- parseSqlFile source
>   case x of
>        Left er -> print er
>        Right l -> writeFile target $ printSql l

================================================================================

getFnTables

read the operators and functions from the catalog of the given database
output four values: binops, prefixops, postfixops, functions
each is a list with type ({functionName} String
                         ,{args} [Type]
                         ,{retType} Type)

> getFnTables :: [Char] -> IO ()
> getFnTables dbName = withConn ("dbname=" ++ dbName) $ \conn -> do
>    putStrLn "module FnTypes where\n"
>    putStrLn "import TypeType\n"

>    binopinfo <- selectRelation conn (typesCtesNamed ++
>                      "\n     select oprname,\n\
>                      \               tl.descr as left,\n\
>                      \               tr.descr as right,\n\
>                      \               tres.descr as result\n\
>                      \          from pg_operator\n\
>                      \          inner join ts tl\n\
>                      \            on oprleft = tl.typoid\n\
>                      \          inner join ts tr\n\
>                      \            on oprright = tr.typoid\n\
>                      \          inner join ts tres\n\
>                      \            on oprresult = tres.typoid\n\
>                      \          where oprleft <> 0 and oprright <> 0\n\
>                      \             and oprname <> '@' --hack for now\n\
>                      \          order by oprname;") []
>    putStrLn $ makeVal "binaryOperatorTypes" $ map (showProt . (\l -> (l!!0,[l!!1,l!!2],l!!3))) binopinfo

>    prefixopinfo <- selectRelation conn (typesCtesNamed ++
>                      "\n     select oprname,\n\
>                      \               tr.descr as right,\n\
>                      \               tres.descr as result\n\
>                      \          from pg_operator\n\
>                      \          inner join ts tr\n\
>                      \            on oprright = tr.typoid\n\
>                      \          inner join ts tres\n\
>                      \            on oprresult = tres.typoid\n\
>                      \          where oprleft = 0\n\
>                      \          order by oprname;") []
>    putStrLn $ makeVal "prefixOperatorTypes" $ map (showProt . (\l -> (l!!0,[l!!1],l!!2))) prefixopinfo

>    postfixopinfo <- selectRelation conn (typesCtesNamed ++
>                      "\n     select oprname,\n\
>                      \               tl.descr as left,\n\
>                      \               tres.descr as result\n\
>                      \          from pg_operator\n\
>                      \          inner join ts tl\n\
>                      \            on oprleft = tl.typoid\n\
>                      \          inner join ts tres\n\
>                      \            on oprresult = tres.typoid\n\
>                      \          where oprright = 0\n\
>                      \          order by oprname;") []
>    putStrLn $ makeVal "postfixOperatorTypes" $ map (showProt . (\l -> (l!!0,[l!!1],l!!2))) postfixopinfo

>    functionsinfo <- selectRelation conn (typesCtesNamed ++
>                       "\n,\n\
>                       \expandedArgs as (\n\
>                       \select pg_proc.oid,proname,proretset, generate_series as argpos,\n\
>                       \       proargtypes[generate_series] as argtype, prorettype\n\
>                       \from pg_proc\n\
>                       \cross join generate_series(0, (select max(array_upper(proargtypes, 1))\n\
>                       \                                  from pg_proc))\n\
>                       \where pg_catalog.pg_function_is_visible(pg_proc.oid)\n\
>                       \      and provariadic = 0\n\
>                       \      and not proisagg\n\
>                       \      and not proiswindow\n\
>                       \),\n\
>                       \filteredArgs as (\n\
>                       \select oid,proname,proretset, argpos,argtype, prorettype\n\
>                       \   from expandedArgs\n\
>                       \   where argtype is not null\n\
>                       \),\n\
>                       \namedTypes as (\n\
>                       \select oid,\n\
>                       \       proname,\n\
>                       \       argpos,\n\
>                       \       aty.descr as argtype,\n\
>                       \       case\n\
>                       \         when proretset\n\
>                       \           then 'SetOfType (' || rt.descr || ')'\n\
>                       \         else rt.descr\n\
>                       \       end as ret\n\
>                       \   from filteredArgs f\n\
>                       \   inner join ts rt\n\
>                       \     on rt.typoid = prorettype\n\
>                       \   inner join ts aty\n\
>                       \     on aty.typoid = argtype\n\
>                       \),\n\
>                       \combinedNamedArgs as (\n\
>                       \select oid, proname,\n\
>                       \       array_agg(argtype) over (partition by oid\n\
>                       \                                order by oid,argpos) as argtypes,\n\
>                       \       ret,\n\
>                       \       rank()  over (partition by oid\n\
>                       \                                order by oid desc,argpos desc) as rank\n\
>                       \from namedTypes\n\
>                       \)\n\
>                       \select oid,proname,\n\
>                       \  array_to_string(argtypes, ',') as argtypes,ret\n\
>                       \from combinedNamedArgs\n\
>                       \where rank = 1\n\
>                       \--we've lost the no args fns at this point, so whack em back in\n\
>                       \union\n\
>                       \select p.oid,\n\
>                       \       proname,\n\
>                       \       '' as argtypes,\n\
>                       \       case\n\
>                       \         when proretset\n\
>                       \           then 'SetOfType (' || rt.descr || ')'\n\
>                       \         else rt.descr\n\
>                       \       end as ret\n\
>                       \   from pg_proc p\n\
>                       \   inner join ts rt\n\
>                       \     on rt.typoid = prorettype\n\
>                       \  where pg_catalog.pg_function_is_visible(p.oid)\n\
>                       \      and provariadic = 0\n\
>                       \      and not proisagg\n\
>                       \      and not proiswindow\n\
>                       \      and pronargs = 0\n\
>                       \order by proname,argtypes;") []
>    putStrLn $ makeVal "functionTypes" $ map showProt $ filterOut $ map convFnLine functionsinfo
>    where
>      convFnLine l = (l!!1, toStrList (l!!2), l!!3)
>      toStrList ss = map trim $ split ',' ss
>      --showFn (n,a,r) = "(" ++ show n ++ ",[" ++ fixTypeArray a ++ "]," ++ r ++ ")"
>      --fixTypeArray a = replace "\\\"" "\"" (dropEnds a)
>      --dropEnds s = drop 1 $ reverse $ drop 1 $ reverse s

 >      parseTypeArray s =
 >        case parseTypeArray s of
 >                              Left er -> error $ show er
 >                              Right t -> t

>      filterOut =
>        filter
>          (\(_,args,ret) -> let ts = (ret:args)
>                            in case () of
>                              _ | length
>                                    (filter
>                                     (`elem`
>                                      ["Pseudo Internal"
>                                      ,"Pseudo LanguageHandler"
>                                      ,"Pseudo Opaque"]) ts) > 0 -> False
>                                | otherwise -> True)
>      showProt (n,a,r) = "(" ++ show n ++ ", [" ++ intercalate "," a ++ "], " ++ r ++ ")"
>      makeVal nm rows = nm ++ " = [\n    "
>                        ++ intercalate ",\n    " rows
>                        ++ "\n    ]"

================================================================================

= getTypeStuff

outputs type information and cast information

> getTypeStuff :: [Char] -> IO ()
> getTypeStuff dbName = withConn ("dbname=" ++ dbName) $ \conn -> do
>    putStrLn "module PGTypes where\n"
>    putStrLn "import TypeType\n"
>    typeinfo <- selectRelation conn (typesCtes ++
>                  "\nselect descr from nonArrayTypeNames\n\
>                  \union\n\
>                  \select descr from arrayTypeNames\n\
>                  \order by descr;") []
>    putStrLn "defaultTypeNames :: [Type]"
>    putStr "defaultTypeNames = [\n    "
>    putStr $ intercalate ",\n    " $ map head typeinfo
>    putStrLn "]"
>    castTable <- selectRelation conn (typesCtesNamed ++
>                                      "\nselect\n\
>                                      \   '(' || cs.descr  || ',' ||\n\
>                                      \   ct.descr || ',' ||\n\
>                                      \   case castcontext\n\
>                                      \     when 'i' then 'ImplicitCastContext'\n\
>                                      \     when 'a' then 'AssignmentCastContext'\n\
>                                      \     when 'e' then 'ExplicitCastContext'\n\
>                                      \   end || ')'\n\
>                                      \from pg_cast p\n\
>                                      \  inner join ts cs\n\
>                                      \    on p.castsource = cs.typoid\n\
>                                      \  inner join ts ct\n\
>                                      \    on p.casttarget = ct.typoid;") []
>    putStrLn "castTable :: [(Type, Type, CastContext)]"
>    putStr "castTable = [\n    "
>    putStr $ intercalate ",\n    " $ map head castTable
>    putStrLn "]"

>    typeCategories <- selectRelation conn (typesCtesNamed ++
>                        "\nselect '(' ||  ts.descr || ', \"' || t.typcategory\n\
>                        \       || '\", ' || case t.typispreferred\n\
>                        \                  when true then 'True'\n\
>                        \                  else 'False' end || ')'\n\
>                        \from ts inner join pg_type t\n\
>                        \     on ts.typoid = t.oid\n\
>                        \order by typcategory,typispreferred desc, descr;") []
>    putStrLn "typeCategories :: [(Type, [Char], Bool)]"
>    putStr "typeCategories = [\n    "
>    putStr $ intercalate ",\n    " $ map head typeCategories
>    putStrLn "]"

> typesCtes :: [Char]
> typesCtes =      "with nonArrayTypeNames as\n\
>                  \(select\n\
>                  \   t.oid as typoid,\n\
>                  \   case typtype\n\
>                  \       when 'b' then\n\
>                  \         'ScalarType \"' || typname || '\"'\n\
>                  \       when 'c' then\n\
>                  \         'CompositeType \"' || typname || '\"'\n\
>                  \       when 'd' then\n\
>                  \         'DomainType \"' || typname || '\"'\n\
>                  \       when 'e' then\n\
>                  \         'EnumType \"' || typname || '\"'\n\
>                  \       when 'p' then 'Pseudo ' ||\n\
>                  \         case typname\n\
>                  \           when 'any' then 'Any'\n\
>                  \           when 'anyarray' then 'AnyArray'\n\
>                  \           when 'anyelement' then 'AnyElement'\n\
>                  \           when 'anyenum' then 'AnyEnum'\n\
>                  \           when 'anynonarray' then 'AnyNonArray'\n\
>                  \           when 'cstring' then 'Cstring'\n\
>                  \           when 'internal' then 'Internal'\n\
>                  \           when 'language_handler' then 'LanguageHandler'\n\
>                  \           when 'opaque' then 'Opaque'\n\
>                  \           when 'record' then 'Record'\n\
>                  \           when 'trigger' then 'Trigger'\n\
>                  \           when 'void' then 'Void'\n\
>                  \           else 'error pseudo ' || typname\n\
>                  \         end\n\
>                  \       else 'typtype error ' || typtype\n\
>                  \    end as descr\n\
>                  \  from pg_catalog.pg_type t\n\
>                  \  where pg_catalog.pg_type_is_visible(t.oid)\n\
>                  \        and not exists(select 1 from pg_catalog.pg_type el\n\
>                  \                       where el.typarray = t.oid)),\n\
>                  \arrayTypeNames as\n\
>                  \(select\n\
>                  \    e.oid as typoid,\n\
>                  \    'ArrayType (' ||\n\
>                  \    n.descr || ')' as descr\n\
>                  \  from pg_catalog.pg_type t\n\
>                  \  inner join pg_type e\n\
>                  \    on t.typarray = e.oid\n\
>                  \  left outer join nonArrayTypeNames n\n\
>                  \    on t.oid = n.typoid\n\
>                  \  where pg_catalog.pg_type_is_visible(t.oid))"

> typesCtesNamed :: [Char]
> typesCtesNamed = typesCtes ++
>                      "\n,ts as (select typoid, descr from nonArrayTypeNames\n\
>                      \     union\n\
>                      \     select typoid, descr from arrayTypeNames)"


> checkFnTypes :: IO ()
> checkFnTypes = mapM_ print checkFunctionTypes

================================================================================

> replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
> replace _ _ [] = []
> replace old new xs@(y:ys) =
>   case stripPrefix old xs of
>     Nothing -> y : replace old new ys
>     Just ys' -> new ++ replace old new ys'


> split :: Char -> String -> [String]
> split _ ""                =  []
> split c s                 =  let (l, s') = break (== c) s
>                            in  l : case s' of
>                                            [] -> []
>                                            (_:s'') -> split c s''

> trim :: String -> String
> trim s = trimSWS $ reverse $ trimSWS $ reverse s
>        where
>          trimSWS :: String -> String
>          trimSWS = dropWhile (`elem` " \n\t")



                  with nonArrayTypeNames as
                  (select
                     t.oid as typoid,
                     case typtype
                         when 'b' then
                           'ScalarType "' || typname || '"'
                         when 'c' then
                           'CompositeType "' || typname || '"'
                         when 'd' then
                           'DomainType "' || typname || '"'
                         when 'e' then
                           'EnumType "' || typname || '"'
                         when 'p' then 'Pseudo ' ||
                           case typname
                             when 'any' then 'Any'
                             when 'anyarray' then 'AnyArray'
                             when 'anyelement' then 'AnyElement'
                             when 'anyenum' then 'AnyEnum'
                             when 'anynonarray' then 'AnyNonArray'
                             when 'cstring' then 'Cstring'
                             when 'internal' then 'Internal'
                             when 'language_handler' then 'LanguageHandler'
                             when 'opaque' then 'Opaque'
                             when 'record' then 'Record'
                             when 'trigger' then 'Trigger'
                             when 'void' then 'Void'
                             else 'error pseudo ' || typname
                           end
                         else 'typtype error ' || typtype
                      end as descr
                    from pg_catalog.pg_type t
                    where pg_catalog.pg_type_is_visible(t.oid)
                          and not exists(select 1 from pg_catalog.pg_type el
                                         where el.typarray = t.oid)),
                  arrayTypeNames as
                  (select
                      e.oid as typoid,
                      'ArrayType (' ||
                      n.descr || ')' as descr
                    from pg_catalog.pg_type t
                    inner join pg_type e
                      on t.typarray = e.oid
                    left outer join nonArrayTypeNames n
                      on t.oid = n.typoid
                    where pg_catalog.pg_type_is_visible(t.oid))
                  ,ts as (select typoid, descr from nonArrayTypeNames
                           union
                           select typoid, descr from arrayTypeNames
)

select '(' ||  ts.descr || ', "' || t.typcategory
       || '", ' || case t.typispreferred
                  when true then 'True'
                  else 'False' end || ')'
from ts inner join pg_type t
     on ts.typoid = t.oid
order by typcategory,typispreferred desc, descr
;












select
   '(' || cs.descr  || ',' ||
   ct.descr || ',' ||
   case castcontext
     when 'i' then 'ImplicitCastContext'
     when 'a' then 'AssignmentCastContext'
     when 'e' then 'ExplicitCastContext'
   end || ')'
from pg_cast p
  inner join ts cs
    on p.castsource = cs.typoid
  inner join ts ct
    on p.casttarget = ct.typoid;

 from pg_cast
 castsource | casttarget | castfunc | castcontext


expandedArgs as (
select pg_proc.oid,proname,proretset, generate_series as argpos,
       proargtypes[generate_series] as argtype, prorettype
from pg_proc
cross join generate_series(0, (select max(array_upper(proargtypes, 1))
                                  from pg_proc))
where pg_catalog.pg_function_is_visible(pg_proc.oid)
      and provariadic = 0
      and not proisagg
      and not proiswindow
),
filteredArgs as (
select oid,proname,proretset, argpos,argtype, prorettype
   from expandedArgs
   where argtype is not null
),
namedTypes as (
select oid,
       proname,
       argpos,
       aty.descr as argtype,
       case
         when proretset
           then 'SetOfType (' || rt.descr || ')'
         else rt.descr
       end as ret
   from filteredArgs f
   inner join ts rt
     on rt.typoid = prorettype
   inner join ts aty
     on aty.typoid = argtype
),
combinedNamedArgs as (
select oid, proname,
       array_agg(argtype) over (partition by oid
                                order by oid,argpos) as argtypes,
       ret,
       rank()  over (partition by oid
                                order by oid desc,argpos desc) as rank
from namedTypes
)
select oid,proname,argtypes,ret from combinedNamedArgs
where rank = 1
--we've lost the no args fns at this point, so whack em back in
union
select p.oid,
       proname,
       '{}'::text[] as argtypes,
       case
         when proretset
           then 'SetOfType (' || rt.descr || ')'
         else rt.descr
       end as ret
   from pg_proc p
   inner join ts rt
     on rt.typoid = prorettype
  where pg_catalog.pg_function_is_visible(p.oid)
      and provariadic = 0
      and not proisagg
      and not proiswindow
      and pronargs = 0

order by oid,proname
;



select count (*)
from pg_proc
where pg_catalog.pg_function_is_visible(pg_proc.oid)
      and provariadic = 0
      and not proisagg
      and not proiswindow;

