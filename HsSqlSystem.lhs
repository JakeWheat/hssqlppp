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
getscope

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
> import ScopeReader

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
>       | (length args == 2 && head args == "getscope") -> getScope (args !! 1)
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

getTableViewTypes

> {-getTableViewTypes :: String -> IO ()
> getTableViewTypes dbName = withConn ("dbname=" ++ dbName) $ \conn -> do
>    putStrLn "module PGSetofTypes where\n"
>    putStrLn "import TypeType\n"
>    attrinfo <- selectRelation conn (typesCtesNamed ++
>                                     " select\n\
>                                     \        case cls.relkind\n\
>                                     \          when 'r' then 'Table'\n\
>                                     \          when 'v' then 'View'\n\
>                                     \          when 'c' then 'Composite'\n\
>                                     \        end || ' \"' || cls.relname || '\" ' ||\n\
>                                     \        '\n    [' || array_to_string(array_agg('(\"' || attname || '\",' || descr || ')')\n\
>                                     \           over (partition by relname order by attnum\n\
>                                     \                range between unbounded preceding and unbounded following), ',\n    ') || ']\n'\n\
>                                     \  from pg_attribute att\n\
>                                     \  inner join pg_class cls\n\
>                                     \    on cls.oid = attrelid\n\
>                                     \  inner join ts\n\
>                                     \    on ts.typoid = att.atttypid\n\
>                                     \  where\n\
>                                     \    pg_catalog.pg_table_is_visible(cls.oid)\n\
>                                     \    and cls.relkind in ('r','v','c')\n\
>                                     \    and not attisdropped\n\
>                                     \order by relkind, relname,attnum;") []
>    putStrLn "attrInfo :: [Type, [(String,Type)]]"
>    putStr "attrInfo = [\n    "
>    putStr $ intercalate ",\n    " $ map head attrinfo
>    putStrLn "]"

>    return ()-}

> getScope :: String -> IO ()
> getScope dbName = do
>   s <- readScope dbName
>   putStrLn "module DefaultScope where"
>   putStrLn "import Data.Map"
>   putStrLn "import TypeType"
>   putStrLn "import Scope"
>   putStrLn "defaultScope :: Scope"
>   putStr "defaultScope = "
>   print s

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
                                order by oid,argpos
                                range between unbounded preceding
                                and unbounded following)) as argtypes,
       ret,
from namedTypes
)
select oid,proname,argtypes,ret from combinedNamedArgs
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



