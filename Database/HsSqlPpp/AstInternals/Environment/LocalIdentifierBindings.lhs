Copyright 2009 Jake Wheat

This module contains the code to manage local identifier bindings
during the type checking process.

Main areas to support are parameters and variables


> module Database.HsSqlPpp.AstInternals.Environment.LocalIdentifierBindings
>     (
>      QualifiedIDs
>     ,LocalIdentifierBindings
>     ,emptyBindings
>     ,updateBindings
>     ,LocalIdentifierBindingsUpdate(..)
>     ,libExpandStar
>     ,libLookupID
>     ) where

> import Control.Monad
> import Data.List
> import Debug.Trace

> import Database.HsSqlPpp.AstInternals.TypeType
> import Database.HsSqlPpp.Utils
> import Database.HsSqlPpp.AstInternals.Environment.EnvironmentInternal

> -- | The main datatype, this holds the catalog and context
> -- information to type check against.
> data LocalIdentifierBindings = LocalIdentifierBindings
>                    {identifierTypes :: [[QualifiedIDs]]
>                    ,starTypes :: [QualifiedIDs]}


> -- | Represents an empty environment. This doesn't contain things
> -- like the \'and\' operator, and so if you try to use it it will
> -- almost certainly not work.
> emptyBindings :: LocalIdentifierBindings
> emptyBindings = LocalIdentifierBindings [] []

> -- | Represents the types of the ids available, currently used for
> -- resolving identifiers inside select expressions. Will probably
> -- change as this is fixed to support more general contexts.  The
> -- components represent the qualifying name (empty string for no
> -- qualifying name), the list of identifier names with their types,
> -- and the list of system column identifier names with their types.
> type QualifiedIDs = (String, [(String,Type)])




= Attribute identifier scoping

The way this scoping works is we have a list of prefixes/namespaces,
which is generally the table/view name, or the alias given to it, and
then a list of identifiers (with no dots) and their types. When we
look up the type of an identifier, if it has an correlation name we
try to match that against a table name or alias in that list, if it is
not present or not unique then throw an error. Similarly with no
correlation name, we look at all the lists, if the id is not present
or not unique then throw an error.

envIdentifierTypes is for expanding *. If we want to access the
common attributes from one of the tables in a using or natural join,
this attribute can be qualified with either of the table names/
aliases. But when we expand the *, we only output these common fields
once, so keep a separate list of these fields used just for expanding
the star. The other twist is that these common fields appear first in
the resultant field list.

System columns: pg also has these - they have names and types like
other attributes, but are not included when expanding stars, so you
only get them when you explicitly ask for them. The main use is using
the oid system column which is heavily used as a target for foreign
key references in the pg catalog.

This system still isn't working right. Subqueries are a
problem. Aspects which don't work right now are:

consider this query:
select relname as relvar_name
    from pg_class
    where ((relnamespace =
           (select oid
              from pg_namespace
              where (nspname = 'public'))) and (relkind = 'r'));

we need to be able to access attributes from pg_class inside the subquery,
but 1) they aren't inserted if you use * in the inner query
2) they can't make an identifier ambiguous, so the oid here in the subquery
is ok even though both the oid from pg_namespace and the oid from pg_class
are in scope.

So there are two problems with the current code:
it's too aggressive at throwing ambiguous identifier errors
it pulls in too many identifiers when expanding star

Solution ideas:
for the ambiguous errors, create a stack of identifiers, then split
the EnvUpdateIDs into two, one to replace the current set, and one to
push a new set on the stack. Then fix the lookup to walk the stack level by level.

for the *, we already have special cases for system columns, and for
join ids. I think the best solution is to provide a separate list of *
columns and types, with a separate env update ctor, and get the type
checker to resolve the list for * expansion rather than doing it here.

This should also handle parameters and variable declarations in plpgsql
functions too, these stack in the same way, with one complication to
do with parameters:

there is an additional complication with plpgsql, which isn't going to
be handled for now: instead of stacking like everything else, for
variable references inside select, insert, update and delete
statements only, which aren't qualified and match a parameter name,
then the parameter is used in lieu of variable declarations or
attributes inside a select expression. This will be handled at some
point.

this is something the lint checker should flag when it's written, it
will also flag any ambiguous identifiers which resolve ok only because
of stacking, this is a standard warning in many flavours of lint
checkers.

One last thing is that we need to make sure identifiers availability doesn't
get inherited too far: e.g. a create function inside a create function
can't access ids from the outer create function. This is pretty easy:
the following things generate identifier bindings:
select expressions, inside the expression
parameter defs
variable defs

since select expressions can't contain statements, we don't need to
worry about e.g. if statements, they want to inherit ids from params
and variable defs, so the default is good.

For environments being updated sequentially: since the environment is
updated in a statement list (i.e. environment updates stack from one
statement to the next within a single statement list), any var defs
can't break out of the containing list, so we are covered e.g. for a
variable def leaking from an inner block to an outer block.

With ids going into select expressions: we want the default which is
parameters, vardefs and ids from containing select expressions to be
inherited. So, in the end the only case to deal with is a create
function inside another create function. This isn't dealt with at the
moment.



> libExpandStar :: LocalIdentifierBindings -> String -> Either [TypeError] [(String,Type)]
> libExpandStar env correlationName =
>     case lookup correlationName $ starTypes env of
>       Nothing -> errorWhen (correlationName == "")
>                            [InternalError "no star expansion found?"] >>
>                  Left [UnrecognisedCorrelationName correlationName]
>       Just l -> Right l

> libLookupID :: LocalIdentifierBindings -> String -> String -> Either [TypeError] Type
> libLookupID env correlationName iden = {-trace ("lookup " ++ show iden ++ " in " ++ show (identifierTypes env)) $-}
>   envLookupID' $ identifierTypes env
>   where
>     envLookupID' (its:itss) =
>       case lookup correlationName its of
>         Nothing -> envLookupID' itss
>         Just s -> case filter (\(n,_) -> n==iden) s of
>                     [] -> if correlationName == ""
>                             then envLookupID' itss
>                             else Left [UnrecognisedIdentifier $ correlationName ++ "." ++ iden]
>                     (_,t):[] -> Right t
>                     _ -> Left [AmbiguousIdentifier iden]
>     envLookupID' [] =
>       Left [if correlationName == ""
>               then UnrecognisedIdentifier iden
>               else UnrecognisedCorrelationName correlationName]

> -- | Applies a list of 'EnvironmentUpdate's to an 'Environment' value
> -- to produce a new Environment value.
> updateBindings :: LocalIdentifierBindings
>                -> Environment
>                -> [LocalIdentifierBindingsUpdate]
>                -> Either [TypeError] LocalIdentifierBindings
> updateBindings lbs' env eus =
>   foldM updateEnv' lbs' eus
>   where
>     updateEnv' lbs eu =
>       case eu of
>         LibStackIDs qids -> return $ lbs {identifierTypes = (expandComposites qids):identifierTypes lbs}
>         LibSetStarExpansion sids -> return $ lbs {starTypes = sids}
>     --take all the composite typed ids, and expand them out
>     expandComposites :: [(String, [(String,Type)])] -> [(String, [(String,Type)])]
>     expandComposites (qi@(_,attrs):qis) =
>         ec attrs ++ qi:expandComposites qis
>         where
>           ec :: [(String,Type)] -> [(String, [(String,Type)])]
>           ec [] = []
>           ec ((nm,CompositeType t):xs) = (nm,compFields t):ec xs
>           ec ((nm,SetOfType(CompositeType t)):xs) = (nm,compFields t):ec xs
>           ec ((nm,UnnamedCompositeType t):xs) = (nm, t):ec xs
>           ec ((nm,SetOfType(UnnamedCompositeType t)):xs) = (nm, t):ec xs
>           ec (_:xs) = ec xs
>     expandComposites [] = []
>     compFields t = fromRight [] $ envCompositePublicAttrs env [] t

> data LocalIdentifierBindingsUpdate =
>     -- | to allow an unqualified identifier reference to work you need to
>     -- supply an extra entry with \"\" as the alias, and all the fields,
>     -- in the case of joins, these unaliased fields need to have the
>     -- duplicates removed and the types resolved
>       LibStackIDs [QualifiedIDs]
>     -- | to allow an unqualified star to work you need to
>     -- supply an extra entry with \"\" as the alias, and all the fields
>     | LibSetStarExpansion [QualifiedIDs]

