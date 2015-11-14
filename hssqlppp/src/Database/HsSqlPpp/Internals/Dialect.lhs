

New approach to dialacts, more rule driven so all the options for a
dialect can appear here.

It would be nice to make the dialects completely rule driven, but I
think this is much less maintainable than making them mainly rule
driven (primarily via the catalogs), but with some special cases
scattered in the source code.

The dialect contains:
the name of the dialect
some parsing options about what syntax is supported
some options about typechecking support

flags to control the details of the dialect (for instance, some
dialects have additional options to specify which kinds of string
literal escapes are valid, so this is like subdialect

stuff about types:

canonical names of types with multiple names (these are only the
built in types, user/catalog driven type aliases are not covered here)
the built in text types
the built in datetime types

default catalog for this dialect


then supplied with hssqlppp are:
base dialect with minimal stuff in it
ansi2011 dialect
recent-ish postgresql dialect
recent-ish sql server dialect
recent-ish oracle dialect

the idea is that if you have one of these dialects, you can start here
then add your own catalog entries and use it as is. If your dialect is
not here, and is it similar enough to an existing dialect, you can
take that dialect and
    a) modify some of the dialect options
    b) modify the default catalog
    and you will get something useful

if the dialect is too different, then you will have to edit the
hssqlppp source.

> {-# LANGUAGE DeriveDataTypeable #-}
> module Database.HsSqlPpp.Internals.Dialect
>     (Dialect(..)
>     ,SyntaxFlavour(..)
>     ,canonicalizeTypeName
>     ,ansiTypeNameToDialect) where

> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> import Data.Data
> import Data.Text (Text)
> import qualified Data.Text as T
> import Data.List (find)
> import Data.Char (toLower)

> data Dialect = Dialect
>      {diName :: String

represent the syntax variations with a crude enum. Later, can make
this more rule driven.

>      ,diSyntaxFlavour :: SyntaxFlavour

map from alternative names to the canonical name of built in
types. This is used e.g. because in ansi the canonical name of boolean
type is 'boolean', and in postgresql the canonical name of this type
is 'bool'.
These should all be in lower case

>      ,diCanonicalTypeNames :: [(Text,[Text])]

the names of the built in text types. This is used to help type check
built in functions like substring?

>      ,diTextTypes :: [Text] -- names of the text types (canonical names must be used)

used to typecheck things like extract

todo: create a single function which takes the ansi name of a type and
returns the dialect specific name (as a maybe) - then don't have to
have a huge number of functions here.

Also, these functions should be -TypeName not -Type.

>      ,diDatetimeTypes :: [Text]
>      ,diNumberTypes :: [Text] -- names of the number types (canonical names must be used)
>      -- this is a map from the canonical ansi name (in hssqlppp)
>      -- to the canonical name in the dialect
>      -- if there is no entry, then it means that type isn't
>      -- supported in this dialect

todo: make a list of exactly what type names are needed in the type
checker and why. This should only be used for the type checker
internally, and not anywhere else. Should do the same for the
other fields above

>      ,namesForAnsiTypes :: [(Text,Text)]

A small issue with having the default catalog like this is that we can
make a programming error where we have a function which takes the
dialect and a catalog, and we use this default catalog instead of the
supplied updated catalog.

>      ,diDefaultCatalog :: Catalog
>      } deriving (Eq,Show,Data,Typeable)

> data SyntaxFlavour = Ansi | Postgres | SqlServer | Oracle
>                      deriving (Eq,Show,Data,Typeable)

> ansiTypeNameToDialect :: Dialect -> Text -> Maybe Text
> ansiTypeNameToDialect d n = lookup n (namesForAnsiTypes d)

> canonicalizeTypeName :: Dialect -> Text -> Text
> canonicalizeTypeName d s =
>     let m = diCanonicalTypeNames d
>     in ct m s
>   where
>     hasType t p = let t' = T.map toLower t
>                   in t' `elem` snd p
>     ct m tn = maybe tn fst
>               $ find (hasType tn) m

TODO:
modify the catalog:
  when adding a type flags to say:
    if this type is a text or datetime or number type
    if it is built in
    if it is undroppable
    if it has a list of builtin aliases
    what the ansi equivalent type name is

this will get rid of most of the fields in the dialect and make it
much easier to keep everything consistent and maintainable

start adding flags for which bits of syntax to support instead of
using the syntax flavour thing
there will be flags like this for typechecking also

support functions to help create minimal dialects
  e.g. take ansi, and safely remove a bunch of types and functions

write the default catalogs and dialects all in one file
  (they are split into two at the moment)

export the ansidialect every module that exposes something with a
dialect (function or data type) - parse, pretty, lex, typecheck, etc.

then you only need to import the dialects module to get the other
dialects

consider how the catalog in the dialect can help with parsing
operators

minimal dialects:
mainly missing types then following through on implications:
no text types
only one text type covering char,varchar,nclob, etc.
no numeric or decimal
on decimal for all integers and precise decimals
no date or time types


Types:
typeextra: needs fixing
how to represent e.g. 'varchar' without precision

invent some sort of concrete syntax (which is parseable) to
represent implicit casts (cast implicit x as varchar)?
