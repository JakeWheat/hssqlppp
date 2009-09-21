Copyright 2009 Jake Wheat

This file contains a bunch of small low level utilities to help with
type checking.

> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.TypeChecking.AstUtils
>     (
>      OperatorType(..)
>     ,checkTypes
>     ,chainTypeCheckFailed
>     ,errorToTypeFail
>     ,errorToTypeFailF
>     ,checkErrorList
>     ,getErrors
>     ,isArrayType
>     ,unwrapArray
>     ,unwrapSetOfComposite
>     ,unwrapSetOf
>     ,unwrapComposite
>     ,consComposite
>     ,unwrapRowCtor
>     ,isOperatorName
>     ) where

> import Data.List

> import Database.HsSqlPpp.TypeChecking.TypeType
> import Database.HsSqlPpp.TypeChecking.EnvironmentInternal

================================================================================

= type checking utils

== checkErrors

if we find a typecheckfailed in the list, then propagate that, else
use the final argument.

> checkTypes :: [Type] -> Either [TypeError] Type -> Either [TypeError] Type
> checkTypes (TypeCheckFailed:_) _ = Right TypeCheckFailed
> checkTypes (_:ts) r = checkTypes ts r
> checkTypes [] r = r

small variant, not sure if both are needed

> chainTypeCheckFailed :: [Type] -> Either a Type -> Either a Type
> chainTypeCheckFailed a b =
>   if any (==TypeCheckFailed) a
>     then Right TypeCheckFailed
>     else b

convert an 'either [typeerror] type' to a type

> errorToTypeFail :: Either [TypeError] Type -> Type
> errorToTypeFail tpe = case tpe of
>                         Left _ -> TypeCheckFailed
>                         Right t -> t

convert an 'either [typeerror] x' to a type, using an x->type function

> errorToTypeFailF :: (t -> Type) -> Either [TypeError] t -> Type
> errorToTypeFailF f tpe = case tpe of
>                                   Left _ -> TypeCheckFailed
>                                   Right t -> f t

used to pass a regular type on iff the list of errors is null

> checkErrorList :: [TypeError] -> Type -> Either [TypeError] Type
> checkErrorList es t = if null es
>                         then Right t
>                         else Left es

extract errors from an either, gives empty list if right

> getErrors :: Either [TypeError] Type -> [TypeError]
> getErrors e = either id (const []) e

===============================================================================

= basic types

random notes on pg types:

== domains:
the point of domains is you can't put constraints on types, but you
can wrap a type up in a domain and put a constraint on it there.

== literals/selectors

source strings are parsed as unknown type: they can be implicitly cast
to almost any type in the right context.

rows ctors can also be implicitly cast to any composite type matching
the elements (now sure how exactly are they matched? - number of
elements, type compatibility of elements, just by context?).

string literals are not checked for valid syntax currently, but this
will probably change so we can type check string literals statically.
Postgres defers all checking to runtime, because it has to cope with
custom data types. This code will allow adding a grammar checker for
each type so you can optionally check the string lits statically.

== notes on type checking types

=== basic type checking
Currently can lookup type names against a default template1 list of
types, or against the current list in a database (which is read before
processing and sql code).

todo: collect type names from current source file to check against
A lot of the infrastructure to do this is already in place. We also
need to do this for all other definitions, etc.

=== Type aliases

Some types in postgresql have multiple names. I think this is
hardcoded in the pg parser.

For the canonical name in this code, we use the name given in the
postgresql pg_type catalog relvar.

TODO: Change the ast canonical names: where there is a choice, prefer
the sql standard name, where there are multiple sql standard names,
choose the most concise or common one, so the ast will use different
canonical names to postgresql.

planned ast canonical names:
numbers:
int2, int4/integer, int8 -> smallint, int, bigint
numeric, decimal -> numeric
float(1) to float(24), real -> float(24)
float, float(25) to float(53), double precision -> float
serial, serial4 -> int
bigserial, serial8 -> bigint
character varying(n), varchar(n)-> varchar(n)
character(n), char(n) -> char(n)

TODO:

what about PrecTypeName? - need to fix the ast and parser (found out
these are called type modifiers in pg)

also, what can setof be applied to - don't know if it can apply to an
array or setof type

array types have to match an exact array type in the catalog, so we
can't create an arbitrary array of any type. Not sure if this is
handled quite correctly in this code.

================================================================================

utilities for working with Types

> isArrayType :: Type -> Bool
> isArrayType (ArrayType _) = True
> isArrayType _ = False

> unwrapArray :: Type -> Type
> unwrapArray (ArrayType t) = t
> unwrapArray x = error $ "internal error: can't get types from non array " ++ show x

> unwrapSetOfComposite :: Type -> Type
> unwrapSetOfComposite (SetOfType a@(UnnamedCompositeType _)) = a
> unwrapSetOfComposite x = error $ "internal error: tried to unwrapSetOfComposite on " ++ show x

> unwrapSetOf :: Type -> Type
> unwrapSetOf (SetOfType a) = a
> unwrapSetOf x = error $ "internal error: tried to unwrapSetOf on " ++ show x

> unwrapComposite :: Type -> [(String,Type)]
> unwrapComposite (UnnamedCompositeType a) = a
> unwrapComposite x = error $ "internal error: cannot unwrapComposite on " ++ show x

> consComposite :: (String,Type) -> Type -> Type
> consComposite l (UnnamedCompositeType a) =
>     UnnamedCompositeType (l:a)
> consComposite a b = error $ "internal error: called consComposite on " ++ show (a,b)

> unwrapRowCtor :: Type -> [Type]
> unwrapRowCtor (RowCtor a) = a
> unwrapRowCtor x = error $ "internal error: cannot unwrapRowCtor on " ++ show x
