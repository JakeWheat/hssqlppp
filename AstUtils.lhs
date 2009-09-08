Copyright 2009 Jake Wheat

This file holds all the non-short bits of code that are mainly used in
TypeChecking.ag.

random implementation note:
If you see one of these: TypeList [] - and don't get it - is used to
represent a variety of different things, like node type checked ok
when the node doesn't produce a type but can produce a type error,
etc.. This could probably be reviewed and made to work a bit better.

> module AstUtils where

> import Data.Maybe
> import Data.List
> import Control.Monad.Error

> import TypeType
> import Scope
> import DefaultScope

> data OperatorType = BinaryOp | PrefixOp | PostfixOp
>                   deriving (Eq,Show)

for now, assume that all the overloaded operators that have the
same name are all either binary, prefix or postfix, otherwise the
getoperatortype would need the types of the arguments to determine
the operator type, and the parser would have to be a lot cleverer

> getOperatorType :: String -> OperatorType
> getOperatorType s = case () of
>                       _ | any (\(x,_,_) -> x == s) (scopeBinaryOperators defaultScope) ->
>                             BinaryOp
>                         | any (\(x,_,_) -> x == s ||
>                                            (x=="-" && s=="u-"))
>                               (scopePrefixOperators defaultScope) ->
>                             PrefixOp
>                         | any (\(x,_,_) -> x == s) (scopePostfixOperators defaultScope) ->
>                             PostfixOp
>                         | s `elem` ["!and", "!or","!like"] -> BinaryOp
>                         | s `elem` ["!not"] -> PrefixOp
>                         | s `elem` ["!isNull", "!isNotNull"] -> PostfixOp
>                         | otherwise ->
>                             error $ "don't know flavour of operator " ++ s

================================================================================
Error reporting

> data Message = Error MySourcePos MessageStuff
>              | Warning MySourcePos MessageStuff
>              | Notice MySourcePos MessageStuff
>                deriving (Eq)
>
> data MessageStuff = ContinueNotInLoop
>                   | CustomMessage String
>                     deriving (Eq,Show)
>
> instance Show Message where
>    show m = showMessage m
>
> showMessage :: Message -> [Char]
> showMessage m = case m of
>                   Error sp s -> showit "Error" sp s
>                   Warning sp s -> showit "Warning" sp s
>                   Notice sp s -> showit "Notice" sp s
>                 where
>                   showit lev (fn,l,c) s = lev ++ "\n" ++ fn ++ ":"
>                                           ++ show l ++ ":" ++ show c ++ ":\n"
>                                           ++ show s ++ "\n"
>

================================================================================

= type checking utils

== unkErr

shorthand used with catMaybe

takes a type and returns any type errors, or if no errors, unknowns,
returns nothing if it doesn't find any type errors or unknowns. Looks
at the immediate type, or inside the first level if passed a type
list, or unnamedcompositetype.


> unkErr :: Type -> Maybe Type
> unkErr t =
>     case t of
>       a@(TypeError _ _) -> Just a
>       UnknownType -> Just UnknownType
>       TypeList l -> doTypeList l
>       UnnamedCompositeType c -> doTypeList (map snd c)
>       _ -> Nothing
>     where
>       -- run through the type list, if there are any errors, collect
>       -- them all into a list
>       -- otherwise, if there are any unknowns, then the type is
>       -- unknown
>       -- otherwise, return nothing
>       doTypeList ts =
>           let unks = filter (\u -> case u of
>                                      UnknownType -> True
>                                      _ -> False) ts
>               errs = filter (\u -> case u of
>                                      TypeError _ _ -> True
>                                      _ -> False) ts
>           in case () of
>                _ | length errs > 0 ->
>                      Just $ case () of
>                                     _ | length errs == 1 -> head errs
>                                       | otherwise -> TypeList errs
>                  | length unks > 0 -> Just UnknownType
>                  | otherwise -> Nothing

> checkErrors :: [Type] -> Type -> Type
> checkErrors (t:ts) r = case unkErr t of
>                        Just e -> e
>                        Nothing -> checkErrors ts r
> checkErrors [] r = r

======


> checkTypeExists :: Scope -> MySourcePos -> Type -> Type
> checkTypeExists scope sp t =
>     if t `elem` (scopeTypes scope)
>       then TypeList [] -- this works with the checkErrors function
>       else TypeError sp (UnknownTypeError t)



================================================================================

= basic types

random notes on pg types:

== domains:
the point of domains is you can't put constraints on types, but you
can wrap a type up in a domain and put a constraint on it there

== literals/selectors

source strings are parsed as unknown type: they can be implicitly cast
to almost any type in the right contexts.

rows ctors can also be implicitly cast to any composite type matching
the elements (how exactly are they matched? - number of elements, type
compatibility of elements, just by context?)

string literals are not checked for valid syntax currently, but this
will probably change so we can type check string literals statically,
whereas pg defers all checking to runtime, because it has to cope with
custom data types. this code isn't going to be able to support such
custom data types very well, so it can get away with doing more static
checks on this sort of thing.

== notes on type checking types

=== basic type checking
at the moment - just check type exists in predetermined list of type
names
todo: option to read types from database catalog at time of type
checking
todo: collect type names from current source file to check against
A lot of the infrastructure to do this is already in place. We also
need to do this for all other definitions, etc.

Type aliases

Some types in postgresql have multiple names. I think this is
hardcoded in the pg parser.

For the canonical name, we use the name given in the postgresql
pg_type catalog relvar.

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

what about PrecTypeName? - need to fix the ast and parser (these are
called type modifiers in pg)

also, what can setof be applied to - don't know if it can apply to an
array or setof type

array types have to match an exact array type in the catalog, so we
can't create an arbitrary array of any type



aliases to protect client code if/when the ast canonical names are
changed



> typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4,
>   typeFloat8,typeVarChar,typeChar,typeBool :: Type
> typeSmallInt = ScalarType "int2"
> typeBigInt = ScalarType "int8"
> typeInt = ScalarType "int4"
> typeNumeric = ScalarType "numeric"
> typeFloat4 = ScalarType "float4"
> typeFloat8 = ScalarType "float8"
> typeVarChar = ScalarType "varchar"
> typeChar = ScalarType "char"
> typeBool = ScalarType "bool"

> canonicalizeType :: Type -> Type
> canonicalizeType t =
>     case t of
>       ScalarType s -> cName s
>       ArrayType a -> ArrayType $ canonicalizeType a
>       SetOfType a -> SetOfType $ canonicalizeType a
>       t1@_ -> t1
>     where
>       cName s = case () of
>                   _ | s `elem` smallIntNames -> typeSmallInt
>                     | s `elem` intNames -> typeInt
>                     | s `elem` bigIntNames -> typeBigInt
>                     | s `elem` numericNames -> typeNumeric
>                     | s `elem` float4Names -> typeFloat4
>                     | s `elem` float8Names -> typeFloat8
>                     | s `elem` varcharNames -> typeVarChar
>                     | s `elem` charNames -> typeChar
>                     | s `elem` boolNames -> typeBool
>                     | otherwise -> ScalarType s
>       smallIntNames = ["int2", "smallint"]
>       intNames = ["int4", "integer", "int"]
>       bigIntNames = ["int8", "bigint"]
>       numericNames = ["numeric", "decimal"]
>       float4Names = ["real", "float4"]
>       float8Names = ["double precision", "float"]
>       varcharNames = ["character varying", "varchar"]
>       charNames = ["character", "char"]
>       boolNames = ["boolean", "bool"]

Internal errors

TODO: work out monad transformers and try to use these. Want to throw
an internal error when a programming error is detected (instead of
e.g. letting the haskell runtime throw a pattern match failure), then
catch it in the top level type check routines in ast.ag, convert it to
a regular either style error, all without dropping into IO.

> data TInternalError = TInternalError String
>                      deriving (Eq, Ord, Show)

> instance Error TInternalError where
>    noMsg  = TInternalError "oh noes!"
>    strMsg = TInternalError



> keywordOperatorTypes :: [(String,[Type],Type)]
> keywordOperatorTypes = [
>   ("!and", [typeBool, typeBool], typeBool)
>  ,("!or", [typeBool, typeBool], typeBool)
>  ,("!like", [ScalarType "text", ScalarType "text"], typeBool)
>  ,("!not", [typeBool], typeBool)
>  ,("!isNull", [Pseudo AnyElement], typeBool)
>  ,("!isNotNull", [Pseudo AnyElement], typeBool)
>  ,("!arrayCtor", [Pseudo AnyElement], Pseudo AnyArray) -- not quite right,
>                                         -- needs variadic support
>                                         -- currently works via a special case
>  ,("!between", [Pseudo AnyElement
>               ,Pseudo AnyElement
>               ,Pseudo AnyElement], Pseudo AnyElement)
>  ,("!substring", [ScalarType "text",typeInt,typeInt], ScalarType "text")
>  ,("!arraySub", [Pseudo AnyArray,typeInt], Pseudo AnyElement)
>  ]
