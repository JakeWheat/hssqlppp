
Code for the sql server type conversion rules (and implicit/explicit
casting)

The plan is to follow the same functions as postgresql support for now:

findCallMatch
resolveResultSetType
checkAssignmentValid

The rules in sql server for implicit casting and function resolution
are quite different to postgresql. The biggest one is that e.g. select
cast(1 as int) + cast('2' as varchar(20)) works in sql server but not
in postgresql.


just hack for operators for now: if one argument is a number type, and
the other is a text type, then cast the text to number.

> {-# LANGUAGE PatternGuards,OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.TypeChecking.TypeConversion.SqlTypeConversion (
>                        findCallMatch
>                       ) where
>
> --import Data.Maybe
> --import Data.List
> --import Data.Either
> --import Debug.Trace
> --import Data.Char
> --import Control.Monad
>
> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> --import Database.HsSqlPpp.Utils.Utils
> --import Database.HsSqlPpp.Internals.TypeChecking.OldTediousTypeUtils
> import qualified Database.HsSqlPpp.Internals.TypeChecking.TypeConversion.OldTypeConversion as T
> import Data.Text (Text)

> findCallMatch :: Dialect -> Catalog -> Text -> [Type] ->  Either [TypeError] OperatorPrototype
> findCallMatch d cat fnName argsType =
>   case argsType of
>      [_a,_b] | Just x <- checkOperator d cat fnName argsType -> Right x
>      _ -> T.findCallMatch d cat fnName argsType

hack to allow implicit casting one of the args to a numeric operator
from a text type to the correct numeric type:

check is an operator
check on arg is numeric, the other text
match an exact operator itself with two args the same numeric type
- in this case, cast the text arg to the numeric type and return a match


> checkOperator :: Dialect -> Catalog -> Text -> [Type] -> Maybe OperatorPrototype
> checkOperator d cat fnName [a,b] | Just t <- ty a b =
>   -- have the argument type in t
>   -- find all the matching fns by name
>   let nm = catLookupFns cat fnName
>   -- keep the ones which have exactly two args with the
>   -- type t, only proceed if there is onne match
>       cands = filter (\(_,as,_,_) -> as == [t,t]) nm
>   in case cands of
>        [c] -> return c
>        _ -> Nothing
>   where
>     ty a' b' | isNumber a' && isText b' = Just a'
>     ty a' b' | isText a' && isNumber b' = Just b'
>     ty _ _ = Nothing
>     isNumber x =
>       x `elem` (map ScalarType $ diNumberTypes d)
>     --isNumber _ = False
>     isText x =
>       x `elem` (map ScalarType $ diTextTypes d)
>     --isText _ = False
> checkOperator _ _ _ _ = Nothing
