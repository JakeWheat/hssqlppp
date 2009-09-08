Copyright 2009 Jake Wheat

This file contains some utilty functions for working with types and
type checking.

> module TypeCheckingH where

> import Data.Maybe
> import Data.List

> import TypeType
> import Scope
> import AstUtils
> import TypeConversion

================================================================================

= type check code

idea is to move these here from TypeChecking.ag if they get a bit big,
not very consistently applied at the moment.

> typeCheckFunCall :: Scope -> MySourcePos -> FunName -> Type -> Type
> typeCheckFunCall scope sp fnName argsType =
>     checkErrors [argsType] ret
>     where
>       ret = case fnName of
>           Operator "!arrayCtor" -> let t = resolveResultSetType scope sp $ unwrapTypeList argsType
>                                    in checkErrors [t] $ ArrayType t
>           Operator "!between" -> let f1 = lookupFn ">=" [as !! 0, as !! 1]
>                                      f2 = lookupFn "<=" [as !! 0, as !! 2]
>                                      f3 = lookupFn "!and" [f1,f2]
>                                  in checkErrors [f1,f2] f3
>                                  where
>                                    as = unwrapTypeList argsType
>           Operator s ->  lookupFn s (unwrapTypeList argsType)
>           SimpleFun f -> lookupFn f (unwrapTypeList argsType)
>           RowCtor -> UnknownType
>       lookupFn s1 args = case findCallMatch scope sp
>                                              (if s1 == "u-" then "-" else s1) args of
>                                Left te -> te
>                                Right (_,_,r) -> r

> typeCheckValuesExpr :: Scope -> MySourcePos -> Type -> Type
> typeCheckValuesExpr scope sp vll =
>         let rowsTs1 = unwrapTypeList vll
>             --convert into [[Type]]
>             rowsTs = map unwrapTypeList rowsTs1
>             --check all same length
>             lengths = map length rowsTs
>             error1 = case () of
>                       _ | length rowsTs1 == 0 ->
>                             TypeError sp NoRowsGivenForValues
>                         | not (all (==head lengths) lengths) ->
>                             TypeError sp
>                                  ValuesListsMustBeSameLength
>                         | otherwise -> TypeList []
>             colNames = map (\(a,b) -> a ++ b) $
>                        zip (repeat "column")
>                            (map show [1..head lengths])
>             colTypeLists = transpose rowsTs
>             colTypes = map (resolveResultSetType scope sp) colTypeLists
>             ty = SetOfType $ UnnamedCompositeType $ zip colNames colTypes
>         in checkErrors (error1:colTypes) ty


================================================================================

= utils

lookup a composite type name, restricting it to only certain kinds of
composite type, returns the composite definition which you can get the
attributes out of

> getAttrs :: Scope -> [CompositeFlavour] -> String -> Maybe CompositeDef
> getAttrs scope f n = find (\(nm,fl,_) ->
>                                fl `elem` f
>                                  && nm == n)
>                          (scopeAttrDefs scope)

combine two relvar types when being joined, pass in a using list and
it checks the types in the using list are comppatible, and eliminates
duplicate columns of the attrs in the using list, returns the relvar
type of the joined tables.

> combineTableTypesWithUsingList :: Scope -> MySourcePos -> [String] -> Type -> Type -> Type
> combineTableTypesWithUsingList scope sp l t1c t2c =
>     --check t1 and t2 have l
>     let t1 = unwrapComposite t1c
>         t2 = unwrapComposite t2c
>         names1 = getNames t1
>         names2 = getNames t2
>         error1 = if not (contained l names1) ||
>                     not (contained l names2)
>                    then TypeError sp MissingJoinAttribute
>                    else TypeList []
>         --check the types
>         joinColumns = map (getColumnType t1 t2) l
>         nonJoinColumns =
>             let notJoin = (\(s,_) -> not (s `elem` l))
>             in filter notJoin t1 ++ filter notJoin t2
>     in checkErrors [error1]
>                    (UnnamedCompositeType $ joinColumns ++ nonJoinColumns)
>     where
>       getNames :: [(String,Type)] -> [String]
>       getNames = map fst
>       contained l1 l2 = all (`elem` l2) l1
>       getColumnType t1 t2 f =
>           let ct1 = getFieldType t1 f
>               ct2 = getFieldType t2 f
>           in (f, resolveResultSetType scope sp [ct1,ct2])
>       getFieldType t f = snd $ fromJust $ find (\(s,_) -> s == f) t
