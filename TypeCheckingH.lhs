Copyright 2009 Jake Wheat

This file contains some utility functions for working with types and
type checking.

> module TypeCheckingH where

> import Data.Maybe
> import Data.List
> import Debug.Trace

> import TypeType
> import Scope
> import AstUtils
> import TypeConversion

================================================================================

= type check code

idea is to move these here from TypeChecking.ag if they get a bit big,
not very consistently applied at the moment.

> typeCheckFunCall :: Scope -> MySourcePos -> String -> Type -> Type
> typeCheckFunCall scope sp fnName argsType' =
>     checkErrors [argsType'] ret
>     where
>       argsType = unwrapTypeList argsType'
>       ret = case fnName of

do the special cases first, some of these will use the varidic support
when it is done and no longer be special cases.

>           "!arrayCtor" -> let t = resolveResultSetType scope sp argsType
>                           in checkErrors [t] $ ArrayType t
>           "!between" -> let f1 = lookupFn ">=" [argsType !! 0, argsType !! 1]
>                             f2 = lookupFn "<=" [argsType !! 0, argsType !! 2]
>                             f3 = lookupFn "!and" [f1,f2]
>                         in checkErrors [f1,f2] f3
>           "coalesce" -> let t = resolveResultSetType scope sp argsType
>                         in checkErrors [t] t
>           "greatest" -> let t = resolveResultSetType scope sp argsType
>                             f1 = lookupFn ">=" [t,t]
>                         in checkErrors [t, f1] t
>           "least" -> let t = resolveResultSetType scope sp argsType
>                          f1 = lookupFn "<=" [t,t]
>                      in checkErrors [t, f1] t
>           "!rowCtor" -> RowCtor argsType

special case the row comparison ops

>           _ | let isRowCtor t = case t of
>                                   RowCtor _ -> True
>                                   _ -> False
>               in fnName `elem` ["=", "<>", "<=", ">=", "<", ">"]
>                      && length argsType == 2
>                      && all isRowCtor argsType ->
>                 checkRowTypesMatch (head argsType) (head $ tail argsType)

>           s ->  lookupFn s argsType
>       lookupFn s1 args = case findCallMatch scope sp
>                                              (if s1 == "u-" then "-" else s1) args of
>                                Left te -> te
>                                Right (_,_,r) -> r
>       checkRowTypesMatch (RowCtor t1s) (RowCtor t2s) =
>         let e1 = if length t1s /= length t2s
>                    then TypeError sp ValuesListsMustBeSameLength
>                    else TypeList []
>             t3s = map (resolveResultSetType scope sp) $ map (\(a,b) -> [a,b]) $ zip t1s t2s
>         in checkErrors (e1:t3s) typeBool
>       checkRowTypesMatch x y  =
>         error $ "internal error: checkRowTypesMatch called with " ++ show x ++ "," ++ show y


> typeCheckValuesExpr :: Scope -> MySourcePos -> Type -> Type
> typeCheckValuesExpr scope sp vll =
>         let rowsTs1 = unwrapTypeList vll
>             --convert into [[Type]]
>             rowsTs = map unwrapTypeList rowsTs1
>             colNames = zipWith (++)
>                            (repeat "column")
>                            (map show [1..length $ head rowsTs])
>         in unionRelTypes scope sp rowsTs colNames

> typeCheckCombineSelect :: Scope -> MySourcePos -> Type -> Type -> Type
> typeCheckCombineSelect scope sp v1 v2 =
>     let colNames = map fst $ unwrapComposite $ unwrapSetOf v1
>     in unionRelTypes scope sp
>                   (map (map snd . unwrapComposite . unwrapSetOf) [v1,v2])
>                   colNames

> unionRelTypes :: Scope -> MySourcePos -> [[Type]] -> [String] -> Type
> unionRelTypes scope sp rowsTs colNames =
>         let lengths = map length rowsTs
>             error1 = case () of
>                       _ | null rowsTs ->
>                             TypeError sp NoRowsGivenForValues
>                         | not (all (==head lengths) lengths) ->
>                             TypeError sp
>                                  ValuesListsMustBeSameLength
>                         | otherwise -> TypeList []
>             colTypeLists = transpose rowsTs
>             colTypes = map (resolveResultSetType scope sp) colTypeLists
>             ty = SetOfType $ UnnamedCompositeType $ zip colNames colTypes
>         in checkErrors (error1:colTypes) ty

================================================================================

= utils

lookup a composite type name, restricting it to only certain kinds of
composite type, returns the composite definition which you can get the
attributes out of which is a pair with the normal columns first, then
the system columns second

> getAttrs :: Scope -> [CompositeFlavour] -> String -> Maybe (CompositeDef, CompositeDef)
> getAttrs scope f n =
>   let a = find (\(nm,fl,_) -> fl `elem` f && nm == n)
>             (scopeAttrDefs scope)
>       b = find (\(nm,_,_) -> nm == n)
>             (scopeAttrSystemColumns scope)
>   in case (a,b) of
>      (Nothing,_) -> Nothing
>      (Just x,Nothing) -> Just (x, (n,TableComposite,UnnamedCompositeType []))
>      (Just x,Just y) -> Just (x,y)

combine two relvar types when being joined, pass in a using list and
it checks the types in the using list are compatible, and eliminates
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
>             let notJoin = (\(s,_) -> s `notElem` l)
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
