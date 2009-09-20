Copyright 2009 Jake Wheat

This file contains some utility functions for working with types and
type checking.

> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.TypeChecking.TypeCheckingH where

> import Data.Maybe
> import Data.List
> import Debug.Trace
> import Data.Either
> import Control.Applicative
> import Control.Monad
> import Control.Arrow
> import Control.Monad.Error

> import Database.HsSqlPpp.TypeChecking.TypeType
> import Database.HsSqlPpp.TypeChecking.AstUtils
> import Database.HsSqlPpp.TypeChecking.TypeConversion
> import Database.HsSqlPpp.TypeChecking.ScopeData
> import Database.HsSqlPpp.TypeChecking.EnvironmentInternal
> import Database.HsSqlPpp.Utils

================================================================================

= type check code

idea is to move these here from TypeChecking.ag if they get a bit big,
not very consistently applied at the moment.

> typeCheckFunCall :: Scope -> String -> [Type] -> Either [TypeError] Type
> typeCheckFunCall scope fnName argsType =
>     chainTypeCheckFailed argsType $
>       case fnName of
>               -- do the special cases first, some of these will use
>               -- the variadic support when it is done and no longer
>               -- be special cases.
>               "!arrayCtor" ->
>                     ArrayType <$> resolveResultSetType scope argsType
>               "!between" -> do
>                     f1 <- lookupFn ">=" [argsType !! 0, argsType !! 1]
>                     f2 <- lookupFn "<=" [argsType !! 0, argsType !! 2]
>                     lookupFn "!and" [f1,f2]
>               "coalesce" -> resolveResultSetType scope argsType
>               "greatest" -> do
>                     t <- resolveResultSetType scope argsType
>                     lookupFn ">=" [t,t]
>                     return t
>               "least" -> do
>                     t <- resolveResultSetType scope argsType
>                     lookupFn "<=" [t,t]
>                     return t
>               "!rowCtor" -> return $ RowCtor argsType
>                     -- special case the row comparison ops
>               _ | let isRowCtor t = case t of
>                                       RowCtor _ -> True
>                                       _ -> False
>                   in fnName `elem` ["=", "<>", "<=", ">=", "<", ">"]
>                          && length argsType == 2
>                          && all isRowCtor argsType ->
>                     checkRowTypesMatch (head argsType) (head $ tail argsType)
>               s -> lookupFn s argsType
>     where
>       lookupFn :: String -> [Type] -> Either [TypeError] Type
>       lookupFn s1 args = do
>         (_,_,r) <- findCallMatch scope
>                              (if s1 == "u-" then "-" else s1) args
>         return r
>       checkRowTypesMatch (RowCtor t1s) (RowCtor t2s) = do
>         when (length t1s /= length t2s) $ Left [ValuesListsMustBeSameLength]
>         --this is wrong - we want all the errors, not just the first set
>         mapM_ (resolveResultSetType scope . (\(a,b) -> [a,b])) $ zip t1s t2s
>         return typeBool
>       checkRowTypesMatch x y  =
>         error $ "internal error: checkRowTypesMatch called with " ++ show x ++ "," ++ show y


> typeCheckValuesExpr :: Scope -> [[Type]] -> Either [TypeError] Type
> typeCheckValuesExpr scope rowsTs =
>         let colNames = zipWith (++)
>                            (repeat "column")
>                            (map show [1..length $ head rowsTs])
>         in unionRelTypes scope rowsTs colNames

> typeCheckCombineSelect :: Scope -> Type -> Type -> Either [TypeError] Type
> typeCheckCombineSelect scope v1 v2 =
>     let colNames = map fst $ unwrapComposite $ unwrapSetOf v1
>     in unionRelTypes scope
>                   (map (map snd . unwrapComposite . unwrapSetOf) [v1,v2])
>                   colNames

> unionRelTypes :: Scope -> [[Type]] -> [String] -> Either [TypeError] Type
> unionRelTypes scope rowsTs colNames =
>   let lengths = map length rowsTs
>   in case () of
>              _ | null rowsTs ->
>                    Left [NoRowsGivenForValues]
>                | not (all (==head lengths) lengths) ->
>                    Left [ValuesListsMustBeSameLength]
>                | otherwise -> do
>                    --i don't think this propagates all the errors, just the first set
>                    mapM (resolveResultSetType scope) (transpose rowsTs) >>=
>                      (return . SetOfType . UnnamedCompositeType . zip colNames)

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

> combineTableTypesWithUsingList :: Scope -> [String] -> Type -> Type -> Either [TypeError] Type
> combineTableTypesWithUsingList scope l t1c t2c = do
>     --check t1 and t2 have l
>     let t1 = unwrapComposite t1c
>         t2 = unwrapComposite t2c
>         names1 = getNames t1
>         names2 = getNames t2
>     when (not (contained l names1) ||
>               not (contained l names2)) $
>          Left [MissingJoinAttribute]
>     --check the types
>     joinColumnTypes <- mapM (getColumnType t1 t2) l
>     let nonJoinColumns =
>             let notJoin = (\(s,_) -> s `notElem` l)
>             in filter notJoin t1 ++ filter notJoin t2
>     return $ UnnamedCompositeType $ zip l joinColumnTypes ++ nonJoinColumns
>     where
>       getNames :: [(String,Type)] -> [String]
>       getNames = map fst
>       contained l1 l2 = all (`elem` l2) l1
>       getColumnType :: [(String,Type)] -> [(String,Type)] -> String -> Either [TypeError] Type
>       getColumnType t1 t2 f =
>           let ct1 = getFieldType t1 f
>               ct2 = getFieldType t2 f
>           in resolveResultSetType scope [ct1,ct2]
>       getFieldType t f = snd $ fromJust $ find (\(s,_) -> s == f) t

> doSelectItemListTpe :: Scope
>                     -> String
>                     -> Type
>                     -> Type
>                     -> Type
> doSelectItemListTpe scope colName colType types =
>     if types == TypeCheckFailed
>        then types
>        else
>          let (correlationName,iden) = splitIdentifier colName
>              newCols = if iden == "*"
>                          then scopeExpandStar scope correlationName
>                          else return [(iden, colType)]
>          in errorToTypeFailF  (foldr consComposite types) newCols

I think this should be alright, an identifier referenced in an
expression can only have zero or one dot in it.

> splitIdentifier :: String -> (String,String)
> splitIdentifier s = let (a,b) = span (/= '.') s
>                     in if b == ""
>                          then ("", a)
>                          else (a,tail b)


returns the type of the relation, and the system columns also

> getRelationType :: Scope -> String -> Either [TypeError] (Type,Type)
> getRelationType scope tbl =
>           case getAttrs scope [TableComposite, ViewComposite] tbl of
>             Just ((_,_,a@(UnnamedCompositeType _))
>                  ,(_,_,s@(UnnamedCompositeType _))) -> Right (a,s)
>             _ -> Left [UnrecognisedRelation tbl]

> commonFieldNames :: Type -> Type -> [String]
> commonFieldNames t1 t2 =
>     intersect (fn t1) (fn t2)
>     where
>       fn (UnnamedCompositeType s) = map fst s
>       fn _ = []

> checkColumnConsistency :: Scope ->  String -> [String] -> [(String,Type)]
>                        -> Either [TypeError] [(String,Type)]
> checkColumnConsistency scope tbl cols' insNameTypePairs = do
>   rt <- getRelationType scope tbl
>   let ttcols :: [(String,Type)]
>       ttcols = unwrapComposite $ fst rt
>       cols :: [String]
>       cols = if null cols'
>                then map fst ttcols
>                else cols'
>   errorWhen (length insNameTypePairs /= length cols) $ [WrongNumberOfColumns]
>   let nonMatchingColumns = cols \\ map fst ttcols
>   when (not $ null nonMatchingColumns) $
>        Left $ map UnrecognisedIdentifier nonMatchingColumns
>   let targetNameTypePairs =
>         map (\l -> (l,fromJust $ lookup l ttcols)) cols
>         --check the types of the insdata match the column targets
>         --name datatype columntype
>       typeTriples = map (\((a,b),c) -> (a,b,c)) $ zip targetNameTypePairs $ map snd insNameTypePairs
>       errs :: [TypeError]
>       errs = concat $ lefts $ map (\(_,b,c) -> checkAssignmentValid scope c b) typeTriples
>   unless (null errs) $ Left errs
>   return ttcols

> checkRelationExists :: Scope -> String -> Maybe TypeError
> checkRelationExists scope tbl =
>           case getAttrs scope [TableComposite, ViewComposite] tbl of
>             Just _ -> Nothing
>             _ -> Just $ UnrecognisedRelation tbl


