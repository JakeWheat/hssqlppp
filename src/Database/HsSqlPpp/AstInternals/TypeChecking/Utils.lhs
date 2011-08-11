
For extra utility functions to help with asts

> module Database.HsSqlPpp.AstInternals.TypeChecking.Utils
>     (addExplicitCasts
>     ,canonicalizeTypeNames) where

> import Data.Data
> import Database.HsSqlPpp.AstInternals.AstInternal
> import Data.Generics.PlateData
> import Database.HsSqlPpp.AstInternals.AstAnnotation
> import Database.HsSqlPpp.AstInternals.TypeType
> import Control.Monad



> -- | Run through a typechecked tree and add in explicit casts where
> -- implicit casts are used to typecheck. Does function and operator
> -- calls, case result expressions, and string, integer and float
> -- literals at the moment, todo: union, array, greatest, least
> addExplicitCasts :: Data a => a -> a
> addExplicitCasts =
>   refix . tr . canonicalizeTypeNames
>   where
>     tr = transformBi $ \x -> case x of
>            FunCall a f as | Just p <- getProtoATys a
>                           , Just ats <- getTys as
>                           , p /= ats
>              -> FunCall a f $ zipWith3 addCastIfNeeded p ats as
>            Case a cs els | (Just f) <- doCase a cs els -> f
>            CaseSimple a v cs els | (Just f) <- doCaseSimple a v cs els -> f
>            i@(NumberLit a _) | Just t <- infType a -> castToT t i
>            i@(StringLit a _) | Just t <- infType a -> castToT t i
>            x1 -> x1
>     -- need to put the type annotation in the created cast to allow
>     -- adding explicit casts to continue up the tree
>     castToT t e = Cast ea {atype = Just t,infType = Just t} e $ typeName 1 t
>     getProtoATys :: Annotation -> Maybe [Type]
>     getProtoATys a = let p = fnProt a
>                      in flip fmap p $ \(_,t,_,_) -> t
>     getTys :: [ScalarExpr] -> Maybe [Type]
>     getTys = mapM $ atype . getAnnotation
>     addCastIfNeeded :: Type -> Type -> ScalarExpr -> ScalarExpr
>     addCastIfNeeded nt ot e =
>       if ot == nt
>       then e
>       else Cast ea e $ typeName 2 nt
>     -- not sure how to avoid inserting a cast for a literal which is
>     -- already being cast when using transformBi, so run through and
>     -- remove the suplerfluous casts in a second sweep
>     -- spot them when there is a cast of a cast of a literal
>     -- and the type of the inner cast is the same as the type of the literal
>     -- not sure if this pass can cause other problems
>     refix = transformBi $ \x -> case x of
>               Cast ca (Cast _ l t) t1 | isLiteral l
>                                       , Just lt <- infType $ getAnnotation l
>                                       , t == typeName 3 lt
>                 -> Cast ca l t1
>               x1 -> x1
>     isLiteral (NumberLit _ _) = True
>     isLiteral (StringLit _ _) = True
>     isLiteral _ = False


> typeName :: Int -> Type -> TypeName
> typeName _ (ScalarType t) = SimpleTypeName ea t
> typeName i e = error $ "don't know how to convert " ++ show e ++ " to typename " ++ show i

> ea :: Annotation
> ea = emptyAnnotation

> doCase :: Annotation -> [([ScalarExpr],ScalarExpr)] -> Maybe ScalarExpr -> Maybe ScalarExpr
> doCase a whths els = do
>   (whths',els') <- doCaseStuff a whths els
>   return $ Case a whths' els'

> doCaseSimple :: Annotation -> ScalarExpr -> [([ScalarExpr],ScalarExpr)] -> Maybe ScalarExpr -> Maybe ScalarExpr
> doCaseSimple a v whths els = do
>   (whths',els') <- doCaseStuff a whths els
>   return $ CaseSimple a v whths' els'

> doCaseStuff :: Annotation -> [([ScalarExpr],ScalarExpr)] -> Maybe ScalarExpr -> Maybe ([([ScalarExpr],ScalarExpr)],Maybe ScalarExpr)
> doCaseStuff a whths els = do
>   --trace (show $ atype a) $ return ()
>   expectedType <- atype a
>   --trace (show $ map (atype . getAnnotation . snd) whths) $ return ()
>   thenTypes <- mapM (atype . getAnnotation . snd) whths
>   --trace (show els) $ return ()
>   thenAndElseTypes <- case els of
>                         Nothing -> return thenTypes
>                         Just els' -> fmap (:thenTypes) $ atype $ getAnnotation els'
>   --trace ("checking cast: " ++ show expectedType ++ " " ++ show thenAndElseTypes)
>   when (all (==expectedType) thenAndElseTypes) Nothing
>   return (map (fixWhTh expectedType) whths
>          ,castElse expectedType)
>   where
>     castElse et = case els of
>                     Nothing -> Nothing
>                     Just els' | Just t' <- atype $ getAnnotation els
>                               , t' /= et
>                       -> Just (Cast ea els' $ typeName 4 et)
>                               | otherwise -> els
>     fixWhTh :: Type -> ([ScalarExpr],ScalarExpr) -> ([ScalarExpr],ScalarExpr)
>     fixWhTh et (whs,th) | Just t' <- atype $ getAnnotation th
>                         , t' /= et = (whs, Cast ea th $ typeName 5 et)
>                         | otherwise = (whs,th)

> -- bit crap, this function is called canonicalizeTypeNames
> -- and it uses a function called canonicalizeTypeName

> -- | Convert all the typenames in the ast to canonical form
> -- e.g. int -> int4
> canonicalizeTypeNames :: Data a => a -> a
> canonicalizeTypeNames = fixTypes . fixTypeNames
>   where
>     fixTypes =
>       transformBi $ \x -> case x of
>         ScalarType t -> ScalarType $ ct t
>         x1 -> x1
>     fixTypeNames =
>       transformBi $ \x -> case x of
>         SimpleTypeName a t -> SimpleTypeName a $ ct t
>         PrecTypeName a t i -> PrecTypeName a (ct t) i
>         Prec2TypeName a t i0 i1 -> Prec2TypeName a (ct t) i0 i1
>         x1 -> x1
>     ct = canonicalizeTypeName

