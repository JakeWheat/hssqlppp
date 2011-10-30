

> {-# LANGUAGE FlexibleInstances,DeriveDataTypeable #-}
>
> module Database.HsSqlPpp.Internals.TediousTypeUtils where

> import Database.HsSqlPpp.Internals.TypesInternal

> isArrayType :: Type -> Bool
> isArrayType (ArrayType _) = True
> isArrayType _ = False
>
> isDomainType :: Type -> Bool
> isDomainType (DomainType _) = True
> isDomainType _ = False
>
> isCompositeType :: Type -> Bool
> isCompositeType (CompositeType _) = True
> isCompositeType (NamedCompositeType _) = True
> isCompositeType (AnonymousRecordType _) = True
> isCompositeType (Pseudo (PgRecord _)) = True
> isCompositeType _ = False
>
> isCompositeOrSetOfCompositeType :: Type -> Bool
> isCompositeOrSetOfCompositeType (Pseudo (SetOfType a)) = isCompositeType a
> isCompositeOrSetOfCompositeType a = isCompositeType a
>
> unwrapArray :: Type -> Either [TypeError] Type
> unwrapArray (ArrayType t) = Right t
> unwrapArray x = Left [InternalError $ "can't get types from non array " ++ show x]
>
> unwrapSetOfWhenComposite :: Type -> Either [TypeError] Type
> unwrapSetOfWhenComposite (Pseudo (SetOfType a@(CompositeType _))) = Right a
> unwrapSetOfWhenComposite x = Left [InternalError $ "tried to unwrapSetOfWhenComposite on " ++ show x]
>
> unwrapSetOfComposite :: Type -> Either [TypeError]  [(String,Type)]
> unwrapSetOfComposite (Pseudo (SetOfType (CompositeType a))) = Right a
> unwrapSetOfComposite x = Left [InternalError $ "tried to unwrapSetOfComposite on " ++ show x]
>
> unwrapSetOf :: Type -> Either [TypeError] Type
> unwrapSetOf (Pseudo (SetOfType a)) = Right a
> unwrapSetOf x = Left [InternalError $ "tried to unwrapSetOf on " ++ show x]
>
> unwrapComposite :: Type -> Either [TypeError] [(String,Type)]
> unwrapComposite (CompositeType a) = Right a
> unwrapComposite x = Left [InternalError $ "cannot unwrapComposite on " ++ show x]
>
> consComposite :: (String,Type) -> Type -> Either [TypeError] Type
> consComposite l (CompositeType a) = Right $ CompositeType (l:a)
> consComposite a b = Left [InternalError $ "called consComposite on " ++ show (a,b)]
>
> unwrapRowCtor :: Type -> Either [TypeError] [Type]
> unwrapRowCtor (AnonymousRecordType a) = Right a
> unwrapRowCtor x = Left [InternalError $ "cannot unwrapRowCtor on " ++ show x]
