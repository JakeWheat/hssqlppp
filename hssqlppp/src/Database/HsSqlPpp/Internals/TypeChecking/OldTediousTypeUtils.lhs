
The reason this file exists is because the representation of types in
hssqlppp (in the module ...Internals.TypesInternal) is not good. I'm
not sure it can be improved that much though.

> {-# LANGUAGE FlexibleInstances,DeriveDataTypeable #-}
>
> module Database.HsSqlPpp.Internals.TypeChecking.OldTediousTypeUtils where

> import Database.HsSqlPpp.Internals.TypesInternal
> import Control.Arrow
> import Data.Text (Text)
> --import qualified Data.Text as T

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
> isCompositeType (AnonymousCompositeType _) = True
> isCompositeType (Pseudo (Record _)) = True
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
> unwrapSetOfComposite :: Type -> Either [TypeError]  [(Text,Type)]
> unwrapSetOfComposite (Pseudo (SetOfType (CompositeType a))) = Right $ map (second teType) a
> unwrapSetOfComposite x = Left [InternalError $ "tried to unwrapSetOfComposite on " ++ show x]
>
> unwrapSetOf :: Type -> Either [TypeError] Type
> unwrapSetOf (Pseudo (SetOfType a)) = Right a
> unwrapSetOf x = Left [InternalError $ "tried to unwrapSetOf on " ++ show x]
>
> unwrapComposite :: Type -> Either [TypeError] [(Text,Type)]
> unwrapComposite (CompositeType a) = Right $ map (second teType) a
> unwrapComposite x = Left [InternalError $ "cannot unwrapComposite on " ++ show x]
>
> consComposite :: (Text,Type) -> Type -> Either [TypeError] Type
> consComposite l (CompositeType a) = Right $ CompositeType (second mkTypeExtra l :a)
> consComposite a b = Left [InternalError $ "called consComposite on " ++ show (a,b)]
>
> unwrapRowCtor :: Type -> Either [TypeError] [Type]
> unwrapRowCtor (AnonymousCompositeType a) = Right a
> unwrapRowCtor x = Left [InternalError $ "cannot unwrapRowCtor on " ++ show x]
