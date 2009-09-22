Copyright 2009 Jake Wheat

The annotation data types and utilities for working with them.

Annotations are used to store source positions, types, errors,
warnings, environment deltas, information, and other stuff a client might
want to use when looking at an ast. Internal annotations which are
used in the type-checking/ annotation process use the attribute
grammar code and aren't exposed.

> {-# LANGUAGE ExistentialQuantification, DeriveDataTypeable,ScopedTypeVariables,
>   RankNTypes #-}
> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.TypeChecking.AstAnnotation
>     (
>      Annotated(..)
>     ,Annotation
>     ,AnnotationElement(..)
>     --,stripAnnotations
>     ,getTopLevelTypes
>     ,getTopLevelInfos
>     ,getTopLevelEnvUpdates
>     ,getTypeAnnotation
>     ,getTypeErrors
>     ,stripAnnotations
>     --,getTypeErrors
>     --,pack
>     ,StatementInfo(..)
>     ,getSIAnnotation
>     ) where

> import Data.Generics
> import Data.Maybe

> import Database.HsSqlPpp.TypeChecking.TypeType
> import Database.HsSqlPpp.TypeChecking.EnvironmentInternal

> -- | Annotation type - one of these is attached to most of the
> -- data types used in the ast.
> type Annotation = [AnnotationElement]

> -- | the elements of an annotation. Source positions are generated by
> -- the parser, the rest come from the separate ast annotation process.
> data AnnotationElement = SourcePos String Int Int
>                        | TypeAnnotation Type
>                        | TypeErrorA TypeError
>                        | StatementInfoA StatementInfo
>                        | EnvUpdates [EnvironmentUpdate]
>                          deriving (Eq, Show,Typeable,Data)

> class Annotated a where
>     ann :: a -> Annotation
>     setAnn :: a -> Annotation -> a
>     changeAnn :: a -> (Annotation -> Annotation) -> a
>     changeAnn a f = setAnn a (f $ ann a)

> -- | run through the ast, and pull the type annotation from each
> -- of the top level items.
> getTopLevelTypes :: Data a => [a] -> [Type]
> getTopLevelTypes st =
>     getTopLevelXs typeAnnot st
>     where
>       typeAnnot :: AnnotationElement -> [Maybe Type]
>       typeAnnot e = case e of
>                            TypeAnnotation t -> [Just t]
>                            _ -> [Nothing]

> getTopLevelXs :: forall a b a1.(Data a1, Typeable b) =>
>                  (b -> [Maybe a]) -> a1 -> [a]
> getTopLevelXs p st =
>     catMaybes $ everythingOne (++) (mkQ [] p) st


> everythingOne :: (r -> r -> r) -> GenericQ r -> GenericQ r
> everythingOne k f x
>  = foldl k (f x) (gmapQ (everythingTwo k f) x)

> everythingZero :: (r -> r -> r) -> GenericQ r -> GenericQ r
> everythingZero k f x
>  = foldl k (f x) (gmapQ f x)

> everythingTwo :: (r -> r -> r) -> GenericQ r -> GenericQ r
> everythingTwo k f x
>  = foldl k (f x) (gmapQ (everythingZero k f) x)

> getTypeAnnotation :: Annotated a => a  -> Type
> getTypeAnnotation at = let as = ann at
>                        in gta as
>                        where
>                          gta (x:xs) = case x of
>                                         TypeAnnotation t -> t
>                                         _ -> gta xs
>                          gta _ = TypeCheckFailed -- error "couldn't find type annotation"

> getSIAnnotation :: Annotated a => a  -> Maybe StatementInfo
> getSIAnnotation at = let as = ann at
>                        in gta as
>                        where
>                          gta (x:xs) = case x of
>                                         StatementInfoA t -> Just t
>                                         _ -> gta xs
>                          gta _ = Nothing

> getEuAnnotation :: Annotated a => a  -> [EnvironmentUpdate]
> getEuAnnotation at = let as = ann at
>                        in gta as
>                        where
>                          gta (x:xs) = case x of
>                                         EnvUpdates t -> t
>                                         _ -> gta xs
>                          gta _ = []


> -- | Run through the ast given and return a list of statementinfos
> -- from the top level items.
> getTopLevelInfos :: Annotated a =>
>                     [a] -- ^ the ast to check
>                  -> [Maybe StatementInfo]
> getTopLevelInfos = map getSIAnnotation

> getTopLevelEnvUpdates :: Annotated a =>
>                     [a] -- ^ the ast to check
>                  -> [[EnvironmentUpdate]]
> getTopLevelEnvUpdates = map getEuAnnotation

> data StatementInfo = DefaultStatementInfo Type
>                    | SelectInfo Type
>                    | InsertInfo String Type
>                    | UpdateInfo String Type
>                    | DeleteInfo String
>                      deriving (Eq,Show,Typeable,Data)

todo:
add environment deltas to statementinfo

question:
if a node has no source position e.g. the all in select all or select
   distinct may correspond to a token or may be synthesized as the
   default if neither all or distinct is present. Should this have the
   source position of where the token would have appeared, should it
   inherit it from its parent, should there be a separate ctor to
   represent a fake node with no source position?


> getAnnotationsRecurse :: (Data a) => a -> [AnnotationElement]
> getAnnotationsRecurse st = listify (\(_::AnnotationElement) -> True) st


hack job, often not interested in the source positions when testing
the asts produced, so this function will reset all the source
positions to empty ("", 0, 0) so we can compare them for equality, etc.
without having to get the positions correct.

> -- | strip all the annotations from a tree. E.g. can be used to compare
> -- two asts are the same, ignoring any source position annotation differences.
> stripAnnotations :: (Data a) => a -> a
> stripAnnotations = everywhere (mkT stripAn)
>                    where
>                      stripAn :: [AnnotationElement] -> [AnnotationElement]
>                      stripAn _ = []


> -- | runs through the ast given and returns a list of all the type errors
> -- in the ast. Recurses into all ast nodes to find type errors.
> -- This is the function to use to see if an ast has passed the type checking process.
> -- Source position information will be added to the return type at some point
> getTypeErrors :: (Data a) => a -> [TypeError]
> getTypeErrors sts =
>     gte $ getAnnotationsRecurse sts
>     where
>       gte (a:as) = case a of
>                     TypeErrorA e -> e:gte as
>                     _ -> gte as
>       gte _ = []

