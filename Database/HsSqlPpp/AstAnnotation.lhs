Copyright 2009 Jake Wheat

The annotation data types and utilities for working with them.

Annotations are used to store source positions, types, errors,
warnings, scope deltas, information, and other stuff a client might
want to use when looking at an ast. Internal annotations which are
used in the type-checking/ annotation process use the attribute
grammar code and aren't exposed.

> {-# OPTIONS -fglasgow-exts #-}

> module Database.HsSqlPpp.AstAnnotation
>     (
>      Annotated(..)
>     ,Annotation
>     ,AnnotationElement(..)
>     ,stripAnnotations
>     ,getTopLevelTypes
>     ,getTypeAnnotation
>     ,getTypeErrors
>     ,pack
>     ,StatementInfo(..)
>     ,getSIAnnotation
>     ) where

> import Database.HsSqlPpp.TypeType

> type Annotation = [AnnotationElement]

> data AnnotationElement = SourcePos String Int Int
>                        | TypeAnnotation Type
>                        | TypeErrorA TypeError
>                        | StatementInfoA StatementInfo
>                          deriving (Eq, Show)

> class Annotated a where
>     ann :: a -> Annotation
>     setAnn :: a -> Annotation -> a
>     changeAnn :: a -> (Annotation -> Annotation) -> a
>     changeAnn a = setAnn a . ($ ann a)
>     changeAnnRecurse :: (Annotation -> Annotation) -> a -> a
>     getAnnChildren :: a -> [Annotatable]

> data Annotatable = forall a . (Annotated a, Show a) => MkAnnotatable a

> instance Show Annotatable
>   where
>   showsPrec p (MkAnnotatable a) = showsPrec p a

> pack :: (Annotated a, Show a) => a -> Annotatable
> pack = MkAnnotatable

 > changeAnnotations :: Annotated a => (Annotation -> Annotation) -> [a] -> [a]
 > changeAnnotations f as = map (changeAnnRecurse f) as

hack job, often not interested in the source positions when testing
the asts produced, so this function will reset all the source
positions to empty ("", 0, 0) so we can compare them for equality, etc.
without having to get the positions correct.

> stripAnnotations :: Annotated a => a -> a
> stripAnnotations = changeAnnRecurse (const [])

> getTopLevelTypes :: Annotated a => [a] -> [Type]
> getTopLevelTypes = map getTypeAnnotation

> getTypeAnnotation :: Annotated a => a  -> Type
> getTypeAnnotation at = let as = ann at
>                        in gta as
>                        where
>                          gta (x:xs) = case x of
>                                         TypeAnnotation t -> t
>                                         _ -> gta xs
>                          gta _ = error "couldn't find type annotation"

> getSIAnnotation :: Annotated a => a  -> StatementInfo
> getSIAnnotation at = let as = ann at
>                        in gta as
>                        where
>                          gta (x:xs) = case x of
>                                         StatementInfoA t -> t
>                                         _ -> gta xs
>                          gta _ = error "couldn't find type annotation"


> getAnnotationsRecurse :: Annotated a => a -> [Annotation]
> getAnnotationsRecurse a =
>   ann a : concatMap getAnnotationsRecurse' (getAnnChildren a)
>   where
>     getAnnotationsRecurse' :: Annotatable -> [Annotation]
>     getAnnotationsRecurse' an =
>       hann an : concatMap getAnnotationsRecurse' (hgac an)
>     hann (MkAnnotatable an) = ann an
>     hgac (MkAnnotatable an) = getAnnChildren an


 > getAnnotationsRecurse :: Annotated a => a -> [Annotation]
 > getAnnotationsRecurse a =
 >   ann a : concatMap getAnnotationsRecurse' (getAnnChildren a)
 >   where
 >     getAnnotationsRecurse' :: Annotatable -> [Annotation]
 >     getAnnotationsRecurse' = undefined
 >     --hann (MkAnnotatable an) = ann an
 >     hgac (MkAnnotatable an) = getAnnChildren an


> getTypeErrors :: Annotated a => [a] -> [TypeError]
> getTypeErrors sts =
>     concatMap (concatMap gte . getAnnotationsRecurse) sts
>     where
>       gte (a:as) = case a of
>                     TypeErrorA e -> [e]
>                     _ -> gte as
>       gte _ = []





> data StatementInfo = DefaultStatementInfo Type
>                    | RelvarInfo CompositeDef
>                    | CreateFunctionInfo FunctionPrototype
>                    | SelectInfo Type
>                    | InsertInfo String Type
>                    | UpdateInfo String Type
>                    | DeleteInfo String
>                    | CreateDomainInfo String Type
>                    | DropInfo [(String,String)]
>                    | DropFunctionInfo [(String,[Type])]
>                      deriving (Eq,Show)





sourcepos
types
errors
warnings
scope deltas
info


Annotation planning
add an annotation field to most nodes in the ast
something like
data Annotation = NonAnnotation
                | SourcePosAnnotation SourcePos
                | CheckedAnnotation SourcePos Type [Messages]

alter the parser to add sourcepositions to these nodes - assume that
   one source position per node will be enough for now (not
   necessarily a good assumption with weird sql syntax), and see if
   this gives us enough for good error messages, etc..

question:
if a node has no source position e.g. the all in select all or select
   distinct may correspond to a token or may be synthesized as the
   default if neither all or distinct is present. Should this have the
   source position of where the token would have appeared, should it
   inherit it from its parent, should there be a separate ctor to
   represent a fake node with no source position?

The way the type checking will then work is that instead of producing
   some attribute values it will produce a transformed ast tree with
   the type and message fields filled in. Then supply some utility
   functions to e.g. extract all the messages, extract all the type
   errors, extract the top level types, etc. Use some sort of tree
   walker to implement these utils

The way types and type errors will work is that instead of / in
   addition to the types being passed in attributes, they'll be saved
   in the transformed tree. Type errors won't percolate up to the top
   level, but sit with the node that is in error. Any parent nodes
   which need this type to calculate their own type, will use a
   separate error to say type unknown. If they can calculate their
   type without depending on a type erroring child node, then they do
   that, so e.g. typing a set of statements with create functions and
   views which use those functions: if the statements inside the
   functions have type errors, we can still find the types of the
   views, assuming that the function params and return type check
   properly, and are correct.
