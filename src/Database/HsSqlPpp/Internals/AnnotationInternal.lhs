
The annotation data types and utilities for working with them.

Annotations are used to store source positions, types, errors,
warnings, catalog deltas, information, and other stuff a client might
want to use when looking at an ast. Internal annotations which are
used in the type-checking/ annotation process use the attribute
grammar code and aren't exposed.

> {-# LANGUAGE DeriveDataTypeable,ScopedTypeVariables  #-}
>
> module Database.HsSqlPpp.Internals.AnnotationInternal
>     (
>      Annotation(..)
>     ,SourcePosition
>     ,ParameterizedStatementType
>     ,getAnnotation
>     ,updateAnnotations
>     ,getAnnotations
>     ,emptyAnnotation
>     ,getTypeAnnotation
>     ,updateAnnotation
>     ) where
>
> import Data.Generics
> ---import Control.Arrow
> import Data.Generics.Uniplate.Data
> --import Debug.Trace
>
> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> --import Database.HsSqlPpp.Utils.Utils
>
> -- | Represents a source file position, usually set by the parser.
> type SourcePosition = (String,Int,Int)

> -- | Statement type is used for getting type information for a
> -- parameterized statement. The first part is the args that the
> -- parameterized statement needs, and the second is the names and types
> -- of the output columns. No way to signal that a statement returns
> -- exactly one row at the moment
> type ParameterizedStatementType = ([Type],[(String,Type)])

> -- | Annotation type - one of these is attached to most of the
> -- data types used in the ast.
> data Annotation = Annotation { -- | source position for this node
>                               asrc :: Maybe SourcePosition
>                                -- | type of the node, 'Nothing' if the tree hasn't been typechecked or if a type error prevents determining the type
>                              ,atype :: Maybe Type
>                                -- | any type errors
>                              ,errs :: [TypeError]
>                                -- | if an implicit cast is needed between this node an its parent, this the target type of cast. If no implicit cast is needed, this is Nothing
>                              ,implicitCast :: Maybe Type
>                                -- | used for getting the in and out types of a parameterized statement, useful only in Statements containing ? placeholders
>                              ,stType :: Maybe ParameterizedStatementType
>                                -- | any catalog updates that a statement produces, used only for ddl Statements
>                              ,catUpd :: [CatalogUpdate]}
>                   deriving (Eq, Show,Typeable,Data)
>

some simple wrappers around uniplate for internal use. I'm not sure
which of these are actually used

> -- | An annotation value with no information.
> emptyAnnotation :: Annotation
> emptyAnnotation = Annotation Nothing Nothing [] Nothing Nothing []

> -- | get the annotation for the root element of the tree passed
> getAnnotation :: Data a => a -> Annotation
> getAnnotation = head . childrenBi

> -- | get all the annotations from a tree
> getAnnotations :: Data a => a -> [Annotation]
> getAnnotations = universeBi -- st --[x | x <- universeBi st]

> -- | update all the annotations in a tree
> updateAnnotations :: Data a => (Annotation -> Annotation) -> a -> a
> updateAnnotations = transformBi

> getTypeAnnotation :: Data a => a -> Maybe Type
> getTypeAnnotation = atype . getAnnotation

don't know how to do this one with uniplate

> -- | Update the first annotation in a tree using the function supplied
> updateAnnotation :: Data a => (Annotation -> Annotation) -> a -> a
> updateAnnotation f = gmapT (mkT f)
