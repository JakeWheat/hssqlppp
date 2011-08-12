
This module contains some utilities and generic code for working with
asts and annotations which depend on the ast types.

> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Internals.AnnotationUtils
>     (
>      getStatementAnnotations
>     ,resetAnnotations
>     ) where
>
> import Data.Data
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Internals.AstInternal
> import Database.HsSqlPpp.Internals.AstAnnotation
>
> -- | Run through the ast and return all the annotations attached to
> --   a Statement node.
> getStatementAnnotations :: Data a => a -> [Annotation]
> getStatementAnnotations st =
>     [getAnnotation s | (s::Statement) <- universeBi st]

> -- | Set all the annotations in a tree to be 'emptyAnnotation'.
> resetAnnotations :: Data a => a -> a
> resetAnnotations =
>     updateAnnotations (const emptyAnnotation)