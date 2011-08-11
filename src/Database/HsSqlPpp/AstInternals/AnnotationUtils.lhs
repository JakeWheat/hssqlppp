
This module contains some utilities and generic code for working with
asts and annotations which depend on the ast types.

> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.AstInternals.AnnotationUtils
>     (
>      getStatementAnnotations
>     ,resetAnnotations
>     ) where
>
> import Data.Data
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.AstInternals.AstInternal
> import Database.HsSqlPpp.AstInternals.AstAnnotation
>
> -- | Run through the ast and return all the annotations attached to
> --   a Statement node.
> getStatementAnnotations :: Data a => a -> [Annotation]
> getStatementAnnotations st =
>     [getAnnotation s | (s::Statement) <- universeBi st]

> resetAnnotations :: Data a => a -> a
> resetAnnotations =
>     updateAnnotations (const emptyAnnotation)