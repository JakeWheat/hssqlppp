Copyright 2009 Jake Wheat

This is the public module to the annotation data types and support
functions (not including those that depend on the ast data types).

> {- | Contains the annotation data types and a few auxiliary functions.
> -}
>
> module Database.HsSqlPpp.Annotation
>     (
>      -- * Annotation data types
>      Annotation(..)
>     ,SourcePosition
>     ,StatementType
>     ,getAnnotation
>     ,updateAnnotations
>     ,updateAnnotation
>     ,getAnnotations
>     ,emptyAnnotation
>     ,resetAnnotations
>     ) where
>
> import Database.HsSqlPpp.AstInternals.AstAnnotation
> import Database.HsSqlPpp.AstInternals.AnnotationUtils
