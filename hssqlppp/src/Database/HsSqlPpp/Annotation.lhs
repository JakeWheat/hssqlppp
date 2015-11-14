
This is the public module to the annotation data types and support
functions (not including those that depend on the ast data types).

> {- | Contains the annotation data types and a few auxiliary functions.
> -}
>
> module Database.HsSqlPpp.Annotation
>     (
>      -- * Annotation data types
>      Annotation(..)
>     --,TypeExtra(..)
>     ,SourcePosition
>     --,ParameterizedStatementType
>     ,getAnnotation
>     --,updateAnnotations
>     ,updateAnnotation
>     --,getAnnotations
>     ,emptyAnnotation
>     --,resetAnnotations
>     ) where
>
> import Database.HsSqlPpp.Internals.AstInternal
> --import Database.HsSqlPpp.Internals.AnnotationUtils
