
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
>     ,ParameterizedStatementType
>     ,getAnnotation
>     --,updateAnnotations
>     ,updateAnnotation
>     --,getAnnotations
>     ,emptyAnnotation
>     --,resetAnnotations
>     ,atype
>     ,setAtype
>     ,errs
>     ,setErrs
>     ,asrc
>     ,setAsrc
>     ) where
>
> import Database.HsSqlPpp.Internals.AstInternal
> --import Database.HsSqlPpp.Internals.AnnotationUtils
