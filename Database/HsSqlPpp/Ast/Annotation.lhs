Copyright 2009 Jake Wheat

This is the public module to the annotation data types and support
functions (not including those that depend on the ast data types).

> {- | Contains the annotation data types and a few auxiliary functions.
> -}

> module Database.HsSqlPpp.Ast.Annotation
>     (
>      -- * Annotation data types
>      Annotation
>     ,AnnotationElement(..)
>     ,StatementType(..)
>     ,stripAnnotations
>     ,updateAnnotation
>     ,getAnnotation
>     ,getAnnotations
>     ) where

> import Database.HsSqlPpp.AstInternals.AstAnnotation
