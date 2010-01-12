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
>      -- * Statement info
>      -- | This is the main annotation attached to each statement. Early days at the moment
>      -- but will be expanded to provide any type errors lurking inside a statement, any useful
>      -- types, e.g. the types of each select and subselect/sub query in a statement,
>      -- any changes to the catalog the statement makes, and possibly much more information.
>     ,StatementType(..)
>     ,stripAnnotations
>     ,updateAnnotation
>     ,getAnnotation
>     ) where

> import Database.HsSqlPpp.AstInternals.AstAnnotation
